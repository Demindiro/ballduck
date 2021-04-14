use crate::ast::{Atom, Expression, Function, Lines, Statement};
use crate::script::CallError;
use crate::tokenizer::Op;
use crate::{ScriptIter, ScriptObject, ScriptType, Variant};
use core::convert::TryInto;
use core::fmt::{self, Debug, Formatter};
use core::mem;
use rustc_hash::FxHashMap;
use std::collections::hash_map::Entry;
use std::rc::Rc;
use unwrap_none::UnwrapNone;

pub(crate) struct CallArgs {
	store_in: Option<u16>,
	func: Box<str>,
	args: Box<[u16]>,
}

pub(crate) enum Instruction {
	Call(Box<(u16, CallArgs)>),
	CallSelf(Box<CallArgs>),
	CallGlobal(Box<CallArgs>),

	Jmp(u32),
	JmpIf(u16, u32),
	RetSome,
	RetNone,

	IterConst(Box<(u16, u32, Box<dyn ScriptIter>)>),
	IterJmp(u16, u32),
	IterInt(u16, isize),

	/*
	AndJmp(u16, u16, u32),
	OrJmp(u16, u16, u32),
	Xor(u16, u16, u32),
	*/
	Add(u16, u16, u16),
	Sub(u16, u16, u16),
	Mul(u16, u16, u16),
	Div(u16, u16, u16),
	Rem(u16, u16, u16),

	LessEq(u16, u16, u16),
	Less(u16, u16, u16),
	Neq(u16, u16, u16),
	Eq(u16, u16, u16),

	Move(u16, u16),
}

#[derive(Debug)]
pub enum ByteCodeError {
	DuplicateLocal,
	DuplicateParameter,
	UndefinedVariable,
}

#[derive(Debug)]
pub(crate) struct ByteCode {
	code: Vec<Instruction>,
	param_count: u16,
	var_count: u16,
	consts: Vec<Variant>,
}

#[derive(Debug)]
pub enum RunError {
	IpOutOfBounds,
	RegisterOutOfBounds,
	NoIterator,
	EnvironmentError(EnvironmentError),
	UndefinedFunction,
	CallError(Box<CallError>),
	IncorrectArgumentCount,
	IncompatibleType,
}

pub struct Environment {
	functions: FxHashMap<Box<str>, EnvironmentFunction>,
}

pub type EnvironmentFunction = Box<dyn Fn(&[Variant]) -> CallResult<RunError>>;
pub type CallResult<E = CallError> = Result<Variant, E>;

#[derive(Debug)]
pub enum EnvironmentError {
	FunctionAlreadyExists,
	UndefinedFunction,
}

/// The parser
impl ByteCode {
	pub(crate) fn parse(
		function: Function,
		methods: &FxHashMap<&str, ()>,
		locals: &FxHashMap<Box<str>, u16>,
	) -> Result<Self, ByteCodeError> {
		let mut instr = Vec::new();
		let mut vars = FxHashMap::with_hasher(Default::default());
		let mut consts = Vec::new();
		let param_count = function.parameters.len() as u16;
		for p in function.parameters {
			if vars.insert(p, vars.len() as u16).is_some() {
				return Err(ByteCodeError::DuplicateParameter);
			}
		}
		let mut var_count = vars.len() as u16;
		let vars = Self::parse_block(
			&function.lines,
			methods,
			locals,
			&mut instr,
			&mut vars,
			&mut consts,
			&mut var_count,
			0,
		)?;
		match instr.last() {
			Some(Instruction::RetSome) | Some(Instruction::RetNone) => (),
			_ => instr.push(Instruction::RetNone),
		}

		if consts.len() > 0 {
			// All consts are using the upper-most registers, move them downwards
			let offset = (u16::MAX - consts.len() as u16).wrapping_add(1);
			for i in instr.iter_mut() {
				use Instruction::*;
				let conv = |c: &mut u16| {
					if *c >= offset {
						*c = u16::MAX - *c + vars
					}
				};
				match i {
					Call(box (_, ca)) | CallSelf(box ca) | CallGlobal(box ca) => {
						for a in ca.args.iter_mut() {
							conv(a);
						}
					}
					Move(_, a) | JmpIf(a, _) => conv(a),
					Add(_, a, b)
					| Sub(_, a, b)
					| Mul(_, a, b)
					| Div(_, a, b)
					| Rem(_, a, b)
					| Eq(_, a, b)
					| Neq(_, a, b)
					| Less(_, a, b)
					| LessEq(_, a, b) => {
						conv(a);
						conv(b);
					}
					IterConst(_) | IterInt(_, _) | IterJmp(_, _) | Jmp(_) | RetSome | RetNone => (),
				}
			}
		}

		Ok(Self {
			code: instr,
			var_count: vars,
			param_count,
			consts,
		})
	}

	fn parse_block<'a>(
		lines: &Lines<'a>,
		methods: &FxHashMap<&str, ()>,
		locals: &FxHashMap<Box<str>, u16>,
		instr: &mut Vec<Instruction>,
		vars: &mut FxHashMap<&'a str, u16>,
		consts: &mut Vec<Variant>,
		curr_var_count: &mut u16,
		mut min_var_count: u16,
	) -> Result<u16, ByteCodeError> {
		let mut frame_vars = Vec::new();
		for line in lines {
			match line {
				Statement::Expression { expr } => {
					Self::parse_expression(
						None,
						expr,
						locals,
						methods,
						instr,
						vars,
						consts,
						&mut min_var_count,
						curr_var_count,
					)?;
				}
				Statement::For { var, expr, lines } => {
					let og_cvc = *curr_var_count;
					let reg = vars.len().try_into().expect("Too many variables");
					vars.insert(var, reg).unwrap_none();
					frame_vars.push(var);
					*curr_var_count += 1;
					match expr {
						Expression::Atom(a) => {
							instr.push(match a {
								Atom::String(a) => Instruction::IterConst(Box::new((
									reg,
									u32::MAX,
									Box::new(a.to_string().into_boxed_str()),
								))),
								Atom::Integer(a) => Instruction::IterInt(reg, *a),
								//Atom::Real(a) => Instruction::IterConst(Box::new(a)),
								Atom::Real(a) => todo!("for Real({})", a),
								Atom::Name(a) => todo!("for {:?}", a),
							})
						}
						_ => todo!(),
					}
					let ic = instr.len() - 1;
					let ip = instr.len() as u32;
					min_var_count = Self::parse_block(
						lines,
						methods,
						locals,
						instr,
						vars,
						consts,
						curr_var_count,
						min_var_count,
					)?;
					instr.push(Instruction::IterJmp(reg, ip));
					let ip = instr.len() as u32;
					match instr.get_mut(ic) {
						Some(Instruction::IterConst(ic)) => ic.1 = ip,
						Some(Instruction::IterInt(..)) => (),
						_ => unreachable!(),
					}
					*curr_var_count = og_cvc;
				}
				Statement::If { expr, lines, else_lines } => {
					// If
					let expr = Self::parse_expression(
						Some(*curr_var_count),
						expr,
						locals,
						methods,
						instr,
						vars,
						consts,
						&mut min_var_count,
						curr_var_count,
					)?;
					let expr = if let Some(expr) = expr {
						expr
					} else {
						*curr_var_count += 1;
						*curr_var_count - 1
					};
					instr.push(Instruction::JmpIf(expr, u32::MAX));
					let ic = instr.len() - 1;
					min_var_count = Self::parse_block(
						lines,
						methods,
						locals,
						instr,
						vars,
						consts,
						curr_var_count,
						min_var_count,
					)?;
					// Skip else
					let skip_else_jmp = else_lines.as_ref().map(|_| {
						instr.push(Instruction::Jmp(u32::MAX));
						instr.len() - 1
					});
					// Jump to right after `if` block if `expr` evaluates to false
					let ip = instr.len() as u32;
					match instr.get_mut(ic) {
						Some(Instruction::JmpIf(_, ic)) => *ic = ip,
						_ => unreachable!(),
					}
					// Else
					if let Some(else_lines) = else_lines {
						min_var_count = Self::parse_block(
							else_lines,
							methods,
							locals,
							instr,
							vars,
							consts,
							curr_var_count,
							min_var_count,
						)?;
						let ip = instr.len() as u32;
						match instr.get_mut(skip_else_jmp.unwrap()) {
							Some(Instruction::Jmp(ic)) => *ic = ip,
							_ => unreachable!(),
						}
					}
				}
				Statement::Return { expr } => {
					if let Some(expr) = expr {
						let r = Self::parse_expression(
							Some(0),
							expr,
							locals,
							methods,
							instr,
							vars,
							consts,
							&mut min_var_count,
							&mut (vars.len() as u16),
						)?;
						if let Some(r) = r {
							instr.push(Instruction::Move(0, r));
						}
						instr.push(Instruction::RetSome);
					} else {
						instr.push(Instruction::RetNone);
					}
				}
				_ => todo!("{:#?}", line),
			}
		}
		min_var_count = min_var_count.max(vars.len() as u16);
		for fv in frame_vars {
			vars.remove(fv).unwrap();
		}
		Ok(min_var_count)
	}

	fn parse_expression(
		store: Option<u16>,
		expr: &Expression,
		locals: &FxHashMap<Box<str>, u16>,
		methods: &FxHashMap<&str, ()>,
		instr: &mut Vec<Instruction>,
		vars: &FxHashMap<&str, u16>,
		consts: &mut Vec<Variant>,
		min_var_count: &mut u16,
		curr_var_count: &mut u16,
	) -> Result<Option<u16>, ByteCodeError> {
		let mut add_const = |v| {
			consts.push(v);
			u16::MAX - consts.len() as u16 + 1
		};
		match expr {
			Expression::Operation { left, op, right } => {
				let store = store.expect("TODO: handle operations without store location");
				let og_cvc = *curr_var_count;
				let (r_left, r_right) = (*curr_var_count, *curr_var_count + 1);
				*curr_var_count += 2;
				*min_var_count = *min_var_count.max(curr_var_count);
				let or_left = Self::parse_expression(
					Some(r_left),
					left,
					locals,
					methods,
					instr,
					vars,
					consts,
					min_var_count,
					curr_var_count,
				)?;
				let left = if let Some(l) = or_left {
					l
				} else {
					*curr_var_count -= 1;
					r_left
				};
				let or_right = Self::parse_expression(
					Some(r_right),
					right,
					locals,
					methods,
					instr,
					vars,
					consts,
					min_var_count,
					curr_var_count,
				)?;
				let right = if let Some(r) = or_right { r } else { r_right };
				instr.push(match op {
					Op::Add => Instruction::Add(store, left, right),
					Op::Sub => Instruction::Sub(store, left, right),
					Op::Mul => Instruction::Mul(store, left, right),
					Op::Div => Instruction::Div(store, left, right),
					Op::Rem => Instruction::Rem(store, left, right),
					Op::Eq => Instruction::Eq(store, left, right),
					Op::Neq => Instruction::Neq(store, left, right),
					Op::Less => Instruction::Less(store, left, right),
					Op::Greater => Instruction::Less(store, right, left),
					Op::LessEq => Instruction::LessEq(store, left, right),
					Op::GreaterEq => Instruction::LessEq(store, right, left),
					o => todo!("{:?}", o),
				});
				*curr_var_count = og_cvc;
				Ok(None)
			}
			Expression::Atom(a) => match *a {
				Atom::Name(name) => {
					if let Some(&reg) = vars.get(name).or_else(|| locals.get(name)) {
						Ok(Some(reg))
					} else {
						return Err(ByteCodeError::UndefinedVariable);
					}
				}
				Atom::Real(r) => Ok(Some(add_const(Variant::Real(r)))),
				Atom::Integer(i) => Ok(Some(add_const(Variant::Integer(i)))),
				Atom::String(s) => Ok(Some(add_const(Variant::Object(Rc::new(s.to_string().into_boxed_str()))))),
			},
			Expression::Function {
				expr,
				name,
				arguments,
			} => {
				let og_cvc = *curr_var_count;
				let expr = if let Some(expr) = expr {
					let r = *curr_var_count;
					*curr_var_count += 1;
					let e = Self::parse_expression(
						Some(r),
						expr,
						locals,
						methods,
						instr,
						vars,
						consts,
						min_var_count,
						curr_var_count,
					)?;
					Some(if let Some(e) = e { e } else { r })
				} else {
					None
				};
				let mut args = Vec::with_capacity(arguments.len());
				for a in arguments {
					let r = *curr_var_count;
					*curr_var_count += 1;
					let e = Self::parse_expression(
						Some(r),
						a,
						locals,
						methods,
						instr,
						vars,
						consts,
						min_var_count,
						curr_var_count,
					)?;
					if let Some(e) = e {
						args.push(e);
						*curr_var_count -= 1;
					} else {
						args.push(r);
					}
				}
				let ca = CallArgs {
					store_in: store,
					func: (*name).into(),
					args: args.into_boxed_slice(),
				};
				instr.push(if let Some(expr) = expr {
					Instruction::Call(Box::new((expr, ca)))
				} else if methods.get(name).is_some() {
					Instruction::CallSelf(Box::new(ca))
				} else {
					Instruction::CallGlobal(Box::new(ca))
				});
				*curr_var_count = og_cvc;
				Ok(None)
			}
			e => todo!("{:#?}", e),
		}
	}
}

/// The interpreter
impl ByteCode {
	pub(crate) fn run(
		&self,
		functions: &FxHashMap<Box<str>, Self>,
		locals: &mut [Variant],
		args: &[Variant],
		env: &Environment,
	) -> CallResult<RunError> {
		if args.len() != self.param_count as usize {
			return Err(RunError::IncorrectArgumentCount);
		}
		let mut vars = Vec::with_capacity(self.var_count as usize + self.consts.len());
		for a in args.iter() {
			vars.push(a.clone());
		}
		vars.resize(self.var_count as usize, Variant::default());
		vars.extend(self.consts.iter().cloned());
		let mut ip = 0;
		let mut iterators = Vec::new();
		let mut call_args = Vec::new();
		loop {
			let err_roob = || RunError::RegisterOutOfBounds;
			let err_uf = || RunError::UndefinedFunction;
			let err_env = |e| RunError::EnvironmentError(e);
			let err_call = |e| RunError::CallError(Box::new(e));
			if let Some(instr) = self.code.get(ip as usize) {
				ip += 1;
				use Instruction::*;
				match instr {
					Call(box (
						reg,
						CallArgs {
							store_in,
							func,
							args,
						},
					)) => {
						for &a in args.iter() {
							call_args.push(vars.get(a as usize).ok_or(err_roob())?.clone());
						}
						let obj = vars.get(*reg as usize).ok_or(err_roob())?;
						let r = obj.call(func, &call_args[..]).map_err(err_call)?;
						call_args.clear();
						if let Some(reg) = store_in {
							*vars.get_mut(*reg as usize).ok_or(err_roob())? = r;
						}
					}
					CallGlobal(box CallArgs {
						store_in,
						func,
						args,
					}) => {
						for &a in args.iter() {
							call_args.push(vars.get(a as usize).ok_or(err_roob())?.clone());
						}
						let r = env.call(func, &call_args[..]).map_err(err_env)?;
						call_args.clear();
						if let Some(reg) = store_in {
							*vars.get_mut(*reg as usize).ok_or(err_roob())? = r;
						}
					}
					CallSelf(box CallArgs {
						store_in,
						func,
						args,
					}) => {
						for &a in args.iter() {
							call_args.push(vars.get(a as usize).ok_or(err_roob())?.clone());
						}
						let r = functions.get(func).ok_or(err_uf())?;
						let r = r.run(functions, locals, &call_args[..], env)?;
						call_args.clear();
						// SAFETY: ditto
						if let Some(reg) = store_in {
							*vars.get_mut(*reg as usize).ok_or(err_roob())? = r;
						}
					}
					RetSome => break Ok(vars.first().ok_or(err_roob())?.clone()),
					RetNone => break Ok(Variant::None),
					IterConst(box (reg, jmp_ip, iter)) => {
						let mut iter = iter.iter();
						if let Some(e) = iter.next() {
							*vars.get_mut(*reg as usize).ok_or(err_roob())? = e;
							iterators.push(iter);
						} else {
							ip = *jmp_ip;
						}
					}
					IterInt(reg, i) => {
						let mut iter = if *i < 0 {
							Box::new(((1 - i)..=0).rev().map(Variant::Integer))
								as Box<dyn Iterator<Item = Variant>>
						} else {
							Box::new((0..*i).map(Variant::Integer))
								as Box<dyn Iterator<Item = Variant>>
						};
						*vars.get_mut(*reg as usize).ok_or(err_roob())? = iter.next().unwrap();
						iterators.push(iter);
					}
					IterJmp(reg, jmp_ip) => {
						if let Some(iter) = iterators.last_mut() {
							if let Some(e) = iter.next() {
								*vars.get_mut(*reg as usize).ok_or(err_roob())? = e;
								ip = *jmp_ip;
							}
						} else {
							return Err(RunError::NoIterator);
						}
					}
					JmpIf(reg, jmp_ip) => {
						if let Variant::Bool(b) = vars.get(*reg as usize).ok_or(err_roob())? {
							if !b {
								ip = *jmp_ip;
							}
						} else {
							return Err(RunError::NoIterator);
						}
					}
					Jmp(jmp_ip) => ip = *jmp_ip,
					Move(r, a) => {
						let e = mem::take(vars.get_mut(*a as usize).ok_or(err_roob())?);
						*vars.get_mut(*r as usize).ok_or(err_roob())? = e;
					}
					Add(r, a, b) => {
						let a = vars.get(*a as usize).ok_or(err_roob())?;
						let b = vars.get(*b as usize).ok_or(err_roob())?;
						let e = (a + b).map_err(err_call)?;
						*vars.get_mut(*r as usize).ok_or(err_roob())? = e;
					}
					Sub(r, a, b) => {
						let a = vars.get(*a as usize).ok_or(err_roob())?;
						let b = vars.get(*b as usize).ok_or(err_roob())?;
						let e = (a - b).map_err(err_call)?;
						*vars.get_mut(*r as usize).ok_or(err_roob())? = e;
					}
					Mul(r, a, b) => {
						let a = vars.get(*a as usize).ok_or(err_roob())?;
						let b = vars.get(*b as usize).ok_or(err_roob())?;
						let e = (a * b).map_err(err_call)?;
						*vars.get_mut(*r as usize).ok_or(err_roob())? = e;
					}
					Div(r, a, b) => {
						let a = vars.get(*a as usize).ok_or(err_roob())?;
						let b = vars.get(*b as usize).ok_or(err_roob())?;
						let e = (a / b).map_err(err_call)?;
						*vars.get_mut(*r as usize).ok_or(err_roob())? = e;
					}
					Rem(r, a, b) => {
						let a = vars.get(*a as usize).ok_or(err_roob())?;
						let b = vars.get(*b as usize).ok_or(err_roob())?;
						let e = (a % b).map_err(err_call)?;
						*vars.get_mut(*r as usize).ok_or(err_roob())? = e;
					}
					LessEq(r, a, b) => {
						let a = vars.get(*a as usize).ok_or(err_roob())?;
						let b = vars.get(*b as usize).ok_or(err_roob())?;
						let e = Variant::Bool(a <= b);
						*vars.get_mut(*r as usize).ok_or(err_roob())? = e;
					}
					Less(r, a, b) => {
						let a = vars.get(*a as usize).ok_or(err_roob())?;
						let b = vars.get(*b as usize).ok_or(err_roob())?;
						let e = Variant::Bool(a < b);
						*vars.get_mut(*r as usize).ok_or(err_roob())? = e;
					}
					Neq(r, a, b) => {
						let a = vars.get(*a as usize).ok_or(err_roob())?;
						let b = vars.get(*b as usize).ok_or(err_roob())?;
						let e = Variant::Bool(a != b);
						*vars.get_mut(*r as usize).ok_or(err_roob())? = e;
					}
					Eq(r, a, b) => {
						let a = vars.get(*a as usize).ok_or(err_roob())?;
						let b = vars.get(*b as usize).ok_or(err_roob())?;
						let e = Variant::Bool(a == b);
						*vars.get_mut(*r as usize).ok_or(err_roob())? = e;
					}
					_ => todo!("{:?}", instr),
				}
			} else {
				break Err(RunError::IpOutOfBounds);
			}
		}
	}
}

impl Environment {
	pub fn new() -> Self {
		Self {
			functions: FxHashMap::with_hasher(Default::default()),
		}
	}

	pub fn add_function(
		&mut self,
		name: String,
		f: EnvironmentFunction,
	) -> Result<(), EnvironmentError> {
		match self.functions.entry(name.into_boxed_str()) {
			Entry::Vacant(e) => {
				e.insert(f);
				Ok(())
			}
			Entry::Occupied(_) => Err(EnvironmentError::FunctionAlreadyExists),
		}
	}

	pub fn call(&self, func: &str, args: &[Variant]) -> CallResult<EnvironmentError> {
		Ok(self
			.functions
			.get(func)
			.ok_or(EnvironmentError::UndefinedFunction)?(args)
		.unwrap())
	}
}

/// This returns each instruction on oneline instead of 5+ with the default Debug
impl Debug for Instruction {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		use Instruction::*;
		match self {
			Call(box (r, a)) => write!(f, "call    {}, {:?}", r, a),
			CallSelf(a) => write!(f, "call    self, {:?}", a),
			CallGlobal(a) => write!(f, "call    env, {:?}", a),
			Move(a, b) => write!(f, "move    {:?}, {:?}", a, b),
			RetSome => write!(f, "ret     0"),
			RetNone => write!(f, "ret     none"),

			IterConst(box (r, p, i)) => write!(f, "iter    {}, {}, {:?}", r, p, i),
			IterInt(r, i) => write!(f, "iter    {}, {}", r, i),
			IterJmp(r, p) => write!(f, "iterjmp {}, {}", r, p),
			JmpIf(r, p) => write!(f, "jmpif   {}, {}", r, p),
			Jmp(p) => write!(f, "jmp     {}", p),

			Add(r, a, b) => write!(f, "add     {}, {}, {}", r, a, b),
			Sub(r, a, b) => write!(f, "sub     {}, {}, {}", r, a, b),
			Mul(r, a, b) => write!(f, "mul     {}, {}, {}", r, a, b),
			Div(r, a, b) => write!(f, "div     {}, {}, {}", r, a, b),
			Rem(r, a, b) => write!(f, "rem     {}, {}, {}", r, a, b),

			Eq(r, a, b) => write!(f, "eq      {}, {}, {}", r, a, b),
			Neq(r, a, b) => write!(f, "neq     {}, {}, {}", r, a, b),
			Less(r, a, b) => write!(f, "less    {}, {}, {}", r, a, b),
			LessEq(r, a, b) => write!(f, "lesseq  {}, {}, {}", r, a, b),
		}
	}
}

impl Debug for CallArgs {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		if let Some(n) = self.store_in {
			write!(f, "{}, \"{}\", {:?}", n, self.func, self.args)
		} else {
			write!(f, "none, \"{}\", {:?}", self.func, self.args)
		}
	}
}
