mod builder;

pub(crate) use builder::ByteCodeBuilder;

use crate::script::CallError;
use crate::{ScriptIter, Variant};
use core::fmt::{self, Debug, Formatter};
use core::mem;
use rustc_hash::FxHashMap;
use std::collections::hash_map::Entry;

struct CallArgs {
	store_in: Option<u16>,
	func: Box<str>,
	args: Box<[u16]>,
}

enum Instruction {
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

	Add(u16, u16, u16),
	Sub(u16, u16, u16),
	Mul(u16, u16, u16),
	Div(u16, u16, u16),
	Rem(u16, u16, u16),
	And(u16, u16, u16),
	Or(u16, u16, u16),
	Xor(u16, u16, u16),
	Shl(u16, u16, u16),
	Shr(u16, u16, u16),

	LessEq(u16, u16, u16),
	Less(u16, u16, u16),
	Neq(u16, u16, u16),
	Eq(u16, u16, u16),

	Move(u16, u16),
}

#[derive(Debug)]
pub enum ByteCodeError {
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

macro_rules! run_op {
	($vars:ident, $r:ident = $a:ident $op:tt $b:ident) => {
		{
			let a = $vars.get(*$a as usize).ok_or(RunError::RegisterOutOfBounds)?;
			let b = $vars.get(*$b as usize).ok_or(RunError::RegisterOutOfBounds)?;
			let e = (a $op b).map_err(|e| RunError::CallError(Box::new(e)))?;
			*$vars.get_mut(*$r as usize).ok_or(RunError::RegisterOutOfBounds)? = e;
		}
	};
}

macro_rules! run_cmp {
	($vars:ident, $r:ident = $a:ident $op:tt $b:ident) => {
		{
			let a = $vars.get(*$a as usize).ok_or(RunError::RegisterOutOfBounds)?;
			let b = $vars.get(*$b as usize).ok_or(RunError::RegisterOutOfBounds)?;
			let e = Variant::Bool(a $op b);
			*$vars.get_mut(*$r as usize).ok_or(RunError::RegisterOutOfBounds)? = e;
		}
	};
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
					Add(r, a, b) => run_op!(vars, r = a + b),
					Sub(r, a, b) => run_op!(vars, r = a - b),
					Mul(r, a, b) => run_op!(vars, r = a * b),
					Div(r, a, b) => run_op!(vars, r = a / b),
					Rem(r, a, b) => run_op!(vars, r = a % b),
					And(r, a, b) => run_op!(vars, r = a & b),
					Or(r, a, b) => run_op!(vars, r = a | b),
					Xor(r, a, b) => run_op!(vars, r = a ^ b),
					Shl(r, a, b) => run_op!(vars, r = a << b),
					Shr(r, a, b) => run_op!(vars, r = a >> b),
					LessEq(r, a, b) => run_cmp!(vars, r = a <= b),
					Less(r, a, b) => run_cmp!(vars, r = a < b),
					Neq(r, a, b) => run_cmp!(vars, r = a != b),
					Eq(r, a, b) => run_cmp!(vars, r = a == b),
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
			And(r, a, b) => write!(f, "and     {}, {}, {}", r, a, b),
			Or(r, a, b) => write!(f, "or      {}, {}, {}", r, a, b),
			Xor(r, a, b) => write!(f, "xor     {}, {}, {}", r, a, b),
			Shl(r, a, b) => write!(f, "shl     {}, {}, {}", r, a, b),
			Shr(r, a, b) => write!(f, "shr     {}, {}, {}", r, a, b),

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
