// Copyright (C) 2021  David Hoppenbrouwers
//
// This file is licensed under the MIT license. See LICENSE for details.

mod builder;
mod tracer;

pub(crate) use builder::{ByteCodeBuilder, ByteCodeError};
pub use tracer::Tracer;

use crate::std_types::*;
use crate::{Array, Dictionary, Environment, ScriptObject, VariantType};
use core::fmt::{self, Debug, Formatter};
use core::intrinsics::unlikely;
use core::mem;
use std::error::Error;
use tracer::*;

pub struct CallArgs {
	store_in: Option<u16>,
	func: Rc<str>,
	args: Box<[u16]>,
}

// Check to ensure the size doesn't go over a certain limit
const _INSTR_SIZE_CHECK: usize = 16 - mem::size_of::<Instruction>();

pub enum Instruction {
	Call(u16, Box<CallArgs>),
	CallSelf {
		store_in: Option<u16>,
		func: u16,
		args: Box<[u16; 16]>,
	},
	CallEnv {
		args: Box<CallArgs>,
	},

	Jmp(*const Instruction),
	JmpIf(u16, *const Instruction),
	JmpNotIf(u16, *const Instruction),
	RetSome(u16),
	RetNone,

	Iter(u16, u16, *const Instruction),
	IterJmp(u16, *const Instruction),
	IterInt {
		reg: u8,
		from: u16,
		to: u16,
		step: u16,
		jmp_ip: *const Instruction,
	},
	IterIntJmp(u16, *const Instruction),
	Break {
		amount: u8,
		amount_int: u8,
		jmp_ip: *const Instruction,
	},

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
	Not(u16, u16),
	Neg(u16, u16),

	LessEq(u16, u16, u16),
	Less(u16, u16, u16),
	Neq(u16, u16, u16),
	Eq(u16, u16, u16),

	Store(u16, u16),
	Load(u16, u16),
	Move(u16, u16),
	CopySelf {
		dest: u16,
	},

	NewArray(u16, usize),
	NewDictionary(u16, usize),
	GetIndex(u16, u16, u16),
	SetIndex(u16, u16, u16),
}

pub struct ByteCode<V>
where
	V: VariantType,
{
	code: Box<[Instruction]>,
	param_count: u16,
	var_count: u16,
	consts: Vec<V>,
	name: Rc<str>,
}

pub struct RunState<'a, V>
where
	V: VariantType,
{
	vars: &'a mut [V],
}

pub type CallResult<T> = Result<T, Box<dyn Error>>;

struct IterIntState {
	current: isize,
	step: isize,
	stop: isize,
}

macro_rules! try_break {
	(box $e:expr) => {
		match $e {
			Ok(v) => v,
			Err(e) => break Err(Box::new(e)),
		}
	};
	($e:expr) => {
		match $e {
			Ok(v) => v,
			Err(e) => break Err(e),
		}
	};
}

macro_rules! reg {
	(ref $vars:ident $reg:ident) => {
		if let Some(v) = $vars.get(*$reg as usize) {
			v
		} else {
			break Err(Box::new(err::RegisterOutOfBounds));
		}
	};
	(mut $vars:ident $reg:ident) => {
		*reg!(ref mut $vars $reg)
	};
	(ref mut $vars:ident $reg:expr) => {
		if let Some(v) = $vars.get_mut(*$reg as usize) {
			v
		} else {
			break Err(Box::new(err::RegisterOutOfBounds));
		}
	};
	($life:tt ref $vars:ident $reg:ident) => {
		if let Some(v) = $vars.get_mut(*$reg as usize) {
			v
		} else {
			break $life Err(Box::new(err::RegisterOutOfBounds));
		}
	};
}

macro_rules! run_op {
	($vars:ident, $r:ident = $a:ident $op:tt $b:ident) => {
		{
			// This allows the compiler to do the 3 registers checks in one go
			if unlikely(*$r as usize >= $vars.len()) {
				break Err(Box::new(err::RegisterOutOfBounds));
			}
			reg!(mut $vars $r) = try_break!(reg!(ref $vars $a).$op(reg!(ref $vars $b)));
		}
	};
	($vars:ident, $r:ident = $a:ident $op:tt) => {
		{
			if unlikely(*$r as usize >= $vars.len()) {
				break Err(Box::new(err::RegisterOutOfBounds));
			}
			reg!(mut $vars $r) = try_break!(reg!(ref $vars $a).$op());
		}
	};
}

macro_rules! run_cmp {
	($vars:ident, $r:ident = $a:ident $op:tt $b:ident) => {
		reg!(mut $vars $r) = (reg!(ref $vars $a) $op reg!(ref $vars $b)).into();
	};
}

impl<V> ByteCode<V>
where
	V: VariantType,
{
	pub(crate) fn run<T>(
		&self,
		object: &ScriptObject<V>,
		functions: &[Self],
		locals: &mut [V],
		args: &[&V],
		env: &Environment<V>,
		tracer: &T,
	) -> Result<V, Box<dyn std::error::Error>>
	where
		T: Tracer<V>,
	{
		if args.len() != self.param_count as usize {
			return Err(err::arg_count());
		}

		let vars_len = self.var_count as usize + self.consts.len();
		let mut vars = Vec::with_capacity(vars_len);
		// Extend is terribly slow, hence manual iteration
		for &a in args.iter() {
			vars.push(a.clone());
		}
		vars.resize_with(self.var_count as usize, V::default);
		for c in self.consts.iter() {
			vars.push(c.clone());
		}

		let mut call_args = [core::ptr::null(); 16];

		let mut iterators = Vec::new();
		let mut iterators_int = Vec::new();

		self.run_loop(
			object,
			functions,
			locals,
			env,
			tracer,
			&mut vars,
			0,
			&mut iterators,
			&mut iterators_int,
			&mut call_args,
		)
	}

	fn run_loop<T>(
		&self,
		object: &ScriptObject<V>,
		functions: &[Self],
		locals: &mut [V],
		env: &Environment<V>,
		tracer: &T,
		vec_vars: &mut Vec<V>,
		vars_offset: usize,
		iterators: &mut Vec<Box<dyn Iterator<Item = V>>>,
		iterators_int: &mut Vec<IterIntState>,
		call_args: &mut [*const V; 16],
	) -> Result<V, Box<dyn std::error::Error>>
	where
		T: Tracer<V>,
	{
		let _trace_run = TraceRun::new(tracer, self);

		let mut curr_instr = self.code.as_ptr();

		let vars_len = self.var_count as usize + self.consts.len();
		let vars_offset_len = vars_offset + vars_len;
		// Adding vars_len speeds things up because idk
		let mut vars = &mut vec_vars[vars_offset..vars_offset_len];

		let ret = 'lp: loop {
			let pc = unsafe { curr_instr.offset_from(self.code.as_ptr()) } as u32;
			let instr = unsafe { curr_instr.as_ref().unwrap_unchecked() };
			{
				let _trace_instruction = TraceInstruction::new(tracer, self, pc, instr);
				{
					let mut vars = RunState { vars };
					tracer.peek(self, &mut vars);
				}
				curr_instr = unsafe { curr_instr.offset(1) };
				use Instruction::*;
				match instr {
					Call(
						reg,
						box CallArgs {
							store_in,
							func,
							args,
						},
					) => {
						// Set arguments
						if unlikely(call_args.len() < args.len()) {
							break Err(err::arg_oob());
						}
						for (i, a) in args.iter().enumerate() {
							call_args[i] = reg!('lp ref vars a) as *const V;
						}
						// SAFETY: All the pointers are valid references.
						let ca: &[&V] =
							unsafe { &*(&call_args[..args.len()] as *const _ as *const _) };

						// Perform call
						let obj = reg!(ref vars reg);
						let trace_call = TraceCall::new(tracer, self, func);
						let r = match obj.call(func, ca, env) {
							Ok(r) => r,
							Err(e) => break Err(e),
						};
						mem::drop(trace_call);

						// Store return value
						if let Some(reg) = store_in {
							reg!(mut vars reg) = r;
						}
					}
					CallEnv {
						args: box CallArgs {
							store_in,
							func,
							args,
						},
					} => {
						// Set arguments
						if unlikely(call_args.len() < args.len()) {
							break Err(err::arg_oob());
						}
						for (i, a) in args.iter().enumerate() {
							call_args[i] = reg!('lp ref vars a) as *const V;
						}
						// SAFETY: All the pointers are valid references.
						let ca: &[&V] =
							unsafe { &*(&call_args[..args.len()] as *const _ as *const _) };

						// Perform call
						let trace_call = TraceCall::new(tracer, self, func);
						let r = match env.call(func, ca) {
							Ok(r) => r,
							Err(e) => break Err(e),
						};
						mem::drop(trace_call);

						// Store return value
						if let Some(reg) = store_in {
							reg!(mut vars reg) = r;
						}
					}
					CallSelf {
						store_in,
						func,
						args,
					} => {
						// Perform call
						let r = try_break!(box functions
							.get(*func as usize)
							.ok_or(err::UndefinedFunction));

						drop(vars);

						// Resize variable stack
						let cvl = vars_offset_len + r.var_count as usize;
						let cvol = cvl + r.consts.len();
						// TODO figure out if it's possible to get the compiler to (partially)
						// inline the resize_with function.
						if vec_vars.len() < cvol {
							vec_vars.resize_with(cvol, V::default);
						}

						// Limiting arg_count allows the compiler to optimize out the bounds
						// checks without risking UB with get_unchecked.
						debug_assert!((r.param_count as usize) < args.len(), "Too many parameters");
						let arg_count = r.param_count as usize % args.len();
						for (i, &a) in args.iter().enumerate() {
							// Manual break is faster than `take()`
							if i >= arg_count {
								break;
							}
							let a = &(vars_offset + a as usize);
							vec_vars[vars_offset_len + i] = reg!('lp ref vec_vars a).clone();
						}
						for (i, c) in r.consts.iter().enumerate() {
							vec_vars[cvl + i] = c.clone();
						}

						let trace_call = TraceSelfCall::new(tracer, self, *func);
						let r = try_break!(r.run_loop(
							object,
							functions,
							locals,
							env,
							tracer,
							vec_vars,
							vars_offset_len,
							iterators,
							iterators_int,
							call_args,
						));
						mem::drop(trace_call);

						vars = &mut vec_vars[vars_offset..vars_offset_len];

						// Store return value
						if let Some(reg) = store_in {
							reg!(mut vars reg) = r;
						}
					}
					RetSome(reg) => break Ok(mem::take(reg!(ref mut vars reg))),
					RetNone => break Ok(V::default()),
					Iter(reg, iter, jmp_ip) => {
						let iter = reg!(ref vars iter);
						let mut iter = try_break!(iter.iter());
						if let Some(e) = iter.next() {
							reg!(mut vars reg) = e;
							iterators.push(iter);
						} else {
							curr_instr = *jmp_ip;
						}
					}
					IterJmp(reg, jmp_ip) => {
						let iter = try_break!(box iterators.last_mut().ok_or(err::NoIterator));
						if let Some(e) = iter.next() {
							reg!(mut vars reg) = e;
							curr_instr = *jmp_ip;
						} else {
							let _ = iterators.pop().unwrap();
						}
					}
					Break {
						amount,
						amount_int,
						jmp_ip,
					} => {
						for _ in 0..*amount {
							if unlikely(iterators.pop().is_none()) {
								return Err(Box::new(err::NoIterator));
							}
						}
						for _ in 0..*amount_int {
							if unlikely(iterators_int.pop().is_none()) {
								return Err(Box::new(err::NoIterator));
							}
						}
						curr_instr = *jmp_ip;
					}
					IterInt {
						reg,
						from,
						to,
						step,
						jmp_ip,
					} => {
						let from = reg!(ref vars from);
						let to = reg!(ref vars to);
						let step = reg!(ref vars step);
						let from = from.as_integer().ok();
						let to = to.as_integer().ok();
						let step = step.as_integer().ok();
						let fts = from.and_then(|f| to.and_then(|t| step.map(|s| (f, t, s))));
						let (from, to, step) = try_break!(box fts.ok_or(err::IncompatibleType));
						if from != to {
							reg!(mut vars reg) = V::new_integer(from);
							iterators_int.push(IterIntState {
								current: from,
								stop: to,
								step,
							});
						} else {
							curr_instr = *jmp_ip;
						}
					}
					IterIntJmp(reg, jmp_ip) => {
						let iter = try_break!(box iterators_int.last_mut().ok_or(err::NoIterator));
						iter.current += iter.step;
						// >= is preferred over > so that step == 0 loops forever
						if (iter.step >= 0 && iter.current < iter.stop)
							|| (iter.step < 0 && iter.current > iter.stop)
						{
							reg!(mut vars reg) = V::new_integer(iter.current);
							curr_instr = *jmp_ip;
						} else {
							let _ = iterators_int.pop().unwrap();
						}
					}
					JmpIf(reg, jmp_ip) => {
						if let Ok(b) = reg!(ref vars reg).as_bool() {
							if !b {
								curr_instr = *jmp_ip;
							}
						} else {
							break Err(Box::new(err::NotBoolean));
						}
					}
					JmpNotIf(reg, jmp_ip) => {
						if let Ok(b) = reg!(ref vars reg).as_bool() {
							if b {
								curr_instr = *jmp_ip;
							}
						} else {
							break Err(Box::new(err::NotBoolean));
						}
					}
					Jmp(jmp_ip) => curr_instr = *jmp_ip,
					Add(r, a, b) => run_op!(vars, r = a add b),
					Sub(r, a, b) => run_op!(vars, r = a sub b),
					Mul(r, a, b) => run_op!(vars, r = a mul b),
					Div(r, a, b) => run_op!(vars, r = a div b),
					Rem(r, a, b) => run_op!(vars, r = a rem b),
					And(r, a, b) => run_op!(vars, r = a bitand b),
					Or(r, a, b) => run_op!(vars, r = a bitor b),
					Xor(r, a, b) => run_op!(vars, r = a bitxor b),
					Shl(r, a, b) => run_op!(vars, r = a lhs b),
					Shr(r, a, b) => run_op!(vars, r = a rhs b),
					LessEq(r, a, b) => run_cmp!(vars, r = a <= b),
					Less(r, a, b) => run_cmp!(vars, r = a < b),
					Neq(r, a, b) => run_cmp!(vars, r = a != b),
					Eq(r, a, b) => run_cmp!(vars, r = a == b),
					Neg(r, a) => run_op!(vars, r = a neg),
					Not(r, a) => run_op!(vars, r = a not),
					Store(r, l) => {
						let l = try_break!(locals.get_mut(*l as usize).ok_or_else(err::loob));
						*l = reg!(ref vars r).clone();
					}
					Load(r, l) => {
						let v = try_break!(locals.get(*l as usize).ok_or_else(err::loob));
						reg!(mut vars r) = v.clone();
					}
					Move(d, s) => reg!(mut vars d) = reg!(ref vars s).clone(),
					CopySelf { dest } => reg!(mut vars dest) = V::new_object(object.clone()),
					NewArray(r, c) => {
						reg!(mut vars r) = V::new_object(ScriptObject(Rc::new(Array::with_len(*c))))
					}
					NewDictionary(r, c) => {
						let d = Rc::new(Dictionary::with_capacity(*c));
						reg!(mut vars r) = V::new_object(ScriptObject(d));
					}
					// FIXME using try_break for these last two statements makes everything slower,
					// presumably because the damn optimizer thinks inserting jmp instructions
					// everywhere is always a good idea if it means a few bytes less of code.
					GetIndex(r, o, i) => {
						reg!(mut vars r) = reg!(ref vars o).index(reg!(ref vars i))?
					}
					SetIndex(r, o, i) => {
						reg!(ref vars o).set_index(reg!(ref vars i), reg!(ref vars r).clone())?
					}
				}
			}
		};

		if let Err(err) = ret.as_ref() {
			// Using slices has a massive performance impact even when no error is thrown,
			// hence get_unchecked_mut.
			// SAFETY: A `&mut vec_vars[vars_offset..vars_offset_len]` confirmed before that
			// this is valid.
			let vars = unsafe { &mut vec_vars.get_unchecked_mut(vars_offset..vars_offset_len) };
			let mut state = RunState { vars };
			tracer.error(self, &mut state, err.as_ref());
		}

		ret
	}

	pub fn name(&self) -> &Rc<str> {
		&self.name
	}
}

impl<V> RunState<'_, V>
where
	V: VariantType,
{
	pub fn variables(&mut self) -> &mut [V] {
		self.vars
	}
}

/// This returns each instruction on oneline instead of 5+ with the default Debug
impl Debug for Instruction {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		use Instruction::*;
		match self {
			Call(r, a) => write!(f, "call    {}, {:?}", r, a),
			CallSelf {
				store_in,
				func,
				args,
			} => {
				if let Some(store_in) = store_in {
					write!(f, "calls   {}, {}, {:?}", store_in, func, args)
				} else {
					write!(f, "calls   none, {}, {:?}", func, args)
				}
			}
			CallEnv { args } => write!(f, "calle   {:?}", args),
			RetSome(reg) => write!(f, "ret     {}", reg),
			RetNone => write!(f, "ret     none"),

			Iter(r, i, p) => write!(f, "iter    {}, {}, {:?}", r, i, p),
			IterJmp(r, p) => write!(f, "iterjp  {}, {:?}", r, p),
			IterInt {
				reg,
				from,
				to,
				step,
				jmp_ip,
			} => write!(f, "iteri   {}, {}, {}, {}, {:?}", reg, from, to, step, jmp_ip),
			IterIntJmp(r, p) => write!(f, "iterijp {}, {:?}", r, p),
			Break {
				amount,
				amount_int,
				jmp_ip,
			} => write!(f, "break   {}, {}, {:?}", amount, amount_int, jmp_ip),

			JmpIf(r, p) => write!(f, "jpif    {}, {:?}", r, p),
			JmpNotIf(r, p) => write!(f, "jpnif   {}, {:?}", r, p),
			Jmp(p) => write!(f, "jp      {:?}", p),

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
			Neg(r, a) => write!(f, "neg     {}, {}", r, a),
			Not(r, a) => write!(f, "not     {}, {}", r, a),

			Eq(r, a, b) => write!(f, "eq      {}, {}, {}", r, a, b),
			Neq(r, a, b) => write!(f, "neq     {}, {}, {}", r, a, b),
			Less(r, a, b) => write!(f, "less    {}, {}, {}", r, a, b),
			LessEq(r, a, b) => write!(f, "lesseq  {}, {}, {}", r, a, b),

			Store(r, a) => write!(f, "store   {}, {}", r, a),
			Load(r, a) => write!(f, "load    {}, {}", r, a),
			Move(a, b) => write!(f, "move    {}, {}", a, b),
			CopySelf { dest } => write!(f, "cpyself {}", dest),

			NewArray(r, c) => write!(f, "newarr  {}, {}", r, c),
			NewDictionary(r, c) => write!(f, "newdict {}, {}", r, c),
			GetIndex(r, o, i) => write!(f, "geti    {}, {}, {}", r, o, i),
			SetIndex(r, o, i) => write!(f, "seti    {}, {}, {}", r, o, i),
		}
	}
}

impl fmt::Debug for CallArgs {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		if let Some(n) = self.store_in {
			write!(f, "{}, \"{}\", {:?}", n, self.func, self.args)
		} else {
			write!(f, "none, \"{}\", {:?}", self.func, self.args)
		}
	}
}

impl<V> fmt::Debug for ByteCode<V>
where
	V: VariantType,
{
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let br = |f: &mut fmt::Formatter| f.write_str(if f.alternate() { "\n" } else { ", " });
		if f.alternate() {
			f.write_str("\n")?;
		}
		write!(f, "parameters: {}", self.param_count)?;
		br(f)?;
		write!(f, "mutable variables: {}", self.var_count)?;
		br(f)?;
		f.write_str("consts:")?;
		for (i, c) in self.consts.iter().enumerate() {
			let i = i as u16 + self.var_count;
			if f.alternate() {
				write!(f, "\n    {:>3}: {:?}", i, c)?;
			} else {
				write!(f, " {}: {:?},", i, c)?;
			}
		}
		br(f)?;
		f.write_str("code:")?;
		for (i, c) in self.code.iter().enumerate() {
			if f.alternate() {
				write!(f, "\n    {:>3}: {:?}", i, c)?;
			} else {
				write!(f, "{}: {:?}", i, c)?;
			}
		}
		Ok(())
	}
}

pub(super) mod err {
	use core::fmt;
	use std::error::Error;

	macro_rules! err {
		($name:ident, $msg:literal) => {
			pub struct $name;

			impl Error for $name {}

			impl fmt::Display for $name {
				fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
					f.write_str($msg)
				}
			}

			impl fmt::Debug for $name {
				fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
					f.write_str(stringify!($name))
				}
			}
		};
	}

	type E = Box<dyn Error>;

	err!(IpOutOfBounds, "Instruction pointer out of bounds");
	err!(RegisterOutOfBounds, "Register out of bounds");
	err!(NoIterator, "No iterators in stack");
	err!(LocalOutOfBounds, "Local out of bounds");
	err!(UndefinedFunction, "Undefined function");
	err!(IncorrectArgumentCount, "Bad argument count");
	err!(ArgumentOutOfBounds, "Argument out of bounds");
	err!(IncompatibleType, "Type is not compatible");
	err!(NotBoolean, "Type is not boolean");

	#[inline(never)]
	#[cold]
	pub fn arg_count() -> E {
		Box::new(IncorrectArgumentCount)
	}

	#[inline(never)]
	#[cold]
	pub fn arg_oob() -> E {
		Box::new(ArgumentOutOfBounds)
	}

	#[inline(never)]
	#[cold]
	pub fn loob() -> E {
		Box::new(LocalOutOfBounds)
	}
}
