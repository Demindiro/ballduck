use crate::bytecode::{ByteCode, CallResult, Environment, RunError};
use core::any::{Any, TypeId};
use core::cmp;
use core::fmt::Debug;
use core::ops::{Add, Mul, Sub};
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

pub struct Class(Arc<Script>);

#[derive(Clone, Debug)]
/// Variant type that encodes a few common types. Having the common types
/// not be hidden behind a dyn trait improves performance greatly
pub enum Variant {
	None,
	Bool(bool),
	Real(f64),
	Integer(isize),
	Object(ScriptObject),
}

impl Default for Variant {
	fn default() -> Self {
		Variant::None
	}
}

pub type ScriptObject = Rc<dyn ScriptType>;

#[derive(Debug)]
pub(crate) struct Script {
	pub(crate) functions: FxHashMap<Box<str>, ByteCode>,
	pub(crate) locals: FxHashMap<Box<str>, u16>,
}

#[derive(Debug)]
pub struct Instance {
	script: Arc<Script>,
	// TODO we have 3 options to solve the `A.foo() -> B.bar() -> A.foo()` problem:
	// * We use `Cell<Box<[_]>>`. This *should* have relatively little overhead but is
	//   very unintuitive, and the second `A.foo()` call will only see old variable names.
	//   It is also likely still less efficient than `RefCell` due to an extra alloc.
	// * We use `RefCell<Box<[_]>>`. This is arguably the most "Rusty" way in terms of
	//   "don't alias stuff", but is not very intuitive compared to other languages
	//   and may also end up being impractical.
	// * We use `Box<[Cell<_>]>`. This will allow mimicking other scripting languages in
	//   terms of (expected) behaviour but may be less efficient than `RefCell`.
	// The second and third option should be measured for performance. If the latter is
	// fast enough (or perhaps even faster) we should use that.
	// For now, the second option is chosen as the third can't be undone without being a
	// massive breaking change.
	variables: RefCell<Box<[Variant]>>,
}

#[derive(Debug)]
pub enum CallError {
	UndefinedFunction,
	BadArgument,
	BadArgumentCount,
	RunError(RunError),
	/// This is specifically intended for operations on `()` AKA "null"
	IsEmpty,
	InvalidOperator,
	IncompatibleType,
	AlreadyBorrowed,
}

pub trait ScriptType: Debug + 'static {
	fn call(&self, function: &str, args: &[Variant]) -> CallResult<CallError>;

	fn dup(&self) -> ScriptObject;

	#[inline]
	fn mul(&self, rhs: &ScriptObject) -> CallResult<CallError> {
		let _ = rhs;
		Err(CallError::InvalidOperator)
	}

	#[inline]
	fn add(&self, rhs: &ScriptObject) -> CallResult<CallError> {
		let _ = rhs;
		Err(CallError::InvalidOperator)
	}

	#[inline]
	fn type_id(&self) -> TypeId {
		Any::type_id(self)
	}
}

/// Copied from [`Any`](std::any::Any). As casting between trait objects is not possible
/// without indirection, this is used instead.
impl dyn ScriptType + 'static {
	#[inline]
	pub fn is<T: 'static>(&self) -> bool {
		TypeId::of::<T>() == self.type_id()
	}

	#[inline]
	pub fn cast<T: 'static>(&self) -> Option<&T> {
		if self.is::<T>() {
			// SAFETY: `is` confirmed that the underlying type of the trait object is indeed T.
			unsafe { Some(&*(self as *const _ as *const _)) }
		} else {
			None
		}
	}
}

/*
impl Mul<Self> for &ScriptObject {
	type Output = CallResult<CallError>;

	fn mul(self, rhs: Self) -> Self::Output {
		self.as_ref().mul(rhs)
	}
}

impl Add<Self> for &ScriptObject {
	type Output = CallResult<CallError>;

	fn add(self, rhs: Self) -> Self::Output {
		self.as_ref().add(rhs)
	}
}
*/

macro_rules! err_op {
	() => {
		return Err(CallError::IncompatibleType);
	};
}

impl Add<Self> for &Variant {
	type Output = CallResult;

	#[inline]
	fn add(self, rhs: Self) -> Self::Output {
		Ok(match self {
			Variant::Real(a) => match rhs {
				Variant::Real(b) => Variant::Real(a + b),
				&Variant::Integer(b) => Variant::Real(a + b as f64),
				_ => err_op!(),
			},
			&Variant::Integer(a) => match rhs {
				Variant::Real(b) => Variant::Real(a as f64 + b),
				Variant::Integer(b) => Variant::Integer(a + b),
				_ => err_op!(),
			},
			_ => err_op!(),
		})
	}
}

impl Sub<Self> for &Variant {
	type Output = CallResult;

	#[inline]
	fn sub(self, rhs: Self) -> Self::Output {
		Ok(match self {
			Variant::Real(a) => match rhs {
				Variant::Real(b) => Variant::Real(a - b),
				&Variant::Integer(b) => Variant::Real(a - b as f64),
				_ => err_op!(),
			},
			&Variant::Integer(a) => match rhs {
				Variant::Real(b) => Variant::Real(a as f64 - b),
				Variant::Integer(b) => Variant::Integer(a - b),
				_ => err_op!(),
			},
			_ => err_op!(),
		})
	}
}

impl Mul<Self> for &Variant {
	type Output = CallResult;

	#[inline]
	fn mul(self, rhs: Self) -> Self::Output {
		Ok(match self {
			Variant::Real(a) => match rhs {
				Variant::Real(b) => Variant::Real(a * b),
				&Variant::Integer(b) => Variant::Real(a * b as f64),
				_ => err_op!(),
			},
			&Variant::Integer(a) => match rhs {
				Variant::Real(b) => Variant::Real(a as f64 * b),
				Variant::Integer(b) => Variant::Integer(a * b),
				_ => err_op!(),
			},
			_ => err_op!(),
		})
	}
}

impl PartialEq<Self> for Variant {
	#[inline]
	// FIXME should we return bool or should we implement a custom form
	// of PartialEq that returns a Result?
	fn eq(&self, rhs: &Self) -> bool {
		match self {
			&Variant::Bool(a) => match rhs {
				&Variant::Bool(b) => a == b,
				_ => false,
			},
			&Variant::Real(a) => match rhs {
				&Variant::Real(b) => a == b,
				&Variant::Integer(b) => a == b as f64,
				_ => false,
			},
			&Variant::Integer(a) => match rhs {
				&Variant::Real(b) => a as f64 == b,
				&Variant::Integer(b) => a == b,
				_ => false,
			},
			_ => false,
		}
	}
}

impl PartialOrd<Self> for Variant {
	#[inline]
	fn partial_cmp(&self, rhs: &Self) -> Option<cmp::Ordering> {
		match self {
			&Variant::Bool(a) => match rhs {
				Variant::Bool(b) => a.partial_cmp(b),
				_ => None,
			},
			&Variant::Real(a) => match rhs {
				Variant::Real(b) => a.partial_cmp(b),
				Variant::Integer(b) => a.partial_cmp(&(*b as f64)),
				_ => None,
			},
			&Variant::Integer(a) => match rhs {
				Variant::Real(b) => (a as f64).partial_cmp(b),
				Variant::Integer(b) => a.partial_cmp(b),
				_ => None,
			},
			_ => None,
		}
	}
}

/// Regular ol' clone doesn't work because of [`Sized`] stuff, so this exists
macro_rules! impl_dup {
	() => {
		fn dup(&self) -> ScriptObject {
			Rc::new(self.clone()) as ScriptObject
		}
	};
}

macro_rules! check_arg_count {
	($args:ident, $count:expr) => {
		if $args.len() != $count {
			return Err(CallError::BadArgumentCount);
		}
	};
}

pub trait ScriptIter: Debug {
	fn iter(&self) -> ScriptIterator;
}

pub type ScriptIterator<'a> = Box<dyn Iterator<Item = Variant> + 'a>;

impl Variant {
	pub fn call(&self, function: &str, args: &[Variant]) -> Result<Variant, CallError> {
		match self {
			Variant::None => Err(CallError::IsEmpty),
			Variant::Bool(_) => Err(CallError::UndefinedFunction),
			Variant::Real(r) => match function {
				"sqrt" => {
					check_arg_count!(args, 0);
					Ok(Variant::Real(r.sqrt()))
				}
				_ => Err(CallError::UndefinedFunction),
			},
			Variant::Integer(_) => Err(CallError::UndefinedFunction),
			Variant::Object(o) => o.call(function, args),
		}
	}
}

impl Script {
	pub(crate) fn new(locals: FxHashMap<Box<str>, u16>) -> Self {
		Self {
			functions: FxHashMap::with_hasher(Default::default()),
			locals,
		}
	}

	pub(crate) fn shrink(&mut self) {
		self.locals.shrink_to_fit();
		self.functions.shrink_to_fit();
	}

	fn call(
		&self,
		function: &str,
		locals: &mut [Variant],
		args: &[Variant],
	) -> Result<Variant, CallError> {
		let mut env = Environment::new();
		env.add_function(
			"print".into(),
			Box::new(|a: &[_]| {
				a.iter().for_each(|a| println!("{:?}", a));
				Ok(Variant::None)
			}),
		)
		.unwrap();

		if let Some(function) = self.functions.get(function) {
			function
				.run(&self.functions, locals, args, &env)
				.map_err(CallError::RunError)
		} else {
			Err(CallError::UndefinedFunction)
		}
	}
}

impl Class {
	pub fn instance(&self) -> Instance {
		Instance {
			script: self.0.clone(),
			variables: RefCell::new(Box::new([])),
		}
	}
}

impl From<Script> for Class {
	fn from(script: Script) -> Self {
		Self(Arc::new(script))
	}
}

impl ScriptType for Instance {
	fn call(&self, function: &str, args: &[Variant]) -> CallResult<CallError> {
		if let Ok(mut vars) = self.variables.try_borrow_mut() {
			self.script.call(function, &mut vars, args)
		} else {
			Err(CallError::AlreadyBorrowed)
		}
	}

	fn dup(&self) -> ScriptObject {
		todo!();
	}
}

impl ScriptType for Box<str> {
	fn call(&self, _: &str, _: &[Variant]) -> CallResult<CallError> {
		todo!()
	}

	impl_dup!();
}

impl ScriptType for char {
	fn call(&self, _: &str, _: &[Variant]) -> CallResult<CallError> {
		todo!()
	}

	impl_dup!();
}

impl ScriptType for String {
	fn call(&self, _: &str, _: &[Variant]) -> CallResult<CallError> {
		todo!()
	}

	impl_dup!();
}

impl ScriptIter for Box<str> {
	fn iter(&self) -> ScriptIterator {
		Box::new(self.chars().map(|c| Variant::Object(Rc::new(c))))
	}
}

impl ScriptIter for String {
	fn iter(&self) -> ScriptIterator {
		Box::new(self.chars().map(|c| Variant::Object(Rc::new(c))))
	}
}
