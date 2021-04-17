// Copyright (C) 2021  David Hoppenbrouwers
//
// This file is licensed under the MIT license. See script/LICENSE for details.

use crate::bytecode::{ByteCode, CallResult, RunError};
use crate::{Environment, Variant};
use core::any::{Any, TypeId};
use core::fmt;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

pub struct Class(Arc<Script>);

pub type ScriptObject = Rc<dyn ScriptType>;

#[derive(Debug)]
pub(crate) struct Script {
	pub(crate) functions: FxHashMap<Rc<str>, ByteCode>,
	pub(crate) locals: FxHashMap<Rc<str>, u16>,
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

pub trait ScriptType: fmt::Debug + 'static {
	fn call(&self, function: &str, args: &[Variant], env: &Environment) -> CallResult;

	#[inline]
	fn mul(&self, rhs: &ScriptObject) -> CallResult {
		let _ = rhs;
		Err(CallError::InvalidOperator)
	}

	#[inline]
	fn add(&self, rhs: &ScriptObject) -> CallResult {
		let _ = rhs;
		Err(CallError::InvalidOperator)
	}

	#[inline]
	fn type_id(&self) -> TypeId {
		Any::type_id(self)
	}

	#[inline]
	fn index(&self, index: &Variant) -> CallResult {
		let _ = index;
		Err(CallError::IncompatibleType)
	}

	#[inline]
	fn set_index(&self, index: &Variant, value: Variant) -> CallResult<()> {
		let _ = (index, value);
		Err(CallError::IncompatibleType)
	}

	#[inline]
	fn to_string(&self) -> String {
		std::any::type_name::<Self>().into()
	}

	#[inline]
	fn iter(&self) -> CallResult<Box<dyn Iterator<Item = Variant>>> {
		Err(CallError::IncompatibleType)
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

impl Script {
	pub(crate) fn new(locals: FxHashMap<Rc<str>, u16>) -> Self {
		Self {
			functions: FxHashMap::with_hasher(Default::default()),
			locals,
		}
	}

	fn call(
		&self,
		function: &str,
		locals: &mut [Variant],
		args: &[Variant],
		env: &Environment,
	) -> CallResult {
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
		let mut locals = Vec::new();
		locals.resize(self.0.locals.len(), Variant::default());
		Instance {
			script: self.0.clone(),
			variables: RefCell::new(locals.into_boxed_slice()),
		}
	}
}

impl From<Script> for Class {
	fn from(script: Script) -> Self {
		Self(Arc::new(script))
	}
}

impl ScriptType for Instance {
	fn call(&self, function: &str, args: &[Variant], env: &Environment) -> CallResult {
		if let Ok(mut vars) = self.variables.try_borrow_mut() {
			self.script.call(function, &mut vars, args, env)
		} else {
			Err(CallError::AlreadyBorrowed)
		}
	}
}

impl fmt::Debug for Class {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		if f.alternate() {
			write!(f, "{:#?}", self.0.functions)
		} else {
			write!(f, "{:?}", self.0.functions)
		}
	}
}
