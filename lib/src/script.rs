// Copyright (C) 2021  David Hoppenbrouwers
//
// This file is licensed under the MIT license. See script/LICENSE for details.

use crate::bytecode::{ByteCode, CallResult, RunError};
use crate::std_types::*;
use crate::Rc;
use crate::{Environment, Tracer, VariantType};
use core::any::{Any, TypeId};
use core::cell::RefCell;
use core::fmt;

pub struct Class<V, T>(Arc<Script<V, T>>)
where
	V: VariantType,
	T: Tracer<V>;

pub type ScriptObject<V> = Rc<dyn ScriptType<V>>;

#[derive(Debug)]
pub(crate) struct Script<V, T>
where
	V: VariantType,
	T: Tracer<V>,
{
	pub(crate) function_map: FxHashMap<Rc<str>, u16>,
	pub(crate) locals: FxHashMap<Rc<str>, u16>,
	pub(crate) functions: Vec<ByteCode<V>>,
	tracer: T,
}

#[derive(Debug)]
pub struct Instance<V, T>
where
	V: VariantType,
	T: Tracer<V>,
{
	script: Arc<Script<V, T>>,
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
	variables: RefCell<Box<[V]>>,
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

pub trait ScriptType<V>: 'static
where
	V: VariantType,
{
	/// Calls the method with the given name on this script instance
	fn call(&self, function: &str, args: &[&V], env: &Environment<V>) -> CallResult<V>;

	#[inline]
	fn type_id(&self) -> TypeId {
		Any::type_id(self)
	}

	#[inline]
	fn index(&self, index: &V) -> CallResult<V> {
		let _ = index;
		Err(CallError::IncompatibleType)
	}

	#[inline]
	fn set_index(&self, index: &V, value: V) -> CallResult<()> {
		let _ = (index, value);
		Err(CallError::IncompatibleType)
	}

	#[inline]
	fn to_string(&self) -> String {
		core::any::type_name::<Self>().into()
	}

	#[inline]
	fn iter(&self) -> CallResult<Box<dyn Iterator<Item = V>>> {
		Err(CallError::IncompatibleType)
	}
}

/// Copied from [`Any`](std::any::Any). As casting between trait objects is not possible
/// without indirection, this is used instead.
impl<V> dyn ScriptType<V> + 'static
where
	V: VariantType,
{
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

impl<V, T: 'static> Script<V, T>
where
	V: VariantType,
	T: Tracer<V>,
{
	pub(crate) fn new(locals: FxHashMap<Rc<str>, u16>, tracer: T) -> Self {
		Self {
			function_map: FxHashMap::with_hasher(Default::default()),
			functions: Vec::new(),
			locals,
			tracer,
		}
	}

	fn call_traced(
		&self,
		function: &str,
		locals: &mut [V],
		args: &[&V],
		env: &Environment<V>,
	) -> CallResult<V> {
		if let Some(&function) = self.function_map.get(function) {
			let function = &self.functions[function as usize];
			function
				.run(&self.functions, locals, args, &env, &self.tracer)
				.map_err(CallError::RunError)
		} else {
			Err(CallError::UndefinedFunction)
		}
	}
}

impl<V, T> Class<V, T>
where
	V: VariantType,
	T: Tracer<V>,
{
	pub fn instance(&self) -> Instance<V, T> {
		let mut locals = Vec::new();
		locals.resize(self.0.locals.len(), V::default());
		Instance {
			script: self.0.clone(),
			variables: RefCell::new(locals.into_boxed_slice()),
		}
	}
}

impl<V, T> From<Script<V, T>> for Class<V, T>
where
	V: VariantType,
	T: Tracer<V>,
{
	fn from(script: Script<V, T>) -> Self {
		Self(Arc::new(script))
	}
}

impl<V, T: 'static> ScriptType<V> for Instance<V, T>
where
	V: VariantType,
	T: Tracer<V>,
{
	fn call(&self, function: &str, args: &[&V], env: &Environment<V>) -> CallResult<V> {
		if let Ok(mut vars) = self.variables.try_borrow_mut() {
			self.script.call_traced(function, &mut vars, args, env)
		} else {
			Err(CallError::AlreadyBorrowed)
		}
	}
}

impl<V, T> fmt::Debug for Class<V, T>
where
	V: VariantType,
	T: Tracer<V>,
{
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		if f.alternate() {
			write!(f, "{:#?}", self.0.functions)
		} else {
			write!(f, "{:?}", self.0.functions)
		}
	}
}
