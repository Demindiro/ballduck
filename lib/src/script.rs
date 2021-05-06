// Copyright (C) 2021  David Hoppenbrouwers
//
// This file is licensed under the MIT license. See script/LICENSE for details.

use crate::bytecode::{ByteCode, CallResult};
use crate::std_types::*;
use crate::Rc;
use crate::{Environment, Tracer, VariantType};
use core::any::{Any, TypeId};
use core::cell::RefCell;
use core::fmt;
use std::error::Error;

pub struct Class<V, T>(Arc<Script<V, T>>)
where
	V: VariantType,
	T: Tracer<V>;

#[derive(Clone)]
pub struct ScriptObject<V>(pub(crate) Rc<dyn ScriptType<V>>)
where
	V: VariantType;

impl<V> ScriptObject<V>
where
	V: VariantType,
{
	#[inline(always)]
	pub fn new(rc: Rc<dyn ScriptType<V>>) -> Self {
		Self(rc)
	}
}

impl<V> core::ops::Deref for ScriptObject<V>
where
	V: VariantType,
{
	type Target = Rc<dyn ScriptType<V>>;
	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

#[derive(Debug)]
pub(crate) struct Script<V, T>
where
	V: VariantType,
	T: Tracer<V>,
{
	pub(crate) function_map: FxHashMap<Rc<str>, u8>,
	pub(crate) locals: FxHashMap<Rc<str>, u8>,
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
pub(crate) enum CallError {
	UndefinedFunction,
	BadArgument,
	BadArgumentCount,
	/// This is specifically intended for operations on `()` AKA "null"
	IsEmpty,
	IncompatibleType,
}

pub trait ScriptType<V>: 'static
where
	V: VariantType,
{
	/// Calls the method with the given name on this script instance
	fn call_self(
		&self,
		object: &ScriptObject<V>,
		function: &str,
		args: &[&V],
		env: &Environment<V>,
	) -> CallResult<V>;

	#[inline]
	fn type_id(&self) -> TypeId {
		Any::type_id(self)
	}

	#[inline]
	fn index(&self, index: &V) -> CallResult<V> {
		let _ = index;
		Err(CallError::incompatible_type())
	}

	#[inline]
	fn set_index(&self, index: &V, value: V) -> CallResult<()> {
		let _ = (index, value);
		Err(CallError::incompatible_type())
	}

	#[inline]
	fn to_string(&self) -> String {
		core::any::type_name::<Self>().into()
	}

	#[inline]
	fn iter(&self) -> CallResult<Box<dyn Iterator<Item = V>>> {
		Err(CallError::incompatible_type())
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
	pub(crate) fn new(locals: FxHashMap<Rc<str>, u8>, tracer: T) -> Self {
		Self {
			function_map: FxHashMap::with_hasher(Default::default()),
			functions: Vec::new(),
			locals,
			tracer,
		}
	}

	fn call_traced(
		&self,
		object: &ScriptObject<V>,
		function: &str,
		locals: &mut [V],
		args: &[&V],
		env: &Environment<V>,
	) -> CallResult<V> {
		let func = self
			.function_map
			.get(function)
			.ok_or_else(CallError::undefined_function)?;
		let function = &self.functions[*func as usize];
		function.run(object, &self.functions, locals, args, &env, &self.tracer)
	}
}

impl<V, T> Class<V, T>
where
	V: VariantType,
	T: Tracer<V>,
{
	pub fn instance(&self) -> ScriptObject<V> {
		let mut locals = Vec::new();
		locals.resize(self.0.locals.len(), V::default());
		ScriptObject::new(Rc::new(Instance {
			script: self.0.clone(),
			variables: RefCell::new(locals.into_boxed_slice()),
		}))
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
	fn call_self(
		&self,
		object: &ScriptObject<V>,
		function: &str,
		args: &[&V],
		env: &Environment<V>,
	) -> CallResult<V> {
		let mut vars = self.variables.try_borrow_mut()?;
		self.script
			.call_traced(object, function, &mut vars, args, env)
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

impl<V> ScriptObject<V>
where
	V: VariantType,
{
	pub fn call(&self, function: &str, args: &[&V], env: &Environment<V>) -> CallResult<V> {
		self.0.call_self(self, function, args, env)
	}
}

impl Error for CallError {}

impl fmt::Display for CallError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			CallError::UndefinedFunction => f.write_str("Undefined function"),
			CallError::BadArgumentCount => f.write_str("Bad argument count"),
			CallError::IncompatibleType => f.write_str("Type is not compatible"),
			CallError::IsEmpty => f.write_str("Type is none"),
			CallError::BadArgument => f.write_str("Bad argument type"),
		}
	}
}

impl CallError {
	#[inline(never)]
	#[cold]
	pub fn incompatible_type() -> Box<dyn Error> {
		Box::new(CallError::IncompatibleType)
	}

	#[inline(never)]
	#[cold]
	pub fn empty() -> Box<dyn Error> {
		Box::new(CallError::IsEmpty)
	}

	#[inline(never)]
	#[cold]
	pub fn undefined_function() -> Box<dyn Error> {
		Box::new(CallError::UndefinedFunction)
	}

	#[inline(never)]
	#[cold]
	pub fn bad_argument_count() -> Box<dyn Error> {
		Box::new(CallError::BadArgumentCount)
	}

	#[inline(never)]
	#[cold]
	pub fn bad_argument() -> Box<dyn Error> {
		Box::new(CallError::BadArgument)
	}
}
