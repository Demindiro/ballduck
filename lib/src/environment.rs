// Copyright (C) 2021  David Hoppenbrouwers
//
// This file is licensed under the MIT license. See script/LICENSE for details.

use crate::std_types::{hash_map::Entry, FxHashMap};
use crate::{CallError, CallResult, Rc, VariantType};

#[derive(Default)]
pub struct Environment<V>
where
	V: VariantType,
{
	functions: FxHashMap<Rc<str>, EnvironmentFunction<V>>,
}

pub type EnvironmentFunction<V> = Box<dyn Fn(&[V]) -> CallResult<V>>;

#[derive(Debug)]
pub enum EnvironmentError {
	FunctionAlreadyExists,
}

impl<V> Environment<V>
where
	V: VariantType,
{
	pub fn new() -> Self {
		Self {
			functions: FxHashMap::with_hasher(Default::default()),
		}
	}

	pub fn add_function(
		&mut self,
		name: String,
		f: EnvironmentFunction<V>,
	) -> Result<(), EnvironmentError> {
		match self.functions.entry(name.into()) {
			Entry::Vacant(e) => {
				e.insert(f);
				Ok(())
			}
			Entry::Occupied(_) => Err(EnvironmentError::FunctionAlreadyExists),
		}
	}

	pub fn call(&self, func: &str, args: &[V]) -> CallResult<V> {
		self.functions
			.get(func)
			.ok_or(CallError::UndefinedFunction)?(args)
	}
}
