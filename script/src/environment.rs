// Copyright (C) 2021  David Hoppenbrouwers
//
// This file is licensed under the MIT license. See script/LICENSE for details.

use crate::{CallError, CallResult, Variant};
use rustc_hash::FxHashMap;
use std::collections::hash_map::Entry;

#[derive(Default)]
pub struct Environment {
	functions: FxHashMap<Box<str>, EnvironmentFunction>,
}

pub type EnvironmentFunction = Box<dyn Fn(&[Variant]) -> CallResult>;

#[derive(Debug)]
pub enum EnvironmentError {
	FunctionAlreadyExists,
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

	pub fn call(&self, func: &str, args: &[Variant]) -> CallResult {
		self.functions
			.get(func)
			.ok_or(CallError::UndefinedFunction)?(args)
	}
}
