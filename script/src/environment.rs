// Copyright (C) 2021  David Hoppenbrouwers
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
