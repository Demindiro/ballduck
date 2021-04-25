// Copyright (C) 2021  David Hoppenbrouwers
//
// This file is licensed under the MIT license. See LICENSE for details.

#![allow(unstable_name_collisions)] // `unwrap_none` and `expect_none` are removed
#![feature(box_patterns)]
#![feature(option_result_unwrap_unchecked)]
#![feature(core_intrinsics)]
#![feature(maybe_uninit_array_assume_init)]
#![feature(maybe_uninit_uninit_array)]
#![feature(stmt_expr_attributes)]
#![feature(optimize_attribute)]
#![cfg_attr(not(feature = "std"), no_std)]

use unwrap_none::UnwrapNone;

mod ast;
mod bytecode;
mod environment;
mod script;
mod tokenizer;
mod types;
mod variant;

pub use bytecode::{ByteCode, CallResult, Instruction, RunState, Tracer};
pub use environment::Environment;
pub use script::{Class, Instance, ScriptObject, ScriptType};
pub use types::{Array, Dictionary};
pub use variant::specialized;
pub use variant::{Variant, VariantType};

use bytecode::{ByteCodeBuilder, ByteCodeError};
use script::{CallError, Script};
use tokenizer::TokenStream;

use core::fmt;

#[cfg(feature = "std")]
pub(crate) mod std_types {
	use core::hash::BuildHasherDefault;
	use rustc_hash::FxHasher;
	pub use std::collections::{hash_map, HashMap, HashSet};
	pub use std::rc::Rc;
	pub use std::sync::Arc;
	pub type FxHashMap<K, V> = HashMap<K, V, BuildHasherDefault<FxHasher>>;
	pub type FxHashSet<K> = HashSet<K, BuildHasherDefault<FxHasher>>;
}

#[cfg(not(feature = "std"))]
pub(crate) mod std_types {
	extern crate alloc;

	pub use alloc::boxed::Box;
	pub use alloc::format;
	pub use alloc::rc::Rc;
	pub use alloc::string::{String, ToString};
	pub use alloc::sync::Arc;
	pub use alloc::vec::Vec;
	pub use hashbrown::{hash_map, HashMap, HashSet};

	use core::hash::BuildHasherDefault;
	use rustc_hash::FxHasher;
	pub type FxHashMap<K, V> = HashMap<K, V, BuildHasherDefault<FxHasher>>;
	pub type FxHashSet<K> = HashSet<K, BuildHasherDefault<FxHasher>>;
}

use std_types::*;

pub struct ParseError<'a> {
	pub line: u32,
	pub column: u32,
	pub source: &'a str,
	error: ParseErrorType<'a>,
}

enum ParseErrorType<'a> {
	DuplicateLocal(&'a str),
	Ast(ast::Error),
	ByteCode(ByteCodeError<'a>),
}

impl<'a> ParseError<'a> {
	fn new(source: &'a str, line: u32, column: u32, error: ParseErrorType<'a>) -> Self {
		Self {
			line,
			column,
			source,
			error,
		}
	}

	fn new_bytecode(source: &'a str, error: ByteCodeError<'a>) -> Self {
		Self::new(
			source,
			error.line,
			error.column,
			ParseErrorType::ByteCode(error),
		)
	}

	fn new_ast(source: &'a str, error: ast::Error) -> Self {
		Self::new(source, error.line, error.column, ParseErrorType::Ast(error))
	}
}

pub fn parse<'a, 'b: 'a, V, T>(
	source: &'a str,
	string_map: &'b mut FxHashSet<Rc<str>>,
	tracer: T,
) -> Result<Class<V, T>, ParseError<'a>>
where
	V: VariantType,
	T: Tracer<V>,
{
	let tks = TokenStream::parse(source).unwrap();
	let ast = ast::Script::parse(tks).map_err(|e| ParseError::new_ast(source, e))?;

	let locals = {
		let locals = ast.variables;
		let mut hm = FxHashMap::with_capacity_and_hasher(locals.len(), Default::default());
		for (i, l) in locals.iter().enumerate() {
			if hm.insert(l.to_string().into(), i as u16).is_some() {
				return Err(ParseError::new(
					source,
					0,
					0,
					ParseErrorType::DuplicateLocal(l),
				));
			}
		}
		hm.shrink_to_fit();
		hm
	};

	let mut script = Script::new(locals, tracer);

	for (i, f) in ast.functions.iter().enumerate() {
		let i = i as u16;
		let name = f.name.into();
		script
			.function_map
			.insert(name, i)
			.expect_none("Duplicate function");
	}
	for f in ast.functions {
		ByteCodeBuilder::parse(f, &script.function_map, &script.locals, string_map)
			.map(|f| script.functions.push(f))
			.map_err(|e| ParseError::new_bytecode(source, e))?;
	}
	script.function_map.shrink_to_fit();
	script.functions.shrink_to_fit();

	Ok(script.into())
}

impl fmt::Display for ParseError<'_> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use fmt::Write;
		match &self.error {
			ParseErrorType::DuplicateLocal(v) => {
				f.write_str("Duplicate local '")?;
				f.write_str(v)?;
				f.write_char('\'')?;
			}
			ParseErrorType::ByteCode(e) => {
				e.fmt(f)?;
			}
			ParseErrorType::Ast(e) => {
				e.fmt(f)?;
			}
		}
		write!(f, " at line {}, column {}", self.line + 1, self.column + 1)
	}
}
