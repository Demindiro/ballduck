// Copyright (C) 2021  David Hoppenbrouwers
//
// This file is licensed under the MIT license. See script/LICENSE for details.

#![allow(unstable_name_collisions)] // `unwrap_none` and `expect_none` are removed
#![feature(box_patterns)]

use unwrap_none::UnwrapNone;

mod ast;
mod bytecode;
mod environment;
mod script;
mod tokenizer;
mod types;
mod variant;

pub use bytecode::CallResult;
pub use environment::Environment;
pub use script::{CallError, Class, ScriptObject, ScriptType};
pub use types::{Array, Dictionary};
pub use variant::{Variant, VariantType};

use bytecode::{ByteCodeBuilder, ByteCodeError};
use script::Script;
use tokenizer::TokenStream;

use core::fmt;
use rustc_hash::{FxHashMap, FxHashSet};
use std::rc::Rc;

pub struct ParseError<'a> {
	pub line: u32,
	pub column: u32,
	pub source: &'a str,
	error: ParseErrorType<'a>,
}

enum ParseErrorType<'a> {
	DuplicateLocal(&'a str),
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
}

pub fn parse<'a, 'b: 'a, V>(
	source: &'a str,
	string_map: &'b mut FxHashSet<Rc<str>>,
) -> Result<Class<V>, ParseError<'a>>
where
	V: VariantType,
{
	let tks = TokenStream::parse(source).unwrap();
	let ast = ast::Script::parse(tks).unwrap();

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

	let mut script = Script::new(locals);

	let mut methods = FxHashMap::with_capacity_and_hasher(ast.functions.len(), Default::default());
	for f in ast.functions.iter() {
		methods.insert(f.name, ()).expect_none("Duplicate function");
	}
	for f in ast.functions {
		let name = f.name.into();
		ByteCodeBuilder::parse(f, &methods, &script.locals, string_map)
			.map(|f| script.functions.insert(name, f))
			.map_err(|e| ParseError::new_bytecode(source, e))?;
	}
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
		}
		write!(f, " at line {}, column {}", self.line + 1, self.column + 1)
	}
}
