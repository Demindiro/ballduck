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
use rustc_hash::{FxHashMap, FxHashSet};
use script::Script;
pub use script::{CallError, Class, ScriptObject, ScriptType};
use std::rc::Rc;
pub use types::{Array, Dictionary};
pub use variant::{Variant, VariantType};

use bytecode::ByteCodeBuilder;
use tokenizer::TokenStream;

pub fn parse<V>(source: &str, string_map: &mut FxHashSet<Rc<str>>) -> Result<Class<V>, ()>
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
				panic!("Duplicate local");
				//return Err(ByteCodeError::DuplicateLocal);
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
		match ByteCodeBuilder::parse(f, &methods, &script.locals, string_map) {
			Ok(f) => {
				script.functions.insert(name, f);
			}
			Err(e) => todo!("{:?}", e),
		}
	}
	script.functions.shrink_to_fit();

	Ok(script.into())
}
