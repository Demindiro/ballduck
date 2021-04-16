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
use rustc_hash::FxHashMap;
use script::Script;
pub use script::{CallError, Class, ScriptObject, ScriptType};
pub use types::{Array, Dictionary};
pub use variant::Variant;

use bytecode::ByteCodeBuilder;
use tokenizer::TokenStream;

pub fn parse(source: &str) -> Result<Class, ()> {
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
		match ByteCodeBuilder::parse(f, &methods, &script.locals) {
			Ok(f) => {
				script.functions.insert(name, f);
			}
			Err(e) => todo!("{:?}", e),
		}
	}
	script.functions.shrink_to_fit();

	Ok(script.into())
}
