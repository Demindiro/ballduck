// Copyright (C) 2021  David Hoppenbrouwers
//
// This file is licensed under the MIT license. See script/LICENSE for details.

use ballscript::{Environment, ScriptType, Variant};
use std::env;
use std::fs;
use std::io;

pub fn main() -> Result<(), io::Error> {
	let mut args = env::args();
	let env = create_env();
	let exec = args.next().unwrap_or_else(|| String::from("ballscript"));
	if let Some(file) = args.next() {
		match fs::read_to_string(file) {
			Ok(source) => match ballscript::parse(&source) {
				Ok(script) => {
					//dbg!(&script);
					let script = script.instance();
					match script.call("main", &[], &env) {
						Ok(_) => Ok(()),
						Err(e) => todo!("{:?}", e),
					}
				}
				Err(e) => Err(e).unwrap(),
			},
			Err(e) => Err(e),
		}
	} else {
		eprintln!("Usage: {} <file>", exec);
		Err(io::Error::new(
			io::ErrorKind::InvalidInput,
			"No script file specified",
		))
	}
}

fn create_env() -> Environment {
	let mut env = Environment::new();
	env.add_function(
		"print".into(),
		Box::new(|a: &[_]| {
			a.iter().for_each(|a| print!("{}", a));
			println!();
			Ok(Variant::None)
		}),
	)
	.unwrap();
	env
}
