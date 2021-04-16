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
