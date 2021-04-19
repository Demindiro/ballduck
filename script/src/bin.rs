// Copyright (C) 2021  David Hoppenbrouwers
//
// This file is licensed under the MIT license. See script/LICENSE for details.

use ballscript::{CallError, Environment, ParseError, ScriptType, Variant};
use rustc_hash::FxHashSet;
use std::{env, fs, io, process};
use ansi_term::Color;

pub fn main() {
	let mut args = env::args();
	let env = create_env();
	let exec = args.next().unwrap_or_else(|| String::from("ballscript"));
	let mut string_map = FxHashSet::with_hasher(Default::default());
	let ret = if let Some(file) = args.next() {
		match fs::read_to_string(&file) {
			Ok(source) => match ballscript::parse(&source, &mut string_map) {
				Ok(script) => {
					dbg!(&script);
					let script = script.instance();
					match script.call("main", &[], &env) {
						Ok(r) => match r {
							Variant::Integer(i) => i as i32,
							_ => 0,
						},
						Err(e) => {
							print_call_error(e);
							1
						}
					}
				}
				Err(e) => {
					let _ = print_parse_error(e);
					1
				}
			},
			Err(e) => {
				eprintln!("Failed to open file '{}': {}", file, e);
				1
			}
		}
	} else {
		eprintln!("Usage: {} <file>", exec);
		1
	};
	process::exit(ret);
}

fn create_env() -> Environment<Variant> {
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

#[cold]
fn print_parse_error(error: ParseError) -> io::Result<()> {
	use io::Write;
	let mut writer = io::BufWriter::new(io::stderr());
	write!(writer, "{}", Color::Red.bold().paint("Error during parsing: "))?;
	writeln!(writer, "{}", Color::White.bold().paint(error.to_string()))?;
	let min = 2;
	let min = if error.line < min {
		0
	} else {
		error.line - min
	};
	for (li, line) in error.source.lines().enumerate().skip(min as usize).take(5) {
		if li == error.line as usize {
			write!(writer, "{}", Color::Blue.bold().paint(format!("{:>4} > ", li + 1)))?;
		} else {
			write!(writer, "{}", Color::Blue.bold().paint(format!("{:>4} | ", li + 1)))?;
		}
		for c in line.bytes() {
			let _ = if c == b'\t' {
				writer.write(b"    ")
			} else {
				writer.write(&[c])
			};
		}
		let _ = writer.write(b"\n");
		if li == error.line as usize {
			let _ = writer.write(b"       ");
			for (i, c) in line.bytes().enumerate() {
				if i == error.column as usize {
					writeln!(writer, "{}", Color::Red.bold().paint("^"))?;
					break;
				} else if c == b'\t' {
					writer.write(b"    ")?;
				} else {
					writer.write(b" ")?;
				}
			}
		}
	}
	writer.flush()
}

#[cold]
fn print_call_error(error: CallError) {
	eprintln!("An error was thrown: {:?}", error);
}
