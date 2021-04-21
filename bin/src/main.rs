// Copyright (C) 2021  David Hoppenbrouwers
//
// This file is licensed under the MIT license. See LICENSE for details.

use ansi_term::Color;
use ballscript::{CallError, Environment, ParseError, ScriptType, Variant};
use rustc_hash::FxHashSet;
use std::{env, fs, io, process};

pub fn main() {
	let mut args = env::args();
	let env = create_env();
	let exec = args.next().unwrap_or_else(|| String::from("ballscript"));
	let mut string_map = FxHashSet::with_hasher(Default::default());

	#[cfg(feature = "print-instructions")]
	let tracer = tracer::Tracer::new();
	#[cfg(not(feature = "print-instructions"))]
	let tracer = ();

	let ret = if let Some(file) = args.next() {
		match fs::read_to_string(&file) {
			Ok(source) => match ballscript::parse(&source, &mut string_map, tracer) {
				Ok(script) => {
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
	write!(
		writer,
		"{}",
		Color::Red.bold().paint("Error during parsing: ")
	)?;
	writeln!(writer, "{}", Color::White.bold().paint(error.to_string()))?;
	let min = 2;
	let min = if error.line < min {
		0
	} else {
		error.line - min
	};
	for (li, line) in error.source.lines().enumerate().skip(min as usize).take(5) {
		if li == error.line as usize {
			write!(
				writer,
				"{}",
				Color::Blue.bold().paint(format!("{:>4} > ", li + 1))
			)?;
		} else {
			write!(
				writer,
				"{}",
				Color::Blue.bold().paint(format!("{:>4} | ", li + 1))
			)?;
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

#[cfg(feature = "print-instructions")]
mod tracer {
	use ballscript as bs;
	use core::fmt;
	use std::cell::RefCell;
	use std::rc::Rc;

	struct TracerData {
		show_vars: [u16; 3],
		show_vars_count: u8,
	}

	#[derive(Clone)]
	pub struct Tracer(Rc<RefCell<TracerData>>);

	impl Tracer {
		pub fn new() -> Self {
			Self(Rc::new(RefCell::new(TracerData {
				show_vars: [0; 3],
				show_vars_count: 0,
			})))
		}
	}

	impl<V> bs::Tracer<V> for Tracer
	where
		V: bs::VariantType,
	{
		fn instruction_pre(
			&self,
			bytecode: &bs::ByteCode<V>,
			ip: u32,
			instruction: &bs::Instruction,
		) {
			print!("[{}] {:>4} | {:?}", bytecode.name(), ip, instruction);
			let mut s = self.0.borrow_mut();
			use bs::Instruction::*;
			match instruction {
				Add(a, b, c) | Mul(a, b, c) => {
					s.show_vars = [*a, *b, *c];
					s.show_vars_count = 3;
				}
				Move(a, b) => {
					s.show_vars = [*a, *b, 0];
					s.show_vars_count = 2;
				}
				_ => s.show_vars_count = 0,
			}
		}

		fn instruction_post(&self, _: &bs::ByteCode<V>, _: u32, _: &bs::Instruction) {}

		fn call_pre(&self, bytecode: &bs::ByteCode<V>, function: &Rc<str>) {
			println!("[{}] Entering {}", bytecode.name(), function);
		}

		fn call_post(&self, bytecode: &bs::ByteCode<V>, function: &Rc<str>) {
			println!("[{}] Exited {}", bytecode.name(), function);
		}

		fn run_pre(&self, bytecode: &bs::ByteCode<V>) {
			println!("[{}] Entered", bytecode.name());
		}

		fn run_post(&self, bytecode: &bs::ByteCode<V>) {
			println!("[{}] Exiting", bytecode.name());
		}

		fn peek(&self, _bytecode: &bs::ByteCode<V>, state: &mut bs::RunState<V>) {
			let s = self.0.borrow();
			print!("\t\t");
			for i in 0..s.show_vars_count {
				print!(
					"{:?}, ",
					state.variables()[s.show_vars[i as usize] as usize]
				);
			}
			println!();
		}
	}

	impl fmt::Debug for Tracer {
		fn fmt(&self, _: &mut fmt::Formatter) -> fmt::Result {
			Ok(())
		}
	}
}
