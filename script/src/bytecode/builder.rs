use super::*;
use crate::ast::{Atom, Expression, Function, Lines, Statement};
use crate::tokenizer::Op;
use crate::Variant;
use core::convert::TryInto;
use rustc_hash::FxHashMap;
use std::rc::Rc;
use unwrap_none::UnwrapNone;

pub(crate) struct ByteCodeBuilder<'e, 's> {
	methods: &'e FxHashMap<&'e str, ()>,
	locals: &'e FxHashMap<Box<str>, u16>,
	instr: Vec<Instruction>,
	vars: FxHashMap<&'s str, u16>,
	consts: Vec<Variant>,
	curr_var_count: u16,
	min_var_count: u16,
	param_count: u16,
}

impl<'e, 's> ByteCodeBuilder<'e, 's> {
	pub(crate) fn parse(
		function: Function,
		methods: &'e FxHashMap<&'e str, ()>,
		locals: &'e FxHashMap<Box<str>, u16>,
	) -> Result<ByteCode, ByteCodeError> {
		let mut builder = Self {
			instr: Vec::new(),
			vars: FxHashMap::with_hasher(Default::default()),
			consts: Vec::new(),
			curr_var_count: function.parameters.len() as u16,
			min_var_count: function.parameters.len() as u16,
			locals,
			methods,
			param_count: function.parameters.len() as u16,
		};
		for p in function.parameters {
			if builder.vars.insert(p, builder.vars.len() as u16).is_some() {
				return Err(ByteCodeError::DuplicateParameter);
			}
		}
		builder.parse_block(&function.lines)?;
		match builder.instr.last() {
			Some(Instruction::RetSome) | Some(Instruction::RetNone) => (),
			_ => builder.instr.push(Instruction::RetNone),
		}

		if builder.consts.len() > 0 {
			// All consts are using the upper-most registers, move them downwards
			let offset = (u16::MAX - builder.consts.len() as u16).wrapping_add(1);
			let min_var_count = builder.min_var_count;
			for i in builder.instr.iter_mut() {
				use Instruction::*;
				let conv = |c: &mut u16| {
					if *c >= offset {
						*c = u16::MAX - *c + min_var_count
					}
				};
				match i {
					Call(box (_, ca)) | CallSelf(box ca) | CallGlobal(box ca) => {
						for a in ca.args.iter_mut() {
							conv(a);
						}
					}
					Move(_, a) | JmpIf(a, _) => conv(a),
					Add(_, a, b)
					| Sub(_, a, b)
					| Mul(_, a, b)
					| Div(_, a, b)
					| Rem(_, a, b)
					| And(_, a, b)
					| Or(_, a, b)
					| Xor(_, a, b)
					| Shr(_, a, b)
					| Shl(_, a, b)
					| Eq(_, a, b)
					| Neq(_, a, b)
					| Less(_, a, b)
					| LessEq(_, a, b) => {
						conv(a);
						conv(b);
					}
					IterConst(_) | IterInt(_, _) | IterJmp(_, _) | Jmp(_) | RetSome | RetNone => (),
				}
			}
		}

		Ok(ByteCode {
			code: builder.instr,
			var_count: builder.min_var_count,
			param_count: builder.param_count,
			consts: builder.consts,
		})
	}

	fn parse_block(&mut self, lines: &Lines<'s>) -> Result<(), ByteCodeError> {
		let mut frame_vars = Vec::new();
		for line in lines {
			match line {
				Statement::Expression { expr } => {
					self.parse_expression(None, expr)?;
				}
				Statement::For { var, expr, lines } => {
					let og_cvc = self.curr_var_count;
					let reg = self.vars.len().try_into().expect("Too many variables");
					self.vars.insert(var, reg).unwrap_none();
					frame_vars.push(var);
					self.curr_var_count += 1;
					match expr {
						Expression::Atom(a) => {
							self.instr.push(match a {
								Atom::String(a) => Instruction::IterConst(Box::new((
									reg,
									u32::MAX,
									Box::new(a.to_string().into_boxed_str()),
								))),
								Atom::Integer(a) => Instruction::IterInt(reg, *a),
								//Atom::Real(a) => Instruction::IterConst(Box::new(a)),
								Atom::Real(a) => todo!("for Real({})", a),
								Atom::Name(a) => todo!("for {:?}", a),
							})
						}
						_ => todo!(),
					}
					let ic = self.instr.len() - 1;
					let ip = self.instr.len() as u32;
					self.parse_block(lines)?;
					self.instr.push(Instruction::IterJmp(reg, ip));
					let ip = self.instr.len() as u32;
					match self.instr.get_mut(ic) {
						Some(Instruction::IterConst(ic)) => ic.1 = ip,
						Some(Instruction::IterInt(..)) => (),
						_ => unreachable!(),
					}
					self.curr_var_count = og_cvc;
				}
				Statement::If {
					expr,
					lines,
					else_lines,
				} => {
					// If
					let expr = self.parse_expression(Some(self.curr_var_count), expr)?;
					let expr = if let Some(expr) = expr {
						expr
					} else {
						self.curr_var_count += 1;
						self.curr_var_count - 1
					};
					self.instr.push(Instruction::JmpIf(expr, u32::MAX));
					let ic = self.instr.len() - 1;
					self.parse_block(lines)?;
					// Skip else
					let skip_else_jmp = else_lines.as_ref().map(|_| {
						self.instr.push(Instruction::Jmp(u32::MAX));
						self.instr.len() - 1
					});
					// Jump to right after `if` block if `expr` evaluates to false
					let ip = self.instr.len() as u32;
					match self.instr.get_mut(ic) {
						Some(Instruction::JmpIf(_, ic)) => *ic = ip,
						_ => unreachable!(),
					}
					// Else
					if let Some(else_lines) = else_lines {
						self.parse_block(else_lines)?;
						let ip = self.instr.len() as u32;
						match self.instr.get_mut(skip_else_jmp.unwrap()) {
							Some(Instruction::Jmp(ic)) => *ic = ip,
							_ => unreachable!(),
						}
					}
				}
				Statement::Return { expr } => {
					if let Some(expr) = expr {
						let r = self.parse_expression(Some(0), expr)?;
						if let Some(r) = r {
							self.instr.push(Instruction::Move(0, r));
						}
						self.instr.push(Instruction::RetSome);
					} else {
						self.instr.push(Instruction::RetNone);
					}
				}
				_ => todo!("{:#?}", line),
			}
		}
		self.min_var_count = self.min_var_count.max(self.vars.len() as u16);
		for fv in frame_vars {
			self.vars.remove(fv).unwrap();
		}
		Ok(())
	}

	fn parse_expression(
		&mut self,
		store: Option<u16>,
		expr: &Expression,
	) -> Result<Option<u16>, ByteCodeError> {
		let mut add_const = |v| {
			self.consts.push(v);
			u16::MAX - self.consts.len() as u16 + 1
		};
		match expr {
			Expression::Operation { left, op, right } => {
				let store = store.expect("TODO: handle operations without store location");
				let og_cvc = self.curr_var_count;
				let (r_left, r_right) = (self.curr_var_count, self.curr_var_count + 1);
				self.curr_var_count += 2;
				self.min_var_count = self.min_var_count.max(self.curr_var_count);
				let or_left = self.parse_expression(Some(r_left), left)?;
				let left = if let Some(l) = or_left {
					l
				} else {
					self.curr_var_count -= 1;
					r_left
				};
				let or_right = self.parse_expression(Some(r_right), right)?;
				let right = if let Some(r) = or_right { r } else { r_right };
				self.instr.push(match op {
					Op::Add => Instruction::Add(store, left, right),
					Op::Sub => Instruction::Sub(store, left, right),
					Op::Mul => Instruction::Mul(store, left, right),
					Op::Div => Instruction::Div(store, left, right),
					Op::Rem => Instruction::Rem(store, left, right),
					Op::And => Instruction::And(store, left, right),
					Op::Or => Instruction::Or(store, left, right),
					Op::Xor => Instruction::Xor(store, left, right),
					Op::ShiftLeft => Instruction::Shl(store, left, right),
					Op::ShiftRight => Instruction::Shr(store, left, right),
					Op::Eq => Instruction::Eq(store, left, right),
					Op::Neq => Instruction::Neq(store, left, right),
					Op::Less => Instruction::Less(store, left, right),
					Op::Greater => Instruction::Less(store, right, left),
					Op::LessEq => Instruction::LessEq(store, left, right),
					Op::GreaterEq => Instruction::LessEq(store, right, left),
					Op::Not | Op::AndThen | Op::OrElse => todo!(),
					Op::Access => panic!("{:?} is not an actual op (bug in AST)", Op::Access),
				});
				self.curr_var_count = og_cvc;
				Ok(None)
			}
			Expression::Atom(a) => match *a {
				Atom::Name(name) => {
					if let Some(&reg) = self.vars.get(name).or_else(|| self.locals.get(name)) {
						Ok(Some(reg))
					} else {
						return Err(ByteCodeError::UndefinedVariable);
					}
				}
				Atom::Real(r) => Ok(Some(add_const(Variant::Real(r)))),
				Atom::Integer(i) => Ok(Some(add_const(Variant::Integer(i)))),
				Atom::String(s) => Ok(Some(add_const(Variant::Object(Rc::new(
					s.to_string().into_boxed_str(),
				))))),
			},
			Expression::Function {
				expr,
				name,
				arguments,
			} => {
				let og_cvc = self.curr_var_count;
				let expr = if let Some(expr) = expr {
					let r = self.curr_var_count;
					self.curr_var_count += 1;
					let e = self.parse_expression(Some(r), expr)?;
					Some(if let Some(e) = e { e } else { r })
				} else {
					None
				};
				let mut args = Vec::with_capacity(arguments.len());
				for a in arguments {
					let r = self.curr_var_count;
					self.curr_var_count += 1;
					let e = self.parse_expression(Some(r), a)?;
					if let Some(e) = e {
						args.push(e);
						self.curr_var_count -= 1;
					} else {
						args.push(r);
					}
				}
				let ca = CallArgs {
					store_in: store,
					func: (*name).into(),
					args: args.into_boxed_slice(),
				};
				self.instr.push(if let Some(expr) = expr {
					Instruction::Call(Box::new((expr, ca)))
				} else if self.methods.get(name).is_some() {
					Instruction::CallSelf(Box::new(ca))
				} else {
					Instruction::CallGlobal(Box::new(ca))
				});
				self.curr_var_count = og_cvc;
				Ok(None)
			}
		}
	}
}
