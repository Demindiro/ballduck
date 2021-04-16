use super::*;
use crate::ast::{Atom, Expression, Function, Lines, Statement};
use crate::tokenizer::{AssignOp, Op};
use crate::Variant;
use core::num::NonZeroU8;
use rustc_hash::FxHashMap;
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
	loops: Vec<LoopContext>,
}

struct LoopContext {
	continues: Vec<u32>,
	breaks: Vec<u32>,
}

#[derive(Debug)]
pub enum ByteCodeError {
	DuplicateParameter,
	DuplicateVar,
	UndefinedVariable,
	UnexpectedBreak,
	UnexpectedContinue,
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
			loops: Vec::new(),
		};
		for p in function.parameters {
			if builder.vars.insert(p, builder.vars.len() as u16).is_some() {
				return Err(ByteCodeError::DuplicateParameter);
			}
		}
		builder.parse_block(&function.lines)?;
		match builder.instr.last() {
			Some(Instruction::RetSome(_)) | Some(Instruction::RetNone) => (),
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
					Call(_, box ca) | CallSelf(box ca) | CallGlobal(box ca) => {
						for a in ca.args.iter_mut() {
							conv(a);
						}
					}
					JmpIf(a, _)
					| JmpNotIf(a, _)
					| Iter(_, a, _)
					| RetSome(a)
					| Store(a, _)
					| Load(a, _)
					| Move(_, a) => conv(a),
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
					| LessEq(_, a, b)
					| SetIndex(a, _, b)
					| GetIndex(a, _, b) => {
						conv(a);
						conv(b);
					}
					IterJmp(_, _) | Jmp(_) | RetNone | NewArray(_, _) | NewDictionary(_, _) => (),
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

					// Parse expression
					let (var_reg, iter_reg) = (self.curr_var_count, self.curr_var_count + 1);
					self.curr_var_count += 2;
					self.vars.insert(var, var_reg).unwrap_none();
					frame_vars.push(var);
					let iter_reg = if let Some(r) = self.parse_expression(Some(iter_reg), expr)? {
						self.curr_var_count -= 1;
						r
					} else {
						iter_reg
					};
					self.instr
						.push(Instruction::Iter(var_reg, iter_reg, u32::MAX));
					let ic = self.instr.len() - 1;
					let ip = self.instr.len() as u32;

					// Parse loop block
					self.loops.push(LoopContext {
						continues: Vec::new(),
						breaks: Vec::new(),
					});
					self.parse_block(lines)?;
					let context = self.loops.pop().unwrap();

					// Make `continue`s jump to the `IterJmp` instruction
					for i in context.continues {
						let ip = self.instr.len() as u32;
						match self.instr.get_mut(i as usize) {
							Some(Instruction::Jmp(ic)) => *ic = ip,
							_ => unreachable!(),
						}
					}

					// Insert `IterJmp` instruction & update the `Iter` with the end address.
					self.instr.push(Instruction::IterJmp(var_reg, ip));
					let ip = self.instr.len() as u32;
					match self.instr.get_mut(ic) {
						Some(Instruction::Iter(_, _, ic)) => *ic = ip,
						_ => unreachable!(),
					}

					// Make `break`s jump to right after the `IterJmp` instruction
					for i in context.breaks {
						match self.instr.get_mut(i as usize) {
							Some(Instruction::Jmp(ic)) => *ic = ip,
							_ => unreachable!(),
						}
					}

					self.curr_var_count = og_cvc;
				}
				Statement::While { expr, lines } => {
					let og_cvc = self.curr_var_count;

					// Insert `Jmp` to the expr evaluation
					let start_ip = self.instr.len();
					self.instr.push(Instruction::Jmp(u32::MAX));

					// Parse loop block
					self.loops.push(LoopContext {
						continues: Vec::new(),
						breaks: Vec::new(),
					});
					self.parse_block(lines)?;
					let context = self.loops.pop().unwrap();

					// Make `continue`s jump to the expression evaluation
					for i in context.continues {
						let ip = self.instr.len() as u32;
						match self.instr.get_mut(i as usize) {
							Some(Instruction::Jmp(ic)) => *ic = ip,
							_ => unreachable!(),
						}
					}

					// Update start jump
					let ip = self.instr.len() as u32;
					match self.instr.get_mut(start_ip) {
						Some(Instruction::Jmp(ic)) => *ic = ip,
						_ => unreachable!(),
					}

					// Parse expression
					let expr_reg = self.curr_var_count;
					self.curr_var_count += 1;
					let expr_reg = if let Some(r) = self.parse_expression(Some(expr_reg), expr)? {
						self.curr_var_count -= 1;
						r
					} else {
						expr_reg
					};
					self.instr.push(Instruction::JmpNotIf(expr_reg, start_ip as u32 + 1));

					// Make `break`s jump to right after the expression evaluation
					for i in context.breaks {
						match self.instr.get_mut(i as usize) {
							Some(Instruction::Jmp(ic)) => *ic = ip,
							_ => unreachable!(),
						}
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
						let r = self.parse_expression(Some(0), expr)?.unwrap_or(0);
						self.instr.push(Instruction::RetSome(r));
					} else {
						self.instr.push(Instruction::RetNone);
					}
				}
				Statement::Assign {
					var,
					assign_op,
					expr,
				} => {
					assert_eq!(*assign_op, AssignOp::None, "TODO handle assign ops");
					if let Some(&reg) = self.vars.get(var) {
						let expr = self.parse_expression(Some(reg), expr)?;
						if let Some(expr) = expr {
							self.instr.push(Instruction::Move(reg, expr));
						}
					} else if let Some(local) = self.locals.get(var as &str) {
						let og_cvc = self.curr_var_count;
						self.curr_var_count += 1;
						let expr = self.parse_expression(Some(self.curr_var_count - 1), expr)?;
						let e = if let Some(expr) = expr {
							self.curr_var_count -= 1;
							expr
						} else {
							self.curr_var_count
						};
						self.instr.push(Instruction::Store(e, *local));
						self.min_var_count = self.min_var_count.max(self.curr_var_count);
						self.curr_var_count = og_cvc;
					} else {
						return Err(ByteCodeError::UndefinedVariable);
					}
				}
				Statement::Declare { var } => {
					if self.vars.insert(var, self.curr_var_count).is_none() {
						self.curr_var_count += 1;
						self.min_var_count = self.min_var_count.max(self.curr_var_count);
					} else {
						return Err(ByteCodeError::DuplicateVar);
					}
				}
				Statement::AssignIndex {
					var,
					index,
					assign_op,
					expr,
				} => {
					assert_eq!(*assign_op, AssignOp::None, "TODO handle assign ops");
					let og_cvc = self.curr_var_count;

					let index_reg = self.curr_var_count;
					let expr_reg = self.curr_var_count + 1;
					self.curr_var_count += 2;
					let index = self.parse_expression(Some(index_reg), index)?;
					let index_reg = if let Some(reg) = index {
						self.curr_var_count -= 1;
						reg
					} else {
						index_reg
					};
					let expr_reg = if let Some(reg) = self.parse_expression(Some(expr_reg), expr)? {
						self.curr_var_count -= 1;
						reg
					} else {
						expr_reg
					};

					let var = if let Some(&reg) = self.vars.get(var) {
						reg
					} else if let Some(local) = self.locals.get(var as &str) {
						let reg = self.curr_var_count;
						self.curr_var_count += 1;
						self.instr.push(Instruction::Load(reg, *local));
						self.min_var_count = self.min_var_count.max(self.curr_var_count);
						reg
					} else {
						return Err(ByteCodeError::UndefinedVariable);
					};

					self.instr
						.push(Instruction::SetIndex(expr_reg, var, index_reg));
					self.curr_var_count = og_cvc;
				}
				Statement::Continue { levels } => {
					let i = self.loops.len().wrapping_sub(*levels as usize + 1);
					let c = self.loops.get_mut(i).ok_or(ByteCodeError::UnexpectedContinue)?;
					c.continues.push(self.instr.len() as u32);
					self.instr.push(Instruction::Jmp(u32::MAX));
				}
				Statement::Break { levels } => {
					let i = self.loops.len().wrapping_sub(*levels as usize + 1);
					let c = self.loops.get_mut(i).ok_or(ByteCodeError::UnexpectedBreak)?;
					c.breaks.push(self.instr.len() as u32);
					self.instr.push(Instruction::Jmp(u32::MAX));
				}
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
					Op::Index => Instruction::GetIndex(store, left, right),
					Op::Access => panic!("{:?} is not an actual op (bug in AST)", Op::Access),
				});
				self.curr_var_count = og_cvc;
				Ok(None)
			}
			Expression::Atom(a) => match *a {
				Atom::Name(name) => {
					if let Some(&reg) = self.vars.get(name) {
						Ok(Some(reg))
					} else if let Some(&local) = self.locals.get(name) {
						let store = store.expect("No register to store local in");
						self.instr.push(Instruction::Load(store, local));
						Ok(None)
					} else {
						Err(ByteCodeError::UndefinedVariable)
					}
				}
				Atom::Real(r) => Ok(Some(add_const(Variant::Real(r)))),
				Atom::Integer(i) => Ok(Some(add_const(Variant::Integer(i)))),
				Atom::String(s) => Ok(Some(add_const(Variant::String(s.to_string().into())))),
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
				let ca = Box::new(CallArgs {
					store_in: store,
					func: (*name).into(),
					args: args.into_boxed_slice(),
				});
				self.instr.push(if let Some(expr) = expr {
					Instruction::Call(expr, ca)
				} else if self.methods.get(name).is_some() {
					Instruction::CallSelf(ca)
				} else {
					Instruction::CallGlobal(ca)
				});
				self.min_var_count = self.min_var_count.max(self.curr_var_count);
				self.curr_var_count = og_cvc;
				Ok(None)
			}
			Expression::Array(array) => {
				let og_cvc = self.curr_var_count;
				let array_reg = self.curr_var_count;
				self.curr_var_count += 1;
				self.instr
					.push(Instruction::NewArray(array_reg, array.len()));
				for (i, expr) in array.iter().enumerate() {
					let r = self.curr_var_count;
					self.curr_var_count += 1;
					let r = if let Some(e) = self.parse_expression(Some(r), expr)? {
						self.curr_var_count -= 1;
						e
					} else {
						r
					};
					self.update_min_vars();
					let i = self.add_const(Variant::Integer(i as isize));
					self.instr.push(Instruction::SetIndex(r, array_reg, i));
				}
				self.curr_var_count = og_cvc;
				Ok(Some(array_reg))
			}
			Expression::Dictionary(dict) => {
				let og_cvc = self.curr_var_count;
				let dict_reg = self.curr_var_count;
				self.curr_var_count += 1;
				self.instr
					.push(Instruction::NewDictionary(dict_reg, dict.len()));
				for (key_expr, val_expr) in dict.iter() {
					let (k, v) = (self.curr_var_count, self.curr_var_count + 1);
					self.curr_var_count += 2;
					let k = if let Some(e) = self.parse_expression(Some(k), key_expr)? {
						self.curr_var_count -= 1;
						e
					} else {
						k
					};
					let v = if let Some(e) = self.parse_expression(Some(v), val_expr)? {
						self.curr_var_count -= 1;
						e
					} else {
						v
					};
					self.update_min_vars();
					self.instr.push(Instruction::SetIndex(v, dict_reg, k));
				}
				self.curr_var_count = og_cvc;
				Ok(Some(dict_reg))
			}
		}
	}

	fn add_const(&mut self, var: Variant) -> u16 {
		self.consts.push(var);
		u16::MAX - self.consts.len() as u16 + 1
	}

	fn update_min_vars(&mut self) {
		self.min_var_count = self.min_var_count.max(self.curr_var_count);
	}
}
