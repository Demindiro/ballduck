// Copyright (C) 2021  David Hoppenbrouwers
//
// This file is licensed under the MIT license. See script/LICENSE for details.

use crate::tokenizer::*;
use core::convert::TryInto;
use core::fmt;

type Integer = isize;
type Real = f64;

pub(crate) struct Script<'src> {
	pub functions: Vec<Function<'src>>,
	pub variables: Vec<&'src str>,
}

pub(crate) struct Function<'src> {
	pub name: &'src str,
	pub parameters: Vec<&'src str>,
	pub lines: Lines<'src>,
}

pub(crate) type Lines<'src> = Vec<Statement<'src>>;

#[derive(Debug)]
pub(crate) enum Statement<'src> {
	Declare {
		line: u32,
		column: u32,
		var: &'src str,
	},
	Assign {
		line: u32,
		column: u32,
		var: &'src str,
		assign_op: AssignOp,
		expr: Expression<'src>,
	},
	AssignIndex {
		line: u32,
		column: u32,
		var: &'src str,
		index: Expression<'src>,
		assign_op: AssignOp,
		expr: Expression<'src>,
	},
	Expression {
		line: u32,
		column: u32,
		expr: Expression<'src>,
	},
	For {
		line: u32,
		column: u32,
		var: &'src str,
		expr: Expression<'src>,
		lines: Lines<'src>,
	},
	While {
		line: u32,
		column: u32,
		expr: Expression<'src>,
		lines: Lines<'src>,
	},
	If {
		line: u32,
		column: u32,
		expr: Expression<'src>,
		lines: Lines<'src>,
		else_lines: Option<Lines<'src>>,
	},
	Return {
		line: u32,
		column: u32,
		expr: Option<Expression<'src>>,
	},
	Continue {
		line: u32,
		column: u32,
		levels: u8,
	},
	Break {
		line: u32,
		column: u32,
		levels: u8,
	},
}

#[derive(Debug, PartialEq)]
pub(crate) enum Atom<'src> {
	Name(&'src str),
	Real(Real),
	Integer(Integer),
	String(&'src str),
	Bool(bool),
}

#[derive(Debug, PartialEq)]
pub(crate) enum Expression<'src> {
	Atom {
		line: u32,
		column: u32,
		atom: Atom<'src>,
	},
	Operation {
		line: u32,
		column: u32,
		op: Op,
		left: Box<Expression<'src>>,
		right: Box<Expression<'src>>,
	},
	Function {
		line: u32,
		column: u32,
		expr: Option<Box<Expression<'src>>>,
		name: &'src str,
		arguments: Vec<Expression<'src>>,
	},
	Array {
		line: u32,
		column: u32,
		array: Vec<Self>,
	},
	Dictionary {
		line: u32,
		column: u32,
		dictionary: Vec<(Self, Self)>,
	},
}

pub struct Error {
	error: ErrorType,
	pub line: u32,
	pub column: u32,
}

pub enum ErrorType {
	UnexpectedIndent(u8),
	UnexpectedToken(String),
	ExpectedToken(String),
	UnexpectedEOF,
	NotANumber,
	InternalError(u32),
}

macro_rules! err {
	($err:ident, $tokens:ident) => {{
		let (l, c) = $tokens.position();
		return Error::new(ErrorType::$err, l, c);
	}};
	(UnexpectedToken, $err_val:expr, $tokens:ident) => {{
		let tk = format!("{:?}", $err_val);
		let (l, c) = $tokens.position();
		return Error::new(ErrorType::UnexpectedToken(tk), l, c);
	}};
	(ExpectedToken, $err_val:expr, $tokens:ident) => {{
		let tk = format!("{:?}", $err_val);
		let (l, c) = $tokens.position();
		return Error::new(ErrorType::ExpectedToken(tk), l, c);
	}};
	($err:ident, $err_val:expr, $tokens:ident) => {{
		let (l, c) = $tokens.position();
		return Error::new(ErrorType::$err($err_val), l, c);
	}};
}

impl<'src> Script<'src> {
	pub(crate) fn parse(mut tokens: TokenStream<'src>) -> Result<Self, Error> {
		let mut functions = Vec::new();
		let mut variables = Vec::new();
		while let Some(tk) = tokens.next() {
			match tk {
				Token::Var => match tokens.next() {
					Some(Token::Name(name)) => variables.push(name),
					_ => todo(&tokens, line!())?,
				},
				Token::Fn => match Function::parse(&mut tokens) {
					Ok(f) => functions.push(f),
					Err(f) => return Err(f),
				},
				Token::Indent(0) => (),
				Token::Indent(i) => err!(UnexpectedIndent, i, tokens),
				_ => err!(UnexpectedToken, tk, tokens),
			}
		}
		Ok(Self {
			functions,
			variables,
		})
	}
}

impl<'src> Function<'src> {
	fn parse(tokens: &mut TokenStream<'src>) -> Result<Self, Error> {
		let name = match tokens.next() {
			Some(Token::Name(name)) => name,
			Some(tk) => err!(UnexpectedToken, tk, tokens),
			None => err!(UnexpectedEOF, tokens),
		};
		match tokens.next() {
			Some(Token::BracketRoundOpen) => (),
			Some(tk) => err!(UnexpectedToken, tk, tokens),
			None => err!(UnexpectedEOF, tokens),
		}

		let mut parameters = Vec::new();
		loop {
			match tokens.next() {
				Some(Token::BracketRoundClose) => break,
				Some(Token::Name(a)) => {
					parameters.push(a);
					match tokens.next() {
						Some(Token::BracketRoundClose) => break,
						Some(Token::Comma) => (),
						_ => todo(tokens, line!())?,
					}
				}
				_ => todo(tokens, line!())?,
			}
		}

		// Ensure there is one and only one tab
		match tokens.next() {
			Some(Token::Indent(i)) if i == 1 => (),
			Some(Token::Indent(i)) => err!(UnexpectedIndent, i, tokens),
			Some(tk) => err!(UnexpectedToken, tk, tokens),
			None => err!(UnexpectedEOF, tokens),
		}

		Ok(Self {
			name,
			parameters,
			lines: Self::parse_block(tokens, 1)?.0,
		})
	}

	fn parse_block(
		tokens: &mut TokenStream<'src>,
		expected_indent: u8,
	) -> Result<(Lines<'src>, u8), Error> {
		let mut lines = Lines::new();
		loop {
			match tokens.next() {
				Some(Token::Name(name)) => {
					let (line, column) = tokens.position();
					match tokens.next() {
						Some(Token::BracketRoundOpen) => {
							tokens.prev();
							tokens.prev();
							let (line, column) = tokens.position();
							let expr = Expression::parse(tokens)?;
							lines.push(Statement::Expression { expr, line, column });
						}
						Some(Token::BracketSquareOpen) => {
							let expr = Expression::new_name(name, tokens);
							let (line, column) = tokens.position();
							let expr = Expression::parse_index_op(expr, tokens)?;
							if let Some(Token::Assign(assign_op)) = tokens.next() {
								if let Expression::Operation { right, .. } = expr {
									lines.push(Statement::AssignIndex {
										var: name,
										index: *right,
										assign_op,
										expr: Expression::parse(tokens)?,
										line,
										column,
									});
								} else {
									unreachable!();
								}
							} else {
								tokens.prev();
								lines.push(Statement::Expression { expr, line, column });
							}
						}
						Some(Token::Assign(op)) => match op {
							AssignOp::None => lines.push(Statement::Assign {
								var: name,
								assign_op: op,
								expr: Expression::parse(tokens)?,
								line,
								column,
							}),
							_ => todo(tokens, line!())?,
						},
						_ => todo(tokens, line!())?,
					}
				}
				Some(Token::For) => {
					let (line, column) = tokens.position();
					let var = match tokens.next() {
						Some(Token::Name(n)) => n,
						Some(tk) => err!(UnexpectedToken, tk, tokens),
						None => err!(UnexpectedEOF, tokens),
					};
					if tokens.next() != Some(Token::In) {
						err!(ExpectedToken, Token::In, tokens);
					}
					let expr = Expression::parse(tokens)?;
					let (blk, indent) = Self::parse_block(tokens, expected_indent + 1)?;
					lines.push(Statement::For {
						var,
						expr,
						lines: blk,
						line,
						column,
					});
					if indent < expected_indent {
						return Ok((lines, indent));
					}
				}
				Some(Token::While) => {
					let (line, column) = tokens.position();
					let expr = Expression::parse(tokens)?;
					let (blk, indent) = Self::parse_block(tokens, expected_indent + 1)?;
					lines.push(Statement::While {
						expr,
						lines: blk,
						line,
						column,
					});
					if indent < expected_indent {
						return Ok((lines, indent));
					}
				}
				Some(Token::If) => {
					let (line, column) = tokens.position();
					let expr = Expression::parse(tokens)?;
					let (blk, indent) = Self::parse_block(tokens, expected_indent + 1)?;
					lines.push(Statement::If {
						expr,
						lines: blk,
						else_lines: None,
						line,
						column,
					});
					if indent < expected_indent {
						return Ok((lines, indent));
					}
					let mut prev_blk = &mut lines;
					while let Some(tk) = tokens.next() {
						let (line, column) = tokens.position();
						if tk == Token::Elif {
							let expr = Expression::parse(tokens)?;
							let (blk, indent) = Self::parse_block(tokens, expected_indent + 1)?;
							let if_blk = Vec::from([Statement::If {
								expr,
								lines: blk,
								else_lines: None,
								line,
								column,
							}]);
							prev_blk = match prev_blk.last_mut().unwrap() {
								Statement::If { else_lines, .. } => {
									*else_lines = Some(if_blk);
									else_lines.as_mut().unwrap()
								}
								_ => unreachable!(),
							};
							if indent < expected_indent {
								return Ok((lines, indent));
							}
						} else if tk == Token::Else {
							let (blk, indent) = Self::parse_block(tokens, expected_indent + 1)?;
							match prev_blk.last_mut().unwrap() {
								Statement::If { else_lines, .. } => *else_lines = Some(blk),
								_ => unreachable!(),
							};
							if indent < expected_indent {
								return Ok((lines, indent));
							}
						} else {
							tokens.prev();
							break;
						}
					}
				}
				Some(Token::Pass) => (),
				Some(Token::Return) => {
					let (line, column) = tokens.position();
					let expr = if let Some(_) = tokens.next() {
						tokens.prev();
						Some(Expression::parse(tokens)?)
					} else {
						None
					};
					lines.push(Statement::Return { expr, line, column });
				}
				Some(Token::Var) => {
					let (line, column) = tokens.position();
					let var = match tokens.next() {
						Some(Token::Name(n)) => n,
						Some(tk) => err!(UnexpectedToken, tk, tokens),
						None => err!(UnexpectedEOF, tokens),
					};
					lines.push(Statement::Declare { var, line, column });
					match tokens.next() {
						Some(Token::Assign(assign_op)) => match assign_op {
							AssignOp::None => {
								let (line, column) = tokens.position();
								let expr = Expression::parse(tokens)?;
								lines.push(Statement::Assign {
									var,
									assign_op,
									expr,
									line,
									column,
								});
							}
							_ => todo(tokens, line!())?,
						},
						Some(Token::Indent(_)) => {
							tokens.prev();
						}
						None => (),
						Some(tk) => err!(UnexpectedToken, tk, tokens),
					}
				}
				Some(tk) if tk == Token::Continue || tk == Token::Break => {
					let (line, column) = tokens.position();
					let levels = match tokens.next() {
						Some(Token::Number(num)) => match parse_number(num) {
							Ok(Atom::Integer(num)) => {
								num.try_into().expect("TODO handle num > u8::MAX")
							}
							Ok(Atom::Real(_)) => err!(UnexpectedToken, Token::Number(num), tokens),
							Err(_) => err!(NotANumber, tokens),
							_ => unreachable!(),
						},
						Some(Token::Indent(_)) => {
							tokens.prev();
							0
						}
						None => 0,
						tk => err!(UnexpectedToken, tk, tokens),
					};
					match tk {
						Token::Continue => lines.push(Statement::Continue {
							levels,
							line,
							column,
						}),
						Token::Break => lines.push(Statement::Break {
							levels,
							line,
							column,
						}),
						_ => unreachable!(),
					}
				}
				None => return Ok((lines, 0)),
				Some(Token::Indent(i)) if i < expected_indent => return Ok((lines, i)),
				Some(Token::Indent(i)) if i == expected_indent => (),
				Some(Token::Indent(i)) => err!(UnexpectedIndent, i, tokens),
				_ => todo(tokens, line!())?,
			};
		}
	}
}

impl<'src> Expression<'src> {
	fn parse(tokens: &mut TokenStream<'src>) -> Result<Self, Error> {
		let lhs = match tokens.next() {
			Some(Token::BracketRoundOpen) => {
				let e = Self::parse(tokens)?;
				if tokens.next() != Some(Token::BracketRoundClose) {
					err!(ExpectedToken, Token::BracketRoundClose, tokens);
				}
				e
			}
			Some(Token::String(s)) => Self::new_str(s, tokens),
			Some(Token::Number(n)) => Self::new_num(n, tokens)?,
			Some(tk) if tk == Token::True || tk == Token::False => Self::new_bool(tk, tokens),
			Some(Token::Name(name)) => match tokens.next() {
				Some(Token::BracketRoundOpen) => Self::new_fn(
					None,
					name,
					Self::parse_expr_list(tokens, Token::BracketRoundClose)?,
					tokens,
				),
				Some(Token::BracketSquareOpen) => {
					Self::parse_index_op(Self::new_name(name, tokens), tokens)?
				}
				Some(_) => {
					tokens.prev();
					Self::new_name(name, tokens)
				}
				_ => todo(tokens, line!())?,
			},
			Some(Token::BracketSquareOpen) => {
				let pos = tokens.position();
				Self::Array {
					array: Self::parse_expr_list(tokens, Token::BracketSquareClose)?,
					line: pos.0,
					column: pos.1,
				}
			}
			Some(Token::BracketCurlyOpen) => {
				let pos = tokens.position();
				Self::Dictionary {
					dictionary: Self::parse_expr_map(tokens, Token::BracketCurlyClose)?,
					line: pos.0,
					column: pos.1,
				}
			}
			_ => todo(tokens, line!())?,
		};

		if let Some(tk) = tokens.next() {
			match tk {
				Token::Op(opl) => match tokens.next() {
					Some(Token::Name(mid)) => {
						let og_mid = mid;
						let mid = Self::new_name(mid, tokens);
						match tokens.next() {
							Some(Token::Op(opr)) => {
								Self::parse_tri_op_start(lhs, opl, mid, opr, tokens)
							}
							Some(Token::BracketRoundOpen) => {
								let args = Self::parse_expr_list(tokens, Token::BracketRoundClose)?;
								match opl {
									Op::Access => Ok(Self::new_fn(Some(lhs), og_mid, args, tokens)),
									op => Ok(Self::new_op(
										lhs,
										op,
										Self::new_fn(None, og_mid, args, tokens),
										tokens,
									)),
								}
							}
							Some(Token::BracketRoundClose) | Some(Token::Indent(_)) => {
								tokens.prev();
								Ok(Self::new_op(lhs, opl, mid, tokens))
							}
							Some(Token::BracketSquareOpen) => {
								let mid = Self::parse_index_op(mid, tokens)?;
								match tokens.next() {
									Some(Token::Op(opr)) => {
										Self::parse_tri_op_start(lhs, opl, mid, opr, tokens)
									}
									Some(Token::BracketRoundClose) | Some(Token::Indent(_)) => {
										tokens.prev();
										Ok(Self::new_op(lhs, opl, mid, tokens))
									}
									None => Ok(Self::new_op(lhs, opl, mid, tokens)),
									_ => todo(tokens, line!())?,
								}
							}
							None => Ok(Self::new_op(lhs, opl, mid, tokens)),
							_ => todo(tokens, line!())?,
						}
					}
					Some(Token::Number(_)) | Some(Token::String(_)) => {
						tokens.prev();
						let mid = match tokens.next().unwrap() {
							Token::Number(n) => Self::new_num(n, tokens)?,
							Token::String(n) => Self::new_str(n, tokens),
							_ => unreachable!(),
						};
						match tokens.next() {
							Some(Token::Op(opr)) => {
								Self::parse_tri_op_start(lhs, opl, mid, opr, tokens)
							}
							Some(Token::BracketRoundClose) | Some(Token::Indent(_)) => {
								tokens.prev();
								Ok(Self::new_op(lhs, opl, mid, tokens))
							}
							None => Ok(Self::new_op(lhs, opl, mid, tokens)),
							_ => todo(tokens, line!())?,
						}
					}
					Some(mid) if mid == Token::True || mid == Token::False => {
						let mid = Self::new_bool(mid, tokens);
						match tokens.next() {
							Some(Token::BracketRoundClose) | Some(Token::Indent(_)) => {
								tokens.prev();
								Ok(Self::new_op(lhs, opl, mid, tokens))
							}
							None => Ok(Self::new_op(lhs, opl, mid, tokens)),
							_ => todo(tokens, line!())?,
						}
					}
					_ => todo(tokens, line!())?,
				},
				Token::BracketRoundClose
				| Token::BracketSquareClose
				| Token::BracketCurlyClose
				| Token::Indent(_)
				| Token::Comma
				| Token::Colon => {
					tokens.prev();
					Ok(lhs)
				}
				_ => todo(tokens, line!())?,
			}
		} else {
			Ok(lhs)
		}
	}

	fn parse_tri_op_start(
		lhs: Self,
		opl: Op,
		mid: Self,
		opr: Op,
		tokens: &mut TokenStream<'src>,
	) -> Result<Self, Error> {
		let rhs = match tokens.next() {
			Some(Token::Name(rhs)) => {
				let lhs = Self::new_name(rhs, tokens);
				match tokens.next() {
					Some(Token::BracketRoundOpen) => todo(tokens, line!())?,
					Some(Token::BracketSquareOpen) => Self::parse_index_op(lhs, tokens)?,
					Some(_) => {
						tokens.prev();
						lhs
					}
					None => lhs,
				}
			}
			Some(Token::Number(rhs)) => Self::new_num(rhs, tokens)?,
			_ => todo(tokens, line!())?,
		};
		Self::parse_tri_op(lhs, opl, mid, opr, rhs, tokens)
	}

	fn parse_tri_op(
		left: Self,
		op_left: Op,
		mid: Self,
		op_right: Op,
		right: Self,
		tokens: &mut TokenStream<'src>,
	) -> Result<Self, Error> {
		let (left, op, right) = if op_left >= op_right {
			tokens.prev();
			match tokens.next().unwrap() {
				Token::BracketRoundClose | Token::BracketSquareClose | Token::BracketCurlyClose => {
					(left, op_right, right)
				}
				_ => {
					tokens.prev();
					let right = Self::parse(tokens)?;
					let left = Self::new_op(left, op_left, mid, tokens);
					(left, op_right, right)
				}
			}
		} else {
			let right = Self::new_op(mid, op_right, right, tokens);
			(left, op_left, right)
		};
		Ok(Self::new_op(left, op, right, tokens))
	}

	fn new_op(left: Self, op: Op, right: Self, tokens: &TokenStream<'src>) -> Self {
		let pos = tokens.position();
		Self::Operation {
			left: Box::new(left),
			op,
			right: Box::new(right),
			line: pos.0,
			column: pos.1,
		}
	}

	fn new_name(n: &'src str, tokens: &TokenStream<'src>) -> Self {
		let pos = tokens.position();
		Self::Atom {
			atom: Atom::Name(n),
			line: pos.0,
			column: pos.1,
		}
	}

	fn new_num(n: &'src str, tokens: &mut TokenStream<'src>) -> Result<Self, Error> {
		let (line, column) = tokens.position();
		parse_number(n)
			.map(|atom| Self::Atom { atom, line, column })
			.or_else(|_| {
				let (l, c) = tokens.position();
				Error::new(ErrorType::NotANumber, l, c)
			})
	}

	fn new_bool(tk: Token, tokens: &mut TokenStream<'src>) -> Self {
		let val = match tk {
			Token::True => true,
			Token::False => false,
			_ => unreachable!(),
		};
		let pos = tokens.position();
		Self::Atom {
			atom: Atom::Bool(val),
			line: pos.0,
			column: pos.1,
		}
	}

	fn new_str(n: &'src str, tokens: &TokenStream<'src>) -> Self {
		let pos = tokens.position();
		Self::Atom {
			atom: Atom::String(n),
			line: pos.0,
			column: pos.1,
		}
	}

	fn new_fn(
		expr: Option<Self>,
		name: &'src str,
		arguments: Vec<Self>,
		tokens: &TokenStream<'src>,
	) -> Self {
		let pos = tokens.position();
		Self::Function {
			expr: expr.map(Box::new),
			name,
			arguments,
			line: pos.0,
			column: pos.1,
		}
	}

	fn parse_expr_list(
		tokens: &mut TokenStream<'src>,
		end_token: Token,
	) -> Result<Vec<Self>, Error> {
		let mut args = Vec::new();
		loop {
			match tokens.next() {
				Some(tk) if tk == end_token => break,
				Some(_) => {
					tokens.prev();
					let expr = Expression::parse(tokens)?;
					args.push(expr);
				}
				e => todo!("{:?}", e),
			};
			match tokens.next() {
				Some(Token::Comma) => (),
				Some(tk) if tk == end_token => break,
				Some(tk) => {
					dbg!(tk);
					todo!()
				} //err!(UnexpectedToken, tk, tokens),
				tk => {
					dbg!(tk, args);
					todo!()
				}
			}
		}
		Ok(args)
	}

	/// Parses all items in the form of `key : value` separated by a `,` until `end_token`
	/// is encountered. The start token must have been consumed already.
	fn parse_expr_map(
		tokens: &mut TokenStream<'src>,
		end_token: Token,
	) -> Result<Vec<(Self, Self)>, Error> {
		let mut args = Vec::new();
		loop {
			let key = match tokens.next() {
				Some(tk) if tk == end_token => break,
				Some(_) => {
					tokens.prev();
					let expr = Expression::parse(tokens)?;
					expr
				}
				e => todo!("{:?}", e),
			};
			match tokens.next() {
				Some(Token::Colon) => (),
				Some(tk) => err!(UnexpectedToken, tk, tokens),
				None => err!(UnexpectedEOF, tokens),
			}
			match tokens.next() {
				Some(tk) if tk == end_token => err!(UnexpectedToken, tk, tokens),
				Some(_) => {
					tokens.prev();
					let expr = Expression::parse(tokens)?;
					args.push((key, expr));
				}
				None => err!(UnexpectedEOF, tokens),
			};
			match tokens.next() {
				Some(Token::Comma) => (),
				Some(tk) if tk == end_token => break,
				Some(tk) => {
					dbg!(tk);
					todo!()
				} //err!(UnexpectedToken, tk, tokens),
				tk => {
					dbg!(tk, args);
					todo!()
				}
			}
		}
		Ok(args)
	}

	/// This function only parses what is between '[' and ']'. It is useful for expressions such
	/// as `a[0] * a[1]` that are hard to parse in one go using `parse_tri_op` or the like.
	/// The preceding '[' is meant to be consumed before calling this function.
	fn parse_index_op(var: Self, tokens: &mut TokenStream<'src>) -> Result<Self, Error> {
		let pos = tokens.position();
		let expr = Self::Operation {
			op: Op::Index,
			left: Box::new(var),
			right: Box::new(Self::parse(tokens)?),
			line: pos.0,
			column: pos.1,
		};
		if tokens.next() != Some(Token::BracketSquareClose) {
			err!(ExpectedToken, Token::BracketSquareClose, tokens);
		}
		Ok(expr)
	}
}

impl Error {
	fn new<T>(error: ErrorType, line: u32, column: u32) -> Result<T, Self> {
		Err(Self {
			error,
			line,
			column,
		})
	}
}

impl fmt::Display for Error {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use fmt::Write;
		match &self.error {
			ErrorType::NotANumber => f.write_str("Not a number"),
			ErrorType::UnexpectedToken(tk) => {
				f.write_str("Unexpected token: '")?;
				f.write_str(&tk)?;
				f.write_char('\'')
			}
			ErrorType::ExpectedToken(tk) => {
				f.write_str("Expected token: '")?;
				f.write_str(&tk)?;
				f.write_char('\'')
			}
			ErrorType::UnexpectedIndent(n) => write!(f, "Unexpected indent by {} tabs", n),
			ErrorType::UnexpectedEOF => f.write_str("Unexpected end of file"),
			ErrorType::InternalError(line) => {
				f.write_str("An internal error occured at line ")?;
				f.write_str(&line.to_string())?;
				f.write_str(" (A bug report would be welcome)")
			}
		}
	}
}

#[derive(Debug, PartialEq)]
enum NumberParseError {
	InvalidBase,
	InvalidDigit,
	Empty,
	SeparatorInWrongPosition,
}

/// Custom number parsing function that allows underscores
fn parse_number(s: &str) -> Result<Atom, NumberParseError> {
	let mut chars = s.chars();
	let (chars, base) = if chars.next() == Some('0') {
		if let Some(c) = chars.next() {
			if let Some(b) = match c {
				'x' => Some(16),
				'b' => Some(2),
				'o' => Some(8),
				'0' | '.' => None,
				_ => return Err(NumberParseError::InvalidBase),
			} {
				(chars, b)
			} else {
				(s.chars(), 10)
			}
		} else {
			return Ok(Atom::Integer(0));
		}
	} else {
		(s.chars(), 10)
	};
	if s == "" {
		Err(NumberParseError::Empty)
	} else {
		let mut chars = chars.peekable();
		let neg = if chars.peek() == Some(&'-') {
			chars.next();
			true
		} else {
			false
		};
		let mut chars = chars.filter(|&c| c != '_').peekable();
		// Real numbers and integers have to be processed separately as the range of a real can
		// exceed that of an integer
		if s.contains('.') {
			// Don't accept '.0', '0.' or even '.'. While many languages accept the former two,
			// I believe they are a poor choice for readability, hence they are disallowed.
			if chars.peek().unwrap() == &'.' {
				return Err(NumberParseError::SeparatorInWrongPosition);
			}
			let mut n = 0.0;
			loop {
				let c = chars.next().unwrap();
				if c == '.' {
					break;
				}
				n *= base as Real;
				n += c.to_digit(base).ok_or(NumberParseError::InvalidDigit)? as Real;
			}
			if chars.peek() == None {
				return Err(NumberParseError::SeparatorInWrongPosition);
			}
			let mut i = 1.0 / base as Real;
			for c in chars {
				n += (c.to_digit(base).ok_or(NumberParseError::InvalidDigit)? as Real) * i;
				i /= base as Real;
			}
			Ok(Atom::Real(if neg { -n } else { n }))
		} else {
			let mut n = 0;
			for c in chars {
				n *= base as Integer;
				// Negative numbers have a larger range than positive numbers (e.g. i8 has range -128..127)
				n -= c.to_digit(base).ok_or(NumberParseError::InvalidDigit)? as Integer;
			}
			Ok(Atom::Integer(if neg { n } else { -n }))
		}
	}
}

/// This function is used when an unhandled case is encountered in the AST
#[inline(never)]
#[cold]
fn todo<T>(tokens: &TokenStream, rust_src_line: u32) -> Result<T, Error> {
	let (line, column) = tokens.position();
	Error::new(ErrorType::InternalError(rust_src_line), line, column)
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn number() {
		assert_eq!(parse_number("0"), Ok(Atom::Integer(0)));
		assert_eq!(parse_number("32"), Ok(Atom::Integer(32)));
		assert_eq!(parse_number("0.0"), Ok(Atom::Real(0.0)));
		match parse_number("13.37") {
			Ok(Atom::Real(f)) => assert!((f - 13.37).abs() <= Real::EPSILON * 13.37),
			r => panic!("{:?}", r),
		}
		assert_eq!(
			parse_number("."),
			Err(NumberParseError::SeparatorInWrongPosition)
		);
		assert_eq!(
			parse_number("0."),
			Err(NumberParseError::SeparatorInWrongPosition)
		);
		assert_eq!(
			parse_number(".0"),
			Err(NumberParseError::SeparatorInWrongPosition)
		);
	}
}
