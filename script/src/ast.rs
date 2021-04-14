use crate::tokenizer::*;

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
		var: &'src str,
	},
	Assign {
		var: &'src str,
		assign_op: AssignOp,
		expr: Expression<'src>,
	},
	Expression {
		expr: Expression<'src>,
	},
	For {
		var: &'src str,
		expr: Expression<'src>,
		lines: Lines<'src>,
	},
	If {
		expr: Expression<'src>,
		lines: Lines<'src>,
		else_lines: Option<Lines<'src>>,
	},
	Return {
		expr: Option<Expression<'src>>,
	},
}

#[derive(Debug, PartialEq)]
pub(crate) enum Atom<'src> {
	Name(&'src str),
	Real(Real),
	Integer(Integer),
	String(&'src str),
}

#[derive(Debug)]
pub(crate) enum Expression<'src> {
	Atom(Atom<'src>),
	Operation {
		op: Op,
		left: Box<Expression<'src>>,
		right: Box<Expression<'src>>,
	},
	Function {
		expr: Option<Box<Expression<'src>>>,
		name: &'src str,
		arguments: Vec<Expression<'src>>,
	},
}

#[derive(Debug)]
pub struct Error {
	error: ErrorType,
	line: u32,
	column: u32,
	rust_src_line: u32,
}

#[derive(Debug)]
pub enum ErrorType {
	UnexpectedIndent(u8),
	UnexpectedToken(String),
	ExpectedToken(String),
	UnexpectedEOF,
	NotANumber,
}

macro_rules! err {
	($err:ident, $tokens:ident) => {{
		let (l, c) = $tokens.position();
		return Error::new(ErrorType::$err, l, c, line!());
	}};
	(UnexpectedToken, $err_val:expr, $tokens:ident) => {{
		let tk = format!("{:?}", $err_val);
		let (l, c) = $tokens.position();
		return Error::new(ErrorType::UnexpectedToken(tk), l, c, line!());
	}};
	(ExpectedToken, $err_val:expr, $tokens:ident) => {{
		let tk = format!("{:?}", $err_val);
		let (l, c) = $tokens.position();
		return Error::new(ErrorType::ExpectedToken(tk), l, c, line!());
	}};
	($err:ident, $err_val:expr, $tokens:ident) => {{
		let (l, c) = $tokens.position();
		return Error::new(ErrorType::$err($err_val), l, c, line!());
	}};
}

impl<'src> Script<'src> {
	pub(crate) fn parse(mut tokens: TokenStream<'src>) -> Result<Self, Error> {
		let mut functions = Vec::new();
		let mut variables = Vec::new();
		while let Some(tk) = tokens.next() {
			match tk {
				Token::Let => match tokens.next() {
					Some(Token::Name(name)) => variables.push(name),
					e => todo!("{:?}", e),
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
						e => todo!("{:?}", e),
					}
				}
				e => todo!("{:?}", e),
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
				Some(Token::Name(_)) => match tokens.next() {
					Some(Token::BracketRoundOpen) => {
						tokens.prev();
						tokens.prev();
						let expr = Expression::parse(tokens)?;
						lines.push(Statement::Expression { expr });
					}
					e => todo!("{:?}", e),
				},
				Some(Token::For) => {
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
					});
					if indent < expected_indent {
						return Ok((lines, indent));
					}
				}
				Some(Token::If) => {
					let expr = Expression::parse(tokens)?;
					let (blk, indent) = Self::parse_block(tokens, expected_indent + 1)?;
					lines.push(Statement::If {
						expr,
						lines: blk,
						else_lines: None,
					});
					if indent < expected_indent {
						return Ok((lines, indent));
					}
					let mut prev_blk = &mut lines;
					while let Some(tk) = tokens.next() {
						if tk == Token::Elif {
							let expr = Expression::parse(tokens)?;
							let (blk, indent) = Self::parse_block(tokens, expected_indent + 1)?;
							let if_blk = Vec::from([Statement::If {
								expr,
								lines: blk,
								else_lines: None,
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
					let expr = if let Some(_) = tokens.next() {
						tokens.prev();
						Some(Expression::parse(tokens)?)
					} else {
						None
					};
					lines.push(Statement::Return { expr });
				}
				None => return Ok((lines, 0)),
				Some(Token::Indent(i)) if i < expected_indent => return Ok((lines, i)),
				Some(Token::Indent(i)) if i == expected_indent => (),
				Some(Token::Indent(i)) => err!(UnexpectedIndent, i, tokens),
				tk => todo!("{:#?} {:?}", lines, tk),
			};
		}
	}
}

impl<'src> Expression<'src> {
	fn parse(tokens: &mut TokenStream<'src>) -> Result<Self, Error> {
		let expr_op = |left, op, right| Expression::Operation {
			left: Box::new(left),
			op,
			right: Box::new(right),
		};
		let expr_fn = |expr: Option<_>, name, arguments| Expression::Function {
			expr: expr.map(Box::new),
			name,
			arguments,
		};
		let expr_num = |n, tokens: &mut TokenStream, sl| {
			if let Ok(n) = parse_number(n) {
				Ok(Expression::Atom(n))
			} else {
				let (l, c) = tokens.position();
				Error::new(ErrorType::NotANumber, l, c, sl)
			}
		};
		let expr_name = |n| Expression::Atom(Atom::Name(n));
		fn parse_fn_args<'src>(
			tokens: &mut TokenStream<'src>,
		) -> Result<Vec<Expression<'src>>, Error> {
			let mut args = Vec::new();
			loop {
				match tokens.next() {
					Some(Token::BracketRoundClose) => break,
					Some(_) => {
						tokens.prev();
						let expr = Expression::parse(tokens)?;
						args.push(expr);
					}
					e => todo!("{:?}", e),
				};
				match tokens.next() {
					Some(Token::Comma) => (),
					Some(Token::BracketRoundClose) => break,
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

		let lhs = match tokens.next() {
			Some(Token::BracketRoundOpen) => {
				let e = Self::parse(tokens)?;
				if tokens.next() != Some(Token::BracketRoundClose) {
					err!(ExpectedToken, Token::BracketRoundClose, tokens);
				}
				e
			}
			Some(Token::String(s)) => Self::Atom(Atom::String(s)),
			Some(Token::Number(n)) => expr_num(n, tokens, line!())?,
			Some(Token::Name(name)) => match tokens.next() {
				Some(Token::BracketRoundOpen) => expr_fn(None, name, parse_fn_args(tokens)?),
				Some(_) => {
					tokens.prev();
					expr_name(name)
				}
				e => todo!("{:?}", e),
			},
			e => todo!("{:?}", e),
		};

		if let Some(tk) = tokens.next() {
			match tk {
				Token::Op(opl) => match tokens.next() {
					Some(Token::Name(mid)) => {
						let og_mid = mid;
						let mid = expr_name(mid);
						match tokens.next() {
							Some(Token::Op(opr)) => match tokens.next() {
								Some(Token::Name(rhs)) => Self::parse_tri_op(
									lhs,
									opl,
									mid,
									opr,
									Self::new_name(rhs),
									tokens,
								),
								e => todo!("{:?}", e),
							},
							Some(Token::BracketRoundOpen) => {
								let args = parse_fn_args(tokens)?;
								match opl {
									Op::Access => Ok(expr_fn(Some(lhs), og_mid, args)),
									op => Ok(expr_op(lhs, op, expr_fn(None, og_mid, args))),
								}
							}
							Some(Token::BracketRoundClose) | Some(Token::Indent(_)) => {
								tokens.prev();
								Ok(expr_op(lhs, opl, mid))
							}
							None => Ok(expr_op(lhs, opl, mid)),
							e => todo!("{:?}", e),
						}
					}
					Some(Token::Number(mid)) => match tokens.next() {
						Some(Token::Op(opr)) => match tokens.next() {
							Some(Token::Name(rhs)) => Self::parse_tri_op(
								lhs,
								opl,
								expr_num(mid, tokens, line!())?,
								opr,
								Self::new_name(rhs),
								tokens,
							),
							Some(Token::Number(rhs)) => Self::parse_tri_op(
								lhs,
								opl,
								expr_num(mid, tokens, line!())?,
								opr,
								expr_num(rhs, tokens, line!())?,
								tokens,
							),
							e => todo!("{:?}", e),
						},
						Some(Token::BracketRoundClose) | Some(Token::Indent(_)) => {
							tokens.prev();
							Ok(expr_op(lhs, opl, expr_num(mid, tokens, line!())?))
						}
						e => todo!("{:?}", e),
					},
					e => todo!("{:?}", e),
				},
				Token::BracketRoundClose | Token::Indent(_) => {
					tokens.prev();
					Ok(lhs)
				}
				// TODO is this correct?
				Token::Comma => {
					tokens.prev();
					Ok(lhs)
				}
				e => todo!("{:?} ------ {:?}   {:?}", lhs, e, tokens.position()),
			}
		} else {
			Ok(lhs)
		}
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
			let right = Self::parse(tokens)?;
			let left = Self::new_op(left, op_left, mid);
			(left, op_right, right)
		} else {
			let right = Self::new_op(mid, op_right, right);
			(left, op_left, right)
		};
		Ok(Self::new_op(left, op, right))
	}

	fn new_op(left: Self, op: Op, right: Self) -> Self {
		Self::Operation {
			left: Box::new(left),
			op,
			right: Box::new(right),
		}
	}

	fn new_name(n: &'src str) -> Self {
		Self::Atom(Atom::Name(n))
	}
}

impl Error {
	fn new<T>(error: ErrorType, line: u32, column: u32, rust_src_line: u32) -> Result<T, Self> {
		Err(Self {
			error,
			line,
			column,
			rust_src_line,
		})
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
