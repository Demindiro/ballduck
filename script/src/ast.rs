use crate::tokenizer::*;

type Integer = isize;
type Real = f64;

#[derive(Debug)]
pub(crate) struct Script<'src> {
	pub functions: Vec<Function<'src>>,
	pub variables: Vec<&'src str>,
}

#[derive(Debug)]
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
	Call {
		func: &'src str,
		args: Vec<Expression<'src>>,
	},
	For {
		var: &'src str,
		expr: Expression<'src>,
		lines: Lines<'src>,
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
	Name(&'src str),
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
	UnexpectedEOF,
	Noop,
	NotANumber,
}

macro_rules! err {
	($err:ident, $line:expr, $column:expr) => {
		return Error::new(ErrorType::$err, $line, $column, line!());
	};
	(UnexpectedToken, $err_val:expr, $line:expr, $column:expr) => {{
		let tk = format!("{:?}", $err_val);
		return Error::new(ErrorType::UnexpectedToken(tk), $line, $column, line!());
	}};
	($err:ident, $err_val:expr, $line:expr, $column:expr) => {
		return Error::new(ErrorType::$err($err_val), $line, $column, line!());
	};
}

impl<'src> Script<'src> {
	pub(crate) fn parse(tokens: TokenStream<'src>) -> Result<Self, Error> {
		let mut functions = Vec::new();
		let mut variables = Vec::new();
		let mut iter = tokens.iter();
		while let Some((tk, line, column)) = iter.next() {
			match tk {
				Token::Let => match iter.next() {
					Some((Token::Name(name), ..)) => variables.push(name),
					Some((_, l, c)) => todo!("none"),
					None => todo!("none"),
				},
				Token::Fn => match Function::parse(&mut iter) {
					Ok(f) => functions.push(f),
					Err(f) => return Err(f),
				},
				Token::Indent(0) => (),
				Token::Indent(i) => err!(UnexpectedIndent, i, line, column),
				_ => err!(UnexpectedToken, tk, line, column),
			}
		}
		Ok(Self {
			functions,
			variables,
		})
	}
}

type TokenGroup<'src> = (Token<'src>, u32, u32);

impl<'src> Function<'src> {
	fn parse(tokens: &mut impl Iterator<Item = TokenGroup<'src>>) -> Result<Self, Error> {
		let name = match tokens.next() {
			Some((Token::Name(name), _, _)) => name,
			Some((tk, l, c)) => err!(UnexpectedToken, tk, l, c),
			None => err!(UnexpectedEOF, 0, 0),
		};
		match tokens.next() {
			Some((Token::BracketRoundOpen, _, _)) => (),
			Some((tk, l, c)) => err!(UnexpectedToken, tk, l, c),
			None => err!(UnexpectedEOF, 0, 0),
		}

		let mut parameters = Vec::new();
		loop {
			match tokens.next() {
				Some((Token::BracketRoundClose, _, _)) => break,
				Some((Token::Name(a), l, c)) => {
					parameters.push(a);
					match tokens.next() {
						Some((Token::BracketRoundClose, ..)) => break,
						Some((Token::Comma, ..)) => (),
						e => todo!("{:?}", e),
					}
				}
				e => todo!("{:?}", e),
			}
		}

		// Ensure there is one and only one tab
		let (l, c) = match tokens.next() {
			Some((Token::Indent(i), l, c)) if i == 1 => (l, c),
			Some((Token::Indent(i), l, c)) => err!(UnexpectedIndent, i, l, c),
			Some((tk, l, c)) => err!(UnexpectedToken, tk, l, c),
			None => err!(UnexpectedEOF, 0, 0),
		};

		Ok(Self {
			name,
			parameters,
			lines: Self::parse_block(tokens, 1, l, c)?.0,
		})
	}

	fn parse_block(
		tokens: &mut impl Iterator<Item = TokenGroup<'src>>,
		expected_indent: u8,
		mut line: u32,
		mut column: u32,
	) -> Result<(Lines<'src>, u8), Error> {
		let mut lines = Lines::new();
		loop {
			match tokens.next() {
				Some((Token::Name(name), ll, lc)) => {
					let mut args = Vec::new();
					match tokens.next() {
						Some((Token::BracketRoundOpen, l, c)) => match tokens.next() {
							Some((Token::BracketRoundClose, ..)) => (),
							Some((mut pre, ..)) => {
								loop {
									let (expr, last_tk) = Expression::parse(pre, tokens)?;
									args.push(expr);
									match last_tk {
										Some(Token::Comma) => (),
										Some(Token::BracketRoundClose) => break,
										None => err!(UnexpectedEOF, 0, 0),
										tk => {
											panic!("Expression did not parse all tokens: {:?}", tk)
										}
									}
									pre = if let Some((tk, ..)) = tokens.next() {
										tk
									} else {
										err!(UnexpectedEOF, 0, 0);
									};
								}
								lines.push(Statement::Call { func: name, args });
							}
							None => err!(UnexpectedEOF, l, c),
						},
						Some(e) => {
							dbg!(e);
							todo!()
						}
						None => err!(UnexpectedEOF, ll, lc),
					}
				}
				Some((Token::For, mut ll, mut lc)) => {
					let var = match tokens.next() {
						Some((Token::Name(n), ..)) => n,
						Some((tk, l, c)) => err!(UnexpectedToken, tk, l, c),
						None => err!(UnexpectedEOF, line, column),
					};
					match tokens.next() {
						Some((Token::In, ..)) => (),
						Some((tk, l, c)) => err!(UnexpectedToken, tk, l, c),
						None => err!(UnexpectedEOF, line, column),
					}
					let (expr, tk) = match tokens.next() {
						Some((tk, ..)) => Expression::parse(tk, tokens)?,
						None => err!(UnexpectedEOF, line, column),
					};
					if let Some(Token::Indent(expected_indent)) = tk {
						lines.push(Statement::For {
							var,
							expr,
							lines: Self::parse_block(tokens, expected_indent, ll, lc)?.0,
						});
					} else {
						err!(UnexpectedToken, tk, 0, 0);
					}
				}
				Some((Token::Pass, ..)) => (),
				Some((Token::Return, l, c)) => {
					if let Some((tk, l, c)) = tokens.next() {
						let expr = match Expression::parse(tk, tokens) {
							Ok(expr) => Some(expr.0),
							e => todo!("{:?}", e),
						};
						lines.push(Statement::Return { expr });
					} else {
						err!(UnexpectedEOF, l, c);
					}
				}
				Some((Token::Indent(0), ..)) | None => return Ok((lines, 0)),
				Some((Token::Indent(i), ..)) if i < expected_indent => return Ok((lines, 0)),
				Some((Token::Indent(i), ..)) if i == expected_indent => (),
				Some((Token::Indent(i), l, c)) => err!(UnexpectedIndent, i, l, c),
				Some((tk, ..)) => todo!("{:?}", tk),
			};
		}
	}
}

impl<'src> Expression<'src> {
	fn parse(
		pre: Token<'src>,
		tokens: &mut impl Iterator<Item = (Token<'src>, u32, u32)>,
	) -> Result<(Self, Option<Token<'src>>), Error> {
		let (lhs, last_tk) = match pre {
			Token::BracketRoundOpen => match tokens.next() {
				Some((pre, ..)) => Self::parse(pre, tokens).map(|(e, t)| (e, t))?,
				None => err!(UnexpectedEOF, 0, 0),
			},
			Token::String(s) => (Expression::Atom(Atom::String(s)), None),
			Token::Number(n) => (
				Expression::Atom(if let Ok(n) = parse_integer(n) {
					n
				} else {
					err!(NotANumber, 0, 0);
				}),
				None,
			),
			Token::Name(name) => match tokens.next() {
				Some((Token::BracketRoundOpen, ..)) => {
					let mut arguments = Vec::new();
					loop {
						match tokens.next() {
							Some((Token::Number(n), l, c)) => {
								if let Ok(n) = parse_integer(n) {
									arguments.push(Expression::Atom(n));
								} else {
									err!(NotANumber, l, c);
								}
							}
							e => todo!("{:?}", e),
						}
						match tokens.next() {
							Some((Token::Comma, ..)) => (),
							Some((Token::BracketRoundClose, ..)) => break,
							Some((tk, l, c)) => err!(UnexpectedToken, tk, l, c),
							None => err!(UnexpectedEOF, 0, 0),
						}
					}
					let expr = None;
					(
						Expression::Function {
							expr,
							name,
							arguments,
						},
						None,
					)
				}
				Some((tk, ..)) => (Expression::Atom(Atom::Name(name)), Some(tk)),
				e => todo!("{:?}", e),
			},
			e => todo!("{:?}", e),
		};
		if let Some(last_tk) = last_tk {
			match last_tk {
				Token::Op(opl) => match tokens.next() {
					Some((Token::Name(mid), ..)) => {
						let og_mid = mid;
						let mid = Expression::Atom(Atom::Name(mid));
						match tokens.next() {
							Some((Token::Op(opr), ..)) => match tokens.next() {
								Some((Token::Name(rhs), ..)) => {
									let (left, op, right) = if opl >= opr {
										let rhs = Expression::parse(Token::Name(rhs), tokens)?.0;
										let lhs = Expression::Operation {
											op: opl,
											left: Box::new(lhs),
											right: Box::new(mid),
										};
										(lhs, opr, rhs)
									} else {
										let rhs = Expression::Atom(Atom::Name(rhs));
										let rhs = Expression::Operation {
											op: opr,
											left: Box::new(mid),
											right: Box::new(rhs),
										};
										(lhs, opl, rhs)
									};
									let (left, right) = (Box::new(left), Box::new(right));
									Ok((
										Expression::Operation { left, op, right },
										tokens.next().map(|v| v.0),
									))
								}
								e => todo!("{:?}", e),
							},
							Some((Token::BracketRoundOpen, ..)) => {
								// FIXME handle function args & check for ending brace
								tokens.next();
								match opl {
									Op::Access => Ok((
										Expression::Function {
											expr: Some(Box::new(lhs)),
											name: og_mid,
											arguments: Vec::new(),
										},
										Some(Token::BracketRoundClose),
									)),
									e => todo!("{:?}", e),
								}
							}
							Some((tk, ..))
								if tk == Token::BracketRoundClose || tk == Token::Indent(0) =>
							{
								Ok((
									Expression::Operation {
										left: Box::new(lhs),
										op: opl,
										right: Box::new(mid),
									},
									Some(tk),
								))
							}
							None => Ok((
								Expression::Operation {
									left: Box::new(lhs),
									op: opl,
									right: Box::new(mid),
								},
								None,
							)),
							e => todo!("{:?}", e),
						}
					}
					e => todo!("{:?}", e),
				},
				e => todo!("{:?}", e),
			}
		} else {
			match tokens.next() {
				Some((Token::BracketRoundClose, ..)) => {
					return Ok((lhs, Some(Token::BracketRoundClose)))
				}
				Some((Token::Comma, ..)) => return Ok((lhs, Some(Token::Comma))),
				Some((Token::Indent(i), ..)) => return Ok((lhs, Some(Token::Indent(i)))),
				e => todo!("{:?}", e),
			}
		}
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
fn parse_integer(s: &str) -> Result<Atom, NumberParseError> {
	let mut chars = s.chars();
	let (mut chars, base) = if chars.next() == Some('0') {
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
		assert_eq!(parse_integer("0"), Ok(Atom::Integer(0)));
		assert_eq!(parse_integer("32"), Ok(Atom::Integer(32)));
		assert_eq!(parse_integer("0.0"), Ok(Atom::Real(0.0)));
		match parse_integer("13.37") {
			Ok(Atom::Real(f)) => assert!((f - 13.37).abs() <= Real::EPSILON * 13.37),
			r => panic!("{:?}", r),
		}
		assert_eq!(
			parse_integer("."),
			Err(NumberParseError::SeparatorInWrongPosition)
		);
		assert_eq!(
			parse_integer("0."),
			Err(NumberParseError::SeparatorInWrongPosition)
		);
		assert_eq!(
			parse_integer(".0"),
			Err(NumberParseError::SeparatorInWrongPosition)
		);
	}
}
