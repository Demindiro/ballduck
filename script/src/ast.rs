use crate::tokenizer::*;
use core::ops::BitOrAssign;

type Integer = isize;
type Real = f64;

#[derive(Debug)]
pub struct Script<'src> {
    functions: Vec<Function<'src>>,
    variables: Vec<&'src str>,
}

#[derive(Debug)]
struct Function<'src> {
    name: &'src str,
    parameters: Vec<&'src str>,
	lines: Lines<'src>,
}

#[derive(Debug)]
struct Lines<'src> {
	lines: Vec<Statement<'src>>,
}

#[derive(Debug)]
enum Statement<'src> {
	Declare { var: &'src str },
	Assign { var: &'src str, assign_op: AssignOp, expr: Expression<'src> },
	Call { func: &'src str, args: Vec<Expression<'src>>, },
}

#[derive(Debug)]
enum Atom<'src> {
    Name(&'src str),
    Real(Real),
    Integer(Integer),
	String(&'src str),
}

#[derive(Debug)]
enum Expression<'src> {
    Atom(Atom<'src>),
    Name(&'src str),
    Operation {
        op: Op,
        left: Box<Expression<'src>>,
        right: Box<Expression<'src>>,
    },
    Function {
        name: &'src str,
        arguments: Vec<Expression<'src>>,
    },
}

#[derive(Debug)]
pub struct Error {
    error: ErrorType,
    line: usize,
    column: usize,
}

#[derive(Debug)]
pub enum ErrorType {
    UnexpectedIndent,
    UnexpectedToken,
	UnexpectedEOL,
	UnexpectedSpace,
	Noop,
}

macro_rules! err {
	($err:ident, $line:expr, $column:expr) => {
		return Error::new(ErrorType::$err, $line, $column);
	};
}

fn skip_whitespace<'s>(iter: &mut impl Iterator<Item = (Token<'s>, usize, usize)>) -> Option<(Token<'s>, usize, usize)> {
	while let Some((tk, line, column)) = iter.next() {
		if tk != Token::Space && tk != Token::Tab {
			return Some((tk, line, column));
		}
	}
	None
}

impl<'src> Script<'src> {
    pub fn parse(tokens: TokenStream<'src>) -> Result<Self, Error> {
        let mut functions = Vec::new();
        let mut variables = Vec::new();
        let mut iter = tokens.iter();
        let mut indent = false;
        while let Some((tk, line, column)) = iter.next() {
            match tk {
                Token::EOL => indent = false,
                Token::Space | Token::Tab => indent = true,
				Token::Let => {
                    if indent {
                        err!(UnexpectedIndent, line, column);
                    } else {
						let name = loop {
							match skip_whitespace(&mut iter) {
								Some((Token::Name(s), _, _)) => break s,
								Some((_, l, c)) => err!(UnexpectedToken, l, c),
								None => err!(UnexpectedEOL, line, column),
							}
						};
						variables.push(name);
                    }
				}
                Token::Fn => {
                    if indent {
                        err!(UnexpectedIndent, line, column);
                    } else {
						match Function::parse(&mut iter) {
							Ok(f) => functions.push(f),
							Err(f) => return Err(f),
						}
                    }
                }
                _ => err!(UnexpectedToken, line, column),
            }
        }
        Ok(Self {
            functions,
            variables,
        })
    }
}

impl<'src> Function<'src> {
	fn parse(tokens: &mut impl Iterator<Item = (Token<'src>, usize, usize)>) -> Result<Self, Error> {
		let name = match skip_whitespace(tokens) {
			Some((Token::Name(name), _, _)) => name,
			Some((_, l, c)) => err!(UnexpectedToken, l, c),
			None => err!(UnexpectedEOL, 0, 0),
		};
		match skip_whitespace(tokens) {
			Some((Token::BracketRoundOpen, _, _)) => (),
			Some((_, l, c)) => err!(UnexpectedToken, l, c),
			None => err!(UnexpectedEOL, 0, 0),
		}

		let parameters = Vec::new();
		loop {
			match skip_whitespace(tokens) {
				Some((Token::BracketRoundClose, _, _)) => break,
				_ => todo!(),
			}
		}

		match skip_whitespace(tokens) {
			Some((Token::EOL, _, _)) => (),
			Some((_, l, c)) => err!(UnexpectedToken, l, c),
			None => err!(UnexpectedEOL, 0, 0),
		}
		let mut tab_count = 0;
		let (mut tk, mut line, mut column) = loop {
			match tokens.next() {
				Some((Token::Tab, _, _)) => tab_count += 1,
				Some(e) => break e,
				None => err!(UnexpectedEOL, 0, 0),
			}
		};
		let tab_count = tab_count;

		let mut lines = Vec::new();
		let mut curr_tabs = 0;
		loop {
			match tk {
				Token::Space => err!(UnexpectedSpace, line, column),
				Token::Tab => curr_tabs += 1,
				Token::EOL => curr_tabs = 0,
				Token::Name(name) => {
					let mut args = Vec::new();
					match skip_whitespace(tokens) {
						Some((Token::EOL, ..)) => break,
						Some((Token::BracketRoundOpen, l, c)) => {
							match skip_whitespace(tokens) {
								Some((Token::BracketRoundClose, ..)) => (),
								Some((pre, ..)) => {
									loop {
										let (expr, last_tk) = Expression::parse(pre, tokens)?;
										args.push(expr);
										match last_tk {
											Token::Comma => (),
											Token::BracketRoundClose => break,
											tk => panic!("Expression did not parse all tokens: {:?}", tk),
										}
									}
									lines.push(Statement::Call { func: name, args });
								}
								None => err!(UnexpectedEOL, l, c),
							}
						}
						Some(e) => { dbg!(e); todo!() },
						None => err!(UnexpectedEOL, 0, 0),
					}
				},
				_ => todo!(),
			}
		}

		let lines = Lines { lines };
		Ok(Self { name, parameters, lines })
	}
}

impl<'src> Expression<'src> {
	fn parse(pre: Token<'src>, tokens: &mut impl Iterator<Item = (Token<'src>, usize, usize)>) -> Result<(Self, Token<'src>), Error> {
		let (lhs, last_tk) = match pre {
			Token::BracketRoundOpen => {
				match skip_whitespace(tokens) {
					Some((pre, ..)) => Self::parse(pre, tokens).map(|(e, t)| (e, Some(t)))?,
					None => err!(UnexpectedEOL, 0, 0),
				}
			},
			Token::String(s) => (Expression::Atom(Atom::String(s)), None),
			e => { dbg!(e); todo!() },
		};
		match skip_whitespace(tokens) {
			Some((Token::BracketRoundClose, ..)) => return Ok((lhs, Token::BracketRoundClose)),
			Some((Token::Comma, ..)) => return Ok((lhs, Token::Comma)),
			_ => todo!(),
		}
		/*
		let lhs = match skip_whitespace(tokens) {
			Some((Token::BracketRoundOpen, ..)) => Self::parse(tokens)?,
			Some((Token::String(s), ..)) => Expression::Atom(Atom::String(s)),
			Some(e) => { dbg!(e); todo!() },
			None => todo!(),
		};
		*/
	}
}

impl Error {
    fn new<T>(error: ErrorType, line: usize, column: usize) -> Result<T, Self> {
        Err(Self {
            error,
            line,
            column,
        })
    }
}
