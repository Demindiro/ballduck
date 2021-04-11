use crate::tokenizer::*;
use core::str::FromStr;

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
}

#[derive(Debug)]
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
    NotANumber,
}

macro_rules! err {
    ($err:ident, $line:expr, $column:expr) => {
        return Error::new(ErrorType::$err, $line, $column);
    };
}

fn skip_whitespace<'s>(
    iter: &mut impl Iterator<Item = (Token<'s>, usize, usize)>,
) -> Option<(Token<'s>, usize, usize)> {
    while let Some((tk, line, column)) = iter.next() {
        if tk != Token::Space && tk != Token::Tab {
            return Some((tk, line, column));
        }
    }
    None
}

impl<'src> Script<'src> {
    pub(crate) fn parse(tokens: TokenStream<'src>) -> Result<Self, Error> {
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

type TokenGroup<'src> = (Token<'src>, usize, usize);

impl<'src> Function<'src> {
    fn parse(tokens: &mut impl Iterator<Item = TokenGroup<'src>>) -> Result<Self, Error> {
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

        // Ensure there is one and only one tab
        let (l, c) = match tokens.next() {
            Some((Token::Tab, l, c)) => (l, c),
            Some((e, l, c)) => err!(UnexpectedToken, l, c),
            None => err!(UnexpectedEOL, 0, 0),
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
        mut line: usize,
        mut column: usize,
    ) -> Result<(Lines<'src>, u8), Error> {
        let mut lines = Lines::new();
        loop {
            match tokens.next() {
                Some((Token::EOL, ..)) => {
                    for i in 0..expected_indent {
                        if let Some((tk, l, c)) = tokens.next() {
                            if tk == Token::Space {
                                err!(UnexpectedToken, l, c);
                            } else if tk != Token::Tab {
                                return Ok((lines, i));
                            }
                        }
                    }
                }
                Some((Token::Tab, l, c)) => err!(UnexpectedToken, l, c),
                Some((Token::Name(name), ll, lc)) => {
                    let mut args = Vec::new();
                    match skip_whitespace(tokens) {
                        Some((Token::EOL, ..)) => continue,
                        Some((Token::BracketRoundOpen, l, c)) => match skip_whitespace(tokens) {
                            Some((Token::BracketRoundClose, ..)) => (),
                            Some((pre, ..)) => {
                                loop {
                                    let (expr, last_tk) = Expression::parse(pre, tokens)?;
                                    args.push(expr);
                                    match last_tk {
                                        Token::Comma => (),
                                        Token::BracketRoundClose => break,
                                        tk => {
                                            panic!("Expression did not parse all tokens: {:?}", tk)
                                        }
                                    }
                                }
                                lines.push(Statement::Call { func: name, args });
                            }
                            None => err!(UnexpectedEOL, l, c),
                        },
                        Some(e) => {
                            dbg!(e);
                            todo!()
                        }
                        None => err!(UnexpectedEOL, ll, lc),
                    }
                }
                Some((Token::For, mut ll, mut lc)) => {
                    let var = match skip_whitespace(tokens) {
                        Some((Token::Name(n), ..)) => n,
                        Some((_, l, c)) => err!(UnexpectedToken, l, c),
                        None => err!(UnexpectedEOL, line, column),
                    };
                    match skip_whitespace(tokens) {
                        Some((Token::In, ..)) => (),
                        Some((_, l, c)) => err!(UnexpectedToken, l, c),
                        None => err!(UnexpectedEOL, line, column),
                    }
                    let (expr, tk) = match skip_whitespace(tokens) {
                        Some((tk, ..)) => Expression::parse(tk, tokens)?,
                        None => err!(UnexpectedEOL, line, column),
                    };
                    if tk == Token::EOL {
                        let expected_indent = expected_indent + 1;
                        'eol: loop {
                            for i in 0..expected_indent {
                                match tokens.next() {
                                    Some((Token::Tab, l, c)) => {
                                        ll = l;
                                        lc = c;
                                    }
                                    Some((Token::EOL, l, c)) => {
                                        ll = l;
                                        lc = c;
                                        continue 'eol;
                                    }
                                    Some((_, l, c)) => err!(UnexpectedToken, l, c),
                                    None => err!(UnexpectedEOL, ll, lc),
                                }
                            }
                            break;
                        }
                        lines.push(Statement::For {
                            var,
                            expr,
                            lines: Self::parse_block(tokens, expected_indent, ll, lc)?.0,
                        });
                    } else {
                        err!(UnexpectedToken, 0, 0);
                    }
                }
                Some((Token::Pass, ..)) => (),
                Some((tk, ..)) => {
                    dbg!(tk);
                    todo!()
                }
                None => return Ok((lines, 0)),
            };
        }
    }
}

impl<'src> Expression<'src> {
    fn parse(
        pre: Token<'src>,
        tokens: &mut impl Iterator<Item = (Token<'src>, usize, usize)>,
    ) -> Result<(Self, Token<'src>), Error> {
        let (lhs, last_tk) = match pre {
            Token::BracketRoundOpen => match skip_whitespace(tokens) {
                Some((pre, ..)) => Self::parse(pre, tokens).map(|(e, t)| (e, Some(t)))?,
                None => err!(UnexpectedEOL, 0, 0),
            },
            Token::String(s) => (Expression::Atom(Atom::String(s)), None),
            Token::Number(n) => (
                Expression::Atom(if let Ok(n) = parse_integer(n) {
                    Atom::Integer(n)
                } else if let Ok(n) = Real::from_str(n) {
                    Atom::Real(n)
                } else {
                    dbg!(n);
                    err!(NotANumber, 0, 0);
                }),
                None,
            ),
            e => {
                dbg!(e);
                todo!()
            }
        };
        match skip_whitespace(tokens) {
            Some((Token::BracketRoundClose, ..)) => return Ok((lhs, Token::BracketRoundClose)),
            Some((Token::Comma, ..)) => return Ok((lhs, Token::Comma)),
            Some((Token::EOL, ..)) => return Ok((lhs, Token::EOL)),
            Some((tk, ..)) => todo!("{:?}", tk),
            None => todo!("none"),
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

enum NumberParseError {
    InvalidBase,
    InvalidDigit,
    Empty,
}

/// Custom integer parsing function that allows underscores
fn parse_integer(s: &str) -> Result<Integer, NumberParseError> {
    let mut chars = s.chars();
    let (mut chars, base) = if chars.next() == Some('0') {
        if let Some(c) = chars.next() {
            let b = match c {
                'x' => 16,
                'b' => 2,
                'o' => 8,
                _ => return Err(NumberParseError::InvalidBase),
            };
            (chars, b)
        } else {
            return Ok(0);
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
        let mut n = 0;
        for c in chars.filter(|&c| c != '_') {
            n *= base as Integer;
            n += c.to_digit(base).ok_or(NumberParseError::InvalidDigit)? as isize;
        }
        Ok(if neg { -n } else { n })
    }
}
