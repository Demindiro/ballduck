#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    Xor,
    Not,
    Eq,
    Neq,
    LessEq,
    GreaterEq,
    Less,
    Greater,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum AssignOp {
    None,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    Xor,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Token<'src> {
    Number(&'src str),
    Name(&'src str),
    String(&'src str),
    Let,
    BracketRoundOpen,
    BracketRoundClose,
    BracketSquareOpen,
    BracketSquareClose,
    BracketCurlyOpen,
    BracketCurlyClose,
    Op(Op),
    Assign(AssignOp),
    If,
    Elif,
    Else,
    While,
    For,
    In,
    Fn,
    EOL,
    Tab,
    Space,
    Return,
    Comma,
    Pass,
}

#[derive(Debug, PartialEq)]
enum TokenError {
    Empty,
    UnterminatedString,
    InvalidOp,
    InvalidAssignOp,
}

#[derive(Debug)]
pub struct TokenStream<'src> {
    tokens: Vec<(Token<'src>, usize, usize)>,
}

#[derive(Debug, PartialEq)]
pub enum TokenStreamError {
    UnterminatedString,
    InvalidOp,
    InvalidAssignOp,
}

impl Token<'_> {
    const OPERATORS: &'static str = "=+-*/%&|^!<>";
    const BRACKETS: &'static str = "()[]{}";

    fn parse(source: &str) -> Result<(Token, usize), TokenError> {
        let mut chars = source.char_indices().peekable();
        while let Some((start, c)) = chars.next() {
            return match c {
                '#' => {
                    while chars.peek() != None && chars.peek().map(|v| v.1) != Some('\n') {
                        chars.next();
                    }
                    continue;
                }
                ' ' => Ok((Token::Space, start + 1)),
                '\t' => Ok((Token::Tab, start + 1)),
                '\n' => Ok((Token::EOL, start + 1)),
                // windows pls
                '\r' if chars.peek().map(|v| v.1) == Some('\n') => Ok((Token::EOL, start + 2)),
                '(' => Ok((Token::BracketRoundOpen, start + 1)),
                ')' => Ok((Token::BracketRoundClose, start + 1)),
                '[' => Ok((Token::BracketSquareOpen, start + 1)),
                ']' => Ok((Token::BracketSquareClose, start + 1)),
                '{' => Ok((Token::BracketCurlyOpen, start + 1)),
                '}' => Ok((Token::BracketCurlyClose, start + 1)),
                ',' => Ok((Token::Comma, start + 1)),
                '"' => loop {
                    if let Some((i, c)) = chars.next() {
                        if c == '"' {
                            let s = &source[start + 1..i];
                            break Ok((Token::String(s), i + 1));
                        }
                    } else {
                        break Err(TokenError::UnterminatedString);
                    }
                },
                _ if Self::OPERATORS.contains(c) => {
                    if let Some((i, n)) = chars.next() {
                        if n == '=' {
                            let i = i + 1;
                            return match c {
                                '+' => Ok((Token::Assign(AssignOp::Add), i)),
                                '-' => Ok((Token::Assign(AssignOp::Sub), i)),
                                '*' => Ok((Token::Assign(AssignOp::Mul), i)),
                                '/' => Ok((Token::Assign(AssignOp::Div), i)),
                                '%' => Ok((Token::Assign(AssignOp::Rem), i)),
                                '&' => Ok((Token::Assign(AssignOp::And), i)),
                                '|' => Ok((Token::Assign(AssignOp::Or), i)),
                                '^' => Ok((Token::Assign(AssignOp::Xor), i)),
                                '=' => Ok((Token::Op(Op::Eq), i)),
                                '!' => Ok((Token::Op(Op::Neq), i)),
                                '<' => Ok((Token::Op(Op::LessEq), i)),
                                '>' => Ok((Token::Op(Op::GreaterEq), i)),
                                _ => Err(TokenError::InvalidAssignOp),
                            };
                        }
                    }
                    let i = start + 1;
                    match c {
                        '+' => Ok((Token::Op(Op::Add), i)),
                        '-' => Ok((Token::Op(Op::Sub), i)),
                        '*' => Ok((Token::Op(Op::Mul), i)),
                        '/' => Ok((Token::Op(Op::Div), i)),
                        '%' => Ok((Token::Op(Op::Rem), i)),
                        '&' => Ok((Token::Op(Op::And), i)),
                        '|' => Ok((Token::Op(Op::Or), i)),
                        '^' => Ok((Token::Op(Op::Xor), i)),
                        '=' => Ok((Token::Assign(AssignOp::None), i)),
                        '<' => Ok((Token::Op(Op::Less), i)),
                        '>' => Ok((Token::Op(Op::Greater), i)),
                        '!' => Ok((Token::Op(Op::Not), i)),
                        _ => unreachable!(),
                    }
                }
                _ if c.is_digit(10) => loop {
                    if let Some((i, c)) = chars.next() {
                        if !c.is_alphanumeric() && c != '_' {
                            let s = &source[start..i];
                            break Ok((Token::Number(s), i));
                        }
                    } else {
                        let s = &source[start..];
                        break Ok((Token::Number(s), source.len()));
                    }
                },
                _ => {
                    let (s, i) = loop {
                        if let Some((i, c)) = chars.next() {
                            if c.is_whitespace()
                                || Self::OPERATORS.contains(c)
                                || Self::BRACKETS.contains(c)
                                || c == ','
                            {
                                break (&source[start..i], i);
                            }
                        } else {
                            break (&source[start..], source.len());
                        }
                    };
                    Ok((
                        match s {
                            "if" => Token::If,
                            "else" => Token::Else,
                            "elif" => Token::Elif,
                            "while" => Token::While,
                            "for" => Token::For,
                            "in" => Token::In,
                            "let" => Token::Let,
                            "fn" => Token::Fn,
                            "return" => Token::Return,
                            "pass" => Token::Pass,
                            _ => Token::Name(s),
                        },
                        i,
                    ))
                }
            };
        }
        Err(TokenError::Empty)
    }
}

impl<'src> TokenStream<'src> {
    pub fn parse(mut source: &'src str) -> Result<Self, TokenStreamError> {
        let mut line = 1;
        let mut column = 1;
        let mut tokens = Vec::new();
        loop {
            match Token::parse(source) {
                Ok((tk, len)) => {
                    if tk == Token::EOL {
                        line += 1;
                        column = 0;
                    }
                    column += len;
                    tokens.push((tk, line, column));
                    source = &source[len..];
                }
                Err(e) => {
                    break match e {
                        TokenError::Empty => {
                            tokens.shrink_to_fit();
                            Ok(Self { tokens })
                        }
                        TokenError::InvalidOp => Err(TokenStreamError::InvalidOp),
                        TokenError::InvalidAssignOp => Err(TokenStreamError::InvalidAssignOp),
                        TokenError::UnterminatedString => Err(TokenStreamError::UnterminatedString),
                    }
                }
            }
        }
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = (Token<'src>, usize, usize)> + '_ {
        self.tokens.iter().cloned()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    mod token {
        use super::*;

        #[test]
        fn empty() {
            assert_eq!(Token::parse(""), Err(TokenError::Empty));
            assert_eq!(Token::parse("# This is a comment"), Err(TokenError::Empty));
        }

        #[test]
        fn number() {
            assert_eq!(Token::parse("0"), Ok((Token::Number("0"), 1)));
            assert_eq!(Token::parse("42_i32"), Ok((Token::Number("42_i32"), 6)));
            assert_eq!(Token::parse("0b10101"), Ok((Token::Number("0b10101"), 7)));
        }

        #[test]
        fn string() {
            assert_eq!(
                Token::parse("\"foo bar 42\""),
                Ok((Token::String("foo bar 42"), 12))
            );
        }

        #[test]
        fn control() {
            assert_eq!(Token::parse("if"), Ok((Token::If, 2)));
            assert_eq!(Token::parse("else"), Ok((Token::Else, 4)));
            assert_eq!(Token::parse("elif"), Ok((Token::Elif, 4)));
            assert_eq!(Token::parse("while"), Ok((Token::While, 5)));
            assert_eq!(Token::parse("for"), Ok((Token::For, 3)));
            assert_eq!(Token::parse("in"), Ok((Token::In, 2)));
            assert_eq!(Token::parse("return"), Ok((Token::Return, 6)));
        }

        #[test]
        fn brackets() {
            assert_eq!(Token::parse("("), Ok((Token::BracketRoundOpen, 1)));
            assert_eq!(Token::parse(")"), Ok((Token::BracketRoundClose, 1)));
            assert_eq!(Token::parse("["), Ok((Token::BracketSquareOpen, 1)));
            assert_eq!(Token::parse("]"), Ok((Token::BracketSquareClose, 1)));
            assert_eq!(Token::parse("{"), Ok((Token::BracketCurlyOpen, 1)));
            assert_eq!(Token::parse("}"), Ok((Token::BracketCurlyClose, 1)));
        }

        #[test]
        fn op() {
            assert_eq!(Token::parse("+"), Ok((Token::Op(Op::Add), 1)));
            assert_eq!(Token::parse("-"), Ok((Token::Op(Op::Sub), 1)));
            assert_eq!(Token::parse("*"), Ok((Token::Op(Op::Mul), 1)));
            assert_eq!(Token::parse("/"), Ok((Token::Op(Op::Div), 1)));
            assert_eq!(Token::parse("%"), Ok((Token::Op(Op::Rem), 1)));
            assert_eq!(Token::parse("&"), Ok((Token::Op(Op::And), 1)));
            assert_eq!(Token::parse("|"), Ok((Token::Op(Op::Or), 1)));
            assert_eq!(Token::parse("^"), Ok((Token::Op(Op::Xor), 1)));
            assert_eq!(Token::parse("!"), Ok((Token::Op(Op::Not), 1)));
            assert_eq!(Token::parse("<"), Ok((Token::Op(Op::Less), 1)));
            assert_eq!(Token::parse(">"), Ok((Token::Op(Op::Greater), 1)));
            assert_eq!(Token::parse("!="), Ok((Token::Op(Op::Neq), 2)));
            assert_eq!(Token::parse("<="), Ok((Token::Op(Op::LessEq), 2)));
            assert_eq!(Token::parse(">="), Ok((Token::Op(Op::GreaterEq), 2)));
        }

        #[test]
        fn assign_op() {
            assert_eq!(Token::parse("="), Ok((Token::Assign(AssignOp::None), 1)));
            assert_eq!(Token::parse("+="), Ok((Token::Assign(AssignOp::Add), 2)));
            assert_eq!(Token::parse("-="), Ok((Token::Assign(AssignOp::Sub), 2)));
            assert_eq!(Token::parse("*="), Ok((Token::Assign(AssignOp::Mul), 2)));
            assert_eq!(Token::parse("/="), Ok((Token::Assign(AssignOp::Div), 2)));
            assert_eq!(Token::parse("%="), Ok((Token::Assign(AssignOp::Rem), 2)));
            assert_eq!(Token::parse("&="), Ok((Token::Assign(AssignOp::And), 2)));
            assert_eq!(Token::parse("|="), Ok((Token::Assign(AssignOp::Or), 2)));
            assert_eq!(Token::parse("^="), Ok((Token::Assign(AssignOp::Xor), 2)));
        }

        #[test]
        fn declare() {
            assert_eq!(Token::parse("let foo"), Ok((Token::Let, 3)));
        }

        #[test]
        fn name() {
            assert_eq!(Token::parse("foo"), Ok((Token::Name("foo"), 3)));
            assert_eq!(Token::parse("_4343"), Ok((Token::Name("_4343"), 5)));
            assert_eq!(Token::parse("hunter2"), Ok((Token::Name("hunter2"), 7)));
        }

        #[test]
        fn other() {
            assert_eq!(Token::parse(" "), Ok((Token::Space, 1)));
            assert_eq!(Token::parse("\t"), Ok((Token::Tab, 1)));
            assert_eq!(Token::parse("\n"), Ok((Token::EOL, 1)));
            assert_eq!(Token::parse("\n"), Ok((Token::EOL, 1)));
            assert_eq!(Token::parse("\r\n"), Ok((Token::EOL, 2)));
            assert_eq!(Token::parse(","), Ok((Token::Comma, 1)));
            assert_eq!(Token::parse("pass"), Ok((Token::Pass, 1)));
        }
    }

    mod stream {
        use super::*;

        #[test]
        fn hello_world() {
            let src = "fn main()\n\tprintln(\"Hello, world!\")";
            let s = TokenStream::parse(src).expect("Failed to parse source");
            let mut s = s.iter().map(|(v, _, _)| v);
            assert_eq!(s.next(), Some(Token::Fn));
            assert_eq!(s.next(), Some(Token::Space));
            assert_eq!(s.next(), Some(Token::Name("main")));
            assert_eq!(s.next(), Some(Token::BracketRoundOpen));
            assert_eq!(s.next(), Some(Token::BracketRoundClose));
            assert_eq!(s.next(), Some(Token::EOL));
            assert_eq!(s.next(), Some(Token::Tab));
            assert_eq!(s.next(), Some(Token::Name("println")));
            assert_eq!(s.next(), Some(Token::BracketRoundOpen));
            assert_eq!(s.next(), Some(Token::String("Hello, world!")));
            assert_eq!(s.next(), Some(Token::BracketRoundClose));
            assert_eq!(s.next(), None);
        }

        #[test]
        fn vector_len() {
            let src = "fn vec2_len(x, y)\n\treturn x * x + y * y";
            let s = TokenStream::parse(src).expect("Failed to parse source");
            let mut s = s.iter().map(|(v, _, _)| v);
            assert_eq!(s.next(), Some(Token::Fn));
            assert_eq!(s.next(), Some(Token::Space));
            assert_eq!(s.next(), Some(Token::Name("vec2_len")));
            assert_eq!(s.next(), Some(Token::BracketRoundOpen));
            assert_eq!(s.next(), Some(Token::Name("x")));
            assert_eq!(s.next(), Some(Token::Comma));
            assert_eq!(s.next(), Some(Token::Space));
            assert_eq!(s.next(), Some(Token::Name("y")));
            assert_eq!(s.next(), Some(Token::BracketRoundClose));
            assert_eq!(s.next(), Some(Token::EOL));
            assert_eq!(s.next(), Some(Token::Tab));
            assert_eq!(s.next(), Some(Token::Return));
            assert_eq!(s.next(), Some(Token::Space));
            assert_eq!(s.next(), Some(Token::Name("x")));
            assert_eq!(s.next(), Some(Token::Space));
            assert_eq!(s.next(), Some(Token::Op(Op::Mul)));
            assert_eq!(s.next(), Some(Token::Space));
            assert_eq!(s.next(), Some(Token::Name("x")));
            assert_eq!(s.next(), Some(Token::Space));
            assert_eq!(s.next(), Some(Token::Op(Op::Add)));
            assert_eq!(s.next(), Some(Token::Space));
            assert_eq!(s.next(), Some(Token::Name("y")));
            assert_eq!(s.next(), Some(Token::Space));
            assert_eq!(s.next(), Some(Token::Op(Op::Mul)));
            assert_eq!(s.next(), Some(Token::Space));
            assert_eq!(s.next(), Some(Token::Name("y")));
            assert_eq!(s.next(), None);
        }
    }
}
