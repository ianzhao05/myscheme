use std::cell::RefCell;
use std::error::Error;
use std::fmt;
use std::iter::Peekable;
use std::str::FromStr;

use crate::datum::*;
use crate::lexer::Token;
use crate::number::Number;

#[derive(Debug, PartialEq)]
pub enum ReaderErrorKind {
    UnexpectedToken(Token),
    UnexpectedEndOfInput,
    IllegalDot,
    InvalidNumber(String),
}

#[derive(Debug, PartialEq)]
pub struct ReaderError {
    kind: ReaderErrorKind,
}

impl ReaderError {
    pub fn new(kind: ReaderErrorKind) -> ReaderError {
        ReaderError { kind }
    }
}

impl Error for ReaderError {}

impl fmt::Display for ReaderError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            ReaderErrorKind::UnexpectedToken(t) => write!(f, "Unexpected token {t}"),
            ReaderErrorKind::UnexpectedEndOfInput => write!(f, "Unexpected end of input"),
            ReaderErrorKind::IllegalDot => write!(f, "Illegal use of dot"),
            ReaderErrorKind::InvalidNumber(n) => write!(f, "Invalid number {n}"),
        }
    }
}

pub fn read<I: Iterator<Item = Token>>(token_iter: I) -> Result<Datum, ReaderError> {
    read_impl(&mut token_iter.peekable())
}

fn read_impl<I: Iterator<Item = Token>>(tip: &mut Peekable<I>) -> Result<Datum, ReaderError> {
    let token = tip.next().ok_or(ReaderError {
        kind: ReaderErrorKind::UnexpectedEndOfInput,
    })?;
    match token {
        Token::Boolean(b) => Ok(Datum::Simple(SimpleDatum::Boolean(b))),
        Token::Character(c) => Ok(Datum::Simple(SimpleDatum::Character(c))),
        Token::String(s) => Ok(Datum::Simple(SimpleDatum::String(RefCell::new(s)))),
        Token::Identifier(s) => Ok(Datum::Simple(SimpleDatum::Symbol(s))),
        Token::Number(n) => {
            let number = Number::from_str(&n);
            match number {
                Ok(n) => Ok(Datum::Simple(SimpleDatum::Number(n))),
                Err(_) => Err(ReaderError {
                    kind: ReaderErrorKind::InvalidNumber(n),
                }),
            }
        }
        Token::LParen => {
            let mut list = Vec::new();
            let mut first = true;
            loop {
                let token = tip.peek().ok_or(ReaderError {
                    kind: ReaderErrorKind::UnexpectedEndOfInput,
                })?;
                match token {
                    Token::RParen => {
                        tip.next();
                        return if first {
                            Ok(Datum::EmptyList)
                        } else {
                            Ok(Datum::Compound(CompoundDatum::List(ListKind::Proper(list))))
                        };
                    }
                    Token::Dot => {
                        tip.next();
                        if first {
                            return Err(ReaderError {
                                kind: ReaderErrorKind::IllegalDot,
                            });
                        }
                        let datum = read_impl(tip)?;
                        let next_token = tip.next().ok_or(ReaderError {
                            kind: ReaderErrorKind::UnexpectedEndOfInput,
                        })?;
                        return if let Token::RParen = next_token {
                            Ok(Datum::Compound(CompoundDatum::List(
                                ListKind::new_improper(list, datum),
                            )))
                        } else {
                            Err(ReaderError {
                                kind: ReaderErrorKind::IllegalDot,
                            })
                        };
                    }
                    _ => list.push(read_impl(tip)?),
                }
                first = false;
            }
        }
        Token::Quote => {
            let datum = read_impl(tip)?;
            Ok(Datum::Compound(CompoundDatum::List(ListKind::Proper(
                vec![Datum::Simple(SimpleDatum::Symbol("quote".into())), datum],
            ))))
        }
        Token::Quasiquote => {
            let datum = read_impl(tip)?;
            Ok(Datum::Compound(CompoundDatum::List(ListKind::Proper(
                vec![
                    Datum::Simple(SimpleDatum::Symbol("quasiquote".into())),
                    datum,
                ],
            ))))
        }
        Token::Unquote => {
            let datum = read_impl(tip)?;
            Ok(Datum::Compound(CompoundDatum::List(ListKind::Proper(
                vec![Datum::Simple(SimpleDatum::Symbol("unquote".into())), datum],
            ))))
        }
        Token::UnquoteSplicing => {
            let datum = read_impl(tip)?;
            Ok(Datum::Compound(CompoundDatum::List(ListKind::Proper(
                vec![
                    Datum::Simple(SimpleDatum::Symbol("unquote-splicing".into())),
                    datum,
                ],
            ))))
        }
        Token::RParen => Err(ReaderError {
            kind: ReaderErrorKind::UnexpectedToken(Token::RParen),
        }),
        Token::Dot => Err(ReaderError {
            kind: ReaderErrorKind::IllegalDot,
        }),
        Token::Vector => {
            let mut vector = Vec::new();
            loop {
                let token = tip.peek().ok_or(ReaderError {
                    kind: ReaderErrorKind::UnexpectedEndOfInput,
                })?;
                if let Token::RParen = token {
                    tip.next();
                    return Ok(Datum::Compound(CompoundDatum::Vector(vector)));
                }
                vector.push(read_impl(tip)?);
            }
        }
    }
}

pub struct Reader<I: Iterator<Item = Token>> {
    token_iter: Peekable<I>,
    error: bool,
}

impl<I: Iterator<Item = Token>> Reader<I> {
    pub fn new(token_iter: I) -> Reader<I> {
        Reader {
            token_iter: token_iter.peekable(),
            error: false,
        }
    }
}

impl<I: Iterator<Item = Token>> Iterator for Reader<I> {
    type Item = Result<Datum, ReaderError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.error || self.token_iter.peek().is_none() {
            None
        } else {
            let datum = read_impl(&mut self.token_iter);
            if datum.is_err() {
                self.error = true;
            }
            Some(datum)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::*;

    #[test]
    fn simple_datum() {
        assert_eq!(
            read(vec![Token::Boolean(true)].into_iter()),
            Ok(bool_datum!(true))
        );

        assert_eq!(
            read(vec![Token::Number("123".to_owned())].into_iter()),
            Ok(int_datum!(123))
        );

        assert_eq!(
            read(vec![Token::Character('a')].into_iter()),
            Ok(char_datum!('a'))
        );

        assert_eq!(
            read(vec![Token::String("foo".to_owned())].into_iter()),
            Ok(str_datum!("foo"))
        );

        assert_eq!(
            read(vec![Token::Identifier("foo".into())].into_iter()).unwrap(),
            symbol_datum!("foo")
        );
    }

    #[test]
    fn empty_list() {
        assert_eq!(
            read(vec![Token::LParen, Token::RParen].into_iter()),
            Ok(Datum::EmptyList)
        );
    }

    #[test]
    fn compound_datum() {
        assert_eq!(
            read(
                vec![
                    Token::LParen,
                    Token::Number("123".to_owned()),
                    Token::RParen,
                ]
                .into_iter(),
            ),
            Ok(proper_list_datum![int_datum!(123)])
        );

        assert_eq!(
            read(
                vec![
                    Token::LParen,
                    Token::Number("123".to_owned()),
                    Token::Number("456".to_owned()),
                    Token::Dot,
                    Token::Identifier("bar".into()),
                    Token::RParen,
                ]
                .into_iter(),
            ),
            Ok(improper_list_datum![
                int_datum!(123), int_datum!(456);
                symbol_datum!("bar"),
            ])
        );

        assert_eq!(
            read(
                vec![
                    Token::Quote,
                    Token::LParen,
                    Token::Number("123".to_owned()),
                    Token::RParen,
                ]
                .into_iter(),
            ),
            Ok(proper_list_datum![
                symbol_datum!("quote"),
                proper_list_datum![int_datum!(123)],
            ])
        );

        assert_eq!(
            read(
                vec![
                    Token::Quasiquote,
                    Token::LParen,
                    Token::Number("123".to_owned()),
                    Token::RParen,
                ]
                .into_iter(),
            ),
            Ok(proper_list_datum![
                symbol_datum!("quasiquote"),
                proper_list_datum![int_datum!(123)],
            ])
        );

        assert_eq!(
            read(vec![Token::Unquote, Token::Number("123".to_owned())].into_iter()),
            Ok(proper_list_datum![
                symbol_datum!("unquote"),
                int_datum!(123),
            ])
        );

        assert_eq!(
            read(
                vec![
                    Token::UnquoteSplicing,
                    Token::LParen,
                    Token::Number("123".to_owned()),
                    Token::RParen,
                ]
                .into_iter(),
            ),
            Ok(proper_list_datum![
                symbol_datum!("unquote-splicing"),
                proper_list_datum![int_datum!(123)],
            ])
        );

        assert_eq!(
            read(
                vec![
                    Token::Vector,
                    Token::Number("123".to_owned()),
                    Token::Number("456".to_owned()),
                    Token::RParen,
                ]
                .into_iter(),
            ),
            Ok(vector_datum![int_datum!(123), int_datum!(456)])
        );
    }

    #[test]
    fn nested_datum() {
        assert_eq!(
            read(
                vec![
                    Token::LParen,
                    Token::LParen,
                    Token::Number("123".to_owned()),
                    Token::RParen,
                    Token::RParen,
                ]
                .into_iter(),
            ),
            Ok(proper_list_datum![proper_list_datum![int_datum!(123)]])
        );

        assert_eq!(
            read(
                vec![
                    Token::LParen,
                    Token::LParen,
                    Token::Identifier("a".into()),
                    Token::Identifier("b".into()),
                    Token::Identifier("c".into()),
                    Token::RParen,
                    Token::LParen,
                    Token::Identifier("+".into()),
                    Token::Number("1".to_owned()),
                    Token::Number("2".to_owned()),
                    Token::RParen,
                    Token::RParen,
                ]
                .into_iter(),
            ),
            Ok(proper_list_datum![
                proper_list_datum![symbol_datum!("a"), symbol_datum!("b"), symbol_datum!("c")],
                proper_list_datum![symbol_datum!("+"), int_datum!(1), int_datum!(2)],
            ])
        );

        assert_eq!(
            read(
                vec![
                    Token::LParen,
                    Token::Identifier("a".into()),
                    Token::Dot,
                    Token::LParen,
                    Token::Identifier("b".into()),
                    Token::Dot,
                    Token::Identifier("c".into()),
                    Token::RParen,
                    Token::RParen,
                ]
                .into_iter(),
            ),
            Ok(improper_list_datum![
                symbol_datum!("a"),
                symbol_datum!("b");
                symbol_datum!("c")
            ])
        );

        assert_eq!(
            read(
                vec![
                    Token::Quasiquote,
                    Token::LParen,
                    Token::Identifier("a".into()),
                    Token::Unquote,
                    Token::LParen,
                    Token::Boolean(true),
                    Token::Character('c'),
                    Token::RParen,
                    Token::Vector,
                    Token::Number("123".to_owned()),
                    Token::Number("456".to_owned()),
                    Token::Number("789".to_owned()),
                    Token::RParen,
                    Token::UnquoteSplicing,
                    Token::LParen,
                    Token::Identifier("b".into()),
                    Token::String("str".to_owned()),
                    Token::RParen,
                    Token::RParen,
                ]
                .into_iter(),
            ),
            Ok(proper_list_datum![
                symbol_datum!("quasiquote"),
                proper_list_datum![
                    symbol_datum!("a"),
                    proper_list_datum![
                        symbol_datum!("unquote"),
                        proper_list_datum![bool_datum!(true), char_datum!('c')],
                    ],
                    vector_datum![int_datum!(123), int_datum!(456), int_datum!(789)],
                    proper_list_datum![
                        symbol_datum!("unquote-splicing"),
                        proper_list_datum![symbol_datum!("b"), str_datum!("str")],
                    ],
                ]
            ])
        );
    }

    #[test]
    fn errors() {
        assert_eq!(
            read(vec![Token::LParen].into_iter()),
            Err(ReaderError {
                kind: ReaderErrorKind::UnexpectedEndOfInput
            })
        );

        assert_eq!(
            read(
                vec![
                    Token::LParen,
                    Token::Number("123".to_owned()),
                    Token::LParen,
                    Token::Number("456".to_owned()),
                    Token::RParen,
                ]
                .into_iter()
            ),
            Err(ReaderError {
                kind: ReaderErrorKind::UnexpectedEndOfInput
            })
        );

        assert_eq!(
            read(vec![Token::Quote,].into_iter()),
            Err(ReaderError {
                kind: ReaderErrorKind::UnexpectedEndOfInput
            })
        );

        assert_eq!(
            read(vec![Token::RParen].into_iter()),
            Err(ReaderError {
                kind: ReaderErrorKind::UnexpectedToken(Token::RParen)
            })
        );

        assert_eq!(
            read(vec![Token::Dot].into_iter()),
            Err(ReaderError {
                kind: ReaderErrorKind::IllegalDot
            })
        );

        assert_eq!(
            read(
                vec![
                    Token::LParen,
                    Token::Number("123".to_owned()),
                    Token::Dot,
                    Token::Number("456".to_owned()),
                    Token::Number("789".to_owned()),
                    Token::RParen,
                ]
                .into_iter()
            ),
            Err(ReaderError {
                kind: ReaderErrorKind::IllegalDot
            })
        );

        assert_eq!(
            read(
                vec![
                    Token::LParen,
                    Token::Number("123".to_owned()),
                    Token::Dot,
                    Token::RParen,
                ]
                .into_iter()
            ),
            Err(ReaderError {
                kind: ReaderErrorKind::UnexpectedToken(Token::RParen)
            })
        );

        assert_eq!(
            read(vec![Token::LParen, Token::Dot, Token::RParen,].into_iter()),
            Err(ReaderError {
                kind: ReaderErrorKind::IllegalDot
            })
        );

        assert_eq!(
            read(vec![Token::Number("1/0".to_owned())].into_iter()),
            Err(ReaderError {
                kind: ReaderErrorKind::InvalidNumber("1/0".to_owned())
            })
        );
    }

    #[test]
    fn read_iterator() {
        let tokens = vec![
            Token::LParen,
            Token::Number("123".to_owned()),
            Token::RParen,
            Token::LParen,
            Token::Number("456".to_owned()),
            Token::RParen,
        ];
        let mut reader = Reader::new(tokens.into_iter());
        assert_eq!(reader.next(), Some(Ok(proper_list_datum![int_datum!(123)])));
        assert_eq!(reader.next(), Some(Ok(proper_list_datum![int_datum!(456)])));
        assert_eq!(reader.next(), None);
    }

    #[test]
    fn read_iterator_error() {
        let tokens = vec![
            Token::LParen,
            Token::Number("123".to_owned()),
            Token::RParen,
            Token::RParen,
            Token::RParen,
        ];
        let mut reader = Reader::new(tokens.into_iter());
        assert_eq!(reader.next(), Some(Ok(proper_list_datum![int_datum!(123)])));
        assert_eq!(
            reader.next(),
            Some(Err(ReaderError {
                kind: ReaderErrorKind::UnexpectedToken(Token::RParen)
            }))
        );
        assert_eq!(reader.next(), None);
    }
}
