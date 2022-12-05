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
}

#[derive(Debug, PartialEq)]
pub struct ReaderError {
    kind: ReaderErrorKind,
}

impl Error for ReaderError {}

impl fmt::Display for ReaderError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            ReaderErrorKind::UnexpectedToken(t) => write!(f, "Unexpected token {t}"),
            ReaderErrorKind::UnexpectedEndOfInput => write!(f, "Unexpected end of input"),
            ReaderErrorKind::IllegalDot => write!(f, "Illegal use of dot"),
        }
    }
}

pub fn read<'a, I: Iterator<Item = &'a Token>>(token_iter: I) -> Result<Datum, ReaderError> {
    read_impl(&mut token_iter.peekable())
}

fn read_impl<'a, I: Iterator<Item = &'a Token>>(
    tip: &mut Peekable<I>,
) -> Result<Datum, ReaderError> {
    let token = tip.next().unwrap();
    match token {
        Token::Boolean(b) => Ok(Datum::Simple(SimpleDatumKind::Boolean(*b))),
        Token::Character(c) => Ok(Datum::Simple(SimpleDatumKind::Character(*c))),
        Token::String(s) => Ok(Datum::Simple(SimpleDatumKind::String(s.clone()))),
        Token::Identifier(s) => Ok(Datum::Simple(SimpleDatumKind::Symbol(s.clone()))),
        Token::Number(n) => {
            let number = Number::from_str(n).unwrap();
            Ok(Datum::Simple(SimpleDatumKind::Number(number)))
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
                            Ok(Datum::Compound(CompoundDatumKind::List(ListKind::Proper(
                                list,
                            ))))
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
                            Ok(Datum::Compound(CompoundDatumKind::List(
                                ListKind::Improper(list, Box::new(datum)),
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
            Ok(Datum::Compound(CompoundDatumKind::List(
                ListKind::Abbreviation(AbbreviationPrefix::Quote, Box::new(datum)),
            )))
        }
        Token::Quasiquote => {
            let datum = read_impl(tip)?;
            Ok(Datum::Compound(CompoundDatumKind::List(
                ListKind::Abbreviation(AbbreviationPrefix::Quasiquote, Box::new(datum)),
            )))
        }
        Token::Unquote => {
            let datum = read_impl(tip)?;
            Ok(Datum::Compound(CompoundDatumKind::List(
                ListKind::Abbreviation(AbbreviationPrefix::Unquote, Box::new(datum)),
            )))
        }
        Token::UnquoteSplicing => {
            let datum = read_impl(tip)?;
            Ok(Datum::Compound(CompoundDatumKind::List(
                ListKind::Abbreviation(AbbreviationPrefix::UnquoteSplicing, Box::new(datum)),
            )))
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
                    return Ok(Datum::Compound(CompoundDatumKind::Vector(vector)));
                } else {
                    vector.push(read_impl(tip)?);
                }
            }
        }
    }
}

pub struct Reader<'a, I: Iterator<Item = &'a Token>> {
    token_iter: Peekable<I>,
    error: bool,
}

impl<'a, I: Iterator<Item = &'a Token>> Reader<'a, I> {
    pub fn new(token_iter: I) -> Reader<'a, I> {
        Reader {
            token_iter: token_iter.peekable(),
            error: false,
        }
    }
}

impl<'a, I: Iterator<Item = &'a Token>> Iterator for Reader<'a, I> {
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
    use crate::number::RealKind;

    fn integer(n: i64) -> Datum {
        Datum::Simple(SimpleDatumKind::Number(Number::Real(RealKind::Integer(
            num::BigInt::from(n),
        ))))
    }

    #[test]
    fn simple_datum() {
        let datum = read(&mut vec![Token::Boolean(true)].iter()).unwrap();
        assert_eq!(datum, Datum::Simple(SimpleDatumKind::Boolean(true)));

        let datum = read(&mut vec![Token::Number("123".to_owned())].iter()).unwrap();
        assert_eq!(datum, integer(123));

        let datum = read(&mut vec![Token::Character('a')].iter()).unwrap();
        assert_eq!(datum, Datum::Simple(SimpleDatumKind::Character('a')));

        let datum = read(&mut vec![Token::String("foo".to_owned())].iter()).unwrap();
        assert_eq!(
            datum,
            Datum::Simple(SimpleDatumKind::String("foo".to_owned()))
        );

        let datum = read(&mut vec![Token::Identifier("foo".to_owned())].iter()).unwrap();
        assert_eq!(
            datum,
            Datum::Simple(SimpleDatumKind::Symbol("foo".to_owned()))
        );
    }

    #[test]
    fn empty_list() {
        let datum = read(&mut vec![Token::LParen, Token::RParen].iter()).unwrap();
        assert_eq!(datum, Datum::EmptyList);
    }

    #[test]
    fn compound_datum() {
        let datum = read(
            &mut vec![
                Token::LParen,
                Token::Number("123".to_owned()),
                Token::RParen,
            ]
            .iter(),
        )
        .unwrap();
        assert_eq!(
            datum,
            Datum::Compound(CompoundDatumKind::List(ListKind::Proper(vec![integer(
                123
            )])))
        );

        let datum = read(
            &mut vec![
                Token::LParen,
                Token::Number("123".to_owned()),
                Token::Number("456".to_owned()),
                Token::Dot,
                Token::Identifier("bar".to_owned()),
                Token::RParen,
            ]
            .iter(),
        )
        .unwrap();
        assert_eq!(
            datum,
            Datum::Compound(CompoundDatumKind::List(ListKind::Improper(
                vec![integer(123), integer(456)],
                Box::new(Datum::Simple(SimpleDatumKind::Symbol("bar".to_owned()))),
            )))
        );

        let datum = read(
            &mut vec![
                Token::Quote,
                Token::LParen,
                Token::Number("123".to_owned()),
                Token::RParen,
            ]
            .iter(),
        )
        .unwrap();
        assert_eq!(
            datum,
            Datum::Compound(CompoundDatumKind::List(ListKind::Abbreviation(
                AbbreviationPrefix::Quote,
                Box::new(Datum::Compound(CompoundDatumKind::List(ListKind::Proper(
                    vec![integer(123)]
                )))),
            )))
        );

        let datum = read(
            &mut vec![
                Token::Quasiquote,
                Token::LParen,
                Token::Number("123".to_owned()),
                Token::RParen,
            ]
            .iter(),
        )
        .unwrap();
        assert_eq!(
            datum,
            Datum::Compound(CompoundDatumKind::List(ListKind::Abbreviation(
                AbbreviationPrefix::Quasiquote,
                Box::new(Datum::Compound(CompoundDatumKind::List(ListKind::Proper(
                    vec![integer(123)]
                )))),
            )))
        );

        let datum =
            read(&mut vec![Token::Unquote, Token::Number("123".to_owned())].iter()).unwrap();
        assert_eq!(
            datum,
            Datum::Compound(CompoundDatumKind::List(ListKind::Abbreviation(
                AbbreviationPrefix::Unquote,
                Box::new(integer(123)),
            )))
        );

        let datum = read(
            &mut vec![
                Token::UnquoteSplicing,
                Token::LParen,
                Token::Number("123".to_owned()),
                Token::RParen,
            ]
            .iter(),
        )
        .unwrap();
        assert_eq!(
            datum,
            Datum::Compound(CompoundDatumKind::List(ListKind::Abbreviation(
                AbbreviationPrefix::UnquoteSplicing,
                Box::new(Datum::Compound(CompoundDatumKind::List(ListKind::Proper(
                    vec![integer(123)]
                )))),
            )))
        );

        let datum = read(
            &mut vec![
                Token::Vector,
                Token::Number("123".to_owned()),
                Token::Number("456".to_owned()),
                Token::RParen,
            ]
            .iter(),
        )
        .unwrap();
        assert_eq!(
            datum,
            Datum::Compound(CompoundDatumKind::Vector(vec![integer(123), integer(456)]))
        );
    }

    #[test]
    fn nested_datum() {
        let datum = read(
            &mut vec![
                Token::LParen,
                Token::LParen,
                Token::Number("123".to_owned()),
                Token::RParen,
                Token::RParen,
            ]
            .iter(),
        )
        .unwrap();
        assert_eq!(
            datum,
            Datum::Compound(CompoundDatumKind::List(ListKind::Proper(vec![
                Datum::Compound(CompoundDatumKind::List(ListKind::Proper(vec![integer(
                    123
                )])))
            ])))
        );

        let datum = read(
            &mut vec![
                Token::LParen,
                Token::LParen,
                Token::Identifier("a".to_owned()),
                Token::Identifier("b".to_owned()),
                Token::Identifier("c".to_owned()),
                Token::RParen,
                Token::LParen,
                Token::Identifier("+".to_owned()),
                Token::Number("1".to_owned()),
                Token::Number("2".to_owned()),
                Token::RParen,
                Token::RParen,
            ]
            .iter(),
        )
        .unwrap();
        assert_eq!(
            datum,
            Datum::Compound(CompoundDatumKind::List(ListKind::Proper(vec![
                Datum::Compound(CompoundDatumKind::List(ListKind::Proper(vec![
                    Datum::Simple(SimpleDatumKind::Symbol("a".to_owned())),
                    Datum::Simple(SimpleDatumKind::Symbol("b".to_owned())),
                    Datum::Simple(SimpleDatumKind::Symbol("c".to_owned())),
                ]))),
                Datum::Compound(CompoundDatumKind::List(ListKind::Proper(vec![
                    Datum::Simple(SimpleDatumKind::Symbol("+".to_owned())),
                    integer(1),
                    integer(2),
                ]))),
            ])))
        );

        let datum = read(
            &mut vec![
                Token::LParen,
                Token::Identifier("a".to_owned()),
                Token::Dot,
                Token::LParen,
                Token::Identifier("b".to_owned()),
                Token::Dot,
                Token::Identifier("c".to_owned()),
                Token::RParen,
                Token::RParen,
            ]
            .iter(),
        )
        .unwrap();
        assert_eq!(
            datum,
            Datum::Compound(CompoundDatumKind::List(ListKind::Improper(
                vec![Datum::Simple(SimpleDatumKind::Symbol("a".to_owned()))],
                Box::new(Datum::Compound(CompoundDatumKind::List(
                    ListKind::Improper(
                        vec![Datum::Simple(SimpleDatumKind::Symbol("b".to_owned()))],
                        Box::new(Datum::Simple(SimpleDatumKind::Symbol("c".to_owned()))),
                    )
                ))),
            )))
        );

        let datum = read(
            &mut vec![
                Token::Quasiquote,
                Token::LParen,
                Token::Identifier("a".to_owned()),
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
                Token::Identifier("b".to_owned()),
                Token::String("str".to_owned()),
                Token::RParen,
                Token::RParen,
            ]
            .iter(),
        )
        .unwrap();
        assert_eq!(
            datum,
            Datum::Compound(CompoundDatumKind::List(ListKind::Abbreviation(
                AbbreviationPrefix::Quasiquote,
                Box::new(Datum::Compound(CompoundDatumKind::List(ListKind::Proper(
                    vec![
                        Datum::Simple(SimpleDatumKind::Symbol("a".to_owned())),
                        Datum::Compound(CompoundDatumKind::List(ListKind::Abbreviation(
                            AbbreviationPrefix::Unquote,
                            Box::new(Datum::Compound(CompoundDatumKind::List(ListKind::Proper(
                                vec![
                                    Datum::Simple(SimpleDatumKind::Boolean(true)),
                                    Datum::Simple(SimpleDatumKind::Character('c')),
                                ]
                            )))),
                        ))),
                        Datum::Compound(CompoundDatumKind::Vector(vec![
                            integer(123),
                            integer(456),
                            integer(789),
                        ])),
                        Datum::Compound(CompoundDatumKind::List(ListKind::Abbreviation(
                            AbbreviationPrefix::UnquoteSplicing,
                            Box::new(Datum::Compound(CompoundDatumKind::List(ListKind::Proper(
                                vec![
                                    Datum::Simple(SimpleDatumKind::Symbol("b".to_owned())),
                                    Datum::Simple(SimpleDatumKind::String("str".to_owned())),
                                ]
                            )))),
                        ))),
                    ]
                )))),
            )))
        );
    }

    #[test]
    fn errors() {
        assert_eq!(
            read(&mut vec![Token::LParen].iter()),
            Err(ReaderError {
                kind: ReaderErrorKind::UnexpectedEndOfInput
            })
        );
        assert_eq!(
            read(
                &mut vec![
                    Token::LParen,
                    Token::Number("123".to_owned()),
                    Token::LParen,
                    Token::Number("456".to_owned()),
                    Token::RParen,
                ]
                .iter()
            ),
            Err(ReaderError {
                kind: ReaderErrorKind::UnexpectedEndOfInput
            })
        );
        assert_eq!(
            read(&mut vec![Token::RParen].iter()),
            Err(ReaderError {
                kind: ReaderErrorKind::UnexpectedToken(Token::RParen)
            })
        );
        assert_eq!(
            read(&mut vec![Token::Dot].iter()),
            Err(ReaderError {
                kind: ReaderErrorKind::IllegalDot
            })
        );
        assert_eq!(
            read(
                &mut vec![
                    Token::LParen,
                    Token::Number("123".to_owned()),
                    Token::Dot,
                    Token::Number("456".to_owned()),
                    Token::Number("789".to_owned()),
                    Token::RParen,
                ]
                .iter()
            ),
            Err(ReaderError {
                kind: ReaderErrorKind::IllegalDot
            })
        );
        assert_eq!(
            read(
                &mut vec![
                    Token::LParen,
                    Token::Number("123".to_owned()),
                    Token::Dot,
                    Token::RParen,
                ]
                .iter()
            ),
            Err(ReaderError {
                kind: ReaderErrorKind::UnexpectedToken(Token::RParen)
            })
        );
        assert_eq!(
            read(&mut vec![Token::LParen, Token::Dot, Token::RParen,].iter()),
            Err(ReaderError {
                kind: ReaderErrorKind::IllegalDot
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
        let mut reader = Reader::new(tokens.iter());
        assert_eq!(
            reader.next(),
            Some(Ok(Datum::Compound(CompoundDatumKind::List(
                ListKind::Proper(vec![integer(123)])
            ))))
        );
        assert_eq!(
            reader.next(),
            Some(Ok(Datum::Compound(CompoundDatumKind::List(
                ListKind::Proper(vec![integer(456)])
            ))))
        );
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
        let mut reader = Reader::new(tokens.iter());
        assert_eq!(
            reader.next(),
            Some(Ok(Datum::Compound(CompoundDatumKind::List(
                ListKind::Proper(vec![integer(123)])
            ))))
        );
        assert_eq!(
            reader.next(),
            Some(Err(ReaderError {
                kind: ReaderErrorKind::UnexpectedToken(Token::RParen)
            }))
        );
        assert_eq!(reader.next(), None);
    }
}
