use std::error::Error;
use std::fmt;

use lazy_static::lazy_static;
use regex::Regex;

#[derive(Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    Boolean(bool),
    Number(String),
    Character(char),
    String(String),
    // (
    LParen,
    // )
    RParen,
    // #(
    Vector,
    // '
    Quote,
    // `
    Quasiquote,
    // ,
    Unquote,
    // ,@
    UnquoteSplicing,
}

#[derive(Debug)]
pub struct LexerError {
    message: String,
}

impl Error for LexerError {}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

pub struct Lexer<'a> {
    exp: &'a str,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(exp: &'a str) -> Self {
        Lexer { exp, pos: 0 }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Result<Token, LexerError>> {
        const INITIAL: &str = r"a-zA-Z!$%&*:<=>?^_~/";
        lazy_static! {
            static ref ATMOSPHERE: Regex = Regex::new(r"\A(\s+|;.*)").unwrap();
            static ref IDENTIFIER: Regex = Regex::new(&format!(
                r"\A([{INITIAL}][{INITIAL}0-9+\-\.@]*|\+|-|\.{{3}})"
            ))
            .unwrap();
            static ref CHARACTER: Regex =
                Regex::new(r#"\A#\\(space|newline|[^a-zA-Z]|[a-zA-z](?:[\s\(\)";]|$))"#).unwrap();
        }

        while self.pos < self.exp.len() {
            let rest = &self.exp[self.pos..];
            let mut it = rest.chars();
            match it.next().unwrap() {
                '(' => {
                    self.pos += 1;
                    return Some(Ok(Token::LParen));
                }
                ')' => {
                    self.pos += 1;
                    return Some(Ok(Token::RParen));
                }
                '\'' => {
                    self.pos += 1;
                    return Some(Ok(Token::Quote));
                }
                '`' => {
                    self.pos += 1;
                    return Some(Ok(Token::Quasiquote));
                }
                ',' => {
                    self.pos += 1;
                    return Some(if it.next().unwrap() == '@' {
                        self.pos += 1;
                        Ok(Token::UnquoteSplicing)
                    } else {
                        Ok(Token::Unquote)
                    });
                }
                '#' => {
                    return if let Some(c) = CHARACTER.captures(rest) {
                        let m = c.get(0).unwrap();
                        self.pos += if m.end() == 4 { 3 } else { m.end() };

                        let ch: &str = c.get(1).unwrap().as_str();
                        Some(Ok(Token::Character(match ch {
                            "space" => ' ',
                            "newline" => '\n',
                            _ => ch.chars().next().unwrap(),
                        })))
                    } else {
                        self.pos += 2;
                        match it.next().unwrap() {
                            't' => Some(Ok(Token::Boolean(true))),
                            'f' => Some(Ok(Token::Boolean(false))),
                            '#' => Some(Ok(Token::Vector)),
                            _ => Some(Err(LexerError {
                                message: format!("invalid sequence at pos {}", self.pos - 2),
                            })),
                        }
                    }
                }
                _ => {
                    if let Some(m) = ATMOSPHERE.find(rest) {
                        self.pos += m.end();
                    } else if let Some(m) = IDENTIFIER.find(rest) {
                        self.pos += m.end();
                        return Some(Ok(Token::Identifier(m.as_str().to_owned())));
                    } else {
                        return Some(Err(LexerError {
                            message: format!("invalid token at pos {}", self.pos),
                        }));
                    }
                }
            }
        }
        None
    }
}

#[macro_export]
macro_rules! tokenize {
    ($e:expr) => {
        $crate::lexer::Lexer::new($e).collect::<Result<Vec<_>, _>>()
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn identifiers() {
        let tokens = tokenize!("foo bar2 _baz !w@w");
        assert_eq!(
            vec![
                Token::Identifier("foo".to_owned()),
                Token::Identifier("bar2".to_owned()),
                Token::Identifier("_baz".to_owned()),
                Token::Identifier("!w@w".to_owned()),
            ],
            tokens.unwrap()
        );
    }

    #[test]
    fn parens() {
        let tokens = tokenize!(" ( (a   b c) () )");
        assert_eq!(
            vec![
                Token::LParen,
                Token::LParen,
                Token::Identifier("a".to_owned()),
                Token::Identifier("b".to_owned()),
                Token::Identifier("c".to_owned()),
                Token::RParen,
                Token::LParen,
                Token::RParen,
                Token::RParen,
            ],
            tokens.unwrap()
        )
    }

    #[test]
    fn comments() {
        let tokens = tokenize!("foo ; this is a comment\n bar");
        assert_eq!(
            vec![
                Token::Identifier("foo".to_owned()),
                Token::Identifier("bar".to_owned())
            ],
            tokens.unwrap()
        );
    }

    #[test]
    fn booleans() {
        let tokens = tokenize!("#t #f");
        assert_eq!(
            vec![Token::Boolean(true), Token::Boolean(false),],
            tokens.unwrap()
        );
    }

    #[test]
    fn characters() {
        let tokens = tokenize!(r"#\a( #\!f #\ #\space #\newline");
        assert_eq!(
            vec![
                Token::Character('a'),
                Token::LParen,
                Token::Character('!'),
                Token::Identifier("f".to_owned()),
                Token::Character(' '),
                Token::Character(' '),
                Token::Character('\n')
            ],
            tokens.unwrap()
        );
    }

    #[test]
    fn characters_invalid() {
        let tokens = tokenize!(r"#\ab");
        assert!(tokens.is_err());
    }

    #[test]
    fn quotes() {
        let tokens = tokenize!("'()`(,(),@())");
        assert_eq!(
            vec![
                Token::Quote,
                Token::LParen,
                Token::RParen,
                Token::Quasiquote,
                Token::LParen,
                Token::Unquote,
                Token::LParen,
                Token::RParen,
                Token::UnquoteSplicing,
                Token::LParen,
                Token::RParen,
                Token::RParen,
            ],
            tokens.unwrap()
        )
    }
}
