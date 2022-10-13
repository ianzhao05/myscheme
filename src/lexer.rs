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
    // .
    Dot,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Identifier(s) => write!(f, "{s}"),
            Token::Boolean(b) => write!(f, "{b}"),
            Token::Number(s) => write!(f, "{s}"),
            Token::Character(c) => write!(f, "{c}"),
            Token::String(s) => write!(f, "{s}"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::Vector => write!(f, "#("),
            Token::Quote => write!(f, "'"),
            Token::Quasiquote => write!(f, "`"),
            Token::Unquote => write!(f, ","),
            Token::UnquoteSplicing => write!(f, ",@"),
            Token::Dot => write!(f, "."),
        }
    }
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

/// Iterator over the lexical tokens in a string.
/// No additional parsing is done except for the removal of
/// outer double quotes for strings and the substitution
/// of character literals for their corresponding characters.
pub struct Lexer<'a> {
    exp: &'a str,
    pos: usize,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer iterator for the given expression.
    pub fn new(exp: &'a str) -> Self {
        Self { exp, pos: 0 }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        const INITIAL: &str = r"a-zA-Z!$%&*:<=>?^_~/";
        const DELIMITER: &str = r#"[\s\(\)";]|$"#;
        lazy_static! {
            static ref ATMOSPHERE: Regex = Regex::new(r"\A(?:\s+|;.*)").unwrap();
            static ref IDENTIFIER: Regex = Regex::new(&format!(
                r"\A([{INITIAL}][{INITIAL}0-9+\-\.@]*|\+|-|\.{{3}})(?:{DELIMITER})"
            ))
            .unwrap();
            static ref CHARACTER: Regex = Regex::new(&format!(
                r"\A#\\(space|newline|[^a-zA-Z]|[a-zA-z](?:{DELIMITER}))"
            ))
            .unwrap();
            static ref STRING: Regex = Regex::new(r#"\A"((?:\\.|[^\\"])*)""#).unwrap();
            // TODO: implement complex numbers and other bases
            static ref NUMBER: Regex = Regex::new(&format!(
                r"(?x)\A
                ([+-]?
                    (?:[0-9]+/[0-9]+|(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)
                    (?:[eE][+-]?[0-9]+)?)
                )(?:{DELIMITER})"
            ))
            .unwrap();
            static ref DOT: Regex = Regex::new(&format!(r"\A\.(?:{DELIMITER})")).unwrap();
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
                    // TODO: Vectors
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
                        Some(match it.next().unwrap() {
                            't' => Ok(Token::Boolean(true)),
                            'f' => Ok(Token::Boolean(false)),
                            '#' => Ok(Token::Vector),
                            _ => Err(LexerError {
                                message: format!("invalid sequence at pos {}", self.pos - 2),
                            }),
                        })
                    };
                }
                '"' => {
                    return if let Some(c) = STRING.captures(rest) {
                        self.pos += c.get(0).unwrap().end();
                        Some(Ok(Token::String(c.get(1).unwrap().as_str().to_owned())))
                    } else {
                        Some(Err(LexerError {
                            message: format!("unclosed string literal at pos {}", self.pos),
                        }))
                    };
                }
                _ => {
                    if let Some(m) = ATMOSPHERE.find(rest) {
                        self.pos += m.end();
                    } else if let Some(c) = NUMBER.captures(rest) {
                        let m = c.get(1).unwrap();
                        self.pos += m.end();
                        return Some(Ok(Token::Number(m.as_str().to_owned())));
                    } else if let Some(c) = IDENTIFIER.captures(rest) {
                        let m = c.get(1).unwrap();
                        self.pos += m.end();
                        return Some(Ok(Token::Identifier(m.as_str().to_owned())));
                    } else if DOT.is_match(rest) {
                        self.pos += 1;
                        return Some(Ok(Token::Dot));
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
        let tokens = tokenize!(" ( (a   b c) (+ 1 2) )");
        assert_eq!(
            vec![
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
            ],
            tokens.unwrap()
        )
    }

    #[test]
    fn comments() {
        let tokens = tokenize!("foo ; this is a comment\n 69");
        assert_eq!(
            vec![
                Token::Identifier("foo".to_owned()),
                Token::Number("69".to_owned())
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
    fn strings() {
        let tokens = tokenize!(r#""foo bar" "foo\\bar" "foo\"bar" "foo\nbar""#);
        assert_eq!(
            vec![
                Token::String("foo bar".to_owned()),
                Token::String(r"foo\\bar".to_owned()),
                Token::String(r#"foo\"bar"#.to_owned()),
                Token::String(r"foo\nbar".to_owned()),
            ],
            tokens.unwrap()
        );
    }

    #[test]
    fn numbers() {
        let tokens = tokenize!("1 +2.3 -4.5 -6. .7 8.9e-1 -1.E+3 1/2");
        assert_eq!(
            vec![
                Token::Number("1".to_owned()),
                Token::Number("+2.3".to_owned()),
                Token::Number("-4.5".to_owned()),
                Token::Number("-6.".to_owned()),
                Token::Number(".7".to_owned()),
                Token::Number("8.9e-1".to_owned()),
                Token::Number("-1.E+3".to_owned()),
                Token::Number("1/2".to_owned()),
            ],
            tokens.unwrap()
        );
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

    #[test]
    fn dots() {
        let tokens = tokenize!("(a . (b .;\nc))");
        assert_eq!(
            vec![
                Token::LParen,
                Token::Identifier("a".to_owned()),
                Token::Dot,
                Token::LParen,
                Token::Identifier("b".to_owned()),
                Token::Dot,
                Token::Identifier("c".to_owned()),
                Token::RParen,
                Token::RParen,
            ],
            tokens.unwrap()
        )
    }
}
