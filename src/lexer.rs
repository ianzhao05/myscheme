use std::error::Error;
use std::fmt;
use std::sync::LazyLock;

use regex::Regex;

use crate::interner::Symbol;

#[derive(Debug, PartialEq)]
pub enum Token {
    Identifier(Symbol),
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

#[derive(Debug, PartialEq)]
pub enum LexerErrorKind {
    InvalidToken,
    InvalidSequence,
    UnclosedString,
}

#[derive(Debug, PartialEq)]
pub struct LexerError {
    kind: LexerErrorKind,
    pos: usize,
}

impl Error for LexerError {}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            LexerErrorKind::InvalidToken => write!(f, "Invalid token at pos {}", self.pos),
            LexerErrorKind::InvalidSequence => write!(f, "Invalid sequence at pos {}", self.pos),
            LexerErrorKind::UnclosedString => write!(f, "Unclosed string at pos {}", self.pos),
        }
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

    pub fn pos(&self) -> usize {
        self.pos
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        const INITIAL: &str = r"a-zA-Z!$%&*:<=>?^_~/";
        const DELIMITER: &str = r#"[\s\(\)";]|$"#;

        static ATMOSPHERE: LazyLock<Regex> =
            LazyLock::new(|| Regex::new(r"\A(?:\s+|;.*)").unwrap());
        static IDENTIFIER: LazyLock<Regex> = LazyLock::new(|| {
            Regex::new(&format!(
                r"\A([{INITIAL}][{INITIAL}0-9+\-\.@]*|\+|-|\.{{3}})(?:{DELIMITER})"
            ))
            .unwrap()
        });
        static CHARACTER: LazyLock<Regex> = LazyLock::new(|| {
            Regex::new(&format!(
                r"\A#\\(space|newline|[^a-zA-Z]|[a-zA-z](?:{DELIMITER}))"
            ))
            .unwrap()
        });
        static STRING: LazyLock<Regex> =
            LazyLock::new(|| Regex::new(r#"\A"((?:\\.|[^\\"])*)""#).unwrap());
        // TODO: implement complex numbers
        static NUMBER: LazyLock<Regex> = LazyLock::new(|| {
            Regex::new(&format!(
                r"(?ix)\A(
                    (?:\#[ie]\#d|(?:\#d)?(?:\#[ie])?)
                    [+-]?
                    (?:[0-9]+/[0-9]+
                      | (?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)
                        (?:[esdfl][+-]?[0-9]+)?)
                  | (?:\#[ie]\#b|\#b(?:\#[ie])?)
                    [+-]?[01]+(?:/[01]+)?
                  | (?:\#[ie]\#o|\#o(?:\#[ie])?)
                    [+-]?[0-7]+(?:/[0-7]+)?
                  | (?:\#[ie]\#x|\#x(?:\#[ie])?)
                    [+-]?[0-9a-f]+(?:/[0-9a-f]+)?
                  )(?:{DELIMITER})"
            ))
            .unwrap()
        });
        static DOT: LazyLock<Regex> =
            LazyLock::new(|| Regex::new(&format!(r"\A\.(?:{DELIMITER})")).unwrap());

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
                        Some(match it.next() {
                            Some('t') => Ok(Token::Boolean(true)),
                            Some('f') => Ok(Token::Boolean(false)),
                            Some('(') => Ok(Token::Vector),
                            _ => {
                                if let Some(c) = NUMBER.captures(rest) {
                                    let m = c.get(1).unwrap();
                                    self.pos += m.end() - 2;
                                    Ok(Token::Number(m.as_str().to_owned()))
                                } else {
                                    Err(LexerError {
                                        kind: LexerErrorKind::InvalidSequence,
                                        pos: self.pos - 2,
                                    })
                                }
                            }
                        })
                    };
                }
                '"' => {
                    return if let Some(c) = STRING.captures(rest) {
                        self.pos += c.get(0).unwrap().end();
                        Some(Ok(Token::String(
                            c[1].to_owned()
                                .replace(r"\\", r"\")
                                .replace(r#"\""#, r#"""#),
                        )))
                    } else {
                        Some(Err(LexerError {
                            kind: LexerErrorKind::UnclosedString,
                            pos: self.pos,
                        }))
                    };
                }
                _ => {
                    if let Some(m) = ATMOSPHERE.find(rest) {
                        self.pos += m.end();
                    } else if let Some(c) = IDENTIFIER.captures(rest) {
                        let m = c.get(1).unwrap();
                        self.pos += m.end();
                        return Some(Ok(Token::Identifier(m.as_str().into())));
                    } else if DOT.is_match(rest) {
                        self.pos += 1;
                        return Some(Ok(Token::Dot));
                    } else if let Some(c) = NUMBER.captures(rest) {
                        let m = c.get(1).unwrap();
                        self.pos += m.end();
                        return Some(Ok(Token::Number(m.as_str().to_owned())));
                    } else {
                        return Some(Err(LexerError {
                            kind: LexerErrorKind::InvalidToken,
                            pos: self.pos,
                        }));
                    }
                }
            }
        }
        None
    }
}

#[derive(Debug)]
pub struct SexpReader {
    pub buf: String,
    pub token_buf: Vec<Token>,
    pos: usize,
    open_count: usize,
    prev_quote: bool,
}

impl SexpReader {
    pub fn new(buf: String) -> Self {
        Self {
            buf,
            token_buf: Vec::new(),
            pos: 0,
            open_count: 0,
            prev_quote: false,
        }
    }

    pub fn try_tokenize(&mut self, partial: bool) -> Result<bool, LexerError> {
        let mut lexer = Lexer::new(&self.buf[self.pos..]);
        let mut has_tokens = false;
        while let Some(token) = lexer.next() {
            has_tokens = true;
            match token {
                Ok(token) => {
                    self.prev_quote = matches!(
                        token,
                        Token::Quote | Token::Quasiquote | Token::Unquote | Token::UnquoteSplicing
                    );
                    match token {
                        Token::LParen | Token::Vector => self.open_count += 1,
                        Token::RParen => {
                            if self.open_count == 0 || (partial && self.open_count == 1) {
                                if partial {
                                    self.pos += lexer.pos();
                                } else {
                                    self.pos = self.buf.len();
                                }
                                self.token_buf.push(token);
                                self.open_count = 0;
                                return Ok(true);
                            }
                            self.open_count -= 1;
                        }
                        _ => {
                            if partial && self.open_count == 0 && !self.prev_quote {
                                self.pos += lexer.pos();
                                self.token_buf.push(token);
                                return Ok(true);
                            }
                        }
                    }
                    self.token_buf.push(token);
                }
                Err(e) => match e.kind {
                    LexerErrorKind::UnclosedString
                        if self.open_count > 0 || self.token_buf.is_empty() =>
                    {
                        self.pos += lexer.pos();
                        return Ok(false);
                    }
                    _ => {
                        self.buf.clear();
                        self.pos = 0;
                        self.open_count = 0;
                        return Err(e);
                    }
                },
            }
        }
        self.pos += lexer.pos();
        Ok(has_tokens && self.open_count == 0 && !self.prev_quote)
    }

    pub fn take_tokens(&mut self) -> Vec<Token> {
        self.buf = self.buf.split_off(self.pos);
        self.pos = 0;
        self.open_count = 0;
        std::mem::take(&mut self.token_buf)
    }

    pub fn read_char(&mut self, peek: bool) -> Option<char> {
        let oc = self.buf[self.pos..].chars().next();
        if !peek {
            if let Some(c) = oc {
                self.pos += c.len_utf8();
            }
        }
        oc
    }

    pub fn char_ready(&self) -> bool {
        self.pos < self.buf.len()
    }

    pub fn reset(&mut self) {
        self.buf.clear();
        self.token_buf.clear();
        self.pos = 0;
        self.open_count = 0;
        self.prev_quote = false;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::tokenize;

    #[test]
    fn identifiers() {
        assert_eq!(
            tokenize!("foo bar2 _baz !w@w"),
            Ok(vec![
                Token::Identifier("foo".into()),
                Token::Identifier("bar2".into()),
                Token::Identifier("_baz".into()),
                Token::Identifier("!w@w".into()),
            ]),
        );
        assert_eq!(
            tokenize!("@"),
            Err(LexerError {
                kind: LexerErrorKind::InvalidToken,
                pos: 0,
            }),
        )
    }

    #[test]
    fn parens() {
        assert_eq!(
            tokenize!(" ( (a   b c) (+ 1 2) )"),
            Ok(vec![
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
            ]),
        )
    }

    #[test]
    fn comments() {
        assert_eq!(
            tokenize!("foo ; this is a comment\n 69"),
            Ok(vec![
                Token::Identifier("foo".into()),
                Token::Number("69".to_owned())
            ]),
        );
    }

    #[test]
    fn booleans() {
        assert_eq!(
            tokenize!("#t #f"),
            Ok(vec![Token::Boolean(true), Token::Boolean(false)]),
        );
    }

    #[test]
    fn characters() {
        assert_eq!(
            tokenize!(r"#\a( #\!f #\ #\space #\newline"),
            Ok(vec![
                Token::Character('a'),
                Token::LParen,
                Token::Character('!'),
                Token::Identifier("f".into()),
                Token::Character(' '),
                Token::Character(' '),
                Token::Character('\n')
            ]),
        );
        assert_eq!(
            tokenize!(r"#\ab"),
            Err(LexerError {
                kind: LexerErrorKind::InvalidSequence,
                pos: 0,
            }),
        );
    }

    #[test]
    fn strings() {
        assert_eq!(
            tokenize!(r#""foo bar" "foo\\bar" "foo\"bar" "foo\nbar""#),
            Ok(vec![
                Token::String("foo bar".to_owned()),
                Token::String(r"foo\bar".to_owned()),
                Token::String(r#"foo"bar"#.to_owned()),
                Token::String(r"foo\nbar".to_owned()),
            ]),
        );
        assert_eq!(
            tokenize!(r#""foo bar"#),
            Err(LexerError {
                kind: LexerErrorKind::UnclosedString,
                pos: 0,
            }),
        );
    }

    #[test]
    fn numbers() {
        assert_eq!(
            tokenize!("1 +2.3 -4.5 -6. .7 8.9e-1 -1.E+3 1/2"),
            Ok(vec![
                Token::Number("1".to_owned()),
                Token::Number("+2.3".to_owned()),
                Token::Number("-4.5".to_owned()),
                Token::Number("-6.".to_owned()),
                Token::Number(".7".to_owned()),
                Token::Number("8.9e-1".to_owned()),
                Token::Number("-1.E+3".to_owned()),
                Token::Number("1/2".to_owned()),
            ]),
        );
    }

    #[test]
    fn quotes() {
        assert_eq!(
            tokenize!("'()`(,(),@())"),
            Ok(vec![
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
            ]),
        )
    }

    #[test]
    fn dots() {
        assert_eq!(
            tokenize!("(a . (b .;\nc))"),
            Ok(vec![
                Token::LParen,
                Token::Identifier("a".into()),
                Token::Dot,
                Token::LParen,
                Token::Identifier("b".into()),
                Token::Dot,
                Token::Identifier("c".into()),
                Token::RParen,
                Token::RParen,
            ]),
        )
    }

    #[test]
    fn vector() {
        assert_eq!(
            tokenize!("#(1 2 3)"),
            Ok(vec![
                Token::Vector,
                Token::Number("1".to_owned()),
                Token::Number("2".to_owned()),
                Token::Number("3".to_owned()),
                Token::RParen,
            ]),
        )
    }
}
