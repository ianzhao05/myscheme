use std::collections::HashMap;
use std::fmt;

use crate::err::SchemeError;
use crate::evaler::{EvalError, EvalErrorKind};
use crate::interpret::until_err;
use crate::lexer::{Lexer, LexerError};
use crate::object::ObjectRef;
use crate::reader::{Reader, ReaderError, ReaderErrorKind};

use super::PrimitiveMap;

fn display(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::new(EvalErrorKind::ArityMismatch {
            expected: 1,
            got: args.len(),
            rest: false,
        }));
    }
    print!("{}", args[0]);
    Ok(ObjectRef::Void)
}

fn write(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::new(EvalErrorKind::ArityMismatch {
            expected: 1,
            got: args.len(),
            rest: false,
        }));
    }
    print!("{:#}", args[0]);
    Ok(ObjectRef::Void)
}

#[derive(Debug, PartialEq)]
pub enum ReadError {
    Reader(ReaderError),
    Lexer(LexerError),
}

impl fmt::Display for ReadError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ReadError::Reader(e) => e.fmt(f),
            ReadError::Lexer(e) => e.fmt(f),
        }
    }
}

fn read(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    if !args.is_empty() {
        return Err(EvalError::new(EvalErrorKind::ArityMismatch {
            expected: 0,
            got: args.len(),
            rest: false,
        }));
    }
    let mut buf = String::new();
    std::io::stdin()
        .read_line(&mut buf)
        .map_err(|_| EvalError::new(EvalErrorKind::IOError))?;
    let (mut le, mut re) = (Ok(()), Ok(()));
    let tokens = Lexer::new(&buf).scan(&mut le, until_err);
    let mut data = Reader::new(tokens).scan(&mut re, until_err);
    let res = match data.next() {
        Some(d) => Ok(ObjectRef::from(d)),
        None => Err(EvalError::new(EvalErrorKind::ReadError(ReadError::Reader(
            ReaderError::new(ReaderErrorKind::UnexpectedEndOfInput),
        )))),
    };
    le.map_err(|e| {
        EvalError::new(EvalErrorKind::ReadError(ReadError::Lexer(match e {
            SchemeError::Lexer(e) => e,
            _ => unreachable!(),
        })))
    })?;
    re.map_err(|e| {
        EvalError::new(EvalErrorKind::ReadError(ReadError::Reader(match e {
            SchemeError::Reader(e) => e,
            _ => unreachable!(),
        })))
    })?;
    res
}

pub fn primitives() -> PrimitiveMap {
    let mut m: PrimitiveMap = HashMap::new();
    m.insert("display", display);
    m.insert("write", write);
    m.insert("read", read);
    m
}

pub const PRELUDE: &str = "
(define (newline)
  (display #\\newline))
";
