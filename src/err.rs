use std::error::Error;
use std::fmt;

use crate::evaler::EvalError;
use crate::lexer::LexerError;
use crate::parser::ParserError;
use crate::reader::ReaderError;

#[derive(Debug, PartialEq)]
pub enum SchemeError {
    Lexer(LexerError),
    Reader(ReaderError),
    Parser(ParserError),
    Eval(EvalError),
}

impl From<LexerError> for SchemeError {
    fn from(e: LexerError) -> Self {
        SchemeError::Lexer(e)
    }
}

impl From<ReaderError> for SchemeError {
    fn from(e: ReaderError) -> Self {
        SchemeError::Reader(e)
    }
}

impl From<ParserError> for SchemeError {
    fn from(e: ParserError) -> Self {
        SchemeError::Parser(e)
    }
}

impl From<EvalError> for SchemeError {
    fn from(e: EvalError) -> Self {
        SchemeError::Eval(e)
    }
}

impl Error for SchemeError {}

impl fmt::Display for SchemeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SchemeError::Lexer(e) => write!(f, "Lexer error: {}", e),
            SchemeError::Reader(e) => write!(f, "Reader error: {}", e),
            SchemeError::Parser(e) => write!(f, "Parser error: {}", e),
            SchemeError::Eval(e) => write!(f, "Eval error: {}", e),
        }
    }
}
