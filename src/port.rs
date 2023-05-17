use std::cell::RefCell;
use std::fmt;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Write};

use crate::datum::Datum;
use crate::lexer::{LexerError, SexpReader};
use crate::reader::{read, ReaderError};

#[derive(Debug, PartialEq)]
pub enum ReadError {
    Reader(ReaderError),
    Lexer(LexerError),
    Eof,
}

impl fmt::Display for ReadError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ReadError::Reader(e) => e.fmt(f),
            ReadError::Lexer(e) => e.fmt(f),
            ReadError::Eof => write!(f, "EOF"),
        }
    }
}

pub struct IPort {
    file: Box<dyn BufRead>,
    sreader: SexpReader,
}

impl IPort {
    fn new(file: Box<dyn BufRead>) -> Self {
        Self {
            file,
            sreader: SexpReader::new(String::new()),
        }
    }

    pub fn read_line(&mut self) -> io::Result<()> {
        self.file.read_line(&mut self.sreader.buf)?;
        Ok(())
    }

    pub fn read(&mut self) -> io::Result<Result<Datum, ReadError>> {
        loop {
            match self.sreader.try_tokenize(true) {
                Ok(ready) => {
                    if ready || self.file.read_line(&mut self.sreader.buf)? == 0 {
                        let tokens = self.sreader.take_tokens();
                        if tokens.is_empty() {
                            return Ok(Err(ReadError::Eof));
                        }
                        let mut it = tokens.into_iter();
                        let datum = read(&mut it).map_err(ReadError::Reader);
                        assert!(it.next().is_none(), "Unconsumed tokens");
                        return Ok(datum);
                    }
                }
                Err(e) => return Ok(Err(ReadError::Lexer(e))),
            }
        }
    }
}

pub enum Port {
    Input(RefCell<Option<IPort>>),
    Output(RefCell<Option<Box<dyn Write>>>),
}

impl Port {
    pub fn new_input(file: &str) -> io::Result<Self> {
        Ok(Port::Input(RefCell::new(Some(IPort::new(Box::new(
            BufReader::new(File::open(file)?),
        ))))))
    }

    pub fn new_output(file: &str) -> io::Result<Self> {
        Ok(Port::Output(RefCell::new(Some(Box::new(File::create(
            file,
        )?)))))
    }

    pub fn new_stdin() -> Self {
        Port::Input(RefCell::new(Some(IPort::new(Box::new(io::stdin().lock())))))
    }

    pub fn new_stdout() -> Self {
        Port::Output(RefCell::new(Some(Box::new(io::stdout()))))
    }
}

impl fmt::Debug for Port {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match self {
            Port::Input(_) => "Port::Input",
            Port::Output(_) => "Port::Output",
        })
    }
}

impl fmt::Display for Port {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match self {
            Port::Input(_) => "#<input-port>",
            Port::Output(_) => "#<output-port>",
        })
    }
}
