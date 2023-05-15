use std::cell::RefCell;
use std::fmt;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Write};

pub enum Port {
    Input(RefCell<Option<Box<dyn BufRead>>>),
    Output(RefCell<Option<Box<dyn Write>>>),
}

impl Port {
    pub fn new_input(file: &str) -> io::Result<Self> {
        Ok(Port::Input(RefCell::new(Some(Box::new(BufReader::new(
            File::open(file)?,
        ))))))
    }

    pub fn new_output(file: &str) -> io::Result<Self> {
        Ok(Port::Output(RefCell::new(Some(Box::new(File::create(
            file,
        )?)))))
    }

    pub fn new_stdin() -> Self {
        Port::Input(RefCell::new(Some(Box::new(io::stdin().lock()))))
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
