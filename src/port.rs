use std::cell::RefCell;
use std::fmt;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Write};

pub enum IPort {
    Open(Box<dyn BufRead>),
    Closed,
}

impl IPort {
    pub fn close(&mut self) {
        match self {
            IPort::Open(_) => {
                *self = IPort::Closed;
            }
            IPort::Closed => (),
        }
    }
}

pub enum OPort {
    Open(Box<dyn Write>),
    Closed,
}

impl OPort {
    pub fn close(&mut self) {
        match self {
            OPort::Open(_) => {
                *self = OPort::Closed;
            }
            OPort::Closed => (),
        }
    }
}

pub enum Port {
    Input(RefCell<IPort>),
    Output(RefCell<OPort>),
}

impl Port {
    pub fn new_input(file: &str) -> io::Result<Self> {
        Ok(Port::Input(RefCell::new(IPort::Open(Box::new(
            BufReader::new(File::open(file)?),
        )))))
    }

    pub fn new_output(file: &str) -> io::Result<Self> {
        Ok(Port::Output(RefCell::new(OPort::Open(Box::new(
            File::create(file)?,
        )))))
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
