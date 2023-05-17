use std::collections::HashMap;

use crate::datum::SimpleDatum;
use crate::evaler::{EvalError, EvalErrorKind};
use crate::object::{Object, ObjectRef};
use crate::port::{Port, ReadError};

use super::{cv_fn, PrimitiveMap};

cv_fn!(string_cv, "string");
cv_fn!(oport_cv, "output-port");
cv_fn!(iport_cv, "input-port");
cv_fn!(char_cv, "char");

fn open_file(args: &[ObjectRef], out: bool) -> Result<ObjectRef, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::new(EvalErrorKind::ArityMismatch {
            expected: 1,
            got: args.len(),
            rest: false,
        }));
    }
    match &args[0].try_deref_or(string_cv)? {
        Object::Atom(SimpleDatum::String(s)) => Ok(ObjectRef::new(Object::Port(
            (if out {
                Port::new_output(&s)
            } else {
                Port::new_input(&s)
            })
            .map_err(|_| EvalError::new(EvalErrorKind::IOError))?,
        ))),
        _ => Err(string_cv(&args[0])),
    }
}

fn close_input_port(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::new(EvalErrorKind::ArityMismatch {
            expected: 1,
            got: args.len(),
            rest: false,
        }));
    }
    match &args[0].try_deref_or(iport_cv)? {
        Object::Port(Port::Input(op)) => {
            op.borrow_mut().take();
            Ok(ObjectRef::Void)
        }
        _ => Err(iport_cv(&args[0])),
    }
}

fn close_output_port(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::new(EvalErrorKind::ArityMismatch {
            expected: 1,
            got: args.len(),
            rest: false,
        }));
    }
    match &args[0].try_deref_or(oport_cv)? {
        Object::Port(Port::Output(op)) => {
            op.borrow_mut().take();
            Ok(ObjectRef::Void)
        }
        _ => Err(oport_cv(&args[0])),
    }
}

fn current_input_port(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    if !args.is_empty() {
        return Err(EvalError::new(EvalErrorKind::ArityMismatch {
            expected: 0,
            got: args.len(),
            rest: false,
        }));
    }
    Ok(ObjectRef::new(Object::Port(Port::new_stdin())))
}

fn current_output_port(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    if !args.is_empty() {
        return Err(EvalError::new(EvalErrorKind::ArityMismatch {
            expected: 0,
            got: args.len(),
            rest: false,
        }));
    }
    Ok(ObjectRef::new(Object::Port(Port::new_stdout())))
}

fn write_char(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    if args.len() != 1 && args.len() != 2 {
        return Err(EvalError::new(EvalErrorKind::ArityMismatch {
            expected: 1,
            got: args.len(),
            rest: true,
        }));
    }
    let c = match &args[0].try_deref_or(char_cv)? {
        Object::Atom(SimpleDatum::Character(c)) => *c,
        _ => return Err(char_cv(&args[0])),
    };
    if args.len() == 1 {
        print!("{c}");
    } else {
        match &args[1].try_deref_or(oport_cv)? {
            Object::Port(Port::Output(op)) => match &mut *op.borrow_mut() {
                Some(w) => write!(w, "{c}").map_err(|_| EvalError::new(EvalErrorKind::IOError))?,
                None => return Err(EvalError::new(EvalErrorKind::ClosedPort)),
            },
            _ => return Err(oport_cv(&args[1])),
        }
    }
    Ok(ObjectRef::Void)
}

fn display_write(args: &[ObjectRef], write: bool) -> Result<ObjectRef, EvalError> {
    if args.len() != 1 && args.len() != 2 {
        return Err(EvalError::new(EvalErrorKind::ArityMismatch {
            expected: usize::MAX,
            got: args.len(),
            rest: false,
        }));
    }
    if args.len() == 1 {
        if write {
            print!("{:#}", args[0]);
        } else {
            print!("{}", args[0]);
        }
    } else {
        match &args[1].try_deref_or(oport_cv)? {
            Object::Port(Port::Output(op)) => match &mut *op.borrow_mut() {
                Some(w) => (if write {
                    write!(w, "{:#}", args[0])
                } else {
                    write!(w, "{}", args[0])
                })
                .map_err(|_| EvalError::new(EvalErrorKind::IOError))?,
                None => return Err(EvalError::new(EvalErrorKind::ClosedPort)),
            },
            _ => return Err(oport_cv(&args[1])),
        }
    }
    Ok(ObjectRef::Void)
}

fn read(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    if args.len() > 1 {
        return Err(EvalError::new(EvalErrorKind::ArityMismatch {
            expected: usize::MAX,
            got: args.len(),
            rest: false,
        }));
    }
    let datum = if args.len() == 0 {
        todo!()
    } else {
        match &args[0].try_deref_or(iport_cv)? {
            Object::Port(Port::Input(ip)) => match &mut *ip.borrow_mut() {
                Some(r) => r
                    .read()
                    .map_err(|_| EvalError::new(EvalErrorKind::IOError))?,
                None => return Err(EvalError::new(EvalErrorKind::ClosedPort)),
            },
            _ => return Err(iport_cv(&args[1])),
        }
    };
    match datum {
        Ok(d) => Ok(ObjectRef::from(d)),
        Err(ReadError::Eof) => Ok(ObjectRef::Eof),
        Err(e) => Err(EvalError::new(EvalErrorKind::ReadError(e))),
    }
}

pub fn primitives() -> PrimitiveMap {
    let mut m: PrimitiveMap = HashMap::new();
    m.insert("open-input-file", |args| open_file(args, false));
    m.insert("open-output-file", |args| open_file(args, true));
    m.insert("close-input-port", close_input_port);
    m.insert("close-output-port", close_output_port);
    m.insert("current-input-port", current_input_port);
    m.insert("current-output-port", current_output_port);
    m.insert("write-char", |args| write_char(args));
    m.insert("display", |args| display_write(args, false));
    m.insert("write", |args| display_write(args, true));
    m.insert("read", read);
    m
}

pub const PRELUDE: &str = "
(define (call-with-input-file filename proc)
  (proc (open-input-file filename)))

(define (call-with-output-file filename proc)
  (proc (open-output-file filename)))

(define (newline . port)
  (apply display (cons #\\newline port)))
";
