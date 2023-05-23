use std::collections::HashMap;

use crate::datum::SimpleDatum;
use crate::evaler::{EvalError, EvalErrorKind};
use crate::object::{Object, ObjectRef};
use crate::port::{Port, ReadError, STDIN, STDOUT};

use super::utils::{char_cv, ensure_arity, iport_cv, oport_cv, string_cv, PrimitiveMap};

fn open_file(args: &[ObjectRef], out: bool) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 1);

    match &args[0].try_deref_or(string_cv)? {
        Object::Atom(SimpleDatum::String(s)) => Ok(ObjectRef::new(Object::Port(
            (if out {
                Port::new_output(&s.borrow())
            } else {
                Port::new_input(&s.borrow())
            })
            .map_err(|_| EvalError::new(EvalErrorKind::IOError))?,
        ))),
        _ => Err(string_cv(&args[0])),
    }
}

fn close_input_port(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 1);

    match &args[0].try_deref_or(iport_cv)? {
        Object::Port(Port::Input(op)) => {
            op.borrow_mut().take();
            Ok(ObjectRef::Void)
        }
        _ => Err(iport_cv(&args[0])),
    }
}

fn close_output_port(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 1);

    match &args[0].try_deref_or(oport_cv)? {
        Object::Port(Port::Output(op)) => {
            op.borrow_mut().take();
            Ok(ObjectRef::Void)
        }
        _ => Err(oport_cv(&args[0])),
    }
}

fn current_input_port(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 0);

    Ok(STDIN.with(|s| s.clone()))
}

fn current_output_port(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 0);

    Ok(STDOUT.with(|s| s.clone()))
}

fn write_char(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 1, 2);

    let c = match &args[0].try_deref_or(char_cv)? {
        Object::Atom(SimpleDatum::Character(c)) => *c,
        _ => return Err(char_cv(&args[0])),
    };
    let port = args
        .get(1)
        .cloned()
        .unwrap_or_else(|| STDOUT.with(|s| s.clone()));
    match port.try_deref_or(oport_cv)? {
        Object::Port(Port::Output(op)) => match &mut *op.borrow_mut() {
            Some(w) => write!(w, "{c}").map_err(|_| EvalError::new(EvalErrorKind::IOError))?,
            None => return Err(EvalError::new(EvalErrorKind::ClosedPort)),
        },
        _ => return Err(oport_cv(&port)),
    }
    Ok(ObjectRef::Void)
}

fn display_write(args: &[ObjectRef], write: bool) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 1, 2);

    let port = args
        .get(1)
        .cloned()
        .unwrap_or_else(|| STDOUT.with(|s| s.clone()));
    match port.try_deref_or(oport_cv)? {
        Object::Port(Port::Output(op)) => match &mut *op.borrow_mut() {
            Some(w) => (if write {
                write!(w, "{:#}", args[0])
            } else {
                write!(w, "{}", args[0])
            })
            .map_err(|_| EvalError::new(EvalErrorKind::IOError))?,
            None => return Err(EvalError::new(EvalErrorKind::ClosedPort)),
        },
        _ => return Err(oport_cv(&port)),
    }
    Ok(ObjectRef::Void)
}

fn read(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 0, 1);

    let port = args
        .first()
        .cloned()
        .unwrap_or_else(|| STDIN.with(|s| s.clone()));
    let datum = match port.try_deref_or(iport_cv)? {
        Object::Port(Port::Input(ip)) => match &mut *ip.borrow_mut() {
            Some(r) => r
                .read()
                .map_err(|_| EvalError::new(EvalErrorKind::IOError))?,
            None => return Err(EvalError::new(EvalErrorKind::ClosedPort)),
        },
        _ => return Err(iport_cv(&args[0])),
    };
    match datum {
        Ok(d) => Ok(ObjectRef::from(d)),
        Err(ReadError::Eof) => Ok(ObjectRef::Eof),
        Err(e) => Err(EvalError::new(EvalErrorKind::ReadError(e))),
    }
}

fn read_peek_char(args: &[ObjectRef], peek: bool) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 0, 1);

    let port = args
        .first()
        .cloned()
        .unwrap_or_else(|| STDIN.with(|s| s.clone()));
    let c = match port.try_deref_or(iport_cv)? {
        Object::Port(Port::Input(ip)) => match &mut *ip.borrow_mut() {
            Some(r) => r
                .read_char(peek)
                .map_err(|_| EvalError::new(EvalErrorKind::IOError))?,
            None => return Err(EvalError::new(EvalErrorKind::ClosedPort)),
        },
        _ => return Err(iport_cv(&args[0])),
    };
    match c {
        Some(c) => Ok(ObjectRef::new(Object::Atom(SimpleDatum::Character(c)))),
        None => Ok(ObjectRef::Eof),
    }
}

fn char_ready(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 0, 1);

    let port = args
        .first()
        .cloned()
        .unwrap_or_else(|| STDIN.with(|s| s.clone()));
    let res = match port.try_deref_or(iport_cv)? {
        Object::Port(Port::Input(ip)) => match &mut *ip.borrow_mut() {
            Some(r) => r.char_ready(),
            None => return Err(EvalError::new(EvalErrorKind::ClosedPort)),
        },
        _ => return Err(iport_cv(&args[0])),
    };
    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Boolean(res))))
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
    m.insert("read-char", |args| read_peek_char(args, false));
    m.insert("peek-char", |args| read_peek_char(args, true));
    m.insert("char-ready?", char_ready);
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
