use std::collections::HashMap;
use std::rc::Rc;

use crate::datum::SimpleDatum;
use crate::evaler::{EvalError, EvalErrorKind};
use crate::object::{Object, ObjectRef};

use super::PrimitiveMap;

fn equiv_am(got: usize) -> EvalError {
    EvalError::new(EvalErrorKind::ArityMismatch {
        expected: 2,
        got,
        rest: false,
    })
}

fn eqv(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    if args.len() != 2 {
        return Err(equiv_am(args.len()));
    }
    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Boolean(
        args[0] == args[1],
    ))))
}

fn eq(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    if args.len() != 2 {
        return Err(equiv_am(args.len()));
    }
    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Boolean(
        match (&args[0], &args[1]) {
            (ObjectRef::Object(o1), ObjectRef::Object(o2)) => match (&**o1, &**o2) {
                (Object::Atom(SimpleDatum::Number(_)), Object::Atom(SimpleDatum::Number(_))) => {
                    Rc::ptr_eq(o1, o2)
                }
                _ => args[0] == args[1],
            },
            _ => args[0] == args[1],
        },
    ))))
}

pub fn equal(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    if args.len() != 2 {
        return Err(equiv_am(args.len()));
    }
    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Boolean(
        ObjectRef::equal(&args[0], &args[1]),
    ))))
}

pub fn primitives() -> PrimitiveMap {
    let mut m: PrimitiveMap = HashMap::new();
    m.insert("eqv?", eqv);
    m.insert("eq?", eq);
    m.insert("equal?", equal);
    m
}

pub const PRELUDE: &str = "";
