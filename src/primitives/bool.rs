use std::collections::HashMap;

use crate::datum::SimpleDatum;
use crate::evaler::{EvalError, EvalErrorKind};
use crate::object::{Object, ObjectRef};

use super::PrimitiveMap;

fn boolean(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::new(EvalErrorKind::ArityMismatch {
            expected: 1,
            got: args.len(),
            rest: false,
        }));
    }
    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Boolean(
        match &args[0] {
            ObjectRef::Object(o) => match **o {
                Object::Atom(SimpleDatum::Boolean(_)) => true,
                _ => false,
            },
            _ => false,
        },
    ))))
}

pub fn primitives() -> PrimitiveMap {
    let mut m: PrimitiveMap = HashMap::new();
    m.insert("boolean?", boolean);
    m
}

pub const PRELUDE: &str = "
(define (not x) (if x #f #t))
";
