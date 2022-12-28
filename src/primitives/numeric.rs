use std::collections::HashMap;

use crate::datum::SimpleDatum;
use crate::evaler::{EvalError, EvalErrorKind};
use crate::number::*;
use crate::object::Object;

use num::BigInt;

use super::PrimitiveMap;

fn num_cv(got: Object) -> EvalError {
    EvalError::new(EvalErrorKind::ContractViolation {
        expected: "number".to_owned(),
        got,
    })
}

fn add(args: &[Object]) -> Result<Object, EvalError> {
    let mut sum = Number::Real(RealKind::Integer(BigInt::from(0)));
    for arg in args {
        match arg {
            Object::Atom(SimpleDatum::Number(n)) => sum += n.clone(),
            _ => return Err(num_cv(arg.clone())),
        }
    }
    Ok(Object::Atom(SimpleDatum::Number(sum)))
}

fn eq(args: &[Object]) -> Result<Object, EvalError> {
    if args.is_empty() {
        return Err(EvalError::new(EvalErrorKind::ArityMismatch {
            expected: 1,
            got: args.len(),
            rest: true,
        }));
    }
    let first = match &args[0] {
        Object::Atom(SimpleDatum::Number(n)) => n,
        other => return Err(num_cv(other.clone())),
    };
    for arg in &args[1..] {
        match arg {
            Object::Atom(SimpleDatum::Number(n)) => {
                if !Number::eq_val(first, n) {
                    return Ok(Object::Atom(SimpleDatum::Boolean(false)));
                }
            }
            _ => return Err(num_cv(arg.clone())),
        }
    }
    Ok(Object::Atom(SimpleDatum::Boolean(true)))
}

pub fn numeric_primitives() -> PrimitiveMap {
    let mut m: PrimitiveMap = HashMap::new();
    m.insert("+", add);
    m.insert("=", eq);
    m
}
