use std::collections::HashMap;

use crate::datum::SimpleDatum;
use crate::evaler::{EvalError, EvalErrorKind};
use crate::number::*;
use crate::object::Object;

use num::BigInt;

use super::PrimitiveMap;

fn add(args: &[Object]) -> Result<Object, EvalError> {
    let mut sum = Number::Real(RealKind::Integer(BigInt::from(0)));
    for arg in args {
        match arg {
            Object::Atom(SimpleDatum::Number(n)) => sum += n.clone(),
            _ => {
                return Err(EvalError::new(EvalErrorKind::ContractViolation(
                    "number".to_owned(),
                )))
            }
        }
    }
    Ok(Object::Atom(SimpleDatum::Number(sum)))
}

pub fn numeric_primitives() -> PrimitiveMap {
    let mut m: PrimitiveMap = HashMap::new();
    m.insert("+", add);
    m
}
