use std::cmp::Ordering;
use std::collections::HashMap;

use crate::datum::SimpleDatum;
use crate::evaler::{EvalError, EvalErrorKind};
use crate::number::*;
use crate::object::{Object, ObjectRef};

use num::BigInt;

use super::PrimitiveMap;

fn num_cv(got: &ObjectRef) -> EvalError {
    EvalError::new(EvalErrorKind::ContractViolation {
        expected: "number".to_owned(),
        got: got.clone(),
    })
}

fn add(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    let mut sum = Number::Real(RealKind::Integer(BigInt::from(0)));
    for arg in args {
        match &**arg {
            Object::Atom(SimpleDatum::Number(n)) => sum += n.clone(),
            _ => return Err(num_cv(arg)),
        }
    }
    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Number(sum))))
}

fn sub(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    if args.is_empty() {
        return Err(EvalError::new(EvalErrorKind::ArityMismatch {
            expected: 1,
            got: args.len(),
            rest: true,
        }));
    }
    let first = match &*args[0] {
        Object::Atom(SimpleDatum::Number(n)) => n,
        _ => return Err(num_cv(&args[0])),
    };
    if args.len() == 1 {
        return Ok(ObjectRef::new(Object::Atom(SimpleDatum::Number(
            -first.clone(),
        ))));
    }
    let mut res = first.clone();
    for arg in &args[1..] {
        match &**arg {
            Object::Atom(SimpleDatum::Number(n)) => res -= n.clone(),
            _ => return Err(num_cv(arg)),
        }
    }
    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Number(res))))
}

fn mul(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    let mut prod = Number::Real(RealKind::Integer(BigInt::from(1)));
    for arg in args {
        match &**arg {
            Object::Atom(SimpleDatum::Number(n)) => prod *= n.clone(),
            _ => return Err(num_cv(arg)),
        }
    }
    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Number(prod))))
}

fn div(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    if args.is_empty() {
        return Err(EvalError::new(EvalErrorKind::ArityMismatch {
            expected: 1,
            got: args.len(),
            rest: true,
        }));
    }
    let first = match &*args[0] {
        Object::Atom(SimpleDatum::Number(n)) => n,
        _ => return Err(num_cv(&args[0])),
    };
    let zero = Number::Real(RealKind::Integer(BigInt::from(0)));
    if args.len() == 1 {
        if Number::eq_val(first, &zero) {
            return Err(EvalError::new(EvalErrorKind::ZeroDivision));
        }
        return Ok(ObjectRef::new(Object::Atom(SimpleDatum::Number(
            Number::Real(RealKind::Integer(BigInt::from(1))) / first.clone(),
        ))));
    }
    let mut res = first.clone();
    for arg in &args[1..] {
        match &**arg {
            Object::Atom(SimpleDatum::Number(n)) => {
                if Number::eq_val(n, &zero) {
                    return Err(EvalError::new(EvalErrorKind::ZeroDivision));
                }
                res /= n.clone()
            }
            _ => return Err(num_cv(arg)),
        }
    }
    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Number(res))))
}

fn eq(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    if args.is_empty() {
        return Err(EvalError::new(EvalErrorKind::ArityMismatch {
            expected: 1,
            got: args.len(),
            rest: true,
        }));
    }
    let first = match &*args[0] {
        Object::Atom(SimpleDatum::Number(n)) => n,
        _ => return Err(num_cv(&args[0])),
    };
    for arg in &args[1..] {
        match &**arg {
            Object::Atom(SimpleDatum::Number(n)) => {
                if !Number::eq_val(first, n) {
                    return Ok(ObjectRef::new(Object::Atom(SimpleDatum::Boolean(false))));
                }
            }
            _ => return Err(num_cv(arg)),
        }
    }
    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Boolean(true))))
}

fn cmp(args: &[ObjectRef], ord: Ordering, strict: bool) -> Result<ObjectRef, EvalError> {
    if args.is_empty() {
        return Err(EvalError::new(EvalErrorKind::ArityMismatch {
            expected: 1,
            got: args.len(),
            rest: true,
        }));
    }
    for w in args.windows(2) {
        match &*w[0] {
            Object::Atom(SimpleDatum::Number(Number::Real(n1))) => match &*w[1] {
                Object::Atom(SimpleDatum::Number(Number::Real(n2))) => {
                    if strict && n1.partial_cmp(n2).unwrap() != ord
                        || !strict && n1.partial_cmp(n2).unwrap() == ord.reverse()
                    {
                        return Ok(ObjectRef::new(Object::Atom(SimpleDatum::Boolean(false))));
                    }
                }
                _ => return Err(num_cv(&w[1])),
            },
            _ => return Err(num_cv(&w[0])),
        }
    }
    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Boolean(true))))
}

pub fn primitives() -> PrimitiveMap {
    let mut m: PrimitiveMap = HashMap::new();
    m.insert("+", add);
    m.insert("-", sub);
    m.insert("*", mul);
    m.insert("/", div);
    m.insert("=", eq);
    m.insert("<", |args| cmp(args, Ordering::Less, true));
    m.insert(">", |args| cmp(args, Ordering::Greater, true));
    m.insert("<=", |args| cmp(args, Ordering::Less, false));
    m.insert(">=", |args| cmp(args, Ordering::Greater, false));
    m
}

pub const PRELUDE: &str = "";
