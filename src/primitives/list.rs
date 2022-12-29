use std::collections::HashMap;

use crate::evaler::{EvalError, EvalErrorKind};
use crate::object::{Object, ObjectRef};

use super::PrimitiveMap;

fn cons(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::new(EvalErrorKind::ArityMismatch {
            expected: 2,
            got: args.len(),
            rest: false,
        }));
    }
    Ok(ObjectRef::new_pair(args[0].clone(), args[1].clone()))
}

fn select(args: &[ObjectRef], first: bool) -> Result<ObjectRef, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::new(EvalErrorKind::ArityMismatch {
            expected: 1,
            got: args.len(),
            rest: false,
        }));
    }
    match &*args[0] {
        Object::Pair(p) => Ok(if first {
            p.borrow().0.clone()
        } else {
            p.borrow().1.clone()
        }),
        _ => Err(EvalError::new(EvalErrorKind::ContractViolation {
            expected: "pair".to_owned(),
            got: args[0].clone(),
        })),
    }
}

pub fn primitives() -> PrimitiveMap {
    let mut m: PrimitiveMap = HashMap::new();
    m.insert("cons", cons);
    m.insert("car", |args| select(args, true));
    m.insert("cdr", |args| select(args, false));
    m
}

pub const PRELUDE: &str = r#"
(define (list . args) args)
"#;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::*;

    #[test]
    fn test_cons() {
        assert!(ObjectRef::equal(
            &cons(&[
                ObjectRef::new(atom_obj!(int_datum!(1))),
                ObjectRef::new(atom_obj!(int_datum!(2)))
            ])
            .unwrap(),
            &ObjectRef::new_pair(
                ObjectRef::new(atom_obj!(int_datum!(1))),
                ObjectRef::new(atom_obj!(int_datum!(2)))
            )
        ));
    }

    #[test]
    fn test_car_cdr() {
        let p = ObjectRef::new_pair(
            ObjectRef::new(atom_obj!(int_datum!(1))),
            ObjectRef::new(atom_obj!(int_datum!(2))),
        );
        assert_eq!(
            select(&[p.clone()], true),
            Ok(ObjectRef::new(atom_obj!(int_datum!(1))))
        );
        assert_eq!(
            select(&[p], false),
            Ok(ObjectRef::new(atom_obj!(int_datum!(2))))
        );
    }
}
