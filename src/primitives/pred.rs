use std::collections::HashMap;

use crate::datum::SimpleDatum;
use crate::evaler::{EvalError, EvalErrorKind};
use crate::object::{Object, ObjectRef};

use super::PrimitiveMap;

#[derive(PartialEq)]
enum Type {
    Boolean,
    Symbol,
    Char,
    Vector,
    Procedure,
    Pair,
    Number,
    String,
    Null,
}

fn pred(args: &[ObjectRef], t: Type) -> Result<ObjectRef, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::new(EvalErrorKind::ArityMismatch {
            expected: 1,
            got: args.len(),
            rest: false,
        }));
    }
    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Boolean(
        match &args[0] {
            ObjectRef::Object(o) => {
                t == match &**o {
                    Object::Atom(a) => match a {
                        SimpleDatum::Boolean(_) => Type::Boolean,
                        SimpleDatum::Number(_) => Type::Number,
                        SimpleDatum::Character(_) => Type::Char,
                        SimpleDatum::String(_) => Type::String,
                        SimpleDatum::Symbol(_) => Type::Symbol,
                    },
                    Object::Pair(_) => Type::Pair,
                    Object::Vector(_) => Type::Vector,
                    Object::Procedure(_) => Type::Procedure,
                }
            }
            ObjectRef::EmptyList => t == Type::Null,
            _ => false,
        },
    ))))
}

pub fn primitives() -> PrimitiveMap {
    let mut m: PrimitiveMap = HashMap::new();
    m.insert("boolean?", |args| pred(args, Type::Boolean));
    m.insert("symbol?", |args| pred(args, Type::Symbol));
    m.insert("char?", |args| pred(args, Type::Char));
    m.insert("vector?", |args| pred(args, Type::Vector));
    m.insert("procedure?", |args| pred(args, Type::Procedure));
    m.insert("pair?", |args| pred(args, Type::Pair));
    m.insert("number?", |args| pred(args, Type::Number));
    m.insert("string?", |args| pred(args, Type::String));
    m.insert("null?", |args| pred(args, Type::Null));
    m
}

pub const PRELUDE: &str = "
(define (not x) (if x #f #t))
";

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::*;

    #[test]
    fn test_number() {
        assert_eq!(
            pred(&[ObjectRef::new(atom_obj!(int_datum!(1)))], Type::Number),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(true))))
        );
        assert_eq!(
            pred(
                &[ObjectRef::new(atom_obj!(rational_datum!(1, 2)))],
                Type::Number
            ),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(true))))
        );
        assert_eq!(
            pred(&[ObjectRef::new(atom_obj!(real_datum!(0.5)))], Type::Number),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(true))))
        );
        assert_eq!(
            pred(
                &[ObjectRef::new(atom_obj!(bool_datum!(true)))],
                Type::Number
            ),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(false))))
        );
    }

    #[test]
    fn test_null() {
        assert_eq!(
            pred(&[ObjectRef::EmptyList], Type::Null),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(true))))
        );
        assert_eq!(
            pred(
                &[ObjectRef::new_pair(
                    ObjectRef::new(atom_obj!(int_datum!(1))),
                    ObjectRef::new(atom_obj!(int_datum!(2)))
                )],
                Type::Null
            ),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(false))))
        );
    }

    #[test]
    fn test_pair() {
        assert_eq!(
            pred(&[ObjectRef::EmptyList], Type::Pair),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(false))))
        );
        assert_eq!(
            pred(
                &[ObjectRef::new_pair(
                    ObjectRef::new(atom_obj!(int_datum!(1))),
                    ObjectRef::new(atom_obj!(int_datum!(2)))
                )],
                Type::Pair
            ),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(true))))
        );
    }
}