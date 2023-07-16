use std::collections::HashMap;

use crate::datum::SimpleDatum;
use crate::evaler::EvalError;
use crate::interner::Symbol;
use crate::object::{Object, ObjectRef};

use super::utils::{ensure_arity, get_string, get_symbol, PrimitiveMap};

fn symbol_to_string(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 1);

    Ok(ObjectRef::new_string(get_symbol(&args[0])?.to_string()))
}

fn string_to_symbol(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 1);

    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Symbol(
        Symbol::from(get_string(&args[0])?.borrow().as_ref()),
    ))))
}

pub(super) fn primitives() -> PrimitiveMap {
    let mut m: PrimitiveMap = HashMap::new();
    m.insert("symbol->string", symbol_to_string);
    m.insert("string->symbol", string_to_symbol);
    m
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::evaler::EvalErrorKind;
    use crate::primitives::utils::symbol_cv;
    use crate::test_util::*;

    #[test]
    fn test_symbol_to_string() {
        assert!(ObjectRef::equal(
            &symbol_to_string(&[ObjectRef::new(Object::Atom(SimpleDatum::Symbol(
                "foo".into()
            )))])
            .unwrap(),
            &ObjectRef::new_string("foo".into())
        ));

        assert_eq!(
            symbol_to_string(&[ObjectRef::new(atom_obj!(int_datum!(3)))]),
            Err(symbol_cv(&ObjectRef::new(atom_obj!(int_datum!(3)))))
        );
    }

    #[test]
    fn test_string_to_symbol() {
        assert_eq!(
            string_to_symbol(&[ObjectRef::new_string("foo".to_owned())]),
            Ok(ObjectRef::new(Object::Atom(SimpleDatum::Symbol(
                "foo".into()
            ))))
        );

        assert_eq!(
            string_to_symbol(&[
                ObjectRef::new_string("foo".to_owned()),
                ObjectRef::new_string("bar".to_owned())
            ]),
            Err(EvalError::new(EvalErrorKind::ArityMismatch {
                expected: 1,
                max_expected: 1,
                got: 2
            }))
        );
    }
}
