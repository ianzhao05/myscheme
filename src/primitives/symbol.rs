use std::collections::HashMap;

use crate::datum::SimpleDatum;
use crate::evaler::EvalError;
use crate::interner::Symbol;
use crate::object::{Object, ObjectRef};

use super::utils::{ensure_arity, string_cv, symbol_cv, PrimitiveMap};

fn symbol_to_string(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 1);

    match &args[0].try_deref_or(symbol_cv)? {
        Object::Atom(SimpleDatum::Symbol(s)) => Ok(ObjectRef::new_string(s.to_string())),
        _ => Err(symbol_cv(&args[0])),
    }
}

fn string_to_symbol(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 1);

    match &args[0].try_deref_or(string_cv)? {
        Object::Atom(SimpleDatum::String(s)) => Ok(ObjectRef::new(Object::Atom(
            SimpleDatum::Symbol(Symbol::from(s.borrow().as_ref())),
        ))),
        _ => Err(string_cv(&args[0])),
    }
}

pub fn primitives() -> PrimitiveMap {
    let mut m: PrimitiveMap = HashMap::new();
    m.insert("symbol->string", symbol_to_string);
    m.insert("string->symbol", string_to_symbol);
    m
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbol_to_string() {
        assert!(ObjectRef::equal(
            &symbol_to_string(&[ObjectRef::new(Object::Atom(SimpleDatum::Symbol(
                "foo".into()
            )))])
            .unwrap(),
            &ObjectRef::new_string("foo".into())
        ));
    }

    #[test]
    fn test_string_to_symbol() {
        assert_eq!(
            string_to_symbol(&[ObjectRef::new_string("foo".to_owned())]),
            Ok(ObjectRef::new(Object::Atom(SimpleDatum::Symbol(
                "foo".into()
            ))))
        );
    }
}
