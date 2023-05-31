use std::collections::HashMap;

use num::bigint::Sign;

use crate::datum::SimpleDatum;
use crate::evaler::EvalError;
use crate::number::{Number, RealKind};
use crate::object::{Object, ObjectRef};

pub type PrimitiveMap = HashMap<&'static str, fn(&[ObjectRef]) -> Result<ObjectRef, EvalError>>;

pub fn merge(maps: &[PrimitiveMap]) -> PrimitiveMap {
    let mut m: PrimitiveMap = HashMap::new();
    for n in maps {
        m.extend(n);
    }
    m
}

pub fn get_len(arg: &ObjectRef) -> Result<usize, EvalError> {
    match arg.try_deref_or(len_cv)? {
        Object::Atom(SimpleDatum::Number(Number::Real(n))) => match n {
            RealKind::Rational(r) if r.is_integer() && r.numer().sign() != Sign::Minus => {
                r.to_integer().try_into().map_err(|_| len_cv(arg))
            }
            RealKind::Integer(i) if i.sign() != Sign::Minus => {
                i.try_into().map_err(|_| len_cv(arg))
            }
            _ => return Err(len_cv(arg)),
        },
        _ => return Err(len_cv(arg)),
    }
}

macro_rules! ensure_arity {
    ($args:expr, $n:expr) => {
        if $args.len() != $n {
            return Err($crate::evaler::EvalError::new(
                $crate::evaler::EvalErrorKind::ArityMismatch {
                    expected: $n,
                    max_expected: $n,
                    got: $args.len(),
                },
            ));
        }
    };

    ($args:expr, $n:expr, $m:expr) => {
        debug_assert!($n <= $m);
        if !($n..=$m).contains(&$args.len()) {
            return Err($crate::evaler::EvalError::new(
                $crate::evaler::EvalErrorKind::ArityMismatch {
                    expected: $n,
                    max_expected: $m,
                    got: $args.len(),
                },
            ));
        }
    };
}
pub(crate) use ensure_arity;

macro_rules! cv_fn {
    ($fn_name:ident, $s:expr) => {
        pub(crate) fn $fn_name(got: &$crate::object::ObjectRef) -> $crate::evaler::EvalError {
            $crate::evaler::EvalError::new($crate::evaler::EvalErrorKind::ContractViolation {
                expected: $s,
                got: got.clone(),
            })
        }
    };
}

cv_fn!(num_cv, "number");
cv_fn!(int_cv, "integer");
cv_fn!(symbol_cv, "symbol");
cv_fn!(char_cv, "char");
cv_fn!(pair_cv, "pair");
cv_fn!(string_cv, "string");
cv_fn!(vector_cv, "vector");
cv_fn!(len_cv, "valid length");
cv_fn!(oport_cv, "output-port");
cv_fn!(iport_cv, "input-port");
cv_fn!(charval_cv, "character value");

#[cfg(test)]
mod tests {
    use super::{get_len, len_cv};
    use crate::object::ObjectRef;
    use crate::test_util::*;

    #[test]
    fn test_get_len() {
        assert_eq!(get_len(&ObjectRef::new(atom_obj!(int_datum!(1)))), Ok(1));
        assert_eq!(get_len(&ObjectRef::new(atom_obj!(int_datum!(56)))), Ok(56));
        assert_eq!(
            get_len(&ObjectRef::new(atom_obj!(rational_datum!(5, 1)))),
            Ok(5)
        );
        assert_eq!(
            get_len(&ObjectRef::new(atom_obj!(int_datum!(-1)))),
            Err(len_cv(&ObjectRef::new(atom_obj!(int_datum!(-1)))))
        );
        assert_eq!(
            get_len(&ObjectRef::new(atom_obj!(real_datum!(3.0)))),
            Err(len_cv(&ObjectRef::new(atom_obj!(real_datum!(3.0)))))
        );
        assert_eq!(
            get_len(&ObjectRef::new(atom_obj!(rational_datum!(8, 5)))),
            Err(len_cv(&ObjectRef::new(atom_obj!(rational_datum!(8, 5)))))
        );
    }
}
