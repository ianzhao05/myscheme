use std::cell::RefCell;
use std::collections::HashMap;

use num::bigint::Sign;

use crate::cont::State;
use crate::datum::SimpleDatum;
use crate::evaler::EvalError;
use crate::interner::Symbol;
use crate::number::{Number, RealKind};
use crate::object::{EnvSpec, Object, ObjectRef};
use crate::port::{IPort, Port};
use crate::trampoline::Bouncer;

pub type PrimitiveMap = HashMap<&'static str, fn(&[ObjectRef]) -> Result<ObjectRef, EvalError>>;
pub type ControlMap = HashMap<&'static str, fn(State) -> Bouncer>;

pub fn get_num(arg: &ObjectRef) -> Result<&Number, EvalError> {
    match arg.try_deref_or(num_cv)? {
        Object::Atom(SimpleDatum::Number(n)) => Ok(n),
        _ => Err(num_cv(arg)),
    }
}

pub fn get_int(arg: &ObjectRef) -> Result<&Number, EvalError> {
    match arg.try_deref_or(int_cv)? {
        Object::Atom(SimpleDatum::Number(n)) if n.is_integer() => Ok(n),
        _ => Err(int_cv(arg)),
    }
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
            _ => Err(len_cv(arg)),
        },
        _ => Err(len_cv(arg)),
    }
}

pub fn get_radix(arg: &ObjectRef) -> Result<u32, EvalError> {
    let radix: u32 = match arg.try_deref_or(radix_cv)? {
        Object::Atom(SimpleDatum::Number(Number::Real(n))) => match n {
            RealKind::Rational(r) if r.is_integer() && r.numer().sign() != Sign::Minus => {
                r.to_integer().try_into().map_err(|_| radix_cv(arg))?
            }
            RealKind::Integer(i) if i.sign() != Sign::Minus => {
                i.try_into().map_err(|_| radix_cv(arg))?
            }
            _ => return Err(radix_cv(arg)),
        },
        _ => return Err(radix_cv(arg)),
    };
    match radix {
        2 | 8 | 10 | 16 => Ok(radix),
        _ => Err(radix_cv(arg)),
    }
}

pub fn get_string(arg: &ObjectRef) -> Result<&RefCell<String>, EvalError> {
    match arg.try_deref_or(string_cv)? {
        Object::Atom(SimpleDatum::String(s)) => Ok(s),
        _ => Err(string_cv(arg)),
    }
}

pub fn get_char(arg: &ObjectRef) -> Result<char, EvalError> {
    match arg.try_deref_or(char_cv)? {
        Object::Atom(SimpleDatum::Character(c)) => Ok(*c),
        _ => Err(char_cv(arg)),
    }
}

pub fn get_symbol(arg: &ObjectRef) -> Result<Symbol, EvalError> {
    match arg.try_deref_or(symbol_cv)? {
        Object::Atom(SimpleDatum::Symbol(s)) => Ok(*s),
        _ => Err(symbol_cv(arg)),
    }
}

pub fn get_pair(arg: &ObjectRef) -> Result<&RefCell<(ObjectRef, ObjectRef)>, EvalError> {
    match arg.try_deref_or(pair_cv)? {
        Object::Pair(p) => Ok(p),
        _ => Err(pair_cv(arg)),
    }
}

pub fn get_vector(arg: &ObjectRef) -> Result<&RefCell<Vec<ObjectRef>>, EvalError> {
    match arg.try_deref_or(vector_cv)? {
        Object::Vector(v) => Ok(v),
        _ => Err(vector_cv(arg)),
    }
}

pub fn get_iport(arg: &ObjectRef) -> Result<&RefCell<Option<IPort>>, EvalError> {
    match arg.try_deref_or(iport_cv)? {
        Object::Port(Port::Input(ip)) => Ok(ip),
        _ => Err(iport_cv(arg)),
    }
}

pub fn get_oport(arg: &ObjectRef) -> Result<&RefCell<Option<Box<dyn std::io::Write>>>, EvalError> {
    match arg.try_deref_or(oport_cv)? {
        Object::Port(Port::Output(op)) => Ok(op),
        _ => Err(oport_cv(arg)),
    }
}

pub fn get_version(arg: &ObjectRef) -> Result<(), EvalError> {
    match arg.try_deref_or(five_cv)? {
        Object::Atom(SimpleDatum::Number(Number::Real(n)))
            if n.is_exact() && n == &RealKind::Integer(5.into()) =>
        {
            Ok(())
        }
        _ => Err(five_cv(arg)),
    }
}

pub fn get_env(arg: &ObjectRef) -> Result<EnvSpec, EvalError> {
    match arg {
        ObjectRef::EnvSpec(env) => Ok(*env),
        _ => Err(env_cv(arg)),
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
cv_fn!(radix_cv, "radix");
cv_fn!(symbol_cv, "symbol");
cv_fn!(char_cv, "char");
cv_fn!(pair_cv, "pair");
cv_fn!(string_cv, "string");
cv_fn!(vector_cv, "vector");
cv_fn!(len_cv, "valid length");
cv_fn!(oport_cv, "output-port");
cv_fn!(iport_cv, "input-port");
cv_fn!(charval_cv, "character value");
cv_fn!(expr_cv, "expr");
cv_fn!(five_cv, "5");
cv_fn!(env_cv, "environment");

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
