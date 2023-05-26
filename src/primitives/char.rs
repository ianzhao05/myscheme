use std::cmp::Ordering;
use std::collections::HashMap;

use crate::datum::SimpleDatum;
use crate::evaler::EvalError;
use crate::number::{Number, RealKind};
use crate::object::{Object, ObjectRef};

use super::utils::{char_cv, charval_cv, ensure_arity, PrimitiveMap};

fn char_cmp(
    args: &[ObjectRef],
    ord: Ordering,
    strict: bool,
    ci: bool,
) -> Result<ObjectRef, EvalError> {
    debug_assert!(ord != Ordering::Equal || strict);
    ensure_arity!(args, 2);

    let mut it = args.iter().map(|arg| match arg.try_deref_or(char_cv)? {
        Object::Atom(SimpleDatum::Character(c)) => Ok(*c),
        _ => Err(char_cv(&args[1])),
    });
    let c1 = it.next().unwrap()?;
    let c2 = it.next().unwrap()?;

    let cmp = if ci {
        c1.to_lowercase().cmp(c2.to_lowercase())
    } else {
        c1.cmp(&c2)
    };
    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Boolean(
        if strict {
            cmp == ord
        } else {
            cmp != ord.reverse()
        },
    ))))
}

fn char_map(
    args: &[ObjectRef],
    f: impl FnOnce(char) -> SimpleDatum,
) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 1);

    match args[0].try_deref_or(char_cv)? {
        Object::Atom(SimpleDatum::Character(c)) => Ok(ObjectRef::new(Object::Atom(f(*c)))),
        _ => Err(char_cv(&args[0])),
    }
}

fn int_to_char(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 1);

    let arg = &args[0];
    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Character(
        match arg.try_deref_or(charval_cv)? {
            Object::Atom(SimpleDatum::Number(Number::Real(n))) => {
                let i: u32 = match n {
                    RealKind::Rational(r) if r.is_integer() => {
                        r.to_integer().try_into().map_err(|_| charval_cv(&arg))?
                    }
                    RealKind::Integer(bi) => bi.try_into().map_err(|_| charval_cv(&arg))?,
                    _ => return Err(charval_cv(&arg)),
                };
                std::char::from_u32(i).ok_or_else(|| charval_cv(&arg))?
            }
            _ => return Err(charval_cv(&arg)),
        },
    ))))
}

pub fn primitives() -> PrimitiveMap {
    let mut m: PrimitiveMap = HashMap::new();
    m.insert("char=?", |args| {
        char_cmp(args, Ordering::Equal, true, false)
    });
    m.insert("char<?", |args| char_cmp(args, Ordering::Less, true, false));
    m.insert("char>?", |args| {
        char_cmp(args, Ordering::Greater, true, false)
    });
    m.insert("char<=?", |args| {
        char_cmp(args, Ordering::Less, false, false)
    });
    m.insert("char>=?", |args| {
        char_cmp(args, Ordering::Greater, false, false)
    });
    m.insert("char-ci=?", |args| {
        char_cmp(args, Ordering::Equal, true, true)
    });
    m.insert("char-ci<?", |args| {
        char_cmp(args, Ordering::Less, true, true)
    });
    m.insert("char-ci>?", |args| {
        char_cmp(args, Ordering::Greater, true, true)
    });
    m.insert("char-ci<=?", |args| {
        char_cmp(args, Ordering::Less, false, true)
    });
    m.insert("char-ci>=?", |args| {
        char_cmp(args, Ordering::Greater, false, true)
    });
    m.insert("char-alphabetic?", |args| {
        char_map(args, |c| SimpleDatum::Boolean(c.is_ascii_alphabetic()))
    });
    m.insert("char-numeric?", |args| {
        char_map(args, |c| SimpleDatum::Boolean(c.is_ascii_digit()))
    });
    m.insert("char-whitespace?", |args| {
        char_map(args, |c| SimpleDatum::Boolean(c.is_ascii_whitespace()))
    });
    m.insert("char-upper-case?", |args| {
        char_map(args, |c| SimpleDatum::Boolean(c.is_ascii_uppercase()))
    });
    m.insert("char-lower-case?", |args| {
        char_map(args, |c| SimpleDatum::Boolean(c.is_ascii_lowercase()))
    });
    m.insert("char-upcase", |args| {
        char_map(args, |c| SimpleDatum::Character(c.to_ascii_uppercase()))
    });
    m.insert("char-downcase", |args| {
        char_map(args, |c| SimpleDatum::Character(c.to_ascii_lowercase()))
    });
    m.insert("char->integer", |args| {
        char_map(args, |c| {
            SimpleDatum::Number(Number::Real(RealKind::Integer((c as u32).into())))
        })
    });
    m.insert("integer->char", int_to_char);
    m
}
