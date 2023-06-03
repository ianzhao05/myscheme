use std::cmp::Ordering;
use std::collections::HashMap;

use crate::datum::SimpleDatum;
use crate::evaler::EvalError;
use crate::number::{Number, RealKind};
use crate::object::{Object, ObjectRef};

use super::utils::{charval_cv, ensure_arity, get_char, PrimitiveMap};

fn char_cmp(
    args: &[ObjectRef],
    ord: Ordering,
    strict: bool,
    ci: bool,
) -> Result<ObjectRef, EvalError> {
    debug_assert!(ord != Ordering::Equal || strict);
    ensure_arity!(args, 2);

    let c1 = get_char(&args[0])?;
    let c2 = get_char(&args[1])?;

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

    Ok(ObjectRef::new(Object::Atom(f(get_char(&args[0])?))))
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::primitives::utils::char_cv;
    use crate::test_util::*;

    #[test]
    fn test_char_cmp() {
        assert_eq!(
            char_cmp(
                &[
                    ObjectRef::new(atom_obj!(char_datum!('x'))),
                    ObjectRef::new(atom_obj!(char_datum!('x'))),
                ],
                Ordering::Equal,
                true,
                false
            ),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(true))))
        );
        assert_eq!(
            char_cmp(
                &[
                    ObjectRef::new(atom_obj!(char_datum!('x'))),
                    ObjectRef::new(atom_obj!(char_datum!('X'))),
                ],
                Ordering::Equal,
                true,
                false
            ),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(false))))
        );
        assert_eq!(
            char_cmp(
                &[
                    ObjectRef::new(atom_obj!(char_datum!('x'))),
                    ObjectRef::new(atom_obj!(char_datum!('X'))),
                ],
                Ordering::Equal,
                true,
                true
            ),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(true))))
        );
        assert_eq!(
            char_cmp(
                &[
                    ObjectRef::new(atom_obj!(char_datum!('a'))),
                    ObjectRef::new(atom_obj!(char_datum!('b'))),
                ],
                Ordering::Less,
                true,
                false
            ),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(true))))
        );
        assert_eq!(
            char_cmp(
                &[
                    ObjectRef::new(atom_obj!(char_datum!('a'))),
                    ObjectRef::new(atom_obj!(char_datum!('a'))),
                ],
                Ordering::Less,
                true,
                false
            ),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(false))))
        );
        assert_eq!(
            char_cmp(
                &[
                    ObjectRef::new(atom_obj!(char_datum!('a'))),
                    ObjectRef::new(atom_obj!(char_datum!('A'))),
                ],
                Ordering::Less,
                false,
                true
            ),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(true))))
        );
        assert_eq!(
            char_cmp(
                &[
                    ObjectRef::new(atom_obj!(char_datum!('a'))),
                    ObjectRef::new(atom_obj!(char_datum!('a'))),
                ],
                Ordering::Less,
                false,
                false
            ),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(true))))
        );
    }

    #[test]
    fn test_char_map() {
        assert_eq!(
            char_map(&[ObjectRef::new(atom_obj!(char_datum!('a')))], |c| {
                SimpleDatum::Character(c.to_ascii_uppercase())
            }),
            Ok(ObjectRef::new(atom_obj!(char_datum!('A'))))
        );
        let s = ObjectRef::new_string("a".to_owned());
        assert_eq!(
            char_map(&[s.clone()], |c| {
                SimpleDatum::Character(c.to_ascii_uppercase())
            }),
            Err(char_cv(&s))
        );
    }

    #[test]
    fn test_int_to_char() {
        assert_eq!(
            int_to_char(&[ObjectRef::new(atom_obj!(int_datum!(97)))]),
            Ok(ObjectRef::new(atom_obj!(char_datum!('a'))))
        );
        assert_eq!(
            int_to_char(&[ObjectRef::new(atom_obj!(int_datum!(0)))]),
            Ok(ObjectRef::new(atom_obj!(char_datum!('\0'))))
        );
        assert_eq!(
            int_to_char(&[ObjectRef::new(atom_obj!(int_datum!(1000000000)))]),
            Err(charval_cv(&ObjectRef::new(atom_obj!(int_datum!(
                1000000000
            )))))
        );
        assert_eq!(
            int_to_char(&[ObjectRef::new(atom_obj!(int_datum!(-1)))]),
            Err(charval_cv(&ObjectRef::new(atom_obj!(int_datum!(-1)))))
        )
    }
}
