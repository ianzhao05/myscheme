use std::cmp::Ordering;
use std::collections::HashMap;

use crate::datum::SimpleDatum;
use crate::evaler::{EvalError, EvalErrorKind};
use crate::number::{Number, RealKind};
use crate::object::{Object, ObjectRef};

use super::utils::{char_cv, ensure_arity, get_len, string_cv, PrimitiveMap};

fn make_string(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 1, 2);

    let k = get_len(&args[0])?;
    let fill = if args.len() == 2 {
        match args[1].try_deref_or(char_cv)? {
            Object::Atom(SimpleDatum::Character(c)) => c.to_string(),
            _ => return Err(char_cv(&args[1])),
        }
    } else {
        "\0".to_owned()
    };
    Ok(ObjectRef::new_string(fill.repeat(k)))
}

fn string(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    Ok(ObjectRef::new_string(
        args.iter()
            .map(|arg| match arg.try_deref_or(char_cv)? {
                Object::Atom(SimpleDatum::Character(c)) => Ok(*c),
                _ => Err(char_cv(&args[1])),
            })
            .collect::<Result<String, _>>()?,
    ))
}

fn string_length(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 1);

    match args[0].try_deref_or(string_cv)? {
        Object::Atom(SimpleDatum::String(s)) => Ok(ObjectRef::new(Object::Atom(
            SimpleDatum::Number(Number::Real(RealKind::Integer(s.borrow().len().into()))),
        ))),
        _ => Err(string_cv(&args[0])),
    }
}

fn string_ref(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 2);

    match args[0].try_deref_or(string_cv)? {
        Object::Atom(SimpleDatum::String(s)) => {
            let i = get_len(&args[1])?;
            let b = s.borrow();
            if i >= b.len() {
                return Err(EvalError::new(EvalErrorKind::IndexOutOfBounds {
                    index: i,
                    len: b.len(),
                }));
            }
            Ok(ObjectRef::new(Object::Atom(SimpleDatum::Character(
                b[i..]
                    .chars()
                    .next()
                    .expect("ASCII string should not error"),
            ))))
        }
        _ => Err(string_cv(&args[0])),
    }
}

fn string_set(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 3);

    match args[0].try_deref_or(string_cv)? {
        Object::Atom(SimpleDatum::String(s)) => {
            let i = get_len(&args[1])?;
            let mut b = s.borrow_mut();
            if i >= b.len() {
                return Err(EvalError::new(EvalErrorKind::IndexOutOfBounds {
                    index: i,
                    len: b.len(),
                }));
            }
            let mut buf = [0];
            match args[2].try_deref_or(char_cv)? {
                Object::Atom(SimpleDatum::Character(c)) => {
                    b.replace_range(i..i + 1, c.encode_utf8(&mut buf));
                    Ok(ObjectRef::Void)
                }
                _ => Err(char_cv(&args[2])),
            }
        }
        _ => Err(string_cv(&args[0])),
    }
}

fn string_fill(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 2);

    match args[0].try_deref_or(string_cv)? {
        Object::Atom(SimpleDatum::String(s)) => match args[1].try_deref_or(char_cv)? {
            Object::Atom(SimpleDatum::Character(c)) => {
                let mut b = s.borrow_mut();
                *b = c.to_string().repeat(b.len());
                Ok(ObjectRef::Void)
            }
            _ => Err(char_cv(&args[1])),
        },
        _ => Err(string_cv(&args[0])),
    }
}

fn string_cmp(
    args: &[ObjectRef],
    ord: Ordering,
    strict: bool,
    ci: bool,
) -> Result<ObjectRef, EvalError> {
    debug_assert!(ord != Ordering::Equal || strict);
    ensure_arity!(args, 2);

    let mut it = args.iter().map(|arg| match arg.try_deref_or(string_cv)? {
        Object::Atom(SimpleDatum::String(s)) => Ok(s.borrow()),
        _ => Err(string_cv(arg)),
    });
    let s1 = it.next().unwrap()?;
    let s2 = it.next().unwrap()?;

    let cmp = if ci {
        s1.to_lowercase().cmp(&s2.to_lowercase())
    } else {
        s1.cmp(&s2)
    };
    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Boolean(
        if strict {
            cmp == ord
        } else {
            cmp != ord.reverse()
        },
    ))))
}

fn string_copy(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 1);

    match args[0].try_deref_or(string_cv)? {
        Object::Atom(SimpleDatum::String(s)) => Ok(ObjectRef::new_string(s.borrow().clone())),
        _ => Err(string_cv(&args[0])),
    }
}

fn substring(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 3);

    match args[0].try_deref_or(string_cv)? {
        Object::Atom(SimpleDatum::String(s)) => {
            let start = get_len(&args[1])?;
            let end = get_len(&args[2])?;
            if start > end {
                return Err(EvalError::new(EvalErrorKind::IndexOutOfBounds {
                    index: start - 1,
                    len: end,
                }));
            }
            let b = s.borrow();
            if end > b.len() {
                return Err(EvalError::new(EvalErrorKind::IndexOutOfBounds {
                    index: end - 1,
                    len: b.len(),
                }));
            }
            Ok(ObjectRef::new_string(b[start..end].to_owned()))
        }
        _ => Err(string_cv(&args[0])),
    }
}

fn string_append(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    let mut acc = String::new();
    for arg in args {
        match arg.try_deref_or(string_cv)? {
            Object::Atom(SimpleDatum::String(s)) => acc.push_str(&s.borrow()),
            _ => return Err(string_cv(&args[1])),
        }
    }
    Ok(ObjectRef::new_string(acc))
}

pub fn primitives() -> PrimitiveMap {
    let mut m: PrimitiveMap = HashMap::new();
    m.insert("make-string", make_string);
    m.insert("string", string);
    m.insert("string-length", string_length);
    m.insert("string-ref", string_ref);
    m.insert("string-set!", string_set);
    m.insert("string-fill!", string_fill);
    m.insert("string=?", |args| {
        string_cmp(args, Ordering::Equal, true, false)
    });
    m.insert("string<?", |args| {
        string_cmp(args, Ordering::Less, true, false)
    });
    m.insert("string>?", |args| {
        string_cmp(args, Ordering::Greater, true, false)
    });
    m.insert("string<=?", |args| {
        string_cmp(args, Ordering::Less, false, false)
    });
    m.insert("string>=?", |args| {
        string_cmp(args, Ordering::Greater, false, false)
    });
    m.insert("string-ci=?", |args| {
        string_cmp(args, Ordering::Equal, true, true)
    });
    m.insert("string-ci<?", |args| {
        string_cmp(args, Ordering::Less, true, true)
    });
    m.insert("string-ci>?", |args| {
        string_cmp(args, Ordering::Greater, true, true)
    });
    m.insert("string-ci<=?", |args| {
        string_cmp(args, Ordering::Less, false, true)
    });
    m.insert("string-ci>=?", |args| {
        string_cmp(args, Ordering::Greater, false, true)
    });
    m.insert("string-copy", string_copy);
    m.insert("substring", substring);
    m.insert("string-append", string_append);
    m
}

pub const PRELUDE: &str = "
(define (string->list s)
  (let loop ((i (- (string-length s) 1)) (acc '()))
    (if (< i 0)
        acc
        (loop (- i 1) (cons (string-ref s i) acc)))))

(define (list->string l)
  (apply string l))

(define (string-copy s)
  (substring s 0 (string-length s)))
";

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::*;

    #[test]
    fn test_make_string() {
        assert!(ObjectRef::equal(
            &make_string(&[ObjectRef::new(atom_obj!(int_datum!(3)))]).unwrap(),
            &ObjectRef::new_string("\0\0\0".to_owned())
        ));
        assert!(ObjectRef::equal(
            &make_string(&[
                ObjectRef::new(atom_obj!(int_datum!(3))),
                ObjectRef::new(atom_obj!(char_datum!('z')))
            ])
            .unwrap(),
            &ObjectRef::new_string("zzz".to_owned())
        ));
    }

    #[test]
    fn test_string() {
        assert!(ObjectRef::equal(
            &string(&[
                ObjectRef::new(atom_obj!(char_datum!('f'))),
                ObjectRef::new(atom_obj!(char_datum!('o'))),
                ObjectRef::new(atom_obj!(char_datum!('o')))
            ])
            .unwrap(),
            &ObjectRef::new_string("foo".to_owned())
        ));
        assert_eq!(
            string(&[
                ObjectRef::new(atom_obj!(char_datum!('f'))),
                ObjectRef::new(atom_obj!(int_datum!(1)))
            ]),
            Err(char_cv(&ObjectRef::new(atom_obj!(int_datum!(1)))))
        );
    }

    #[test]
    fn test_string_length() {
        assert_eq!(
            string_length(&[ObjectRef::new_string("foo".to_owned())]).unwrap(),
            ObjectRef::new(atom_obj!(int_datum!(3)))
        );
        assert_eq!(
            string_length(&[ObjectRef::new_string("".to_owned())]).unwrap(),
            ObjectRef::new(atom_obj!(int_datum!(0)))
        );
        assert_eq!(
            string_length(&[ObjectRef::new(atom_obj!(char_datum!('x')))]),
            Err(string_cv(&ObjectRef::new(atom_obj!(char_datum!('x')))))
        );
    }

    #[test]
    fn test_string_ref() {
        assert_eq!(
            string_ref(&[
                ObjectRef::new_string("01234".to_owned()),
                ObjectRef::new(atom_obj!(int_datum!(3)))
            ]),
            Ok(ObjectRef::new(atom_obj!(char_datum!('3'))))
        );
        assert_eq!(
            string_ref(&[
                ObjectRef::new_string("01234".to_owned()),
                ObjectRef::new(atom_obj!(int_datum!(5)))
            ]),
            Err(EvalError::new(EvalErrorKind::IndexOutOfBounds {
                index: 5,
                len: 5
            }))
        );
    }

    #[test]
    fn test_string_set() {
        let s = ObjectRef::new_string("abcde".to_owned());
        assert_eq!(
            string_set(&[
                s.clone(),
                ObjectRef::new(atom_obj!(int_datum!(2))),
                ObjectRef::new(atom_obj!(char_datum!('x')))
            ]),
            Ok(ObjectRef::Void)
        );
        assert!(ObjectRef::equal(
            &s,
            &ObjectRef::new_string("abxde".to_owned())
        ));
        assert_eq!(
            string_set(&[
                s.clone(),
                ObjectRef::new(atom_obj!(int_datum!(5))),
                ObjectRef::new(atom_obj!(char_datum!('x')))
            ]),
            Err(EvalError::new(EvalErrorKind::IndexOutOfBounds {
                index: 5,
                len: 5
            }))
        );
        assert_eq!(
            string_set(&[
                s.clone(),
                ObjectRef::new(atom_obj!(int_datum!(0))),
                ObjectRef::new(atom_obj!(int_datum!(1)))
            ]),
            Err(char_cv(&ObjectRef::new(atom_obj!(int_datum!(1)))))
        );
    }

    #[test]
    fn test_string_fill() {
        let s = ObjectRef::new_string("abcde".to_owned());
        assert_eq!(
            string_fill(&[s.clone(), ObjectRef::new(atom_obj!(char_datum!('x')))]),
            Ok(ObjectRef::Void)
        );
        assert!(ObjectRef::equal(
            &s,
            &ObjectRef::new_string("xxxxx".to_owned())
        ));
    }

    #[test]
    fn test_string_cmp() {
        assert_eq!(
            string_cmp(
                &[
                    ObjectRef::new_string("hello world".to_owned()),
                    ObjectRef::new_string("hello world".to_owned())
                ],
                Ordering::Equal,
                true,
                false
            ),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(true))))
        );
        assert_eq!(
            string_cmp(
                &[
                    ObjectRef::new_string("hello world".to_owned()),
                    ObjectRef::new_string("Hello World".to_owned())
                ],
                Ordering::Equal,
                true,
                false
            ),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(false))))
        );
        assert_eq!(
            string_cmp(
                &[
                    ObjectRef::new_string("hello world".to_owned()),
                    ObjectRef::new_string("Hello World".to_owned())
                ],
                Ordering::Equal,
                true,
                true
            ),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(true))))
        );
        assert_eq!(
            string_cmp(
                &[
                    ObjectRef::new_string("aardvark".to_owned()),
                    ObjectRef::new_string("zebra".to_owned())
                ],
                Ordering::Less,
                true,
                false
            ),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(true))))
        );
        assert_eq!(
            string_cmp(
                &[
                    ObjectRef::new_string("AAA".to_owned()),
                    ObjectRef::new_string("aaa".to_owned())
                ],
                Ordering::Less,
                true,
                false
            ),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(true))))
        );
        assert_eq!(
            string_cmp(
                &[
                    ObjectRef::new_string("AAA".to_owned()),
                    ObjectRef::new_string("aaa".to_owned())
                ],
                Ordering::Less,
                true,
                true
            ),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(false))))
        );
    }

    #[test]
    fn test_string_copy() {
        let s = ObjectRef::new_string("hello world".to_owned());
        assert!(ObjectRef::equal(&s, &string_copy(&[s.clone()]).unwrap()));
        assert_ne!(s, string_copy(&[s.clone()]).unwrap());
    }

    #[test]
    fn test_substring() {
        assert!(ObjectRef::equal(
            &substring(&[
                ObjectRef::new_string("hello world".to_owned()),
                ObjectRef::new(atom_obj!(int_datum!(3))),
                ObjectRef::new(atom_obj!(int_datum!(8)))
            ])
            .unwrap(),
            &ObjectRef::new_string("lo wo".to_owned())
        ));
        assert_eq!(
            substring(&[
                ObjectRef::new_string("abc".to_owned()),
                ObjectRef::new(atom_obj!(int_datum!(2))),
                ObjectRef::new(atom_obj!(int_datum!(1)))
            ]),
            Err(EvalError::new(EvalErrorKind::IndexOutOfBounds {
                index: 1,
                len: 1
            }))
        );
    }

    #[test]
    fn test_string_append() {
        assert!(ObjectRef::equal(
            &string_append(&[
                ObjectRef::new_string("abc ".to_owned()),
                ObjectRef::new_string("def ".to_owned()),
                ObjectRef::new_string("ghi".to_owned())
            ])
            .unwrap(),
            &ObjectRef::new_string("abc def ghi".to_owned())
        ));
    }
}
