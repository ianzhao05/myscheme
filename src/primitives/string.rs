use std::cmp::Ordering;
use std::collections::HashMap;

use crate::datum::SimpleDatum;
use crate::evaler::{EvalError, EvalErrorKind};
use crate::number::{Number, RealKind};
use crate::object::{Object, ObjectRef};

use super::vector::get_len;
use super::{cv_fn, PrimitiveMap};

cv_fn!(string_cv, "string");
cv_fn!(char_cv, "char");

fn make_string(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    if args.len() != 1 && args.len() != 2 {
        return Err(EvalError::new(EvalErrorKind::ArityMismatch {
            expected: 1,
            got: args.len(),
            rest: true,
        }));
    }
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
    if args.len() != 1 {
        return Err(EvalError::new(EvalErrorKind::ArityMismatch {
            expected: 1,
            got: args.len(),
            rest: false,
        }));
    }
    match &args[0].try_deref_or(string_cv)? {
        Object::Atom(SimpleDatum::String(s)) => Ok(ObjectRef::new(Object::Atom(
            SimpleDatum::Number(Number::Real(RealKind::Integer(s.borrow().len().into()))),
        ))),
        _ => Err(string_cv(&args[0])),
    }
}

fn string_ref(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::new(EvalErrorKind::ArityMismatch {
            expected: 2,
            got: args.len(),
            rest: false,
        }));
    }

    match &args[0].try_deref_or(string_cv)? {
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
    if args.len() != 3 {
        return Err(EvalError::new(EvalErrorKind::ArityMismatch {
            expected: 3,
            got: args.len(),
            rest: false,
        }));
    }
    match &args[0].try_deref_or(string_cv)? {
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
            match &args[2].try_deref_or(char_cv)? {
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
    if args.len() != 2 {
        return Err(EvalError::new(EvalErrorKind::ArityMismatch {
            expected: 3,
            got: args.len(),
            rest: false,
        }));
    }
    match &args[0].try_deref_or(string_cv)? {
        Object::Atom(SimpleDatum::String(s)) => match &args[1].try_deref_or(char_cv)? {
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

fn cmp(args: &[ObjectRef], ord: Ordering, strict: bool, ci: bool) -> Result<ObjectRef, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::new(EvalErrorKind::ArityMismatch {
            expected: 2,
            got: args.len(),
            rest: false,
        }));
    }
    let mut it = args.iter().map(|arg| match arg.try_deref_or(string_cv)? {
        Object::Atom(SimpleDatum::String(s)) => Ok(s.borrow()),
        _ => Err(string_cv(&args[1])),
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

fn substring(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::new(EvalErrorKind::ArityMismatch {
            expected: 3,
            got: args.len(),
            rest: false,
        }));
    }
    match &args[0].try_deref_or(string_cv)? {
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
    m.insert("string=?", |args| cmp(args, Ordering::Equal, true, false));
    m.insert("string<?", |args| cmp(args, Ordering::Less, true, false));
    m.insert("string>?", |args| cmp(args, Ordering::Greater, true, false));
    m.insert("string<=?", |args| cmp(args, Ordering::Less, false, false));
    m.insert("string>=?", |args| {
        cmp(args, Ordering::Greater, false, false)
    });
    m.insert("string-ci=?", |args| cmp(args, Ordering::Equal, true, true));
    m.insert("string-ci<?", |args| cmp(args, Ordering::Less, true, true));
    m.insert("string-ci>?", |args| {
        cmp(args, Ordering::Greater, true, true)
    });
    m.insert("string-ci<=?", |args| {
        cmp(args, Ordering::Less, false, true)
    });
    m.insert("string-ci>=?", |args| {
        cmp(args, Ordering::Greater, false, true)
    });
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
