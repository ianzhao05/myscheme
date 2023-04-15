use std::collections::HashMap;

use crate::datum::SimpleDatum;
use crate::evaler::{EvalError, EvalErrorKind};
use crate::object::{Object, ObjectRef};

use super::PrimitiveMap;

fn pair_cv(got: &ObjectRef) -> EvalError {
    EvalError::new(EvalErrorKind::ContractViolation {
        expected: "pair".to_owned(),
        got: got.clone(),
    })
}

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
    match &args[0].try_deref(pair_cv)? {
        Object::Pair(p) => Ok(if first {
            p.borrow().0.clone()
        } else {
            p.borrow().1.clone()
        }),
        _ => Err(pair_cv(&args[0])),
    }
}

fn null(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::new(EvalErrorKind::ArityMismatch {
            expected: 1,
            got: args.len(),
            rest: false,
        }));
    }
    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Boolean(
        args[0] == ObjectRef::EmptyList,
    ))))
}

pub fn primitives() -> PrimitiveMap {
    let mut m: PrimitiveMap = HashMap::new();
    m.insert("cons", cons);
    m.insert("car", |args| select(args, true));
    m.insert("cdr", |args| select(args, false));
    m.insert("null?", null);
    m
}

pub const PRELUDE: &str = r#"
(define (list . args) args)

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

(define (length l)
  (define (length-help l acc)
    (if (null? l) acc (length-help (cdr l) (+ acc 1))))
  (length-help l 0))

(define (append . args)
  (define (append-help ls args)
    (if (null? args)
        ls
        (let f ((ls ls))
          (if (null? ls)
              (append-help (car args) (cdr args))
              (cons (car ls) (f (cdr ls)))))))
  (append-help '() args))

(define (reverse l)
  (define (reverse-help l acc)
    (if (null? l) acc (reverse-help (cdr l) (cons (car l) acc))))
  (reverse-help l '()))

(define (memq o l)
  (if (null? l) #f
      (if (eq? (car l) o) l (memq o (cdr l)))))

(define (memv o l)
  (if (null? l) #f
      (if (eqv? (car l) o) l (memv o (cdr l)))))

(define (member o l)
  (if (null? l) #f
      (if (equal? (car l) o) l (member o (cdr l)))))
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
        assert_eq!(
            select(&[ObjectRef::EmptyList], false),
            Err(EvalError::new(EvalErrorKind::ContractViolation {
                expected: "pair".to_owned(),
                got: ObjectRef::EmptyList
            }))
        );
    }

    #[test]
    fn test_null() {
        assert_eq!(
            null(&[ObjectRef::EmptyList]),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(true))))
        );
        assert_eq!(
            null(&[ObjectRef::new_pair(
                ObjectRef::new(atom_obj!(int_datum!(1))),
                ObjectRef::new(atom_obj!(int_datum!(2)))
            )]),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(false))))
        );
    }
}
