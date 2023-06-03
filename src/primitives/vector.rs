use std::collections::HashMap;

use crate::datum::SimpleDatum;
use crate::evaler::{EvalError, EvalErrorKind};
use crate::number::{Number, RealKind};
use crate::object::{Object, ObjectRef};

use super::utils::{ensure_arity, get_len, get_vector, PrimitiveMap};

fn vector(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    Ok(ObjectRef::new_vector(args.to_vec()))
}

fn make_vector(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 1, 2);

    let k = get_len(&args[0])?;
    let fill = if args.len() == 2 {
        args[1].clone()
    } else {
        ObjectRef::Void
    };
    Ok(ObjectRef::new_vector(vec![fill; k]))
}

fn vector_length(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 1);

    let v = get_vector(&args[0])?.borrow();
    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Number(
        Number::Real(RealKind::Integer(v.len().into())),
    ))))
}

fn vector_ref(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 2);

    let v = get_vector(&args[0])?.borrow();
    let i = get_len(&args[1])?;

    if i >= v.len() {
        return Err(EvalError::new(EvalErrorKind::IndexOutOfBounds {
            index: i,
            len: v.len(),
        }));
    }
    Ok(v[i].clone())
}

fn vector_set(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 3);

    let mut v = get_vector(&args[0])?.borrow_mut();
    let i = get_len(&args[1])?;

    if i >= v.len() {
        return Err(EvalError::new(EvalErrorKind::IndexOutOfBounds {
            index: i,
            len: v.len(),
        }));
    }
    v[i] = args[2].clone();
    Ok(ObjectRef::Void)
}

fn vector_fill(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 2);

    let mut v = get_vector(&args[0])?.borrow_mut();
    for e in v.iter_mut() {
        *e = args[1].clone();
    }
    Ok(ObjectRef::Void)
}

pub fn primitives() -> PrimitiveMap {
    let mut m: PrimitiveMap = HashMap::new();
    m.insert("vector", vector);
    m.insert("make-vector", make_vector);
    m.insert("vector-length", vector_length);
    m.insert("vector-ref", vector_ref);
    m.insert("vector-set!", vector_set);
    m.insert("vector-fill!", vector_fill);
    m
}

pub const PRELUDE: &str = "
(define (list->vector lst)
  (let* ((len (length lst)) (vec (make-vector len)))
    (do ((i 0 (+ i 1))
         (t lst (cdr t)))
        ((= i len) vec)
      (vector-set! vec i (car t)))))

(define (vector->list vec)
  (let h ((i (vector-length vec)) (acc '()))
    (if (zero? i) acc
        (h (- i 1) (cons (vector-ref vec (- i 1)) acc)))))

(define (vector-fill! vec val)
  (do ((i 0 (+ i 1)))
      ((= i (vector-length vec)))
    (vector-set! vec i val)))
";

#[cfg(test)]
mod tests {
    use super::*;
    use crate::primitives::utils::{len_cv, vector_cv};
    use crate::test_util::*;

    #[test]
    fn test_make_vector() {
        assert!(ObjectRef::equal(
            &make_vector(&[ObjectRef::new(atom_obj!(int_datum!(3)))]).unwrap(),
            &ObjectRef::new_vector(vec![ObjectRef::Void; 3])
        ));

        assert!(ObjectRef::equal(
            &make_vector(&[
                ObjectRef::new(atom_obj!(int_datum!(3))),
                ObjectRef::new(atom_obj!(int_datum!(1)))
            ])
            .unwrap(),
            &ObjectRef::new_vector(vec![ObjectRef::new(atom_obj!(int_datum!(1))); 3])
        ));

        assert_eq!(
            make_vector(&[ObjectRef::new(atom_obj!(int_datum!(-1)))]),
            Err(len_cv(&ObjectRef::new(atom_obj!(int_datum!(-1)))))
        );

        assert_eq!(
            make_vector(&[ObjectRef::new(atom_obj!(real_datum!(3.0)))]),
            Err(len_cv(&ObjectRef::new(atom_obj!(real_datum!(3.0)))))
        );
    }

    #[test]
    fn test_vector_length() {
        assert_eq!(
            vector_length(&[ObjectRef::new_vector(vec![
                ObjectRef::new(atom_obj!(int_datum!(1))),
                ObjectRef::new(atom_obj!(int_datum!(2))),
                ObjectRef::new(atom_obj!(int_datum!(3)))
            ])]),
            Ok(ObjectRef::new(atom_obj!(int_datum!(3))))
        );

        assert_eq!(
            vector_length(&[ObjectRef::EmptyList]),
            Err(vector_cv(&ObjectRef::EmptyList))
        );
    }

    #[test]
    fn test_vector_ref() {
        assert_eq!(
            vector_ref(&[
                ObjectRef::new_vector(vec![
                    ObjectRef::new(atom_obj!(int_datum!(1))),
                    ObjectRef::new(atom_obj!(int_datum!(2))),
                    ObjectRef::new(atom_obj!(int_datum!(3)))
                ]),
                ObjectRef::new(atom_obj!(int_datum!(1)))
            ]),
            Ok(ObjectRef::new(atom_obj!(int_datum!(2))))
        );

        assert_eq!(
            vector_ref(&[
                ObjectRef::new_vector(vec![
                    ObjectRef::new(atom_obj!(int_datum!(1))),
                    ObjectRef::new(atom_obj!(int_datum!(2))),
                    ObjectRef::new(atom_obj!(int_datum!(3)))
                ]),
                ObjectRef::new(atom_obj!(int_datum!(3)))
            ]),
            Err(EvalError::new(EvalErrorKind::IndexOutOfBounds {
                index: 3,
                len: 3
            }))
        );
    }

    #[test]
    fn test_vector_set() {
        let v = ObjectRef::new_vector(vec![
            ObjectRef::new(atom_obj!(int_datum!(1))),
            ObjectRef::new(atom_obj!(int_datum!(2))),
            ObjectRef::new(atom_obj!(int_datum!(3))),
        ]);
        assert_eq!(
            vector_set(&[
                v.clone(),
                ObjectRef::new(atom_obj!(int_datum!(1))),
                ObjectRef::new(atom_obj!(int_datum!(42)))
            ]),
            Ok(ObjectRef::Void)
        );
        assert_eq!(
            vector_ref(&[v.clone(), ObjectRef::new(atom_obj!(int_datum!(1)))]),
            Ok(ObjectRef::new(atom_obj!(int_datum!(42))))
        );
    }

    #[test]
    fn test_vector_fill() {
        let v = ObjectRef::new_vector(vec![
            ObjectRef::new(atom_obj!(int_datum!(1))),
            ObjectRef::new(atom_obj!(int_datum!(2))),
            ObjectRef::new(atom_obj!(int_datum!(3))),
        ]);
        assert_eq!(
            vector_fill(&[v.clone(), ObjectRef::new(atom_obj!(int_datum!(42)))]),
            Ok(ObjectRef::Void)
        );
        assert!(ObjectRef::equal(
            &v,
            &ObjectRef::new_vector(vec![ObjectRef::new(atom_obj!(int_datum!(42))); 3])
        ));
    }
}
