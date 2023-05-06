use std::collections::HashMap;
use std::rc::Rc;

use crate::datum::SimpleDatum;
use crate::evaler::{EvalError, EvalErrorKind};
use crate::object::{Object, ObjectRef};

use super::PrimitiveMap;

fn equiv_am(got: usize) -> EvalError {
    EvalError::new(EvalErrorKind::ArityMismatch {
        expected: 2,
        got,
        rest: false,
    })
}

fn eqv(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    if args.len() != 2 {
        return Err(equiv_am(args.len()));
    }
    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Boolean(
        args[0] == args[1],
    ))))
}

fn eq(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    if args.len() != 2 {
        return Err(equiv_am(args.len()));
    }
    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Boolean(
        match (&args[0], &args[1]) {
            (ObjectRef::Object(o1), ObjectRef::Object(o2)) => match (&**o1, &**o2) {
                (Object::Atom(SimpleDatum::Number(_)), Object::Atom(SimpleDatum::Number(_))) => {
                    Rc::ptr_eq(o1, o2)
                }
                _ => args[0] == args[1],
            },
            _ => args[0] == args[1],
        },
    ))))
}

pub fn primitives() -> PrimitiveMap {
    let mut m: PrimitiveMap = HashMap::new();
    m.insert("eqv?", eqv);
    m.insert("eq?", eq);
    m
}

pub const PRELUDE: &str = "
(define (equal? a b)
  (cond
    ((pair? a)
     (and (pair? b) (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
    ((vector? a)
     (and (vector? b)
          (let ((al (vector-length a)) (bl (vector-length b)))
            (and (= al bl)
                 (let h ((i 0))
                   (if (= i al) #t
                       (and (equal? (vector-ref a i) (vector-ref b i))
                            (h (+ i 1)))))))))
    (else (eqv? a b))))
";
