use std::collections::HashMap;
use std::rc::Rc;

use crate::datum::SimpleDatum;
use crate::evaler::EvalError;
use crate::object::{Object, ObjectRef};

use super::utils::{ensure_arity, PrimitiveMap};

fn eqv(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 2);

    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Boolean(
        args[0] == args[1],
    ))))
}

fn eq(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 2);

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

pub(super) fn primitives() -> PrimitiveMap {
    let mut m: PrimitiveMap = HashMap::new();
    m.insert("eqv?", eqv);
    m.insert("eq?", eq);
    m
}

pub(super) const PRELUDE: &str = "
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
    ((string? a)
     (and (string? b) (string=? a b)))
    (else (eqv? a b))))
";
