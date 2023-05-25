use std::collections::HashMap;
use std::rc::Rc;

use crate::cont::{Acc, Cont, State};
use crate::evaler::{EvalError, EvalErrorKind};
use crate::object::{Object, ObjectRef};
use crate::proc::{Continuation, Procedure};
use crate::trampoline::Bouncer;

type ControlMap = HashMap<&'static str, fn(State) -> Bouncer>;

fn callcc(state: State) -> Bouncer {
    let State {
        acc: _,
        cont: _,
        env,
        rib,
        stack,
    } = state;
    if rib.len() != 1 {
        return Bouncer::Land(Err(EvalError::new(EvalErrorKind::ArityMismatch {
            expected: 1,
            max_expected: 1,
            got: rib.len(),
        })));
    }
    let proc = rib[0].clone();
    let cont_obj = Continuation::new(stack.clone());
    Bouncer::Bounce(State {
        acc: Acc::Obj(Ok(proc)),
        cont: Rc::new(Cont::Apply),
        env,
        rib: vec![ObjectRef::new(Object::Procedure(Procedure::Continuation(
            cont_obj,
        )))],
        stack,
    })
}

fn apply(state: State) -> Bouncer {
    let State {
        acc: _,
        cont: _,
        env,
        rib,
        stack,
    } = state;
    if rib.len() < 2 {
        return Bouncer::Land(Err(EvalError::new(EvalErrorKind::ArityMismatch {
            expected: 2,
            max_expected: usize::MAX,
            got: rib.len(),
        })));
    }
    let mut nrib: Vec<_> = rib[1..rib.len() - 1].iter().cloned().collect();
    let last = rib.last().unwrap();
    let mut arg = last.clone();
    loop {
        match arg {
            ObjectRef::Object(o) => match &*o {
                Object::Pair(p) => {
                    let b = p.borrow();
                    nrib.push(b.0.clone());
                    arg = b.1.clone();
                }
                _ => {
                    return Bouncer::Land(Err(EvalError::new(EvalErrorKind::ContractViolation {
                        expected: "list",
                        got: last.clone(),
                    })))
                }
            },
            ObjectRef::EmptyList => break,
            _ => {
                return Bouncer::Land(Err(EvalError::new(EvalErrorKind::ContractViolation {
                    expected: "list",
                    got: last.clone(),
                })))
            }
        }
    }
    Bouncer::Bounce(State {
        acc: Acc::Obj(Ok(rib[0].clone())),
        cont: Rc::new(Cont::Apply),
        env,
        rib: nrib,
        stack,
    })
}

pub fn primitives() -> ControlMap {
    let mut m: ControlMap = HashMap::new();
    m.insert("apply", apply);
    m.insert("call-with-current-continuation", callcc);
    m
}

pub const PRELUDE: &str = "
(define (map1 proc l)
  (if (null? l) '() (cons (proc (car l)) (map1 proc (cdr l)))))

(define (map proc list1 . lists)
  (let ((lists (cons list1 lists)))
    (if (let loop ((ls lists))
          (if (null? ls) #t
              (if (null? (car ls)) #f
                  (loop (cdr ls)))))
        (cons (apply proc (map1 car lists))
              (apply map proc (map1 cdr lists)))
        '())))

(define (for-each proc list1 . lists)
  (let ((lists (cons list1 lists)))
    (if (let loop ((ls lists))
          (if (null? ls) #t
              (if (null? (car ls)) #f
                  (loop (cdr ls)))))
        (begin (apply proc (map1 car lists))
               (apply for-each proc (map1 cdr lists))))))
";
