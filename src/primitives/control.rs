use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::cont::{Acc, Cont, Frame, State, WindsOp};
use crate::env::Env;
use crate::evaler::{EvalError, EvalErrorKind};
use crate::expr::Expr;
use crate::interner::Symbol;
use crate::object::{Object, ObjectRef};
use crate::proc::{Continuation, Procedure};
use crate::trampoline::Bouncer;

use super::utils::ControlMap;

fn callcc(state: State) -> Bouncer {
    let State {
        env,
        rib,
        stack,
        winds,
        ..
    } = state;
    if rib.len() != 1 {
        return Bouncer::Land(Err(EvalError::new(EvalErrorKind::ArityMismatch {
            expected: 1,
            max_expected: 1,
            got: rib.len(),
        })));
    }
    let proc = rib[0].clone();
    let cont_obj = Continuation::new(stack.clone(), winds.clone());
    Bouncer::Bounce(State {
        acc: Acc::Obj(Ok(proc)),
        cont: Rc::new(Cont::Apply),
        env,
        rib: vec![ObjectRef::new(Object::Procedure(Procedure::Continuation(
            cont_obj,
        )))],
        stack,
        winds,
    })
}

fn apply(state: State) -> Bouncer {
    let State {
        env,
        rib,
        stack,
        winds,
        ..
    } = state;
    if rib.len() < 2 {
        return Bouncer::Land(Err(EvalError::new(EvalErrorKind::ArityMismatch {
            expected: 2,
            max_expected: usize::MAX,
            got: rib.len(),
        })));
    }
    let mut nrib: Vec<_> = rib[1..rib.len() - 1].to_vec();
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
        winds,
    })
}

fn dynamic_wind(state: State) -> Bouncer {
    let State {
        env,
        rib,
        stack,
        winds,
        ..
    } = state;
    if rib.len() != 3 {
        return Bouncer::Land(Err(EvalError::new(EvalErrorKind::ArityMismatch {
            expected: 3,
            max_expected: 3,
            got: rib.len(),
        })));
    }
    let (in_thunk, body_thunk, out_thunk) = {
        let mut rib = rib.into_iter();
        (
            rib.next().unwrap(),
            rib.next().unwrap(),
            rib.next().unwrap(),
        )
    };
    let sym = Symbol::from("res");
    let env = Rc::new(RefCell::new(Env::new(Some(env))));
    Bouncer::Bounce(State {
        acc: Acc::Obj(Ok(in_thunk.clone())),
        cont: Rc::new(Cont::Apply),
        env: env.clone(),
        rib: Vec::new(),
        stack: Some(Rc::new(Frame {
            cont: Rc::new(Cont::WindsOp {
                op: WindsOp::Push(in_thunk, out_thunk.clone()),
                cont: Rc::new(Cont::ApplyThunk {
                    thunk: body_thunk,
                    cont: Rc::new(Cont::Define {
                        name: sym,
                        cont: Rc::new(Cont::WindsOp {
                            op: WindsOp::Pop,
                            cont: Rc::new(Cont::ApplyThunk {
                                thunk: out_thunk,
                                cont: Rc::new(Cont::Begin {
                                    next: Rc::new(Expr::Variable(sym)),
                                    cont: Rc::new(Cont::Return),
                                }),
                            }),
                        }),
                    }),
                }),
            }),
            env,
            rib: Vec::new(),
            next: stack,
        })),
        winds,
    })
}

pub fn cprimitives() -> ControlMap {
    let mut m: ControlMap = HashMap::new();
    m.insert("apply", apply);
    m.insert("call-with-current-continuation", callcc);
    m.insert("dynamic-wind", dynamic_wind);
    m
}

pub const PRELUDE: &str = "
(define (make-promise proc)
  (let ((result-ready? #f)
        (result #f))
    (lambda ()
      (if result-ready?
          result
          (let ((x (proc)))
            (if result-ready?
                result
                (begin
                  (set! result-ready? #t)
                  (set! result x)
                  result)))))))

(define (force promise) (promise))

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
