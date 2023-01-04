use std::pin::Pin;
use std::ptr::NonNull;
use std::{cell::RefCell, rc::Rc};

use crate::env::Env;
use crate::evaler::{eval_def, eval_expr, eval_proc_call, EvalError};
use crate::expr::{Expr, ExprOrDef};
use crate::object::{Object, ObjectRef};

pub type BouncerFn<'a> = Box<dyn FnOnce(Result<ObjectRef, EvalError>) -> Bouncer<'a> + 'a>;

pub enum Bouncer<'a> {
    Bounce(
        &'a Expr,
        Rc<RefCell<Env>>,
        Option<Pin<Rc<Object>>>,
        BouncerFn<'a>,
    ),
    BounceCall(
        Option<Pin<Rc<Object>>>,
        &'a Expr,
        &'a [Expr],
        Rc<RefCell<Env>>,
        BouncerFn<'a>,
        Vec<ObjectRef>,
    ),
    BounceApply(
        Pin<Rc<Object>>,
        NonNull<Vec<ExprOrDef>>,
        Rc<RefCell<Env>>,
        BouncerFn<'a>,
        usize,
        ObjectRef,
    ),
    Land(Result<ObjectRef, EvalError>),
}

pub fn trampoline(bouncer: Bouncer) -> Result<ObjectRef, EvalError> {
    let mut bouncer = bouncer;
    loop {
        match bouncer {
            Bouncer::Bounce(expr, env, handle, k) => {
                bouncer = eval_expr(expr, env, handle, k);
            }
            Bouncer::BounceCall(handle, proc, exprs, env, k, acc) => {
                bouncer = eval_proc_call(proc, exprs, env, handle, k, acc);
            }
            Bouncer::BounceApply(handle, body, env, k, i, res) => {
                let eods = unsafe { body.as_ref() };
                bouncer = match &eods[i] {
                    ExprOrDef::Expr(expr) => {
                        if i == eods.len() - 1 {
                            eval_expr(expr, env.clone(), Some(handle), k)
                        } else {
                            eval_expr(
                                expr,
                                env.clone(),
                                None,
                                Box::new(move |res| match res {
                                    Ok(res) => {
                                        Bouncer::BounceApply(handle, body, env, k, i + 1, res)
                                    }
                                    e => k(e),
                                }),
                            )
                        }
                    }
                    ExprOrDef::Definition(def) => match eval_def(def, env.clone()) {
                        Ok(_) => Bouncer::BounceApply(handle, body, env, k, i + 1, res),
                        Err(e) => k(Err(e)),
                    },
                    ExprOrDef::MixedBegin(_) => unreachable!(),
                }
            }
            Bouncer::Land(obj) => return obj,
        }
    }
}
