use super::utils::ControlMap;
use std::collections::HashMap;
use std::rc::Rc;

use crate::cont::{Acc, Cont, State};
use crate::env::Env;
use crate::evaler::{EvalError, EvalErrorKind};
use crate::expr::ExprOrDef;
use crate::parser::parse;
use crate::trampoline::Bouncer;

use super::utils::expr_cv;

pub fn eval(state: State) -> Bouncer {
    let State {
        acc: _,
        cont: _,
        env: _,
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
    let Ok(datum) = rib[0].clone().try_into() else {
        return Bouncer::Land(Err(expr_cv(&rib[0])));
    };
    let expr = match parse(datum) {
        Ok(ExprOrDef::Expr(expr)) => expr,
        Ok(_) => return Bouncer::Land(Err(expr_cv(&rib[0]))),
        Err(e) => return Bouncer::Land(Err(EvalError::new(EvalErrorKind::ParserError(e)))),
    };
    Bouncer::Bounce(State {
        acc: Acc::Expr(expr),
        cont: Rc::new(Cont::Return),
        env: Env::primitives(),
        rib: vec![],
        stack,
    })
}

pub fn cprimitives() -> ControlMap {
    let mut m: ControlMap = HashMap::new();
    m.insert("eval", eval);
    m
}
