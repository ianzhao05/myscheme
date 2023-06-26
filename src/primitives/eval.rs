use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::cont::{Acc, Cont, State};
use crate::env::Env;
use crate::evaler::{EvalError, EvalErrorKind};
use crate::expr::ExprOrDef;
use crate::object::{EnvSpec, ObjectRef};
use crate::parser::parse;
use crate::primitives::utils::get_version;
use crate::trampoline::Bouncer;

use super::utils::{ensure_arity, expr_cv, get_env, ControlMap, PrimitiveMap};

pub fn eval(state: State) -> Bouncer {
    let State { rib, stack, .. } = state;
    if rib.len() != 2 {
        return Bouncer::Land(Err(EvalError::new(EvalErrorKind::ArityMismatch {
            expected: 2,
            max_expected: 2,
            got: rib.len(),
        })));
    }
    let env_spec = match get_env(&rib[1]) {
        Ok(env_spec) => env_spec,
        Err(e) => return Bouncer::Land(Err(e)),
    };
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
        env: match env_spec {
            EnvSpec::SchemeReport => Env::primitives(),
            EnvSpec::Null => Rc::new(RefCell::new(Env::new(None))),
        },
        rib: vec![],
        stack,
        winds: None,
    })
}

pub fn return_env(args: &[ObjectRef], env: EnvSpec) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 1);
    get_version(&args[0])?;
    Ok(ObjectRef::EnvSpec(env))
}

pub fn cprimitives() -> ControlMap {
    let mut m: ControlMap = HashMap::new();
    m.insert("eval", eval);
    m
}

pub fn primitives() -> PrimitiveMap {
    let mut m: PrimitiveMap = HashMap::new();
    m.insert("scheme-report-environment", |args| {
        return_env(args, EnvSpec::SchemeReport)
    });
    m.insert("null-environment", |args| return_env(args, EnvSpec::Null));
    m
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::object::ObjectRef;
    use crate::primitives::utils::five_cv;
    use crate::test_util::*;

    #[test]
    fn env_spec() {
        assert_eq!(
            return_env(
                &[ObjectRef::new(atom_obj!(int_datum!(5)))],
                EnvSpec::SchemeReport
            ),
            Ok(ObjectRef::EnvSpec(EnvSpec::SchemeReport))
        );
        assert_eq!(
            return_env(
                &[ObjectRef::new(atom_obj!(rational_datum!(5, 1)))],
                EnvSpec::Null
            ),
            Ok(ObjectRef::EnvSpec(EnvSpec::Null))
        );
        assert_eq!(
            return_env(
                &[ObjectRef::new(atom_obj!(int_datum!(6)))],
                EnvSpec::SchemeReport
            ),
            Err(five_cv(&ObjectRef::new(atom_obj!(int_datum!(6)))))
        );
        assert_eq!(
            return_env(
                &[ObjectRef::new(atom_obj!(real_datum!(5.0)))],
                EnvSpec::Null
            ),
            Err(five_cv(&ObjectRef::new(atom_obj!(real_datum!(5.0)))))
        );
    }
}
