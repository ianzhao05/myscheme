use crate::cont::State;
use crate::evaler::{eval_expr, EvalError};
use crate::object::ObjectRef;

pub enum Bouncer {
    Bounce(State),
    Land(Result<ObjectRef, EvalError>),
}

pub(crate) fn trampoline(mut bouncer: Bouncer) -> Result<ObjectRef, EvalError> {
    loop {
        match bouncer {
            Bouncer::Bounce(state) => bouncer = eval_expr(state),
            Bouncer::Land(res) => return res,
        }
    }
}
