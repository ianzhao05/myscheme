use std::fmt;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::{cell::RefCell, rc::Rc};

use crate::cont::{Acc, Cont, Frame, State, Wind};
use crate::env::Env;
use crate::evaler::{EvalError, EvalErrorKind};
use crate::expr::ProcData;
use crate::object::ObjectRef;
use crate::trampoline::Bouncer;

static NEXT_ID: AtomicUsize = AtomicUsize::new(0);

pub trait Call {
    fn call(&self, state: State) -> Bouncer;
}

#[derive(Debug, PartialEq, Clone)]
pub enum Procedure {
    Primitive(Primitive),
    UserDefined(UserDefined),
    Continuation(Continuation),
}

impl Call for Procedure {
    fn call(&self, state: State) -> Bouncer {
        match self {
            Procedure::Primitive(p) => p.call(state),
            Procedure::UserDefined(p) => p.call(state),
            Procedure::Continuation(p) => p.call(state),
        }
    }
}

impl fmt::Display for Procedure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Procedure::Primitive(p) => p.fmt(f),
            Procedure::UserDefined(p) => p.fmt(f),
            Procedure::Continuation(p) => p.fmt(f),
        }
    }
}

#[derive(Clone)]
pub enum PrimitiveFunc {
    Args(fn(&[ObjectRef]) -> Result<ObjectRef, EvalError>),
    State(fn(State) -> Bouncer),
}

#[derive(Clone)]
pub struct Primitive {
    name: &'static str,
    func: PrimitiveFunc,
}

impl fmt::Debug for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Primitive")
            .field("name", &self.name)
            .finish()
    }
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#<primitive:{}>", self.name)
    }
}

impl Primitive {
    pub fn new(name: &'static str, func: PrimitiveFunc) -> Self {
        Self { name, func }
    }
}

impl PartialEq for Primitive {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Call for Primitive {
    fn call(&self, state: State) -> Bouncer {
        match &self.func {
            PrimitiveFunc::Args(f) => Bouncer::Bounce(State {
                acc: Acc::Obj(f(&state.rib)),
                cont: Rc::new(Cont::Return),
                env: state.env,
                rib: Vec::new(),
                stack: state.stack,
                winds: state.winds,
            }),
            PrimitiveFunc::State(f) => f(state),
        }
    }
}

#[derive(Clone)]
pub struct UserDefined {
    id: usize,
    data: Rc<ProcData>,
    env: Rc<RefCell<Env>>,
}

impl fmt::Debug for UserDefined {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("UserDefined")
            .field("id", &self.id)
            .field("data", &self.data)
            .finish()
    }
}

impl fmt::Display for UserDefined {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("#<procedure>")
    }
}

impl UserDefined {
    pub fn new(data: Rc<ProcData>, env: Rc<RefCell<Env>>) -> Result<Self, EvalError> {
        let args = data.args.iter().chain(data.rest.iter());
        let mut set = std::collections::HashSet::new();
        for arg in args {
            if set.contains(arg) {
                return Err(EvalError::new(EvalErrorKind::DuplicateArg(*arg)));
            }
            set.insert(arg);
        }
        Ok(Self {
            id: NEXT_ID.fetch_add(1, Ordering::Relaxed),
            data,
            env,
        })
    }
}

impl PartialEq for UserDefined {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Call for UserDefined {
    fn call(&self, state: State) -> Bouncer {
        let args = state.rib;
        if args.len() < self.data.args.len()
            || (self.data.rest.is_none() && args.len() != self.data.args.len())
        {
            return Bouncer::Land(Err(EvalError::new(EvalErrorKind::ArityMismatch {
                expected: self.data.args.len(),
                max_expected: if self.data.rest.is_some() {
                    usize::MAX
                } else {
                    self.data.args.len()
                },
                got: args.len(),
            })));
        }
        let env = Rc::new(RefCell::new(Env::new(Some(self.env.clone()))));
        let mut benv = env.borrow_mut();
        let mut args_iter = args.iter().cloned();
        for (arg, val) in self.data.args.iter().zip(args_iter.by_ref()) {
            benv.insert(*arg, val);
        }
        if let Some(rest) = &self.data.rest {
            benv.insert(
                *rest,
                args_iter.rfold(ObjectRef::EmptyList, |a, b| ObjectRef::new_pair(b, a)),
            );
        }
        drop(benv);
        Bouncer::Bounce(State {
            acc: Acc::Obj(Ok(ObjectRef::Undefined)),
            cont: Cont::from_body(&self.data.body, None),
            env,
            rib: Vec::new(),
            stack: state.stack,
            winds: state.winds,
        })
    }
}

#[derive(Clone)]
pub struct Continuation {
    id: usize,
    stack: Option<Rc<Frame>>,
    winds: Option<Rc<Wind>>,
}

impl fmt::Debug for Continuation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Continuation")
            .field("id", &self.id)
            .finish()
    }
}

impl fmt::Display for Continuation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("#<continuation>")
    }
}

impl Continuation {
    pub fn new(stack: Option<Rc<Frame>>, winds: Option<Rc<Wind>>) -> Self {
        Self {
            id: NEXT_ID.fetch_add(1, Ordering::Relaxed),
            stack,
            winds,
        }
    }
}

impl PartialEq for Continuation {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Call for Continuation {
    fn call(&self, state: State) -> Bouncer {
        let State {
            env, rib, winds, ..
        } = state;
        if rib.len() != 1 {
            return Bouncer::Land(Err(EvalError::new(EvalErrorKind::ArityMismatch {
                expected: 1,
                max_expected: 1,
                got: rib.len(),
            })));
        }
        let arg = rib[0].clone();
        Bouncer::Bounce(State {
            acc: Acc::Obj(Ok(arg)),
            cont: Rc::new(Cont::DoWinds {
                from: winds.clone(),
                to: self.winds.clone(),
                cont: Rc::new(Cont::Return),
            }),
            env,
            rib: Vec::new(),
            stack: self.stack.clone(),
            winds,
        })
    }
}
