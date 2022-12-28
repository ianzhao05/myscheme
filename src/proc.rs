use std::fmt::Debug;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::{cell::RefCell, rc::Rc};

use crate::env::Env;
use crate::evaler::{eval_body, EvalError, EvalErrorKind};
use crate::expr::ProcData;
use crate::object::ObjectRef;

static NEXT_ID: AtomicUsize = AtomicUsize::new(0);

pub trait Call {
    fn call(&self, args: &[ObjectRef]) -> Result<ObjectRef, EvalError>;
}

#[derive(Debug, PartialEq, Clone)]
pub enum Procedure {
    Primitive(Primitive),
    UserDefined(UserDefined),
}

impl Call for Procedure {
    fn call(&self, args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
        match self {
            Procedure::Primitive(p) => p.call(args),
            Procedure::UserDefined(p) => p.call(args),
        }
    }
}

#[derive(Clone)]
pub struct Primitive {
    name: &'static str,
    func: fn(&[ObjectRef]) -> Result<ObjectRef, EvalError>,
}

impl Debug for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Primitive")
            .field("name", &self.name)
            .finish()
    }
}

impl Primitive {
    pub fn new(name: &'static str, func: fn(&[ObjectRef]) -> Result<ObjectRef, EvalError>) -> Self {
        Self { name, func }
    }
}

impl PartialEq for Primitive {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Call for Primitive {
    fn call(&self, args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
        (self.func)(args)
    }
}

#[derive(Clone)]
pub struct UserDefined {
    id: usize,
    data: ProcData,
    env: Rc<RefCell<Env>>,
}

impl Debug for UserDefined {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("UserDefined")
            .field("id", &self.id)
            .field("data", &self.data)
            .finish()
    }
}

impl UserDefined {
    pub fn new(data: ProcData, env: Rc<RefCell<Env>>) -> Result<Self, EvalError> {
        let args = &data.args;
        let mut set = std::collections::HashSet::new();
        for arg in args {
            if set.contains(arg) {
                return Err(EvalError::new(EvalErrorKind::DuplicateArg(arg.to_owned())));
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
    fn call(&self, args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
        if args.len() < self.data.args.len()
            || (self.data.rest.is_none() && args.len() != self.data.args.len())
        {
            return Err(EvalError::new(EvalErrorKind::ArityMismatch {
                expected: self.data.args.len(),
                got: args.len(),
                rest: self.data.rest.is_some(),
            }));
        }
        let env = Rc::new(RefCell::new(Env::new(Some(self.env.clone()))));
        let mut benv = env.borrow_mut();
        let mut args_iter = args.iter().cloned();
        for (arg, val) in self.data.args.iter().zip(args_iter.by_ref()) {
            benv.insert(arg, val);
        }
        if let Some(rest) = &self.data.rest {
            benv.insert(
                rest,
                args_iter
                    .rev()
                    .fold(ObjectRef::EmptyList, |a, b| ObjectRef::new_pair(b, a)),
            );
        }
        drop(benv);
        eval_body(&self.data.body, env)
    }
}
