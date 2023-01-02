use std::fmt::Debug;
use std::pin::Pin;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::{cell::RefCell, rc::Rc};

use crate::env::Env;
use crate::evaler::{EvalError, EvalErrorKind};
use crate::expr::ProcData;
use crate::object::{Object, ObjectRef};
use crate::trampoline::{Bouncer, BouncerFn};

static NEXT_ID: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug, PartialEq, Clone)]
pub enum Procedure {
    Primitive(Primitive),
    UserDefined(UserDefined),
}

impl Procedure {
    pub fn call<'a>(
        &self,
        args: Vec<ObjectRef>,
        k: BouncerFn<'a>,
        handle: Pin<Rc<Object>>,
    ) -> Bouncer<'a> {
        match self {
            Procedure::Primitive(p) => k(p.call(args)),
            Procedure::UserDefined(p) => p.call(args, k, handle),
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

    fn call(&self, args: Vec<ObjectRef>) -> Result<ObjectRef, EvalError> {
        (self.func)(&args)
    }
}

impl PartialEq for Primitive {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
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

    fn call<'a>(
        &self,
        args: Vec<ObjectRef>,
        k: BouncerFn<'a>,
        handle: Pin<Rc<Object>>,
    ) -> Bouncer<'a> {
        if args.len() < self.data.args.len()
            || (self.data.rest.is_none() && args.len() != self.data.args.len())
        {
            return k(Err(EvalError::new(EvalErrorKind::ArityMismatch {
                expected: self.data.args.len(),
                got: args.len(),
                rest: self.data.rest.is_some(),
            })));
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
        Bouncer::BounceApply(
            handle,
            NonNull::from(&self.data.body.0),
            env,
            k,
            0,
            ObjectRef::Void,
        )
    }
}

impl PartialEq for UserDefined {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
