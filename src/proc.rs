use std::fmt::Debug;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::{cell::RefCell, rc::Rc};

use crate::env::Env;
use crate::evaler::EvalError;
use crate::expr::ProcData;
use crate::object::Object;

static NEXT_ID: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug, PartialEq, Clone)]
pub enum Procedure {
    Primitive(Primitive),
    UserDefined(UserDefined),
}

impl Procedure {
    pub fn call(&self, args: &[Object]) -> Result<Object, EvalError> {
        match self {
            Procedure::Primitive(p) => (p.func)(args),
            Procedure::UserDefined(u) => todo!(),
        }
    }
}

#[derive(Clone)]
pub struct Primitive {
    name: &'static str,
    func: fn(&[Object]) -> Result<Object, EvalError>,
}

impl Debug for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Primitive")
            .field("name", &self.name)
            .finish()
    }
}

impl Primitive {
    pub fn new(name: &'static str, func: fn(&[Object]) -> Result<Object, EvalError>) -> Self {
        Self { name, func }
    }
}

impl PartialEq for Primitive {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

#[derive(Debug, Clone)]
pub struct UserDefined {
    id: usize,
    data: ProcData,
    env: Rc<RefCell<Env>>,
}

impl UserDefined {
    pub fn new(data: ProcData, env: Rc<RefCell<Env>>) -> Self {
        Self {
            id: NEXT_ID.fetch_add(1, Ordering::Relaxed),
            data,
            env,
        }
    }
}

impl PartialEq for UserDefined {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
