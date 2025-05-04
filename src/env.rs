use std::collections::{hash_map::Entry, HashMap};
use std::{cell::RefCell, rc::Rc};

use crate::eval_str;
use crate::interner::Symbol;
use crate::object::ObjectRef;
use crate::parser::syn_env::SynEnv;
use crate::primitives::{primitives, PRELUDE};

#[derive(Debug, Clone)]
struct EnvInner {
    parent: Option<Rc<Env>>,
    bindings: HashMap<Symbol, ObjectRef>,
}

#[derive(Debug, Clone)]
pub struct Env(RefCell<EnvInner>);

impl Env {
    pub fn new(parent: Option<Rc<Env>>, bindings: HashMap<Symbol, ObjectRef>) -> Rc<Self> {
        Rc::new(Self(RefCell::new(EnvInner { parent, bindings })))
    }

    pub fn new_empty(parent: Option<Rc<Env>>) -> Rc<Self> {
        Self::new(parent, HashMap::new())
    }

    pub fn get(&self, name: Symbol) -> Option<ObjectRef> {
        let inner = self.0.borrow();
        match inner.bindings.get(&name) {
            Some(obj) => Some(obj.clone()),
            None => match &inner.parent {
                Some(parent) => parent.get(name),
                None => None,
            },
        }
    }

    pub fn insert(&self, name: Symbol, val: ObjectRef) {
        self.0.borrow_mut().bindings.insert(name, val);
    }

    pub fn set(&self, name: Symbol, val: ObjectRef) -> bool {
        let mut inner = self.0.borrow_mut();
        if let Entry::Occupied(mut e) = inner.bindings.entry(name) {
            e.insert(val);
            true
        } else {
            match &inner.parent {
                Some(parent) => parent.set(name, val),
                None => false,
            }
        }
    }
}

pub type CombinedEnv = (Rc<Env>, Rc<SynEnv>);

pub fn primitive_env() -> CombinedEnv {
    thread_local! {
        static PRIMITIVES_ENV: CombinedEnv = {
            let env = (Env::new(None, primitives()), SynEnv::new_empty(None));
            eval_str(PRELUDE, &env).unwrap_or_else(|e| {
                panic!("Error while evaluating prelude: {e}");
            });
            env
        }
    }
    PRIMITIVES_ENV.with(|(env, syn_env)| {
        (
            Rc::new(Env::clone(env)),
            SynEnv::new_empty(Some(Rc::clone(syn_env))),
        )
    })
}
