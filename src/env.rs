use std::collections::{hash_map::Entry, HashMap};
use std::{cell::RefCell, rc::Rc};

use crate::eval_str;
use crate::interner::Symbol;
use crate::object::ObjectRef;
use crate::parser::macros::Macro;
use crate::primitives::{primitives, PRELUDE};

#[derive(Debug, Clone)]
pub enum EnvBinding {
    Variable(ObjectRef),
    Macro(Rc<Macro>),
}

#[derive(Debug, Clone)]
struct EnvInner {
    parent: Option<Rc<Env>>,
    bindings: HashMap<Symbol, EnvBinding>,
}

#[derive(Debug, Clone)]
pub struct Env(RefCell<EnvInner>);

impl Env {
    pub fn new(parent: Option<Rc<Env>>, bindings: HashMap<Symbol, EnvBinding>) -> Rc<Self> {
        Rc::new(Self(RefCell::new(EnvInner { parent, bindings })))
    }

    pub fn new_empty(parent: Option<Rc<Env>>) -> Rc<Self> {
        Self::new(parent, HashMap::new())
    }

    pub fn primitives() -> Rc<Self> {
        thread_local! {
            static PRIMITIVES_ENV: Rc<Env> = {
                let env = Env::new(None, primitives());
                eval_str(&PRELUDE, Rc::clone(&env)).unwrap_or_else(|e| {
                    panic!("Error while evaluating prelude: {e}");
                });
                env
            }
        }
        PRIMITIVES_ENV.with(|env| Rc::new(Self::clone(env)))
    }

    pub fn get(&self, name: Symbol) -> Option<ObjectRef> {
        let inner = self.0.borrow();
        match inner.bindings.get(&name) {
            Some(EnvBinding::Variable(obj)) => Some(obj.clone()),
            Some(EnvBinding::Macro(_)) => None,
            None => match &inner.parent {
                Some(parent) => parent.get(name),
                None => None,
            },
        }
    }

    pub fn insert(&self, name: Symbol, val: ObjectRef) {
        self.0
            .borrow_mut()
            .bindings
            .insert(name, EnvBinding::Variable(val));
    }

    pub fn set(&self, name: Symbol, val: ObjectRef) -> bool {
        let mut inner = self.0.borrow_mut();
        if let Entry::Occupied(mut e) = inner.bindings.entry(name) {
            match e.get() {
                EnvBinding::Variable(_) => {
                    e.insert(EnvBinding::Variable(val));
                    true
                }
                EnvBinding::Macro(_) => false,
            }
        } else {
            match &inner.parent {
                Some(parent) => parent.set(name, val),
                None => false,
            }
        }
    }

    pub fn get_macro(&self, name: Symbol) -> Option<Rc<Macro>> {
        let inner = self.0.borrow();
        match inner.bindings.get(&name) {
            Some(EnvBinding::Macro(mac)) => Some(Rc::clone(mac)),
            Some(EnvBinding::Variable(_)) => None,
            None => match &inner.parent {
                Some(parent) => parent.get_macro(name),
                None => None,
            },
        }
    }

    pub fn insert_macro(&self, name: Symbol, mac: Macro) {
        self.0
            .borrow_mut()
            .bindings
            .insert(name, EnvBinding::Macro(Rc::new(mac)));
    }
}
