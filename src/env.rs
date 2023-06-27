use std::collections::{hash_map::Entry, HashMap};
use std::{cell::RefCell, rc::Rc};

use crate::eval_str;
use crate::interner::Symbol;
use crate::object::ObjectRef;
use crate::primitives::{primitives, PRELUDE};

#[derive(Debug, Clone)]
pub struct Env {
    parent: Option<Rc<RefCell<Env>>>,
    bindings: HashMap<Symbol, ObjectRef>,
}

impl Env {
    pub fn new(parent: Option<Rc<RefCell<Env>>>) -> Self {
        Self {
            parent,
            bindings: HashMap::new(),
        }
    }

    pub fn primitives() -> Rc<RefCell<Self>> {
        thread_local! {
            static PRIMITIVES_ENV: Rc<RefCell<Env>> = {
                let env = Rc::new(RefCell::new(Env {
                    parent: None,
                    bindings: primitives(),
                }));
                eval_str(&PRELUDE, env.clone()).unwrap_or_else(|e| {
                    panic!("Error while evaluating prelude: {e}");
                });
                env
            }
        }
        PRIMITIVES_ENV.with(|env| Rc::new((**env).clone()))
    }

    pub fn get(&self, name: Symbol) -> Option<ObjectRef> {
        match self.bindings.get(&name) {
            Some(obj) => Some(obj.clone()),
            None => match &self.parent {
                Some(parent) => parent.borrow().get(name),
                None => None,
            },
        }
    }

    pub fn insert(&mut self, name: Symbol, val: ObjectRef) {
        self.bindings.insert(name, val);
    }

    pub fn set(&mut self, name: Symbol, val: ObjectRef) -> bool {
        if let Entry::Occupied(mut e) = self.bindings.entry(name) {
            e.insert(val);
            true
        } else {
            match &self.parent {
                Some(parent) => parent.borrow_mut().set(name, val),
                None => false,
            }
        }
    }
}
