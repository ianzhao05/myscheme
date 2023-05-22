use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::evaler::eval;
use crate::interner::Symbol;
use crate::object::ObjectRef;
use crate::primitives::{primitives, PRELUDE};

#[derive(Debug)]
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
        let env = Rc::new(RefCell::new(Self {
            parent: None,
            bindings: primitives(),
        }));
        PRELUDE.with(|prelude| {
            for eod in prelude {
                match eval(eod.clone(), env.clone()) {
                    Ok(_) => (),
                    Err(e) => panic!("Error evaluating prelude: {}", e),
                }
            }
        });
        env
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
        if self.bindings.contains_key(&name) {
            self.bindings.insert(name, val);
            true
        } else {
            match &self.parent {
                Some(parent) => parent.borrow_mut().set(name, val),
                None => false,
            }
        }
    }
}
