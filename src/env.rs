use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::evaler::eval;
use crate::object::ObjectRef;
use crate::primitives::{prelude, primitives};

#[derive(Debug)]
pub struct Env {
    parent: Option<Rc<RefCell<Env>>>,
    bindings: HashMap<String, ObjectRef>,
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
        for eod in prelude() {
            match eval(eod, env.clone()) {
                Ok(_) => (),
                Err(e) => panic!("Error evaluating prelude: {}", e),
            }
        }
        env
    }

    pub fn get(&self, name: &str) -> Option<ObjectRef> {
        match self.bindings.get(name) {
            Some(obj) => Some(obj.clone()),
            None => match &self.parent {
                Some(parent) => parent.borrow().get(name),
                None => None,
            },
        }
    }

    pub fn insert(&mut self, name: &str, val: ObjectRef) {
        self.bindings.insert(name.to_owned(), val);
    }

    pub fn set(&mut self, name: &str, val: ObjectRef) -> bool {
        if self.bindings.contains_key(name) {
            self.bindings.insert(name.to_owned(), val);
            true
        } else {
            match &self.parent {
                Some(parent) => parent.borrow_mut().set(name, val),
                None => false,
            }
        }
    }
}
