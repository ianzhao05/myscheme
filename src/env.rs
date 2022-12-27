use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::object::Object;
use crate::primitives::primitives;

#[derive(Debug)]
pub struct Env {
    parent: Option<Rc<RefCell<Env>>>,
    bindings: HashMap<String, Object>,
}

impl Env {
    pub fn new(parent: Option<Rc<RefCell<Env>>>) -> Self {
        Self {
            parent,
            bindings: HashMap::new(),
        }
    }

    pub fn primitives() -> Self {
        Self {
            parent: None,
            bindings: primitives(),
        }
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        match self.bindings.get(name) {
            Some(obj) => Some(obj.clone()),
            None => match &self.parent {
                Some(parent) => parent.borrow().get(name),
                None => None,
            },
        }
    }

    pub fn set(&mut self, name: &str, val: Object) {
        self.bindings.insert(name.to_owned(), val);
    }
}
