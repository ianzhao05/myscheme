use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::macros::Macro;
use crate::interner::Symbol;

#[derive(Debug, Clone)]
pub(super) enum EnvBinding {
    Macro(Rc<Macro>),
    Ident(Symbol),
}

impl PartialEq for EnvBinding {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Macro(m1), Self::Macro(m2)) => Rc::ptr_eq(m1, m2),
            (Self::Ident(s1), Self::Ident(s2)) => s1 == s2,
            _ => false,
        }
    }
}

pub(super) type SynEnvMap = HashMap<Symbol, EnvBinding>;

#[derive(Debug, Clone)]
struct SynEnvInner {
    parent: Option<Rc<SynEnv>>,
    bindings: SynEnvMap,
}

#[derive(Debug, Clone)]
pub struct SynEnv(RefCell<SynEnvInner>);

impl SynEnv {
    pub(super) fn new(parent: Option<Rc<SynEnv>>, bindings: SynEnvMap) -> Rc<Self> {
        Rc::new(Self(RefCell::new(SynEnvInner { parent, bindings })))
    }

    pub fn builtin() -> Rc<Self> {
        Self::new(None, HashMap::new())
    }

    pub(super) fn get(&self, name: Symbol) -> EnvBinding {
        let inner = self.0.borrow();
        match inner.bindings.get(&name) {
            Some(binding) => binding.clone(),
            None => match &inner.parent {
                Some(parent) => parent.get(name),
                None => EnvBinding::Ident(name),
            },
        }
    }

    pub(super) fn insert_macro(&self, name: Symbol, mac: Rc<Macro>) {
        self.0
            .borrow_mut()
            .bindings
            .insert(name, EnvBinding::Macro(mac));
    }
}
