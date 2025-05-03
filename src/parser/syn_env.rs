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

#[derive(Debug, Clone)]
struct SynEnvInner {
    parent: Option<Rc<SynEnv>>,
    bindings: HashMap<Symbol, EnvBinding>,
}

#[derive(Debug, Clone)]
pub struct SynEnv(RefCell<SynEnvInner>);

impl SynEnv {
    pub(super) fn new(
        parent: Option<Rc<SynEnv>>,
        bindings: HashMap<Symbol, EnvBinding>,
    ) -> Rc<Self> {
        Rc::new(Self(RefCell::new(SynEnvInner { parent, bindings })))
    }

    pub fn builtin() -> Rc<Self> {
        Self::new(None, HashMap::new())
    }

    pub(super) fn get(&self, name: Symbol) -> Option<EnvBinding> {
        let inner = self.0.borrow();
        match inner.bindings.get(&name) {
            Some(binding) => Some(binding.clone()),
            None => match &inner.parent {
                Some(parent) => parent.get(name),
                None => None,
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
