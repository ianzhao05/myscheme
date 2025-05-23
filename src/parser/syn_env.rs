use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};

use super::macros::Macro;
use super::{Datum, ParserError};
use crate::interner::Symbol;

#[derive(Debug, Clone)]
pub(super) enum EnvBinding {
    Macro(Rc<Macro>),
    MacroSelf(Weak<Macro>),
    Ident(Symbol),
}

impl PartialEq for EnvBinding {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Macro(m1), Self::Macro(m2)) => Rc::ptr_eq(m1, m2),
            (Self::MacroSelf(m1), Self::MacroSelf(m2)) => Weak::ptr_eq(m1, m2),
            (Self::Macro(m1), Self::MacroSelf(m2)) => Weak::ptr_eq(&Rc::downgrade(m1), m2),
            (Self::MacroSelf(m1), Self::Macro(m2)) => Weak::ptr_eq(m1, &Rc::downgrade(m2)),
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

    pub fn new_empty(parent: Option<Rc<SynEnv>>) -> Rc<Self> {
        Self::new(parent, HashMap::new())
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

    pub(super) fn insert_macro(
        self: &Rc<Self>,
        name: Symbol,
        rules: Datum,
        rec: bool,
    ) -> Result<(), ParserError> {
        let mac = Macro::new(name, rules, self.clone(), rec)?;
        self.0
            .borrow_mut()
            .bindings
            .insert(name, EnvBinding::Macro(mac));
        Ok(())
    }
}
