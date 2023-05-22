use std::fmt;
use std::{cell::RefCell, rc::Rc};

use crate::env::Env;
use crate::evaler::EvalError;
use crate::expr::*;
use crate::interner::Symbol;
use crate::object::ObjectRef;

#[derive(Debug)]
pub enum Acc {
    Expr(Rc<Expr>),
    Obj(Result<ObjectRef, EvalError>),
}

#[derive(Debug)]
pub struct State {
    pub acc: Acc,
    pub cont: Rc<Cont>,
    pub env: Rc<RefCell<Env>>,
    pub rib: Vec<ObjectRef>,
    pub stack: Option<Rc<Frame>>,
}

impl State {
    pub fn new(acc: Acc, env: Rc<RefCell<Env>>, cont: Option<Rc<Cont>>) -> Self {
        Self {
            acc,
            cont: match cont {
                Some(cont) => cont,
                None => Rc::new(Cont::Return),
            },
            env,
            rib: Vec::new(),
            stack: None,
        }
    }
}

#[derive(Clone)]
pub struct Frame {
    pub cont: Rc<Cont>,
    pub env: Rc<RefCell<Env>>,
    pub rib: Vec<ObjectRef>,
    pub next: Option<Rc<Frame>>,
}

impl fmt::Debug for Frame {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Frame")
            .field("cont", &self.cont)
            .field("rib", &self.rib)
            .field("next", &self.next)
            .finish()
    }
}

#[derive(Debug, Clone)]
pub enum Cont {
    Return,
    Apply,
    Proc {
        operator: Rc<Expr>,
    },
    Argument {
        next: Rc<Expr>,
        cont: Rc<Cont>,
    },
    SimpleLet {
        arg: Symbol,
        body: Rc<Expr>,
        cont: Rc<Cont>,
    },
    Conditional {
        consequent: Rc<Expr>,
        alternate: Option<Rc<Expr>>,
        cont: Rc<Cont>,
    },
    Assignment {
        variable: Symbol,
        cont: Rc<Cont>,
    },
    Begin {
        next: Rc<Expr>,
        cont: Rc<Cont>,
    },
    Define {
        name: Symbol,
        cont: Rc<Cont>,
    },
    DefineProc {
        name: Symbol,
        data: Rc<ProcData>,
        cont: Rc<Cont>,
    },
}

impl Cont {
    pub fn from_body(body: &[ExprOrDef], cont: Option<Rc<Cont>>) -> Rc<Cont> {
        let mut bcont = match cont {
            Some(cont) => cont,
            None => Rc::new(Cont::Return),
        };
        for eod in body.iter().rev().cloned() {
            match eod {
                ExprOrDef::Expr(next) => {
                    bcont = Rc::new(Cont::Begin { next, cont: bcont });
                }
                ExprOrDef::Definition(def) => match &*def {
                    Definition::Variable { name, value } => {
                        bcont = Rc::new(Cont::Begin {
                            next: value.clone(),
                            cont: Rc::new(Cont::Define {
                                name: *name,
                                cont: bcont,
                            }),
                        });
                    }
                    Definition::Procedure { name, data } => {
                        bcont = Rc::new(Cont::DefineProc {
                            name: *name,
                            data: data.clone(),
                            cont: bcont,
                        })
                    }
                    Definition::Begin(defs) => {
                        bcont = Self::from_defs(defs, Some(bcont));
                    }
                },
                _ => todo!(),
            }
        }
        bcont
    }

    pub fn from_defs(defs: &[Rc<Definition>], cont: Option<Rc<Cont>>) -> Rc<Cont> {
        let mut bcont = match cont {
            Some(cont) => cont,
            None => Rc::new(Cont::Return),
        };
        for def in defs.iter().rev().cloned() {
            match &*def {
                Definition::Variable { name, value } => {
                    bcont = Rc::new(Cont::Begin {
                        next: value.clone(),
                        cont: Rc::new(Cont::Define {
                            name: *name,
                            cont: bcont,
                        }),
                    });
                }
                Definition::Procedure { name, data } => {
                    bcont = Rc::new(Cont::DefineProc {
                        name: *name,
                        data: data.clone(),
                        cont: bcont,
                    })
                }
                Definition::Begin(defs) => {
                    bcont = Self::from_defs(defs, Some(bcont));
                }
            }
        }
        bcont
    }

    pub fn is_tail(&self) -> bool {
        match self {
            Cont::Return => true,
            _ => false,
        }
    }
}
