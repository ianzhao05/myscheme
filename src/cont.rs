use std::fmt::Debug;
use std::{cell::RefCell, rc::Rc};

use crate::env::Env;
use crate::evaler::EvalError;
use crate::expr::*;
use crate::object::ObjectRef;

#[derive(Debug, Clone)]
pub enum Acc {
    Expr(Rc<Expr>),
    Obj(Result<ObjectRef, EvalError>),
}

#[derive(Debug, Clone)]
pub struct State {
    pub acc: Acc,
    pub cont: Rc<Cont>,
    pub env: Rc<RefCell<Env>>,
    pub rib: Vec<ObjectRef>,
    pub stack: Option<Rc<Frame>>,
}

impl State {
    pub fn new(expr: Rc<Expr>, env: Rc<RefCell<Env>>) -> Self {
        Self {
            acc: Acc::Expr(expr),
            cont: Rc::new(Cont::Return),
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

impl Debug for Frame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
        arg: String,
        body: Rc<Expr>,
        cont: Rc<Cont>,
    },
    Conditional {
        consequent: Rc<Expr>,
        alternate: Option<Rc<Expr>>,
        cont: Rc<Cont>,
    },
    Assignment {
        variable: String,
        cont: Rc<Cont>,
    },
    Begin {
        next: Rc<Expr>,
        cont: Rc<Cont>,
    },
    Define {
        name: String,
        cont: Rc<Cont>,
    },
    DefineProc {
        name: String,
        data: Rc<ProcData>,
        cont: Rc<Cont>,
    },
}

impl Cont {
    pub fn from_body(body: &[ExprOrDef]) -> Rc<Cont> {
        let mut bcont = Rc::new(Cont::Return);
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
                                name: name.clone(),
                                cont: bcont,
                            }),
                        });
                    }
                    Definition::Procedure { name, data } => {
                        bcont = Rc::new(Cont::DefineProc {
                            name: name.clone(),
                            data: data.clone(),
                            cont: bcont,
                        })
                    }
                    Definition::Begin(_) => todo!(),
                },
                _ => todo!(),
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
