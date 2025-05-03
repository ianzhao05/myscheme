use std::fmt;
use std::rc::Rc;

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
    pub env: Rc<Env>,
    pub rib: Vec<ObjectRef>,
    pub stack: Option<Rc<Frame>>,
    pub winds: Option<Rc<Wind>>,
}

impl State {
    pub fn new(acc: Acc, env: Rc<Env>, cont: Option<Rc<Cont>>) -> Self {
        Self {
            acc,
            cont: match cont {
                Some(cont) => cont,
                None => Rc::new(Cont::Return),
            },
            env,
            rib: Vec::new(),
            stack: None,
            winds: None,
        }
    }
}

#[derive(Clone)]
pub struct Frame {
    pub cont: Rc<Cont>,
    pub env: Rc<Env>,
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

impl Drop for Frame {
    fn drop(&mut self) {
        let mut next = self.next.take();
        while let Some(frame) = next {
            if let Ok(mut frame) = Rc::try_unwrap(frame) {
                next = frame.next.take();
            } else {
                break;
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Wind {
    pub in_thunk: ObjectRef,
    pub out_thunk: ObjectRef,
    pub next: Option<Rc<Wind>>,
}

impl Drop for Wind {
    fn drop(&mut self) {
        let mut next = self.next.take();
        while let Some(wind) = next {
            if let Ok(mut wind) = Rc::try_unwrap(wind) {
                next = wind.next.take();
            } else {
                break;
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum WindsOp {
    Set(Option<Rc<Wind>>),
    Push(ObjectRef, ObjectRef),
    Pop,
}

#[derive(Debug, Clone)]
pub enum Cont {
    Return,
    ReturnVal {
        val: ObjectRef,
    },
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
    DoWinds {
        from: Option<Rc<Wind>>,
        to: Option<Rc<Wind>>,
        cont: Rc<Cont>,
    },
    WindsOp {
        op: WindsOp,
        cont: Rc<Cont>,
    },
    ApplyThunk {
        thunk: ObjectRef,
        cont: Rc<Cont>,
    },
}

impl Cont {
    pub fn from_body(body: &[ExprOrDef], cont: Option<Rc<Cont>>) -> Rc<Cont> {
        body.iter().rfold(
            cont.unwrap_or_else(|| Rc::new(Cont::Return)),
            |cont, eod| match eod {
                ExprOrDef::Expr(next) => Rc::new(Cont::Begin {
                    next: next.clone(),
                    cont,
                }),
                ExprOrDef::Definition(def) => match &**def {
                    Definition::Variable { name, value } => Rc::new(Cont::Begin {
                        next: value.clone(),
                        cont: Rc::new(Cont::Define { name: *name, cont }),
                    }),
                    Definition::Procedure { name, data } => Rc::new(Cont::DefineProc {
                        name: *name,
                        data: data.clone(),
                        cont,
                    }),
                    Definition::Begin(defs) => Self::from_defs(defs, Some(cont)),
                },
                ExprOrDef::MixedBegin(eods) => Self::from_body(eods, Some(cont)),
            },
        )
    }

    pub fn from_defs(defs: &[Rc<Definition>], cont: Option<Rc<Cont>>) -> Rc<Cont> {
        defs.iter().rfold(
            cont.unwrap_or_else(|| Rc::new(Cont::Return)),
            |cont, def| match &**def {
                Definition::Variable { name, value } => Rc::new(Cont::Begin {
                    next: value.clone(),
                    cont: Rc::new(Cont::Define { name: *name, cont }),
                }),
                Definition::Procedure { name, data } => Rc::new(Cont::DefineProc {
                    name: *name,
                    data: data.clone(),
                    cont,
                }),
                Definition::Begin(defs) => Self::from_defs(defs, Some(cont)),
            },
        )
    }

    pub fn is_tail(&self) -> bool {
        matches!(self, Cont::Return)
    }
}
