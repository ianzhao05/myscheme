use std::cell::RefCell;
use std::rc::Rc;

use crate::datum::Datum;
use crate::interner::Symbol;
use crate::number::Number;

#[derive(Debug, PartialEq, Clone)]
pub enum SelfEvaluatingKind {
    Boolean(bool),
    Number(Number),
    Character(char),
    String(RefCell<String>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum LiteralKind {
    Quotation(Datum),
    SelfEvaluating(SelfEvaluatingKind),
}

#[derive(Debug, PartialEq, Clone)]
pub struct ProcData {
    pub args: Vec<Symbol>,
    pub rest: Option<Symbol>,
    pub body: Vec<ExprOrDef>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Definition {
    Variable { name: Symbol, value: Rc<Expr> },
    Procedure { name: Symbol, data: Rc<ProcData> },
    Begin(Vec<Rc<Definition>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Variable(Symbol),
    Literal(LiteralKind),
    ProcCall {
        operator: Rc<Expr>,
        operands: Vec<Rc<Expr>>,
    },
    Lambda(Rc<ProcData>),
    Conditional {
        test: Rc<Expr>,
        consequent: Rc<Expr>,
        alternate: Option<Rc<Expr>>,
    },
    Assignment {
        variable: Symbol,
        value: Rc<Expr>,
    },
    Begin(Vec<Rc<Expr>>),
    Undefined,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExprOrDef {
    Expr(Rc<Expr>),
    Definition(Rc<Definition>),
    MixedBegin(Vec<ExprOrDef>),
}

impl ExprOrDef {
    pub fn new_expr(expr: Expr) -> Self {
        Self::Expr(Rc::new(expr))
    }

    pub fn new_def(def: Definition) -> Self {
        Self::Definition(Rc::new(def))
    }
}
