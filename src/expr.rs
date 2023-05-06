use std::rc::Rc;

use crate::datum::Datum;
use crate::number::Number;

#[derive(Debug, PartialEq, Clone)]
pub enum SelfEvaluatingKind {
    Boolean(bool),
    Number(Number),
    Character(char),
    String(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum LiteralKind {
    Quotation(Datum),
    SelfEvaluating(SelfEvaluatingKind),
}

#[derive(Debug, PartialEq, Clone)]
pub struct ProcData {
    pub args: Vec<String>,
    pub rest: Option<String>,
    pub body: Vec<ExprOrDef>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Definition {
    Variable { name: String, value: Rc<Expr> },
    Procedure { name: String, data: Rc<ProcData> },
    Begin(Vec<Rc<Definition>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum QQTemplateData {
    Datum(Datum),
    Unquotation(QQTemplate),
    List(ListQQTemplate),
    Vector(Vec<QQTemplateOrSplice>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum QQTemplate {
    Level0(Box<Expr>),
    LevelD(usize, Box<QQTemplateData>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum QQTemplateOrSplice {
    Template(QQTemplate),
    Splice(QQTemplate),
}

#[derive(Debug, PartialEq, Clone)]
pub enum ListQQTemplate {
    Proper(Vec<QQTemplateOrSplice>),
    Improper(Vec<QQTemplateOrSplice>, QQTemplate),
    Quote(QQTemplate),
    QQ(QQTemplate),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Variable(String),
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
        variable: String,
        value: Rc<Expr>,
    },
    Begin(Vec<Rc<Expr>>),
    SimpleLet {
        arg: String,
        value: Rc<Expr>,
        body: Rc<Expr>,
    },
    Quasiquotation(QQTemplate),
    Undefined,
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Expr::Variable(a), Expr::Variable(b)) => {
                a == b || (cfg!(test) && a.starts_with("__temp_") && b.starts_with("__temp_"))
            }
            (Expr::Literal(a), Expr::Literal(b)) => a == b,
            (
                Expr::ProcCall {
                    operator: a1,
                    operands: b1,
                },
                Expr::ProcCall {
                    operator: a2,
                    operands: b2,
                },
            ) => a1 == a2 && b1 == b2,
            (Expr::Lambda(d1), Expr::Lambda(d2)) => match (&**d1, &**d2) {
                (
                    ProcData {
                        args: a1,
                        rest: r1,
                        body: b1,
                    },
                    ProcData {
                        args: a2,
                        rest: r2,
                        body: b2,
                    },
                ) => {
                    (a1 == a2
                        || (cfg!(test)
                            && a1.len() == a2.len()
                            && a1.iter().all(|x| x.starts_with("__temp_"))
                            && a2.iter().all(|x| x.starts_with("__temp_"))))
                        && (r1 == r2
                            || (cfg!(test)
                                && r1.is_some()
                                && r2.is_some()
                                && r1.as_ref().unwrap().starts_with("__temp_")
                                && r2.as_ref().unwrap().starts_with("__temp_")))
                        && b1 == b2
                }
            },
            (
                Expr::Conditional {
                    test: a1,
                    consequent: b1,
                    alternate: c1,
                },
                Expr::Conditional {
                    test: a2,
                    consequent: b2,
                    alternate: c2,
                },
            ) => a1 == a2 && b1 == b2 && c1 == c2,
            (
                Expr::Assignment {
                    variable: a1,
                    value: b1,
                },
                Expr::Assignment {
                    variable: a2,
                    value: b2,
                },
            ) => {
                (a1 == a2 || (cfg!(test) && a1.starts_with("__temp_") && a2.starts_with("__temp_")))
                    && b1 == b2
            }
            (Expr::Begin(a), Expr::Begin(b)) => a == b,
            (
                Expr::SimpleLet {
                    arg: a1,
                    value: b1,
                    body: c1,
                },
                Expr::SimpleLet {
                    arg: a2,
                    value: b2,
                    body: c2,
                },
            ) => {
                (a1 == a2 || (cfg!(test) && a1.starts_with("__temp_") && a2.starts_with("__temp_")))
                    && b1 == b2
                    && c1 == c2
            }
            (Expr::Quasiquotation(a), Expr::Quasiquotation(b)) => a == b,
            (Expr::Undefined, Expr::Undefined) => true,
            _ => false,
        }
    }
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
