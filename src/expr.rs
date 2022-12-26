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
    Vector(Vec<Expr>),
    SelfEvaluating(SelfEvaluatingKind),
}

#[derive(Debug, PartialEq, Clone)]
pub enum CondClause {
    Normal(Expr, Vec<Expr>),
    Arrow(Expr, Expr),
    Else(Vec<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum CaseClause {
    Normal(Vec<Datum>, Vec<Expr>),
    Else(Vec<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct ProcData {
    pub args: Vec<String>,
    pub rest: Option<String>,
    pub body: Body,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Body {
    pub defs: Vec<Definition>,
    pub exprs: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Definition {
    Variable { name: String, value: Box<Expr> },
    Procedure { name: String, data: ProcData },
    Begin(Vec<Definition>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum LetKind {
    Normal,
    Named(String),
    Star,
    Rec,
}
#[derive(Debug, PartialEq, Clone)]
pub struct IterationSpec {
    pub variable: String,
    pub init: Expr,
    pub step: Option<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum DerivedExprKind {
    Cond(Vec<CondClause>),
    Case {
        key: Box<Expr>,
        clauses: Vec<CaseClause>,
    },
    And(Vec<Expr>),
    Or(Vec<Expr>),
    Let {
        kind: LetKind,
        bindings: Vec<(String, Expr)>,
        body: Body,
    },
    Begin(Vec<Expr>),
    Do {
        specs: Vec<IterationSpec>,
        term: (Box<Expr>, Vec<Expr>),
        body: Vec<Expr>,
    },
    Delay(Box<Expr>),
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

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Variable(String),
    Literal(LiteralKind),
    ProcCall {
        operator: Box<Expr>,
        operands: Vec<Expr>,
    },
    Lambda(ProcData),
    Conditional {
        test: Box<Expr>,
        consequent: Box<Expr>,
        alternate: Option<Box<Expr>>,
    },
    Assignment {
        variable: String,
        value: Box<Expr>,
    },
    DerivedExpr(DerivedExprKind),
    Quasiquotation(QQTemplate),
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExprOrDef {
    Expr(Expr),
    Definition(Definition),
    MixedBegin(Vec<ExprOrDef>),
}
