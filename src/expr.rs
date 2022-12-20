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
pub struct Body {
    pub defs: Vec<Definition>,
    pub exprs: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Definition {
    Variable {
        name: String,
        value: Box<Expr>,
    },
    Procedure {
        name: String,
        args: Vec<String>,
        rest: Option<String>,
        body: Body,
    },
    Begin(Vec<Definition>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum LetKind {
    Normal,
    Named(String),
    Star,
    Rec,
}
type Bindings = Vec<(String, Expr)>;

#[derive(Debug, PartialEq, Clone)]
pub struct IterationSpec {
    pub variable: String,
    pub init: Expr,
    pub step: Option<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum DerivedExprKind {
    Cond(Vec<CondClause>),
    Case(Vec<CaseClause>),
    And(Vec<Expr>),
    Or(Vec<Expr>),
    Let {
        kind: LetKind,
        bindings: Bindings,
        body: Body,
    },
    Begin(Vec<Expr>),
    Do {
        spec: Vec<IterationSpec>,
        term: (Box<Expr>, Vec<Expr>),
        body: Vec<Expr>,
    },
    Delay(Box<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum QQTemplateData {
    Datum(Datum),
    Unquotation(Box<QQTemplate>),
    SplicingUnquotation(Box<QQTemplate>),
    List(ListQQTemplate),
    Vector(Vec<QQTemplate>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum QQTemplate {
    Level0(Box<Expr>),
    LevelD(Box<QQTemplateData>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum ListQQTemplate {
    Proper(Vec<QQTemplate>),
    Improper(Vec<QQTemplate>, Box<QQTemplate>),
    QQ(QuasiquotationKind),
}

#[derive(Debug, PartialEq, Clone)]
pub struct QuasiquotationKind {
    pub level: u32,
    pub body: QQTemplate,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Definition(Definition),
    Variable(String),
    Literal(LiteralKind),
    ProcCall {
        operator: Box<Expr>,
        operands: Vec<Expr>,
    },
    Lambda {
        args: Vec<String>,
        rest: Option<String>,
        body: Body,
    },
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
    Quasiquotation(QuasiquotationKind),
    MixedBegin(Vec<Expr>),
}
