use crate::datum::SimpleDatum;
use crate::interner::Symbol;

#[derive(Debug, Clone)]
pub struct Transformer {
    literals: Vec<Symbol>,
    rules: Vec<SyntaxRule>,
}

#[derive(Debug, Clone)]
pub struct SyntaxRule {
    pattern: ListPattern,
}

#[derive(Debug, Clone)]
pub enum ListPattern {
    Proper(Vec<Pattern>),
    Improper(Vec<Pattern>, Box<Pattern>),
    Ellipsized(Vec<Pattern>),
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Simple(SimpleDatum),
    List(ListPattern),
}
