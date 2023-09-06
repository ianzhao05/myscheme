use std::cell::RefCell;
use std::rc::Rc;

use crate::datum::{CompoundDatum, Datum, ListKind, SimpleDatum};
use crate::env::Env;
use crate::interner::Symbol;

use super::{ParserError, ParserErrorKind};

#[derive(Debug, Clone)]
pub struct Macro {
    transformer: Transformer,
    env: Rc<RefCell<Env>>,
}

impl Macro {
    pub fn new(transformer: Transformer, env: Rc<RefCell<Env>>) -> Self {
        Self { transformer, env }
    }
}
fn bs_err() -> ParserError {
    ParserError {
        kind: ParserErrorKind::BadSyntax("syntax-rules".into()),
    }
}

#[derive(Debug, Clone)]
pub struct Transformer {
    literals: Vec<Symbol>,
    rules: Vec<SyntaxRule>,
}

impl TryFrom<Datum> for Transformer {
    type Error = ParserError;

    fn try_from(rules: Datum) -> Result<Self, Self::Error> {
        let Datum::Compound(CompoundDatum::List(ListKind::Proper(rules))) = rules else {
            return Err(ParserError {
                kind: ParserErrorKind::MissingSyntaxRules,
            });
        };
        let mut it = rules.into_iter();
        if it.next() != Some(Datum::Simple(SimpleDatum::Symbol("syntax-rules".into()))) {
            return Err(ParserError {
                kind: ParserErrorKind::MissingSyntaxRules,
            });
        }
        let Some(Datum::Compound(CompoundDatum::List(ListKind::Proper(literals)))) = it.next()
        else {
            return Err(bs_err());
        };
        Ok(Self {
            literals: literals
                .into_iter()
                .map(|d| match d {
                    Datum::Simple(SimpleDatum::Symbol(s)) => Ok(s),
                    _ => Err(bs_err()),
                })
                .collect::<Result<Vec<_>, _>>()?,
            rules: it
                .map(SyntaxRule::try_from)
                .collect::<Result<Vec<_>, _>>()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct SyntaxRule {
    pattern: ListPattern,
    template: Template,
}

impl TryFrom<Datum> for SyntaxRule {
    type Error = ParserError;

    fn try_from(datum: Datum) -> Result<Self, Self::Error> {
        let Datum::Compound(CompoundDatum::List(ListKind::Proper(pattern_template_datum))) = datum
        else {
            return Err(bs_err());
        };
        let mut it = pattern_template_datum.into_iter();
        let (Some(pattern), Some(template), None) = (it.next(), it.next(), it.next()) else {
            return Err(bs_err());
        };
        Ok(Self {
            pattern: ListPattern::try_from(pattern)?,
            template: Template::try_from(template)?,
        })
    }
}

#[derive(Debug, Clone)]
pub enum ListPattern {
    Proper(Vec<Pattern>),
    Improper(Vec<Pattern>, Box<Pattern>),
    Ellipsized(Vec<Pattern>),
}

impl TryFrom<Datum> for ListPattern {
    type Error = ParserError;

    fn try_from(datum: Datum) -> Result<Self, Self::Error> {
        let Datum::Compound(CompoundDatum::List(list)) = datum else {
            return Err(bs_err());
        };
        match list {
            ListKind::Proper(l) => match l.last() {
                Some(Datum::Simple(SimpleDatum::Symbol(s))) if s == &"...".into() => {
                    Ok(ListPattern::Ellipsized(
                        l.into_iter()
                            .rev()
                            .skip(1)
                            .rev()
                            .map(Pattern::try_from)
                            .collect::<Result<Vec<_>, _>>()?,
                    ))
                }
                Some(_) => Ok(ListPattern::Proper(
                    l.into_iter()
                        .map(Pattern::try_from)
                        .collect::<Result<Vec<_>, _>>()?,
                )),
                None => Err(bs_err()),
            },
            ListKind::Improper(l, e) => Ok(ListPattern::Improper(
                l.into_iter()
                    .map(Pattern::try_from)
                    .collect::<Result<Vec<_>, _>>()?,
                Box::new(Pattern::try_from(*e)?),
            )),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Simple(SimpleDatum),
    List(ListPattern),
}

impl TryFrom<Datum> for Pattern {
    type Error = ParserError;

    fn try_from(datum: Datum) -> Result<Self, Self::Error> {
        match datum {
            Datum::Simple(SimpleDatum::Symbol(s)) if s == "...".into() => Err(bs_err()),
            Datum::Simple(s) => Ok(Pattern::Simple(s)),
            compound => Ok(Pattern::List(compound.try_into()?)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Template {
    Simple(SimpleDatum),
    List(ListTemplate),
}

impl TryFrom<Datum> for Template {
    type Error = ParserError;

    fn try_from(datum: Datum) -> Result<Self, Self::Error> {
        match datum {
            Datum::Simple(SimpleDatum::Symbol(s)) if s == "...".into() => Err(bs_err()),
            Datum::Simple(s) => Ok(Template::Simple(s)),
            compound => Ok(Template::List(compound.try_into()?)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ListTemplate {
    Proper(Vec<Element>),
    Improper(Vec<Element>, Box<Template>),
}

impl TryFrom<Datum> for ListTemplate {
    type Error = ParserError;

    fn try_from(datum: Datum) -> Result<Self, Self::Error> {
        let Datum::Compound(CompoundDatum::List(list)) = datum else {
            return Err(bs_err());
        };
        let (l, e) = match list {
            ListKind::Proper(l) => (l, None),
            ListKind::Improper(l, e) => (l, Some(e)),
        };
        let mut it = l.into_iter().peekable();
        let mut elements = Vec::new();
        while let Some(template_datum) = it.next() {
            elements.push(Element {
                template: template_datum.try_into()?,
                ellipsized: match it.peek() {
                    Some(Datum::Simple(SimpleDatum::Symbol(s))) if s == &"...".into() => {
                        it.next();
                        true
                    }
                    _ => false,
                },
            })
        }
        match e {
            Some(e) => Ok(ListTemplate::Improper(
                elements,
                Box::new(Template::try_from(*e)?),
            )),
            None => Ok(ListTemplate::Proper(elements)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Element {
    template: Template,
    ellipsized: bool,
}
