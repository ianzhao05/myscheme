use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use itertools::Itertools;

use crate::datum::{CompoundDatum, Datum, ListKind, SimpleDatum};
use crate::interner::Symbol;

use super::syn_env::{SynEnv, SynEnvMap};
use super::{ParserError, ParserErrorKind};

#[derive(Debug)]
pub struct Macro {
    transformer: Transformer,
    env_def: Rc<SynEnv>,
}

impl Macro {
    pub fn new(rules: Datum, env: Rc<SynEnv>) -> Result<Self, ParserError> {
        let transformer = Transformer::try_from((rules, env.as_ref()))?;
        Ok(Self {
            transformer,
            env_def: env,
        })
    }

    pub fn expand(&self, operands: &[Datum], env_use: &Rc<SynEnv>) -> Option<(Datum, Rc<SynEnv>)> {
        self.transformer
            .transcribe(&operands[1..], env_use, &self.env_def)
    }
}

fn bs_err(i: i32) -> ParserError {
    ParserError {
        kind: ParserErrorKind::BadSyntax(format!("syntax-rules {i}").into()),
    }
}

#[derive(Debug, Clone, Copy)]
enum Compound<'a> {
    Proper(&'a [Datum]),
    Improper(&'a [Datum], &'a Datum),
    Vector(&'a [Datum]),
}

impl<'a> From<&'a CompoundDatum> for Compound<'a> {
    fn from(d: &'a CompoundDatum) -> Self {
        match d {
            CompoundDatum::List(ListKind::Proper(l)) => Self::Proper(l),
            CompoundDatum::List(ListKind::Improper(l, e)) => Self::Improper(l, e),
            CompoundDatum::Vector(v) => Self::Vector(v),
        }
    }
}

#[derive(Debug)]
struct Transformer {
    literals: HashSet<Symbol>,
    rules: Vec<SyntaxRule>,
}

impl TryFrom<(Datum, &SynEnv)> for Transformer {
    type Error = ParserError;

    fn try_from((rules, env): (Datum, &SynEnv)) -> Result<Self, Self::Error> {
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
        let literals = match it.next() {
            Some(Datum::Compound(CompoundDatum::List(ListKind::Proper(literals)))) => literals
                .into_iter()
                .map(|d| match d {
                    Datum::Simple(SimpleDatum::Symbol(s)) => Ok(s),
                    _ => Err(bs_err(1)),
                })
                .try_collect()?,
            Some(Datum::EmptyList) => Default::default(),
            _ => return Err(bs_err(0)),
        };
        Ok(Self {
            literals,
            rules: it.map(|r| SyntaxRule::try_from((r, env))).try_collect()?,
        })
    }
}

impl Transformer {
    fn transcribe(
        &self,
        operands: &[Datum],
        env_use: &Rc<SynEnv>,
        env_def: &SynEnv,
    ) -> Option<(Datum, Rc<SynEnv>)> {
        let list = Compound::Proper(operands);
        self.rules.iter().find_map(|rule| {
            let subst = rule
                .pattern
                .match_operands(list, &self.literals, env_use, env_def)?;
            Some((
                rule.template.expand(&subst),
                SynEnv::new(Some(env_use.clone()), rule.env_new.clone()),
            ))
        })
    }
}

#[derive(Debug)]
struct SyntaxRule {
    pattern: CompoundPattern,
    template: Template,
    env_new: SynEnvMap,
}

impl TryFrom<(Datum, &SynEnv)> for SyntaxRule {
    type Error = ParserError;

    fn try_from((datum, env): (Datum, &SynEnv)) -> Result<Self, Self::Error> {
        let Datum::Compound(CompoundDatum::List(ListKind::Proper(pattern_template_datum))) = datum
        else {
            return Err(bs_err(2));
        };
        let (pattern, template) = pattern_template_datum
            .into_iter()
            .collect_tuple()
            .ok_or_else(|| bs_err(3))?;
        let pattern = CompoundPattern::try_from_first(pattern)?;
        let mut template = Template::try_from(template)?;
        let mut rewrites = HashMap::new();
        let mut env_new = SynEnvMap::new();
        template.map_symbols(&mut |s| {
            if pattern.contains_var(s) {
                return s;
            }
            if let Some(&sf) = rewrites.get(&s) {
                return sf;
            }
            let sf = super::freshen_name(s);
            rewrites.insert(s, sf);
            env_new.insert(sf, env.get(s));
            sf
        });
        Ok(Self {
            pattern,
            template,
            env_new,
        })
    }
}

#[derive(Debug)]
enum CompoundPattern {
    Proper(Vec<Pattern>),
    Improper(Vec<Pattern>, Box<Pattern>),
    Ellipsized(Vec<Pattern>, Box<Pattern>),
    Vector(Vec<Pattern>),
    EllipsizedVector(Vec<Pattern>, Box<Pattern>),
}

impl TryFrom<Datum> for CompoundPattern {
    type Error = ParserError;

    fn try_from(datum: Datum) -> Result<Self, Self::Error> {
        match datum {
            Datum::Compound(CompoundDatum::List(list)) => Self::try_from_list(list, false),
            Datum::EmptyList => Ok(Self::Proper(vec![])),
            Datum::Compound(CompoundDatum::Vector(mut v)) => match v.last() {
                Some(&Datum::Simple(SimpleDatum::Symbol(s))) if s == "...".into() => {
                    v.pop();
                    let e = v.pop().ok_or_else(|| bs_err(10))?;
                    Ok(Self::EllipsizedVector(
                        v.into_iter().map(Pattern::try_from).try_collect()?,
                        Box::new(Pattern::try_from(e)?),
                    ))
                }
                _ => Ok(Self::Vector(
                    v.into_iter().map(Pattern::try_from).try_collect()?,
                )),
            },
            _ => Err(bs_err(4)),
        }
    }
}

impl CompoundPattern {
    fn try_from_first(datum: Datum) -> Result<Self, ParserError> {
        let Datum::Compound(CompoundDatum::List(list)) = datum else {
            return Err(bs_err(4));
        };
        Self::try_from_list(list, true)
    }

    fn try_from_list(list: ListKind, first: bool) -> Result<Self, ParserError> {
        let f = |l: Vec<Datum>| {
            let mut li = l.into_iter();
            if first {
                let Some(Datum::Simple(SimpleDatum::Symbol(_))) = li.next() else {
                    return Err(bs_err(7));
                };
            }
            li.map(Pattern::try_from).try_collect()
        };
        match list {
            ListKind::Proper(mut l) => match l.last() {
                Some(&Datum::Simple(SimpleDatum::Symbol(s))) if s == "...".into() => {
                    l.pop();
                    let e = l.pop().ok_or_else(|| bs_err(9))?;
                    Ok(Self::Ellipsized(f(l)?, Box::new(Pattern::try_from(e)?)))
                }
                Some(_) => Ok(Self::Proper(f(l)?)),
                None => Err(bs_err(5)),
            },
            ListKind::Improper(l, e) => Ok(Self::Improper(f(l)?, Box::new(Pattern::try_from(*e)?))),
        }
    }

    fn contains_var(&self, name: Symbol) -> bool {
        let pred = |p: &Pattern| p.contains_var(name);
        match self {
            Self::Proper(l) | Self::Vector(l) => l.iter().any(pred),
            Self::Improper(l, e) | Self::Ellipsized(l, e) | Self::EllipsizedVector(l, e) => {
                l.iter().any(pred) || pred(e)
            }
        }
    }

    fn match_operands<'a>(
        &self,
        operands: Compound<'a>,
        literals: &HashSet<Symbol>,
        env_use: &SynEnv,
        env_def: &SynEnv,
    ) -> Option<HashMap<Symbol, &'a Datum>> {
        match (self, operands) {
            (Self::Proper(l), Compound::Proper(os)) | (Self::Vector(l), Compound::Vector(os)) => {
                if l.len() != os.len() {
                    return None;
                }
                std::iter::zip(l, os)
                    .map(|(p, o)| p.match_operand(o, literals, env_use, env_def))
                    .fold_options(HashMap::new(), |mut acc, s| {
                        acc.extend(s);
                        acc
                    })
            }
            (Self::Improper(l, t), Compound::Proper(os) | Compound::Improper(os, _)) => {
                let n = l.len();
                if os.len() < n {
                    return None;
                }
                let tail = match operands {
                    Compound::Proper(os) => Compound::Proper(&os[n..]),
                    Compound::Improper(os, ot) => Compound::Improper(&os[n..], ot),
                    _ => unreachable!(),
                };
                std::iter::zip(l, &os[..n])
                    .map(|(p, o)| p.match_operand(o, literals, env_use, env_def))
                    .fold_options(HashMap::new(), |mut acc, s| {
                        acc.extend(s);
                        acc
                    })
                    .and_then(|mut subst| {
                        t.match_operands(tail, literals, env_use, env_def)
                            .map(|tail_subst| {
                                subst.extend(tail_subst);
                                subst
                            })
                    })
            }
            (Self::Ellipsized(l, e), Compound::Proper(os))
            | (Self::EllipsizedVector(l, e), Compound::Vector(os)) => {
                let n = l.len();
                if os.len() < n {
                    return None;
                }
                l.iter()
                    .chain(std::iter::repeat(e.as_ref()))
                    .zip(os)
                    .map(|(p, o)| p.match_operand(o, literals, env_use, env_def))
                    .fold_options(HashMap::new(), |mut acc, s| {
                        acc.extend(s);
                        acc
                    })
            }
            _ => None,
        }
    }
}

#[derive(Debug)]
enum Pattern {
    Simple(SimpleDatum),
    List(CompoundPattern),
}

impl TryFrom<Datum> for Pattern {
    type Error = ParserError;

    fn try_from(datum: Datum) -> Result<Self, Self::Error> {
        match datum {
            Datum::Simple(SimpleDatum::Symbol(s)) if s == "...".into() => Err(bs_err(6)),
            Datum::Simple(s) => Ok(Pattern::Simple(s)),
            compound => Ok(Pattern::List(compound.try_into()?)),
        }
    }
}

impl Pattern {
    fn contains_var(&self, name: Symbol) -> bool {
        match self {
            Self::Simple(SimpleDatum::Symbol(s)) => *s == name,
            Self::Simple(_) => false,
            Self::List(l) => l.contains_var(name),
        }
    }

    fn match_operand<'a>(
        &self,
        operand: &'a Datum,
        literals: &HashSet<Symbol>,
        env_use: &SynEnv,
        env_def: &SynEnv,
    ) -> Option<HashMap<Symbol, &'a Datum>> {
        match (self, operand) {
            (Self::Simple(SimpleDatum::Symbol(s)), Datum::Simple(SimpleDatum::Symbol(o)))
                if s == o && literals.contains(s) =>
            {
                (env_use.get(*s) == env_def.get(*s)).then(HashMap::new)
            }
            (Self::Simple(SimpleDatum::Symbol(s)), _) => Some([(*s, operand)].into()),
            (Self::List(l), Datum::Compound(o)) => {
                l.match_operands(o.into(), literals, env_use, env_def)
            }
            (Self::List(l), Datum::EmptyList) => {
                l.match_operands(Compound::Proper(&[]), literals, env_use, env_def)
            }
            (Self::Simple(sd), Datum::Simple(o)) => (sd == o).then(HashMap::new),
            _ => None,
        }
    }

    fn match_operands<'a>(
        &self,
        operands: Compound<'a>,
        literals: &HashSet<Symbol>,
        env_use: &SynEnv,
        env_def: &SynEnv,
    ) -> Option<HashMap<Symbol, &'a Datum>> {
        match self {
            Self::List(l) => l.match_operands(operands, literals, env_use, env_def),
            _ => None,
        }
    }
}

#[derive(Debug)]
enum Template {
    Simple(SimpleDatum),
    List(ListTemplate),
}

impl TryFrom<Datum> for Template {
    type Error = ParserError;

    fn try_from(datum: Datum) -> Result<Self, Self::Error> {
        match datum {
            Datum::Simple(SimpleDatum::Symbol(s)) if s == "...".into() => Err(bs_err(7)),
            Datum::Simple(s) => Ok(Self::Simple(s)),
            compound => Ok(Self::List(compound.try_into()?)),
        }
    }
}

impl Template {
    fn map_symbols(&mut self, f: &mut impl FnMut(Symbol) -> Symbol) {
        match self {
            Self::Simple(SimpleDatum::Symbol(s)) => {
                *s = f(*s);
            }
            Self::Simple(_) => {}
            Self::List(l) => l.map_symbols(f),
        }
    }

    fn expand(&self, subst: &HashMap<Symbol, &Datum>) -> Datum {
        match self {
            Self::Simple(sd @ SimpleDatum::Symbol(s)) => subst
                .get(s)
                .map(|&d| d.clone())
                .unwrap_or_else(|| Datum::Simple(sd.clone())),
            Self::Simple(sd) => Datum::Simple(sd.clone()),
            Self::List(l) => l.expand(subst),
        }
    }
}

#[derive(Debug)]
enum ListTemplate {
    Proper(Vec<Element>),
    Improper(Vec<Element>, Box<Template>),
}

impl TryFrom<Datum> for ListTemplate {
    type Error = ParserError;

    fn try_from(datum: Datum) -> Result<Self, Self::Error> {
        let Datum::Compound(CompoundDatum::List(list)) = datum else {
            return Err(bs_err(8));
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
            Some(e) => Ok(Self::Improper(elements, Box::new(Template::try_from(*e)?))),
            None => Ok(Self::Proper(elements)),
        }
    }
}

impl ListTemplate {
    fn map_symbols(&mut self, f: &mut impl FnMut(Symbol) -> Symbol) {
        match self {
            Self::Proper(elements) => {
                for element in elements {
                    element.template.map_symbols(f);
                }
            }
            Self::Improper(elements, template) => {
                for element in elements {
                    element.template.map_symbols(f);
                }
                template.map_symbols(f);
            }
        }
    }

    fn expand(&self, subst: &HashMap<Symbol, &Datum>) -> Datum {
        match self {
            Self::Proper(elements) => {
                let list = elements
                    .iter()
                    .map(|element| {
                        if element.ellipsized {
                            todo!()
                        }
                        element.template.expand(subst)
                    })
                    .collect();
                Datum::Compound(CompoundDatum::List(ListKind::Proper(list)))
            }
            Self::Improper(elements, template) => {
                let list = elements
                    .iter()
                    .map(|element| element.template.expand(subst))
                    .collect();
                let tail = template.expand(subst);
                Datum::Compound(CompoundDatum::List(ListKind::Improper(
                    list,
                    Box::new(tail),
                )))
            }
        }
    }
}

#[derive(Debug)]
struct Element {
    template: Template,
    ellipsized: bool,
}
