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

    pub fn expand(
        &self,
        mut operands: Vec<Datum>,
        env_use: &Rc<SynEnv>,
    ) -> Option<(Datum, Rc<SynEnv>)> {
        operands.remove(0);
        self.transformer
            .transcribe(operands, env_use, &self.env_def)
    }
}

fn bs_err(i: i32) -> ParserError {
    ParserError {
        kind: ParserErrorKind::BadSyntax(format!("syntax-rules {i}").into()),
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
                .collect::<Result<HashSet<_>, _>>()?,
            Some(Datum::EmptyList) => Default::default(),
            _ => return Err(bs_err(0)),
        };
        Ok(Self {
            literals,
            rules: it
                .map(|r| SyntaxRule::try_from((r, env)))
                .collect::<Result<Vec<_>, _>>()?,
        })
    }
}

impl Transformer {
    fn transcribe(
        &self,
        operands: Vec<Datum>,
        env_use: &Rc<SynEnv>,
        env_def: &SynEnv,
    ) -> Option<(Datum, Rc<SynEnv>)> {
        let list = ListKind::Proper(operands);
        self.rules.iter().find_map(|rule| {
            let subst = rule
                .pattern
                .match_operands(&list, &self.literals, env_use, env_def)?;
            Some((
                rule.template.expand(&subst),
                SynEnv::new(Some(env_use.clone()), rule.env_new.clone()),
            ))
        })
    }
}

#[derive(Debug)]
struct SyntaxRule {
    pattern: ListPattern,
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
        let mut it = pattern_template_datum.into_iter();
        let (Some(pattern), Some(template), None) = (it.next(), it.next(), it.next()) else {
            return Err(bs_err(3));
        };
        let pattern = ListPattern::try_from_first(pattern)?;
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
enum ListPattern {
    Proper(Vec<Pattern>),
    Improper(Vec<Pattern>, Box<Pattern>),
    Ellipsized(Vec<Pattern>),
}

impl TryFrom<Datum> for ListPattern {
    type Error = ParserError;

    fn try_from(datum: Datum) -> Result<Self, Self::Error> {
        let Datum::Compound(CompoundDatum::List(list)) = datum else {
            return Err(bs_err(4));
        };
        Self::try_from_list(list, false)
    }
}

impl ListPattern {
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
            Ok(li)
        };
        match list {
            ListKind::Proper(l) => match l.last() {
                Some(Datum::Simple(SimpleDatum::Symbol(s))) if s == &"...".into() => {
                    Ok(ListPattern::Ellipsized(
                        f(l)?
                            .rev()
                            .skip(1)
                            .rev()
                            .map(Pattern::try_from)
                            .collect::<Result<Vec<_>, _>>()?,
                    ))
                }
                Some(_) => Ok(ListPattern::Proper(
                    f(l)?
                        .map(Pattern::try_from)
                        .collect::<Result<Vec<_>, _>>()?,
                )),
                None => Err(bs_err(5)),
            },
            ListKind::Improper(l, e) => Ok(ListPattern::Improper(
                f(l)?
                    .map(Pattern::try_from)
                    .collect::<Result<Vec<_>, _>>()?,
                Box::new(Pattern::try_from(*e)?),
            )),
        }
    }

    fn contains_var(&self, name: Symbol) -> bool {
        let pred = |p: &Pattern| p.contains_var(name);
        match self {
            ListPattern::Proper(l) => l.iter().any(pred),
            ListPattern::Improper(l, e) => l.iter().any(pred) || pred(e),
            ListPattern::Ellipsized(l) => l.iter().any(pred),
        }
    }

    fn match_operands<'a>(
        &self,
        operands: &'a ListKind,
        literals: &HashSet<Symbol>,
        env_use: &SynEnv,
        env_def: &SynEnv,
    ) -> Option<HashMap<Symbol, &'a Datum>> {
        match (self, operands) {
            (ListPattern::Proper(l), ListKind::Proper(os)) => {
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
            (ListPattern::Improper(_, _), _) => todo!(),
            (ListPattern::Ellipsized(_), _) => todo!(),
            _ => None,
        }
    }
}

#[derive(Debug)]
enum Pattern {
    Simple(SimpleDatum),
    List(ListPattern),
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
            Pattern::Simple(SimpleDatum::Symbol(s)) => *s == name,
            Pattern::Simple(_) => false,
            Pattern::List(l) => l.contains_var(name),
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
            (Pattern::Simple(SimpleDatum::Symbol(s)), Datum::Simple(SimpleDatum::Symbol(o)))
                if s == o && literals.contains(s) =>
            {
                (env_use.get(*s) == env_def.get(*s)).then(HashMap::new)
            }
            (Pattern::Simple(SimpleDatum::Symbol(s)), _) => Some([(*s, operand)].into()),
            (Pattern::List(l), Datum::Compound(CompoundDatum::List(o))) => {
                l.match_operands(o, literals, env_use, env_def)
            }
            (Pattern::Simple(sd), Datum::Simple(o)) => (sd == o).then(HashMap::new),
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
            Datum::Simple(s) => Ok(Template::Simple(s)),
            compound => Ok(Template::List(compound.try_into()?)),
        }
    }
}

impl Template {
    fn map_symbols(&mut self, f: &mut impl FnMut(Symbol) -> Symbol) {
        match self {
            Template::Simple(SimpleDatum::Symbol(s)) => {
                *s = f(*s);
            }
            Template::Simple(_) => {}
            Template::List(l) => l.map_symbols(f),
        }
    }

    fn expand(&self, subst: &HashMap<Symbol, &Datum>) -> Datum {
        match self {
            Template::Simple(sd @ SimpleDatum::Symbol(s)) => subst
                .get(s)
                .map(|&d| d.clone())
                .unwrap_or_else(|| Datum::Simple(sd.clone())),
            Template::Simple(sd) => Datum::Simple(sd.clone()),
            Template::List(l) => l.expand(subst),
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
            Some(e) => Ok(ListTemplate::Improper(
                elements,
                Box::new(Template::try_from(*e)?),
            )),
            None => Ok(ListTemplate::Proper(elements)),
        }
    }
}

impl ListTemplate {
    fn map_symbols(&mut self, f: &mut impl FnMut(Symbol) -> Symbol) {
        match self {
            ListTemplate::Proper(elements) => {
                for element in elements {
                    element.template.map_symbols(f);
                }
            }
            ListTemplate::Improper(elements, template) => {
                for element in elements {
                    element.template.map_symbols(f);
                }
                template.map_symbols(f);
            }
        }
    }

    fn expand(&self, subst: &HashMap<Symbol, &Datum>) -> Datum {
        match self {
            ListTemplate::Proper(elements) => {
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
            ListTemplate::Improper(elements, template) => {
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
