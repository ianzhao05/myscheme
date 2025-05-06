use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use itertools::Itertools;

use crate::datum::{CompoundDatum, Datum, ListKind, SimpleDatum};
use crate::interner::Symbol;

use super::syn_env::{SynEnv, SynEnvMap};
use super::{ParserError, ParserErrorKind};

#[derive(Debug)]
pub struct Macro {
    name: Symbol,
    transformer: Transformer,
    env_def: Rc<SynEnv>,
}

impl Macro {
    pub fn new(name: Symbol, rules: Datum, env: Rc<SynEnv>) -> Result<Self, ParserError> {
        let transformer = Transformer::try_from((rules, env.as_ref()))?;
        Ok(Self {
            name,
            transformer,
            env_def: env,
        })
    }

    pub fn expand(
        &self,
        operands: &[Datum],
        env_use: &Rc<SynEnv>,
    ) -> Result<(Datum, Rc<SynEnv>), ParserError> {
        self.transformer
            .transcribe(&operands[1..], env_use, &self.env_def)
            .transpose()?
            .ok_or(ParserError {
                kind: ParserErrorKind::BadSyntax(self.name),
            })
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
        let rules = it
            .map(|r| SyntaxRule::try_from((r, env, &literals)))
            .try_collect()?;
        Ok(Self { literals, rules })
    }
}

impl Transformer {
    fn transcribe(
        &self,
        operands: &[Datum],
        env_use: &Rc<SynEnv>,
        env_def: &SynEnv,
    ) -> Option<Result<(Datum, Rc<SynEnv>), ParserError>> {
        let list = Compound::Proper(operands);
        self.rules.iter().find_map(|rule| {
            let subst = rule
                .pattern
                .match_operands(list, &self.literals, env_use, env_def)?;
            Some((|| {
                Ok((
                    rule.template.expand(&subst, &mut Vec::new())?,
                    SynEnv::new(Some(env_use.clone()), rule.env_new.clone()),
                ))
            })())
        })
    }
}

#[derive(Debug)]
struct SyntaxRule {
    pattern: CompoundPattern,
    template: Template,
    env_new: SynEnvMap,
}

impl TryFrom<(Datum, &SynEnv, &HashSet<Symbol>)> for SyntaxRule {
    type Error = ParserError;

    fn try_from(
        (datum, env, literals): (Datum, &SynEnv, &HashSet<Symbol>),
    ) -> Result<Self, Self::Error> {
        let Datum::Compound(CompoundDatum::List(ListKind::Proper(pattern_template_datum))) = datum
        else {
            return Err(bs_err(2));
        };
        let (pattern, template) = pattern_template_datum
            .into_iter()
            .collect_tuple()
            .ok_or_else(|| bs_err(3))?;
        let pattern = CompoundPattern::try_from_first(pattern)?;
        let mut pattern_vars = HashSet::new();
        let mut duplicate = None;
        pattern.iter_vars(&mut |s| {
            if !literals.contains(&s) && !pattern_vars.insert(s) {
                duplicate.get_or_insert(s);
            }
        });
        if let Some(s) = duplicate {
            return Err(ParserError {
                kind: ParserErrorKind::DuplicatePatternVar(s),
            });
        }
        let mut template = Template::try_from(template)?;
        let mut rewrites = HashMap::new();
        let mut env_new = SynEnvMap::new();
        template.map_symbols(&mut |s| {
            if pattern_vars.contains(&s) {
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

enum Subst<'a> {
    Ellipsized(Vec<Subst<'a>>),
    Datum(&'a Datum),
}

struct SubstMap<'a>(HashMap<Symbol, Subst<'a>>);

impl<'a> SubstMap<'a> {
    fn new() -> Self {
        Self(HashMap::new())
    }

    fn single(s: Symbol, d: &'a Datum) -> Self {
        Self([(s, Subst::Datum(d))].into())
    }

    fn merge(mut acc: Self, s: Self) -> Self {
        acc.0.extend(s.0);
        acc
    }

    fn populate_ellipsis(&mut self, s: Symbol) {
        self.0.insert(s, Subst::Ellipsized(vec![]));
    }

    fn merge_ellipsis(mut acc: Self, s: Self) -> Self {
        for (k, v) in s.0 {
            match acc.0.get_mut(&k) {
                Some(Subst::Ellipsized(vs)) => vs.push(v),
                _ => panic!("expected map to be populated for ellipsized for {k}"),
            }
        }
        acc
    }

    fn get(&self, s: Symbol, idx: &[usize]) -> Option<Result<&'a Datum, ParserError>> {
        use std::ops::ControlFlow::*;
        match idx.iter().try_fold(self.0.get(&s)?, |acc, &i| match acc {
            Subst::Datum(_) => Break(acc),
            Subst::Ellipsized(vs) => Continue(vs.get(i).expect("idx bounded by get_size")),
        }) {
            Break(Subst::Datum(d)) | Continue(Subst::Datum(d)) => Some(Ok(d)),
            _ => Some(Err(ParserError {
                kind: ParserErrorKind::InsufficientEllipses(s),
            })),
        }
    }

    fn get_size(&self, s: Symbol, idx: &[usize]) -> Option<usize> {
        match idx.iter().try_fold(self.0.get(&s)?, |acc, &i| match acc {
            Subst::Datum(_) => None,
            Subst::Ellipsized(vs) => Some(vs.get(i).expect("idx bounded by get_size")),
        })? {
            Subst::Datum(_) => None,
            Subst::Ellipsized(vs) => Some(vs.len()),
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

    fn iter_vars(&self, f: &mut impl FnMut(Symbol)) {
        let mut g = |p: &Pattern| p.iter_vars(f);
        match self {
            Self::Proper(l) | Self::Vector(l) => l.iter().for_each(g),
            Self::Improper(l, e) | Self::Ellipsized(l, e) | Self::EllipsizedVector(l, e) => {
                l.iter().for_each(&mut g);
                g(e);
            }
        }
    }

    fn match_operands<'a>(
        &self,
        operands: Compound<'a>,
        literals: &HashSet<Symbol>,
        env_use: &SynEnv,
        env_def: &SynEnv,
    ) -> Option<SubstMap<'a>> {
        match (self, operands) {
            (Self::Proper(l), Compound::Proper(os)) | (Self::Vector(l), Compound::Vector(os)) => {
                if l.len() != os.len() {
                    return None;
                }
                std::iter::zip(l, os)
                    .map(|(p, o)| p.match_operand(o, literals, env_use, env_def))
                    .fold_options(SubstMap::new(), SubstMap::merge)
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
                    .chain(std::iter::once(
                        t.match_operands(tail, literals, env_use, env_def),
                    ))
                    .fold_options(SubstMap::new(), SubstMap::merge)
            }
            (Self::Ellipsized(l, e), Compound::Proper(os))
            | (Self::EllipsizedVector(l, e), Compound::Vector(os)) => {
                let n = l.len();
                if os.len() < n {
                    return None;
                }
                let mut subst = std::iter::zip(l, &os[..n])
                    .map(|(p, o)| p.match_operand(o, literals, env_use, env_def))
                    .fold_options(SubstMap::new(), SubstMap::merge)?;
                e.iter_vars(&mut |s| {
                    if !literals.contains(&s) {
                        subst.populate_ellipsis(s);
                    }
                });
                std::iter::zip(std::iter::repeat(e.as_ref()), &os[n..])
                    .map(|(p, o)| p.match_operand(o, literals, env_use, env_def))
                    .fold_options(subst, SubstMap::merge_ellipsis)
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
    fn iter_vars(&self, f: &mut impl FnMut(Symbol)) {
        match self {
            Self::Simple(SimpleDatum::Symbol(s)) => f(*s),
            Self::Simple(_) => {}
            Self::List(l) => l.iter_vars(f),
        }
    }

    fn match_operand<'a>(
        &self,
        operand: &'a Datum,
        literals: &HashSet<Symbol>,
        env_use: &SynEnv,
        env_def: &SynEnv,
    ) -> Option<SubstMap<'a>> {
        match (self, operand) {
            (Self::Simple(SimpleDatum::Symbol(s)), Datum::Simple(SimpleDatum::Symbol(o)))
                if s == o && literals.contains(s) =>
            {
                (env_use.get(*s) == env_def.get(*s)).then(SubstMap::new)
            }
            (Self::Simple(SimpleDatum::Symbol(s)), _) => Some(SubstMap::single(*s, operand)),
            (Self::List(l), Datum::Compound(o)) => {
                l.match_operands(o.into(), literals, env_use, env_def)
            }
            (Self::List(l), Datum::EmptyList) => {
                l.match_operands(Compound::Proper(&[]), literals, env_use, env_def)
            }
            (Self::Simple(sd), Datum::Simple(o)) => (sd == o).then(SubstMap::new),
            _ => None,
        }
    }

    fn match_operands<'a>(
        &self,
        operands: Compound<'a>,
        literals: &HashSet<Symbol>,
        env_use: &SynEnv,
        env_def: &SynEnv,
    ) -> Option<SubstMap<'a>> {
        match self {
            Self::List(l) => l.match_operands(operands, literals, env_use, env_def),
            _ => None,
        }
    }
}

#[derive(Debug)]
enum Template {
    Simple(SimpleDatum),
    List(CompoundTemplate),
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

    fn expand(&self, subst: &SubstMap, idx: &mut Vec<usize>) -> Result<Datum, ParserError> {
        match self {
            Self::Simple(sd @ SimpleDatum::Symbol(s)) => subst
                .get(*s, idx)
                .transpose()
                .map(|d| d.cloned().unwrap_or_else(|| Datum::Simple(sd.clone()))),
            Self::Simple(sd) => Ok(Datum::Simple(sd.clone())),
            Self::List(l) => l.expand(subst, idx),
        }
    }

    fn get_size(&self, subst: &SubstMap, idx: &[usize]) -> Result<Option<usize>, ParserError> {
        match self {
            Self::Simple(SimpleDatum::Symbol(s)) => Ok(subst.get_size(*s, idx)),
            Self::Simple(_) => Ok(None),
            Self::List(l) => l.get_size(subst, idx),
        }
    }
}

#[derive(Debug)]
enum CompoundTemplate {
    Proper(Vec<Element>),
    Improper(Vec<Element>, Box<Template>),
    Vector(Vec<Element>),
}

impl TryFrom<Datum> for CompoundTemplate {
    type Error = ParserError;

    fn try_from(datum: Datum) -> Result<Self, Self::Error> {
        let (l, e, is_vec) = match datum {
            Datum::Compound(CompoundDatum::List(ListKind::Proper(l))) => (l, None, false),
            Datum::Compound(CompoundDatum::List(ListKind::Improper(l, e))) => (l, Some(e), false),
            Datum::Compound(CompoundDatum::Vector(v)) => (v, None, true),
            _ => return Err(bs_err(8)),
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
            None => Ok(if is_vec { Self::Vector } else { Self::Proper }(elements)),
        }
    }
}

impl CompoundTemplate {
    fn map_symbols(&mut self, f: &mut impl FnMut(Symbol) -> Symbol) {
        match self {
            Self::Proper(elems) | Self::Vector(elems) => {
                for elem in elems {
                    elem.template.map_symbols(f);
                }
            }
            Self::Improper(elems, tail) => {
                for elem in elems {
                    elem.template.map_symbols(f);
                }
                tail.map_symbols(f);
            }
        }
    }

    fn expand(&self, subst: &SubstMap, idx: &mut Vec<usize>) -> Result<Datum, ParserError> {
        let mut expand_list = |elems: &[Element]| {
            elems
                .iter()
                .map(|elem| elem.expand(subst, idx))
                .process_results(|i| i.flatten().collect())
        };
        match self {
            Self::Proper(l) => Ok(Datum::Compound(CompoundDatum::List(ListKind::Proper(
                expand_list(l)?,
            )))),
            Self::Improper(l, t) => Ok(Datum::Compound(CompoundDatum::List(ListKind::Improper(
                expand_list(l)?,
                Box::new(t.expand(subst, idx)?),
            )))),
            Self::Vector(l) => Ok(Datum::Compound(CompoundDatum::Vector(expand_list(l)?))),
        }
    }

    fn get_size(&self, subst: &SubstMap, idx: &[usize]) -> Result<Option<usize>, ParserError> {
        match self {
            Self::Proper(l) | Self::Improper(l, _) | Self::Vector(l) => {
                l.iter().map(|e| e.get_size(subst, idx))
            }
        }
        .chain(match self {
            Self::Improper(_, t) => Some(t.get_size(subst, idx)),
            _ => None,
        })
        .process_results(|it| match it.flatten().all_equal_value() {
            Ok(sz) => Ok(Some(sz)),
            Err(None) => Ok(None),
            Err(Some((x, y))) => Err(ParserError {
                kind: ParserErrorKind::IncompatibleEllipsisCounts(x, y),
            }),
        })?
    }
}

#[derive(Debug)]
struct Element {
    template: Template,
    ellipsized: bool,
}

impl Element {
    fn expand(&self, subst: &SubstMap, idx: &mut Vec<usize>) -> Result<Vec<Datum>, ParserError> {
        if self.ellipsized {
            let sz = self.template.get_size(subst, idx)?.ok_or(ParserError {
                kind: ParserErrorKind::ExtraEllipses,
            })?;
            (0..sz)
                .map(|i| {
                    idx.push(i);
                    let res = self.template.expand(subst, idx);
                    idx.pop();
                    res
                })
                .try_collect()
        } else {
            self.template.expand(subst, idx).map(|d| vec![d])
        }
    }

    fn get_size(&self, subst: &SubstMap, idx: &[usize]) -> Result<Option<usize>, ParserError> {
        self.template.get_size(subst, idx)
    }
}
