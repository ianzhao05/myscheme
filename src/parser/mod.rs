pub mod macros;
mod quasiquote;
pub mod syn_env;

use std::cell::RefCell;
use std::collections::{hash_map::Entry, HashMap, HashSet};
use std::error::Error;
use std::fmt;
use std::rc::Rc;

use crate::datum::*;
use crate::expr::*;
use crate::interner::Symbol;

use itertools::Itertools;
use uuid::Uuid;

use self::syn_env::{EnvBinding, SynEnv};

fn gen_temp_name() -> Symbol {
    if cfg!(test) {
        "#temp_var".into()
    } else {
        format!("#{}", Uuid::new_v4().simple()).into()
    }
}

thread_local! {
    static INV_FRESH: RefCell<HashMap<Symbol, Symbol>> = RefCell::new(HashMap::new());
}

fn freshen_name(name: Symbol) -> Symbol {
    let fresh = if cfg!(test) {
        format!("{name}#fresh").into()
    } else {
        format!("{name}#{}", Uuid::new_v4().simple()).into()
    };
    INV_FRESH.with_borrow_mut(|inv| inv.insert(fresh, name));
    fresh
}

fn revert_name(fresh: Symbol) -> Symbol {
    INV_FRESH
        .with_borrow(|inv| inv.get(&fresh).copied())
        .unwrap_or(fresh)
}

fn lookup_name(env: &SynEnv, symb: Symbol) -> Result<Symbol, ParserError> {
    match env.get(symb) {
        EnvBinding::Ident(kw) if is_keyword(kw) => Err(ParserError {
            kind: ParserErrorKind::BadSyntax(kw),
        }),
        EnvBinding::Ident(name) => Ok(name),
        EnvBinding::Macro(_) | EnvBinding::MacroSelf(_) => Err(ParserError {
            kind: ParserErrorKind::BadSyntax(symb),
        }),
    }
}

fn create_proc<B>(
    env: Rc<SynEnv>,
    mut args: Vec<Symbol>,
    mut rest: Option<Symbol>,
    body: B,
) -> Result<ProcData, ParserError>
where
    B: FnOnce(Rc<SynEnv>, &[Symbol], Option<&Symbol>) -> Result<Vec<ExprOrDef>, ParserError>,
{
    let mut bindings = HashMap::new();
    for arg in args.iter_mut().chain(&mut rest) {
        match bindings.entry(*arg) {
            Entry::Occupied(_) => {
                return Err(ParserError {
                    kind: ParserErrorKind::DuplicateArgument(*arg),
                })
            }
            Entry::Vacant(e) => {
                *arg = freshen_name(*arg);
                e.insert(EnvBinding::Ident(*arg));
            }
        }
    }
    let env = SynEnv::new(Some(env), bindings);
    let body = body(env, &args, rest.as_ref())?;
    Ok(ProcData { args, rest, body })
}

fn create_let<B>(
    env: Rc<SynEnv>,
    arg: Symbol,
    value: Rc<Expr>,
    body: B,
) -> Result<Expr, ParserError>
where
    B: FnOnce(Rc<SynEnv>, Symbol) -> Result<Vec<ExprOrDef>, ParserError>,
{
    let arg_fresh = freshen_name(arg);
    let bindings = [(arg, EnvBinding::Ident(arg_fresh))].into();
    let env = SynEnv::new(Some(env), bindings);
    Ok(Expr::SimpleLet {
        arg: arg_fresh,
        value,
        body: body(env, arg_fresh)?,
    })
}

#[derive(Debug, PartialEq)]
pub enum ParserErrorKind {
    BadSyntax(Symbol),
    DuplicateArgument(Symbol),
    IllegalEmptyList,
    IllegalVector,
    IllegalImproperList,
    IllegalDefine,
    IllegalUnquote,
    IllegalUnquoteSplicing,
    MissingExpression,
    IllegalSyntaxRules,
    IllegalDefineSyntax,
    MissingSyntaxRules,
    DuplicatePatternVar(Symbol),
    IncompatibleEllipsisCounts(usize, usize),
    InsufficientEllipses(Symbol),
    ExtraEllipses,
}

#[derive(Debug, PartialEq)]
pub struct ParserError {
    kind: ParserErrorKind,
}

impl Error for ParserError {}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ParserErrorKind::*;
        match &self.kind {
            BadSyntax(s) => write!(f, "Bad syntax {s}"),
            DuplicateArgument(s) => {
                write!(f, "Duplicate argument {s}")
            }
            IllegalEmptyList => write!(f, "Illegal empty list"),
            IllegalVector => write!(f, "Vector must be quoted"),
            IllegalImproperList => write!(f, "Illegal improper list"),
            IllegalDefine => write!(f, "Definition not allowed here"),
            IllegalUnquote => write!(f, "unquote not allowed here"),
            IllegalUnquoteSplicing => {
                write!(f, "unquote-splicing not allowed here")
            }
            MissingExpression => write!(f, "Missing expression"),
            IllegalDefineSyntax => {
                write!(f, "define-syntax only allowed on top level")
            }
            IllegalSyntaxRules => {
                write!(f, "syntax-rules not allowed here")
            }
            MissingSyntaxRules => {
                write!(f, "Expected syntax-rules for macro definition")
            }
            DuplicatePatternVar(s) => {
                write!(f, "Duplicate pattern variable {s}")
            }
            IncompatibleEllipsisCounts(x, y) => {
                write!(f, "Incompatible ellipsis counts {x} vs. {y}")
            }
            InsufficientEllipses(s) => {
                write!(f, "Insufficient ellipses for pattern variable {s}")
            }
            ExtraEllipses => {
                write!(f, "Extra ellipses in template")
            }
        }
    }
}

fn is_keyword(symb: Symbol) -> bool {
    thread_local! {
        static KW_SET: HashSet<Symbol> = [
            "quote",
            "lambda",
            "if",
            "set!",
            "begin",
            "cond",
            "and",
            "or",
            "case",
            "let",
            "let*",
            "letrec",
            "do",
            "delay",
            "quasiquote",
            "define",
            "unquote",
            "unquote-splicing",
            "define-syntax",
            "syntax-rules",
        ]
        .into_iter()
        .map(Symbol::from)
        .collect();
    }
    KW_SET.with(|ks| ks.contains(&symb))
}

fn process_proc<I: DoubleEndedIterator<Item = Datum>>(
    list: ListKind,
    body: I,
    named: bool,
    env: &Rc<SynEnv>,
) -> Result<(Option<Symbol>, ProcData), ParserError> {
    let bs_err = || ParserError {
        kind: ParserErrorKind::BadSyntax((if named { "define" } else { "lambda" }).into()),
    };
    let process_forms = |forms: Vec<Datum>| -> Result<(Option<Symbol>, Vec<Symbol>), ParserError> {
        let mut name = None;
        let mut fi = forms.into_iter();
        if named {
            let first = fi.next().ok_or(ParserError {
                kind: ParserErrorKind::IllegalEmptyList,
            })?;
            name = match first {
                Datum::Simple(SimpleDatum::Symbol(name)) => Some(name),
                _ => return Err(bs_err()),
            };
        }
        Ok((
            name,
            fi.map(|d| match d {
                Datum::Simple(SimpleDatum::Symbol(s)) => Ok(s),
                _ => Err(bs_err()),
            })
            .try_collect()?,
        ))
    };
    match list {
        ListKind::Proper(forms) => {
            let (name, args) = process_forms(forms)?;
            Ok((
                name,
                create_proc(env.clone(), args, None, |env, _, _| {
                    process_body(body, &env)
                })?,
            ))
        }
        ListKind::Improper(forms, rest) => {
            let (name, args) = process_forms(forms)?;
            Ok((
                name,
                create_proc(
                    env.clone(),
                    args,
                    match *rest {
                        Datum::Simple(SimpleDatum::Symbol(s)) => Some(s),
                        _ => return Err(bs_err()),
                    },
                    |env, _, _| process_body(body, &env),
                )?,
            ))
        }
    }
}

fn process_define<I: DoubleEndedIterator<Item = Datum>>(
    var: Datum,
    body: I,
    env: &Rc<SynEnv>,
) -> Result<Rc<Definition>, ParserError> {
    let bs_err = || ParserError {
        kind: ParserErrorKind::BadSyntax("define".into()),
    };
    match var {
        Datum::Simple(SimpleDatum::Symbol(name)) => {
            let expr = body
                .exactly_one()
                .map_err(|_| bs_err())
                .and_then(|b| parse_expr(b, env))?;
            Ok(Rc::new(Definition::Variable { name, value: expr }))
        }
        Datum::Compound(CompoundDatum::List(list)) => {
            let (name, proc_data) = process_proc(list, body, true, env)?;
            Ok(Rc::new(Definition::Procedure {
                name: name.expect("Name should be present"),
                data: Rc::new(proc_data),
            }))
        }
        _ => Err(bs_err()),
    }
}

fn process_body<I: Iterator<Item = Datum>>(
    data: I,
    env: &Rc<SynEnv>,
) -> Result<Vec<ExprOrDef>, ParserError> {
    let mut eods = Vec::new();
    let mut last_is_expr = false;
    for d in data {
        let eod = parse(d, env)?;
        match eod {
            ExprOrDef::Definition(_) => {
                last_is_expr = false;
            }
            ExprOrDef::Expr(_) => {
                last_is_expr = true;
            }
            ExprOrDef::MixedBegin(_) => {
                return Err(ParserError {
                    kind: ParserErrorKind::IllegalDefine,
                })
            }
        }
        eods.push(eod);
    }
    if last_is_expr {
        Ok(eods)
    } else {
        Err(ParserError {
            kind: ParserErrorKind::MissingExpression,
        })
    }
}

fn process_keyword<I: DoubleEndedIterator<Item = Datum>>(
    kw: Symbol,
    mut operands: I,
    env: &Rc<SynEnv>,
) -> Result<ExprOrDef, ParserError> {
    let bs_err = || ParserError {
        kind: ParserErrorKind::BadSyntax(kw),
    };
    let parse_expr_with_env = |d| parse_expr(d, env);
    let process_binding = |binding| {
        let Datum::Compound(CompoundDatum::List(ListKind::Proper(parts))) = binding else {
            return Err(bs_err());
        };
        let mut pi = parts.into_iter();
        if let (Some(Datum::Simple(SimpleDatum::Symbol(name))), Some(value), None) =
            (pi.next(), pi.next(), pi.next())
        {
            Ok((name, value))
        } else {
            Err(bs_err())
        }
    };
    match kw {
        _ if kw == "define".into() => {
            let var = operands.next().ok_or_else(bs_err)?;
            Ok(ExprOrDef::Definition(process_define(var, operands, env)?))
        }
        _ if kw == "quote".into() => {
            let mut operand = operands.exactly_one().map_err(|_| bs_err())?;
            operand.map_symbols(&mut revert_name);
            Ok(ExprOrDef::new_expr(Expr::Literal(LiteralKind::Quotation(
                operand,
            ))))
        }
        _ if kw == "lambda".into() => {
            let formals = operands.next().ok_or_else(bs_err)?;
            match formals {
                Datum::Compound(CompoundDatum::List(list)) => {
                    let (_, args) = process_proc(list, operands, false, env)?;
                    Ok(ExprOrDef::new_expr(Expr::Lambda(Rc::new(args))))
                }
                Datum::Simple(SimpleDatum::Symbol(rest)) => {
                    Ok(ExprOrDef::new_expr(Expr::Lambda(Rc::new(create_proc(
                        env.clone(),
                        vec![],
                        Some(rest),
                        |env, _, _| process_body(operands, &env),
                    )?))))
                }
                Datum::EmptyList => Ok(ExprOrDef::new_expr(Expr::Lambda(Rc::new(create_proc(
                    env.clone(),
                    vec![],
                    None,
                    |env, _, _| process_body(operands, &env),
                )?)))),
                _ => Err(bs_err()),
            }
        }
        _ if kw == "begin".into() => {
            let mut children = vec![];
            let mut has_def = false;
            let mut has_expr = false;
            for operand in operands {
                let child = parse(operand, env)?;
                match child {
                    ExprOrDef::Definition(_) => has_def = true,
                    _ => has_expr = true,
                }
                children.push(child);
            }
            if has_def && has_expr {
                Ok(ExprOrDef::MixedBegin(children))
            } else if has_def {
                Ok(ExprOrDef::new_def(Definition::Begin(
                    children
                        .into_iter()
                        .map(|eod| match eod {
                            ExprOrDef::Definition(d) => d,
                            _ => unreachable!("should not encounter non-definition"),
                        })
                        .collect(),
                )))
            } else {
                Ok(ExprOrDef::new_expr(Expr::Begin(
                    children
                        .into_iter()
                        .map(|eod| match eod {
                            ExprOrDef::Expr(e) => e,
                            _ => unreachable!("should not encounter non-expression"),
                        })
                        .collect(),
                )))
            }
        }
        _ if kw == "set!".into() => {
            let (variable, value) = operands.collect_tuple().ok_or_else(bs_err)?;
            Ok(ExprOrDef::new_expr(Expr::Assignment {
                variable: match variable {
                    Datum::Simple(SimpleDatum::Symbol(symb)) => lookup_name(env, symb)?,
                    _ => return Err(bs_err()),
                },
                value: parse_expr(value, env)?,
            }))
        }
        _ if kw == "if".into() => {
            let (test, consequent) = operands.next_tuple().ok_or_else(bs_err)?;
            let alternate = operands.at_most_one().map_err(|_| bs_err())?;
            Ok(ExprOrDef::new_expr(Expr::Conditional {
                test: parse_expr(test, env)?,
                consequent: parse_expr(consequent, env)?,
                alternate: alternate.map(parse_expr_with_env).transpose()?,
            }))
        }
        _ if kw == "cond".into() => {
            let mut acc: Option<Rc<Expr>> = None;
            for clause in operands.rev() {
                let Datum::Compound(CompoundDatum::List(ListKind::Proper(parts))) = clause else {
                    return Err(bs_err());
                };
                let mut pi = parts.into_iter();
                let first = pi.next().ok_or(ParserError {
                    kind: ParserErrorKind::IllegalEmptyList,
                })?;
                match first {
                    Datum::Simple(SimpleDatum::Symbol(s))
                        if env.get(s) == EnvBinding::Ident("else".into()) =>
                    {
                        if acc.is_some() {
                            return Err(bs_err());
                        }
                        let seq: Vec<_> = pi.map(parse_expr_with_env).try_collect()?;
                        if seq.is_empty() {
                            return Err(bs_err());
                        }
                        acc = Some(if seq.len() == 1 {
                            seq.into_iter().next().unwrap()
                        } else {
                            Rc::new(Expr::Begin(seq))
                        });
                    }
                    _ => {
                        let test = parse_expr(first, env)?;
                        let Some(second) = pi.next() else {
                            if acc.is_some() {
                                let temp = gen_temp_name();
                                acc = Some(Rc::new(Expr::SimpleLet {
                                    arg: temp,
                                    value: test,
                                    body: vec![ExprOrDef::new_expr(Expr::Conditional {
                                        test: Rc::new(Expr::Variable(temp)),
                                        consequent: Rc::new(Expr::Variable(temp)),
                                        alternate: acc,
                                    })],
                                }));
                            } else {
                                acc = Some(test);
                            }
                            continue;
                        };
                        match second {
                            Datum::Simple(SimpleDatum::Symbol(s))
                                if env.get(s) == EnvBinding::Ident("=>".into()) =>
                            {
                                let third = pi.next().ok_or_else(bs_err)?;
                                if pi.next().is_some() {
                                    return Err(bs_err());
                                }
                                let recipient = parse_expr(third, env)?;
                                let temp = gen_temp_name();
                                acc = Some(Rc::new(Expr::SimpleLet {
                                    arg: temp,
                                    value: test,
                                    body: vec![ExprOrDef::new_expr(Expr::Conditional {
                                        test: Rc::new(Expr::Variable(temp)),
                                        consequent: Rc::new(Expr::ProcCall {
                                            operator: recipient,
                                            operands: vec![Rc::new(Expr::Variable(temp))],
                                        }),
                                        alternate: acc,
                                    })],
                                }));
                            }
                            second => {
                                let seq: Vec<_> = std::iter::once(second)
                                    .chain(pi)
                                    .map(parse_expr_with_env)
                                    .try_collect()?;
                                acc = Some(Rc::new(Expr::Conditional {
                                    test,
                                    consequent: if seq.len() == 1 {
                                        seq.into_iter().next().unwrap()
                                    } else {
                                        Rc::new(Expr::Begin(seq))
                                    },
                                    alternate: acc,
                                }));
                            }
                        }
                    }
                }
            }
            match acc {
                Some(a) => Ok(ExprOrDef::Expr(a)),
                None => Err(bs_err()),
            }
        }
        _ if kw == "and".into() => operands
            .rev()
            .map(parse_expr_with_env)
            .process_results(|ops| {
                ops.reduce(|acc, op| {
                    Rc::new(Expr::Conditional {
                        test: op,
                        consequent: acc,
                        alternate: Some(Rc::new(Expr::Literal(LiteralKind::SelfEvaluating(
                            SelfEvaluatingKind::Boolean(false),
                        )))),
                    })
                })
                .unwrap_or_else(|| {
                    Rc::new(Expr::Literal(LiteralKind::SelfEvaluating(
                        SelfEvaluatingKind::Boolean(true),
                    )))
                })
            })
            .map(ExprOrDef::Expr),
        _ if kw == "or".into() => operands
            .rev()
            .map(parse_expr_with_env)
            .process_results(|ops| {
                ops.reduce(|acc, op| {
                    let temp = gen_temp_name();
                    Rc::new(Expr::SimpleLet {
                        arg: temp,
                        value: op,
                        body: vec![ExprOrDef::new_expr(Expr::Conditional {
                            test: Rc::new(Expr::Variable(temp)),
                            consequent: Rc::new(Expr::Variable(temp)),
                            alternate: Some(acc),
                        })],
                    })
                })
                .unwrap_or_else(|| {
                    Rc::new(Expr::Literal(LiteralKind::SelfEvaluating(
                        SelfEvaluatingKind::Boolean(false),
                    )))
                })
            })
            .map(ExprOrDef::Expr),
        _ if kw == "case".into() => {
            let key = parse_expr(operands.next().ok_or_else(bs_err)?, env)?;
            let temp = gen_temp_name();
            let mut acc: Option<Rc<Expr>> = None;
            for clause in operands.rev() {
                let Datum::Compound(CompoundDatum::List(ListKind::Proper(parts))) = clause else {
                    return Err(bs_err());
                };
                let mut pi = parts.into_iter();
                let first = pi.next().ok_or(ParserError {
                    kind: ParserErrorKind::IllegalEmptyList,
                })?;
                let seq: Vec<_> = pi.map(parse_expr_with_env).try_collect()?;
                if seq.is_empty() {
                    return Err(bs_err());
                }
                match first {
                    Datum::Simple(SimpleDatum::Symbol(s))
                        if env.get(s) == EnvBinding::Ident("else".into()) =>
                    {
                        if acc.is_some() {
                            return Err(bs_err());
                        }
                        acc = Some(if seq.len() == 1 {
                            seq.into_iter().next().unwrap()
                        } else {
                            Rc::new(Expr::Begin(seq))
                        });
                    }
                    Datum::Compound(CompoundDatum::List(ListKind::Proper(_))) => {
                        acc = Some(Rc::new(Expr::Conditional {
                            test: Rc::new(Expr::ProcCall {
                                operator: Rc::new(Expr::Variable("memv".into())),
                                operands: vec![
                                    Rc::new(Expr::Variable(temp)),
                                    Rc::new(Expr::Literal(LiteralKind::Quotation(first))),
                                ],
                            }),
                            consequent: if seq.len() == 1 {
                                seq.into_iter().next().unwrap()
                            } else {
                                Rc::new(Expr::Begin(seq))
                            },
                            alternate: acc,
                        }));
                    }
                    _ => {
                        return Err(bs_err());
                    }
                }
            }
            Ok(ExprOrDef::new_expr(Expr::SimpleLet {
                arg: temp,
                value: key,
                body: vec![ExprOrDef::Expr(acc.ok_or_else(bs_err)?)],
            }))
        }
        _ if kw == "let".into() => {
            let mut first = operands.next().ok_or_else(bs_err)?;
            let name: Option<Symbol> = if let Datum::Simple(SimpleDatum::Symbol(n)) = first {
                first = operands.next().ok_or_else(bs_err)?;
                Some(n)
            } else {
                None
            };
            let (args, vals) = match first {
                Datum::Compound(CompoundDatum::List(ListKind::Proper(binding_data))) => {
                    binding_data
                        .into_iter()
                        .map(|binding| {
                            let (arg, val) = process_binding(binding)?;
                            Ok((arg, parse_expr_with_env(val)?))
                        })
                        .process_results(|iter| iter.unzip())?
                }
                Datum::EmptyList => (vec![], vec![]),
                _ => return Err(bs_err()),
            };
            match name {
                Some(n) => Ok(ExprOrDef::new_expr(create_let(
                    env.clone(),
                    n,
                    Rc::new(Expr::Undefined),
                    |env, nf| {
                        Ok(vec![
                            ExprOrDef::new_expr(Expr::Assignment {
                                variable: nf,
                                value: Rc::new(Expr::Lambda(Rc::new(create_proc(
                                    env,
                                    args,
                                    None,
                                    |env, _, _| process_body(operands, &env),
                                )?))),
                            }),
                            ExprOrDef::new_expr(Expr::ProcCall {
                                operator: Rc::new(Expr::Variable(nf)),
                                operands: vals,
                            }),
                        ])
                    },
                )?)),
                None => Ok(ExprOrDef::new_expr(Expr::ProcCall {
                    operator: Rc::new(Expr::Lambda(Rc::new(create_proc(
                        env.clone(),
                        args,
                        None,
                        |env, _, _| process_body(operands, &env),
                    )?))),
                    operands: vals,
                })),
            }
        }
        _ if kw == "let*".into() => {
            let mut bindings = match operands.next() {
                Some(Datum::Compound(CompoundDatum::List(ListKind::Proper(binding_data)))) => {
                    binding_data
                }
                Some(Datum::EmptyList) => vec![],
                _ => return Err(bs_err()),
            }
            .into_iter()
            .map(process_binding);
            match bindings.next().transpose()? {
                None => Ok(ExprOrDef::new_expr(Expr::ProcCall {
                    operator: Rc::new(Expr::Lambda(Rc::new(create_proc(
                        env.clone(),
                        vec![],
                        None,
                        |env, _, _| process_body(operands, &env),
                    )?))),
                    operands: vec![],
                })),
                Some((arg, val)) => {
                    fn rec<
                        I: Iterator<Item = Datum>,
                        B: Iterator<Item = Result<(Symbol, Datum), ParserError>>,
                    >(
                        env: Rc<SynEnv>,
                        operands: I,
                        mut bindings: B,
                    ) -> Result<Vec<ExprOrDef>, ParserError> {
                        match bindings.next().transpose()? {
                            None => process_body(operands, &env),
                            Some((arg, val)) => {
                                let val = parse_expr(val, &env)?;
                                create_let(env, arg, val, |env, _| rec(env, operands, bindings))
                                    .map(|l| vec![ExprOrDef::new_expr(l)])
                            }
                        }
                    }
                    create_let(env.clone(), arg, parse_expr(val, env)?, |env, _| {
                        rec(env, operands, bindings)
                    })
                    .map(ExprOrDef::new_expr)
                }
            }
        }
        _ if kw == "letrec".into() => {
            let (args, vals) = match operands.next() {
                Some(Datum::Compound(CompoundDatum::List(ListKind::Proper(binding_data)))) => {
                    binding_data
                        .into_iter()
                        .map(process_binding)
                        .process_results(|iter| iter.unzip())?
                }
                Some(Datum::EmptyList) => (vec![], vec![]),
                _ => return Err(bs_err()),
            };
            if args.is_empty() {
                return Ok(ExprOrDef::new_expr(Expr::ProcCall {
                    operator: Rc::new(Expr::Lambda(Rc::new(ProcData {
                        args: vec![],
                        rest: None,
                        body: process_body(operands, env)?,
                    }))),
                    operands: vec![],
                }));
            }
            let temps: Vec<_> = args.iter().map(|_| gen_temp_name()).collect();
            let undefs = {
                let data = Rc::new(Expr::Undefined);
                vec![data; args.len()]
            };
            Ok(ExprOrDef::new_expr(Expr::ProcCall {
                operator: Rc::new(Expr::Lambda(Rc::new(create_proc(
                    env.clone(),
                    args,
                    None,
                    |env, args, _| {
                        let mut body = process_body(operands, &env)?;
                        let setters = std::iter::zip(args, &temps)
                            .map(|(arg, temp)| {
                                ExprOrDef::new_expr(Expr::Assignment {
                                    variable: *arg,
                                    value: Rc::new(Expr::Variable(*temp)),
                                })
                            })
                            .collect();
                        body.insert(
                            0,
                            ExprOrDef::new_expr(Expr::ProcCall {
                                operator: Rc::new(Expr::Lambda(Rc::new(ProcData {
                                    args: temps,
                                    rest: None,
                                    body: setters,
                                }))),
                                operands: vals
                                    .into_iter()
                                    .map(|v| parse_expr(v, &env))
                                    .try_collect()?,
                            }),
                        );
                        Ok(body)
                    },
                )?))),
                operands: undefs,
            }))
        }
        _ if kw == "do".into() => {
            let first = operands.next().ok_or_else(bs_err)?;
            if !matches!(
                first,
                Datum::Compound(CompoundDatum::List(ListKind::Proper(_))) | Datum::EmptyList
            ) {
                return Err(bs_err());
            }
            let mut vars = vec![];
            let mut inits = vec![];
            let mut steps = vec![];
            if let Datum::Compound(CompoundDatum::List(ListKind::Proper(spec_data))) = first {
                for spec in spec_data {
                    let Datum::Compound(CompoundDatum::List(ListKind::Proper(parts))) = spec else {
                        return Err(bs_err());
                    };
                    if parts.len() != 2 && parts.len() != 3 {
                        return Err(bs_err());
                    }
                    let mut pi = parts.into_iter();
                    let Datum::Simple(SimpleDatum::Symbol(name)) = pi.next().unwrap() else {
                        return Err(bs_err());
                    };
                    let name = lookup_name(env, name)?;
                    inits.push(parse_expr(pi.next().unwrap(), env)?);
                    let step = pi
                        .next()
                        .map(parse_expr_with_env)
                        .transpose()?
                        .unwrap_or_else(|| Rc::new(Expr::Variable(name)));
                    vars.push(name);
                    steps.push(step);
                }
            }
            let Some(Datum::Compound(CompoundDatum::List(ListKind::Proper(term)))) =
                operands.next()
            else {
                return Err(bs_err());
            };
            let mut ei = term.into_iter().map(parse_expr_with_env);
            let test = ei.next().ok_or_else(bs_err)??;
            let seq = ei.try_collect()?;
            let mut body: Vec<_> = operands.map(parse_expr_with_env).try_collect()?;
            let temp = gen_temp_name();
            body.push(Rc::new(Expr::ProcCall {
                operator: Rc::new(Expr::Variable(temp)),
                operands: steps,
            }));
            Ok(ExprOrDef::new_expr(Expr::SimpleLet {
                arg: temp,
                value: Rc::new(Expr::Undefined),
                body: vec![
                    ExprOrDef::new_expr(Expr::Assignment {
                        variable: temp,
                        value: Rc::new(Expr::Lambda(Rc::new(ProcData {
                            args: vars,
                            rest: None,
                            body: vec![ExprOrDef::new_expr(Expr::Conditional {
                                test,
                                consequent: Rc::new(Expr::Begin(seq)),
                                alternate: Some(Rc::new(Expr::Begin(body))),
                            })],
                        }))),
                    }),
                    ExprOrDef::new_expr(Expr::ProcCall {
                        operator: Rc::new(Expr::Variable(temp)),
                        operands: inits,
                    }),
                ],
            }))
        }
        _ if kw == "delay".into() => {
            let expr = operands
                .exactly_one()
                .map_err(|_| bs_err())
                .and_then(parse_expr_with_env)?;
            Ok(ExprOrDef::new_expr(Expr::ProcCall {
                operator: Rc::new(Expr::Variable("make-promise".into())),
                operands: vec![Rc::new(Expr::Lambda(Rc::new(ProcData {
                    args: vec![],
                    rest: None,
                    body: vec![ExprOrDef::Expr(expr)],
                })))],
            }))
        }
        _ if kw == "quasiquote".into() => {
            let template = operands
                .exactly_one()
                .map_err(|_| bs_err())
                .and_then(|op| quasiquote::process_qq(op, 0, env))?;
            Ok(ExprOrDef::Expr(template))
        }
        _ if kw == "unquote".into() => Err(ParserError {
            kind: ParserErrorKind::IllegalUnquote,
        }),
        _ if kw == "unquote-splicing".into() => Err(ParserError {
            kind: ParserErrorKind::IllegalUnquoteSplicing,
        }),
        _ if kw == "define-syntax".into() => Err(ParserError {
            kind: ParserErrorKind::IllegalDefineSyntax,
        }),
        _ if kw == "syntax-rules".into() => Err(ParserError {
            kind: ParserErrorKind::IllegalSyntaxRules,
        }),
        _ => unreachable!("keyword should have been handled"),
    }
}

fn parse_expr(datum: Datum, env: &Rc<SynEnv>) -> Result<Rc<Expr>, ParserError> {
    match parse(datum, env)? {
        ExprOrDef::Expr(expr) => Ok(expr),
        _ => Err(ParserError {
            kind: ParserErrorKind::IllegalDefine,
        }),
    }
}

#[derive(Debug)]
pub enum ParserOutput {
    ExprOrDef(ExprOrDef),
    SyntaxDefinition,
}

#[derive(Debug)]
pub struct ParserResult(Result<ParserOutput, ParserError>);

impl IntoIterator for ParserResult {
    type Item = Result<ExprOrDef, ParserError>;
    type IntoIter = ParserResultIntoIter;

    fn into_iter(self) -> Self::IntoIter {
        ParserResultIntoIter(Some(self))
    }
}

#[derive(Debug)]
pub struct ParserResultIntoIter(Option<ParserResult>);

impl Iterator for ParserResultIntoIter {
    type Item = Result<ExprOrDef, ParserError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.0.take()?.0 {
            Ok(ParserOutput::ExprOrDef(eod)) => Some(Ok(eod)),
            Ok(ParserOutput::SyntaxDefinition) => None,
            Err(e) => Some(Err(e)),
        }
    }
}

pub fn parse_top_level(datum: Datum, env: &Rc<SynEnv>) -> ParserResult {
    let aux = || match datum {
        Datum::Compound(CompoundDatum::List(ListKind::Proper(list)))
            if list.first()
                == Some(&Datum::Simple(SimpleDatum::Symbol("define-syntax".into()))) =>
        {
            let bs_err = || ParserError {
                kind: ParserErrorKind::BadSyntax("define-syntax".into()),
            };
            let it = list.into_iter().skip(1);
            let Some((Datum::Simple(SimpleDatum::Symbol(name)), rules)) = it.collect_tuple() else {
                return Err(bs_err());
            };
            env.insert_macro(name, rules, true)?;
            Ok(ParserOutput::SyntaxDefinition)
        }
        other => parse(other, env).map(ParserOutput::ExprOrDef),
    };
    ParserResult(aux())
}

pub fn parse(datum: Datum, env: &Rc<SynEnv>) -> Result<ExprOrDef, ParserError> {
    match datum {
        Datum::Simple(simple) => match simple {
            SimpleDatum::Boolean(b) => Ok(ExprOrDef::new_expr(Expr::Literal(
                LiteralKind::SelfEvaluating(SelfEvaluatingKind::Boolean(b)),
            ))),
            SimpleDatum::Number(n) => Ok(ExprOrDef::new_expr(Expr::Literal(
                LiteralKind::SelfEvaluating(SelfEvaluatingKind::Number(n)),
            ))),
            SimpleDatum::Character(c) => Ok(ExprOrDef::new_expr(Expr::Literal(
                LiteralKind::SelfEvaluating(SelfEvaluatingKind::Character(c)),
            ))),
            SimpleDatum::String(s) => Ok(ExprOrDef::new_expr(Expr::Literal(
                LiteralKind::SelfEvaluating(SelfEvaluatingKind::String(s)),
            ))),
            SimpleDatum::Symbol(symb) => {
                Ok(ExprOrDef::new_expr(Expr::Variable(lookup_name(env, symb)?)))
            }
        },
        Datum::Compound(compound) => match compound {
            CompoundDatum::List(list) => match list {
                ListKind::Proper(list) => {
                    if let Some(&Datum::Simple(SimpleDatum::Symbol(symb))) = list.first() {
                        match env.get(symb) {
                            EnvBinding::Macro(mac) => {
                                let (expanded, env) = mac.expand(&list, env)?;
                                return parse(expanded, &env);
                            }
                            EnvBinding::MacroSelf(mac) => {
                                let mac =
                                    mac.upgrade().expect("macro self-reference should be alive");
                                let (expanded, env) = mac.expand(&list, env)?;
                                return parse(expanded, &env);
                            }
                            EnvBinding::Ident(kw) if is_keyword(kw) => {
                                return process_keyword(kw, list.into_iter().skip(1), env);
                            }
                            _ => {}
                        }
                    }
                    let mut li = list.into_iter();
                    let first = li.next().ok_or(ParserError {
                        kind: ParserErrorKind::IllegalEmptyList,
                    })?;
                    let operator = parse_expr(first, env)?;
                    let rest = li.map(|e| parse_expr(e, env)).try_collect()?;
                    Ok(ExprOrDef::new_expr(Expr::ProcCall {
                        operator,
                        operands: rest,
                    }))
                }
                ListKind::Improper(_, _) => Err(ParserError {
                    kind: ParserErrorKind::IllegalImproperList,
                }),
            },
            CompoundDatum::Vector(_) => Err(ParserError {
                kind: ParserErrorKind::IllegalVector,
            }),
        },
        Datum::EmptyList => Err(ParserError {
            kind: ParserErrorKind::IllegalEmptyList,
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::*;

    fn parse(datum: Datum) -> Result<ExprOrDef, ParserError> {
        super::parse(datum, &SynEnv::new_empty(None))
    }

    #[test]
    fn literals() {
        assert_eq!(
            parse(bool_datum!(true)),
            Ok(ExprOrDef::new_expr(bool_expr!(true)))
        );
        assert_eq!(
            parse(int_datum!(42)),
            Ok(ExprOrDef::new_expr(int_expr!(42)))
        );
        assert_eq!(
            parse(char_datum!('c')),
            Ok(ExprOrDef::new_expr(char_expr!('c')))
        );
        assert_eq!(
            parse(str_datum!("foo")),
            Ok(ExprOrDef::new_expr(str_expr!("foo")))
        );
    }

    #[test]
    fn proc_calls() {
        assert_eq!(
            parse(proper_list_datum![symbol_datum!("f")]),
            Ok(ExprOrDef::new_expr(Expr::ProcCall {
                operator: Rc::new(var_expr!("f")),
                operands: vec_rc![],
            }))
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("g"),
                int_datum!(1),
                int_datum!(2),
            ]),
            Ok(ExprOrDef::new_expr(Expr::ProcCall {
                operator: Rc::new(var_expr!("g")),
                operands: vec_rc![int_expr!(1), int_expr!(2)],
            }))
        );
    }

    #[test]
    fn illegal_literals() {
        assert_eq!(
            parse(Datum::EmptyList),
            Err(ParserError {
                kind: ParserErrorKind::IllegalEmptyList,
            })
        );
        assert_eq!(
            parse(improper_list_datum![int_datum!(1); int_datum!(2)]),
            Err(ParserError {
                kind: ParserErrorKind::IllegalImproperList,
            })
        );
        assert_eq!(
            parse(vector_datum![int_datum!(1), int_datum!(2)]),
            Err(ParserError {
                kind: ParserErrorKind::IllegalVector,
            })
        );
    }

    #[test]
    fn definitions() {
        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("define"),
                symbol_datum!("x"),
                int_datum!(42),
            ]),
            Ok(ExprOrDef::new_def(Definition::Variable {
                name: "x".into(),
                value: Rc::new(int_expr!(42)),
            }))
        );

        assert_eq!(
            parse_expr(
                proper_list_datum![symbol_datum!("define"), symbol_datum!("x"), int_datum!(42),],
                &SynEnv::new_empty(None)
            ),
            Err(ParserError {
                kind: ParserErrorKind::IllegalDefine,
            })
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("define"),
                proper_list_datum![symbol_datum!("f"), symbol_datum!("x")],
                proper_list_datum![symbol_datum!("define"), symbol_datum!("y"), int_datum!(1)],
                proper_list_datum![symbol_datum!("+"), symbol_datum!("x"), symbol_datum!("y")],
            ]),
            Ok(ExprOrDef::new_def(Definition::Procedure {
                name: "f".into(),
                data: Rc::new(ProcData {
                    args: vec!["x#fresh".into()],
                    rest: None,
                    body: vec![
                        ExprOrDef::new_def(Definition::Variable {
                            name: "y".into(),
                            value: Rc::new(int_expr!(1)),
                        }),
                        ExprOrDef::new_expr(Expr::ProcCall {
                            operator: Rc::new(var_expr!("+")),
                            operands: vec_rc![var_expr!("x#fresh"), var_expr!("y")],
                        }),
                    ],
                })
            }))
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("define"),
                improper_list_datum![
                    symbol_datum!("f"),
                    symbol_datum!("x"),
                    symbol_datum!("y");
                    symbol_datum!("z")
                ],
                symbol_datum!("z"),
            ]),
            Ok(ExprOrDef::new_def(Definition::Procedure {
                name: "f".into(),
                data: Rc::new(ProcData {
                    args: vec!["x#fresh".into(), "y#fresh".into()],
                    rest: Some("z#fresh".into()),
                    body: vec![ExprOrDef::new_expr(var_expr!("z#fresh"))],
                })
            }))
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("define"),
                proper_list_datum![symbol_datum!("f"), int_datum!(1),],
                int_datum!(2),
            ]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("define".into()),
            })
        );

        assert_eq!(
            parse(proper_list_datum![symbol_datum!("define")]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("define".into())
            })
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("define"),
                int_datum!(1),
                int_datum!(2),
            ]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("define".into())
            })
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("define"),
                symbol_datum!("x"),
                int_datum!(1),
                int_datum!(2),
            ]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("define".into())
            })
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("define"),
                symbol_datum!("x"),
                int_datum!(1),
                int_datum!(2),
            ]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("define".into())
            })
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("define"),
                proper_list_datum![symbol_datum!("f"), symbol_datum!("x")],
            ]),
            Err(ParserError {
                kind: ParserErrorKind::MissingExpression
            })
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("define"),
                proper_list_datum![symbol_datum!("f"), symbol_datum!("x")],
                int_datum!(1),
                proper_list_datum![symbol_datum!("define"), symbol_datum!("y"), int_datum!(1)],
            ]),
            Err(ParserError {
                kind: ParserErrorKind::MissingExpression
            })
        );
    }

    #[test]
    fn lambdas() {
        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("lambda"),
                proper_list_datum![symbol_datum!("x"), symbol_datum!("y")],
                proper_list_datum![symbol_datum!("+"), symbol_datum!("x"), symbol_datum!("y")],
            ]),
            Ok(ExprOrDef::new_expr(Expr::Lambda(Rc::new(ProcData {
                args: vec!["x#fresh".into(), "y#fresh".into()],
                rest: None,
                body: vec![ExprOrDef::new_expr(Expr::ProcCall {
                    operator: Rc::new(var_expr!("+")),
                    operands: vec_rc![var_expr!("x#fresh"), var_expr!("y#fresh")],
                })],
            }))))
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("lambda"),
                improper_list_datum![
                    symbol_datum!("x"),
                    symbol_datum!("y");
                    symbol_datum!("z")
                ],
                proper_list_datum![symbol_datum!("define"), symbol_datum!("a"), int_datum!(42)],
                proper_list_datum![
                    symbol_datum!("+"),
                    symbol_datum!("x"),
                    symbol_datum!("y"),
                    proper_list_datum![symbol_datum!("car"), symbol_datum!("z")],
                ],
            ]),
            Ok(ExprOrDef::new_expr(Expr::Lambda(Rc::new(ProcData {
                args: vec!["x#fresh".into(), "y#fresh".into()],
                rest: Some("z#fresh".into()),
                body: vec![
                    ExprOrDef::new_def(Definition::Variable {
                        name: "a".into(),
                        value: Rc::new(int_expr!(42)),
                    }),
                    ExprOrDef::new_expr(Expr::ProcCall {
                        operator: Rc::new(var_expr!("+")),
                        operands: vec_rc![
                            var_expr!("x#fresh"),
                            var_expr!("y#fresh"),
                            Expr::ProcCall {
                                operator: Rc::new(var_expr!("car")),
                                operands: vec_rc![var_expr!("z#fresh")],
                            }
                        ],
                    })
                ],
            }))))
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("lambda"),
                symbol_datum!("args"),
                int_datum!(1)
            ]),
            Ok(ExprOrDef::new_expr(Expr::Lambda(Rc::new(ProcData {
                args: vec![],
                rest: Some("args#fresh".into()),
                body: vec![ExprOrDef::new_expr(int_expr!(1))],
            }))))
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("lambda"),
                Datum::EmptyList,
                int_datum!(1)
            ]),
            Ok(ExprOrDef::new_expr(Expr::Lambda(Rc::new(ProcData {
                args: vec![],
                rest: None,
                body: vec![ExprOrDef::new_expr(int_expr!(1))],
            }))))
        );

        assert_eq!(
            parse(proper_list_datum![symbol_datum!("lambda")]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("lambda".into())
            })
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("lambda"),
                proper_list_datum![symbol_datum!("x")]
            ]),
            Err(ParserError {
                kind: ParserErrorKind::MissingExpression
            })
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("lambda"),
                proper_list_datum![symbol_datum!("x")],
                int_datum!(1),
                proper_list_datum![symbol_datum!("define"), symbol_datum!("y"), int_datum!(1)],
            ]),
            Err(ParserError {
                kind: ParserErrorKind::MissingExpression
            })
        );
    }

    #[test]
    fn quotes() {
        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("quote"),
                proper_list_datum![symbol_datum!("a"), symbol_datum!("b")],
            ]),
            Ok(ExprOrDef::new_expr(Expr::Literal(LiteralKind::Quotation(
                proper_list_datum![symbol_datum!("a"), symbol_datum!("b")],
            ))))
        );

        assert_eq!(
            parse(proper_list_datum![symbol_datum!("quote")]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("quote".into())
            })
        );
    }

    #[test]
    fn assignments() {
        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("set!"),
                symbol_datum!("x"),
                int_datum!(1),
            ]),
            Ok(ExprOrDef::new_expr(Expr::Assignment {
                variable: "x".into(),
                value: Rc::new(int_expr!(1)),
            }))
        );

        assert_eq!(
            parse(proper_list_datum![symbol_datum!("set!")]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("set!".into())
            })
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("set!"),
                symbol_datum!("x"),
                int_datum!(1),
                int_datum!(2),
            ]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("set!".into())
            })
        );
    }

    #[test]
    fn ifs() {
        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("if"),
                int_datum!(1),
                int_datum!(2),
                int_datum!(3),
            ]),
            Ok(ExprOrDef::new_expr(Expr::Conditional {
                test: Rc::new(int_expr!(1)),
                consequent: Rc::new(int_expr!(2)),
                alternate: Some(Rc::new(int_expr!(3))),
            }))
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("if"),
                int_datum!(1),
                int_datum!(2),
            ]),
            Ok(ExprOrDef::new_expr(Expr::Conditional {
                test: Rc::new(int_expr!(1)),
                consequent: Rc::new(int_expr!(2)),
                alternate: None,
            }))
        );

        assert_eq!(
            parse(proper_list_datum![symbol_datum!("if")]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("if".into())
            })
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("if"),
                int_datum!(1),
                int_datum!(2),
                int_datum!(3),
                int_datum!(4),
            ]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("if".into())
            })
        );
    }

    #[test]
    fn begins() {
        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("begin"),
                proper_list_datum![symbol_datum!("define"), symbol_datum!("x"), int_datum!(42)],
                str_datum!("hello"),
                int_datum!(1),
                int_datum!(2),
                int_datum!(3),
            ]),
            Ok(ExprOrDef::MixedBegin(vec![
                ExprOrDef::new_def(Definition::Variable {
                    name: "x".into(),
                    value: Rc::new(int_expr!(42))
                }),
                ExprOrDef::new_expr(str_expr!("hello")),
                ExprOrDef::new_expr(int_expr!(1)),
                ExprOrDef::new_expr(int_expr!(2)),
                ExprOrDef::new_expr(int_expr!(3)),
            ]))
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("begin"),
                proper_list_datum![symbol_datum!("define"), symbol_datum!("x"), int_datum!(42)],
                proper_list_datum![symbol_datum!("define"), symbol_datum!("y"), int_datum!(43)],
            ]),
            Ok(ExprOrDef::new_def(Definition::Begin(vec_rc![
                Definition::Variable {
                    name: "x".into(),
                    value: Rc::new(int_expr!(42)),
                },
                Definition::Variable {
                    name: "y".into(),
                    value: Rc::new(int_expr!(43)),
                },
            ])))
        );

        assert_eq!(
            parse(proper_list_datum![symbol_datum!("begin")]),
            Ok(ExprOrDef::new_expr(Expr::Begin(Vec::new())))
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("begin"),
                symbol_datum!("x"),
                symbol_datum!("y"),
                symbol_datum!("z"),
            ]),
            Ok(ExprOrDef::new_expr(Expr::Begin(vec_rc![
                var_expr!("x"),
                var_expr!("y"),
                var_expr!("z"),
            ])))
        )
    }

    #[test]
    fn conds() {
        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("cond"),
                proper_list_datum![bool_datum!(false), int_datum!(0)],
                proper_list_datum![
                    proper_list_datum![symbol_datum!("="), int_datum!(1), int_datum!(2)],
                    int_datum!(3),
                    int_datum!(4),
                ],
                proper_list_datum![
                    proper_list_datum![symbol_datum!("cons"), int_datum!(5), int_datum!(6)],
                    symbol_datum!("=>"),
                    symbol_datum!("car"),
                ],
                proper_list_datum![symbol_datum!("else"), int_datum!(3)],
            ]),
            Ok(ExprOrDef::new_expr(Expr::Conditional {
                test: Rc::new(bool_expr!(false)),
                consequent: Rc::new(int_expr!(0)),
                alternate: Some(Rc::new(Expr::Conditional {
                    test: Rc::new(Expr::ProcCall {
                        operator: Rc::new(var_expr!("=")),
                        operands: vec_rc![int_expr!(1), int_expr!(2)]
                    }),
                    consequent: Rc::new(Expr::Begin(vec_rc![int_expr!(3), int_expr!(4)])),
                    alternate: Some(Rc::new(Expr::SimpleLet {
                        arg: "#temp_var".into(),
                        value: Rc::new(Expr::ProcCall {
                            operator: Rc::new(var_expr!("cons")),
                            operands: vec_rc![int_expr!(5), int_expr!(6)]
                        }),
                        body: vec![ExprOrDef::new_expr(Expr::Conditional {
                            test: Rc::new(var_expr!("#temp_var")),
                            consequent: Rc::new(Expr::ProcCall {
                                operator: Rc::new(var_expr!("car")),
                                operands: vec_rc![var_expr!("#temp_var")]
                            }),
                            alternate: Some(Rc::new(int_expr!(3))),
                        })]
                    }))
                })),
            }))
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("cond"),
                proper_list_datum![symbol_datum!("else"), int_datum!(0)],
                proper_list_datum![bool_datum!(true), int_datum!(1)],
            ]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("cond".into())
            })
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("cond"),
                proper_list_datum![symbol_datum!("else"), int_datum!(0)],
                proper_list_datum![symbol_datum!("else"), int_datum!(1)],
            ]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("cond".into())
            })
        );
    }

    #[test]
    fn ands_ors() {
        assert_eq!(
            parse(proper_list_datum![symbol_datum!("and")]),
            Ok(ExprOrDef::new_expr(bool_expr!(true)))
        );

        assert_eq!(
            parse(proper_list_datum![symbol_datum!("and"), int_datum!(1),]),
            Ok(ExprOrDef::new_expr(int_expr!(1)))
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("and"),
                bool_datum!(true),
                int_datum!(1),
            ]),
            Ok(ExprOrDef::new_expr(Expr::Conditional {
                test: Rc::new(bool_expr!(true)),
                consequent: Rc::new(int_expr!(1)),
                alternate: Some(Rc::new(bool_expr!(false))),
            }))
        );

        assert_eq!(
            parse(proper_list_datum![symbol_datum!("or"), int_datum!(1),]),
            Ok(ExprOrDef::new_expr(int_expr!(1)))
        );

        assert_eq!(
            parse(proper_list_datum![symbol_datum!("or")]),
            Ok(ExprOrDef::new_expr(bool_expr!(false)))
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("or"),
                int_datum!(1),
                bool_datum!(true),
            ]),
            Ok(ExprOrDef::new_expr(Expr::SimpleLet {
                arg: "#temp_var".into(),
                value: Rc::new(int_expr!(1)),
                body: vec![ExprOrDef::new_expr(Expr::Conditional {
                    test: Rc::new(var_expr!("#temp_var")),
                    consequent: Rc::new(var_expr!("#temp_var")),
                    alternate: Some(Rc::new(bool_expr!(true))),
                })]
            }))
        );
    }

    #[test]
    fn cases() {
        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("case"),
                int_datum!(42),
                proper_list_datum![proper_list_datum![int_datum!(1)], int_datum!(2)],
                proper_list_datum![
                    proper_list_datum![int_datum!(3), int_datum!(4)],
                    int_datum!(5),
                ],
                proper_list_datum![symbol_datum!("else"), int_datum!(6)],
            ]),
            Ok(ExprOrDef::new_expr(Expr::SimpleLet {
                arg: "#temp_var".into(),
                value: Rc::new(int_expr!(42)),
                body: vec![ExprOrDef::new_expr(Expr::Conditional {
                    test: Rc::new(Expr::ProcCall {
                        operator: Rc::new(var_expr!("memv")),
                        operands: vec_rc![
                            var_expr!("#temp_var"),
                            Expr::Literal(LiteralKind::Quotation(proper_list_datum![int_datum!(
                                1
                            )]))
                        ]
                    }),
                    consequent: Rc::new(int_expr!(2)),
                    alternate: Some(Rc::new(Expr::Conditional {
                        test: Rc::new(Expr::ProcCall {
                            operator: Rc::new(var_expr!("memv")),
                            operands: vec_rc![
                                var_expr!("#temp_var"),
                                Expr::Literal(LiteralKind::Quotation(proper_list_datum![
                                    int_datum!(3),
                                    int_datum!(4)
                                ]))
                            ]
                        }),
                        consequent: Rc::new(int_expr!(5)),
                        alternate: Some(Rc::new(int_expr!(6))),
                    })),
                })]
            }))
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("case"),
                int_datum!(42),
                proper_list_datum![symbol_datum!("else"), int_datum!(0)],
                proper_list_datum![int_datum!(1), int_datum!(2)],
            ]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("case".into())
            })
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("case"),
                int_datum!(42),
                proper_list_datum![symbol_datum!("else"), int_datum!(0)],
                proper_list_datum![symbol_datum!("else"), int_datum!(1)],
            ]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("case".into())
            })
        );
    }

    #[test]
    fn lets() {
        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("let"),
                Datum::EmptyList,
                int_datum!(0)
            ]),
            Ok(ExprOrDef::new_expr(Expr::ProcCall {
                operator: Rc::new(Expr::Lambda(Rc::new(ProcData {
                    args: vec![],
                    rest: None,
                    body: vec![ExprOrDef::new_expr(int_expr!(0))],
                }))),
                operands: vec_rc![]
            }))
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("let"),
                proper_list_datum![proper_list_datum![symbol_datum!("x"), int_datum!(1)]],
                proper_list_datum![symbol_datum!("define"), symbol_datum!("y"), int_datum!(2)],
                proper_list_datum![symbol_datum!("+"), symbol_datum!("x"), symbol_datum!("y")],
            ]),
            Ok(ExprOrDef::new_expr(Expr::ProcCall {
                operator: Rc::new(Expr::Lambda(Rc::new(ProcData {
                    args: vec!["x#fresh".into()],
                    rest: None,
                    body: vec![
                        ExprOrDef::new_def(Definition::Variable {
                            name: "y".into(),
                            value: Rc::new(int_expr!(2)),
                        }),
                        ExprOrDef::new_expr(Expr::ProcCall {
                            operator: Rc::new(var_expr!("+")),
                            operands: vec_rc![var_expr!("x#fresh"), var_expr!("y")],
                        }),
                    ],
                }))),
                operands: vec_rc![int_expr!(1)],
            }))
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("let*"),
                proper_list_datum![
                    proper_list_datum![symbol_datum!("x"), int_datum!(1)],
                    proper_list_datum![symbol_datum!("y"), symbol_datum!("x")]
                ],
                symbol_datum!("y"),
            ]),
            Ok(ExprOrDef::new_expr(Expr::SimpleLet {
                arg: "x#fresh".into(),
                value: Rc::new(int_expr!(1)),
                body: vec![ExprOrDef::new_expr(Expr::SimpleLet {
                    arg: "y#fresh".into(),
                    value: Rc::new(var_expr!("x#fresh")),
                    body: vec![ExprOrDef::new_expr(var_expr!("y#fresh"))],
                })]
            }))
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("letrec"),
                proper_list_datum![proper_list_datum![symbol_datum!("x"), int_datum!(1)]],
                symbol_datum!("x"),
            ]),
            Ok(ExprOrDef::new_expr(Expr::ProcCall {
                operator: Rc::new(Expr::Lambda(Rc::new(ProcData {
                    args: vec!["x#fresh".into()],
                    rest: None,
                    body: vec![
                        ExprOrDef::new_expr(Expr::ProcCall {
                            operator: Rc::new(Expr::Lambda(Rc::new(ProcData {
                                args: vec!["#temp_var".into()],
                                rest: None,
                                body: vec![ExprOrDef::new_expr(Expr::Assignment {
                                    variable: "x#fresh".into(),
                                    value: Rc::new(var_expr!("#temp_var"))
                                })]
                            }))),
                            operands: vec_rc![int_expr!(1)]
                        }),
                        ExprOrDef::new_expr(var_expr!("x#fresh")),
                    ],
                }))),
                operands: vec_rc![Expr::Undefined],
            }))
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("let"),
                symbol_datum!("foo"),
                proper_list_datum![proper_list_datum![symbol_datum!("x"), int_datum!(1)]],
                proper_list_datum![symbol_datum!("foo"), symbol_datum!("x")],
            ]),
            Ok(ExprOrDef::new_expr(Expr::SimpleLet {
                arg: "foo#fresh".into(),
                value: Rc::new(Expr::Undefined),
                body: vec![
                    ExprOrDef::new_expr(Expr::Assignment {
                        variable: "foo#fresh".into(),
                        value: Rc::new(Expr::Lambda(Rc::new(ProcData {
                            args: vec!["x#fresh".into()],
                            rest: None,
                            body: vec![ExprOrDef::new_expr(Expr::ProcCall {
                                operator: Rc::new(var_expr!("foo#fresh")),
                                operands: vec_rc![var_expr!("x#fresh")]
                            })]
                        })))
                    }),
                    ExprOrDef::new_expr(Expr::ProcCall {
                        operator: Rc::new(var_expr!("foo#fresh")),
                        operands: vec_rc![int_expr!(1)]
                    })
                ]
            }))
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("let*"),
                symbol_datum!("foo"),
                proper_list_datum![proper_list_datum![symbol_datum!("x"), int_datum!(1)]],
                proper_list_datum![symbol_datum!("foo"), symbol_datum!("x")],
            ],),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("let*".into())
            })
        );

        assert_eq!(
            parse(proper_list_datum![symbol_datum!("let")]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("let".into())
            })
        );

        assert_eq!(
            parse(proper_list_datum![symbol_datum!("let"), int_datum!(1)]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("let".into())
            })
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("let"),
                proper_list_datum![symbol_datum!("x"), int_datum!(1)],
                symbol_datum!("x")
            ]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("let".into())
            })
        );
    }

    #[test]
    fn dos() {
        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("do"),
                proper_list_datum![
                    proper_list_datum![
                        symbol_datum!("x"),
                        int_datum!(1),
                        proper_list_datum![symbol_datum!("+"), int_datum!(1), symbol_datum!("x")],
                    ],
                    proper_list_datum![symbol_datum!("y"), int_datum!(5),]
                ],
                proper_list_datum![
                    proper_list_datum![symbol_datum!("="), symbol_datum!("x"), symbol_datum!("y")],
                    proper_list_datum![symbol_datum!("+"), symbol_datum!("x"), symbol_datum!("y")],
                ],
            ]),
            Ok(ExprOrDef::new_expr(Expr::SimpleLet {
                arg: "#temp_var".into(),
                value: Rc::new(Expr::Undefined),
                body: vec![
                    ExprOrDef::new_expr(Expr::Assignment {
                        variable: "#temp_var".into(),
                        value: Rc::new(Expr::Lambda(Rc::new(ProcData {
                            args: vec!["x".into(), "y".into()],
                            rest: None,
                            body: vec![ExprOrDef::new_expr(Expr::Conditional {
                                test: Rc::new(Expr::ProcCall {
                                    operator: Rc::new(var_expr!("=")),
                                    operands: vec_rc![var_expr!("x"), var_expr!("y")]
                                }),
                                consequent: Rc::new(Expr::Begin(vec_rc![Expr::ProcCall {
                                    operator: Rc::new(var_expr!("+")),
                                    operands: vec_rc![var_expr!("x"), var_expr!("y")]
                                }])),
                                alternate: Some(Rc::new(Expr::Begin(vec_rc![Expr::ProcCall {
                                    operator: Rc::new(var_expr!("#temp_var")),
                                    operands: vec_rc![
                                        Expr::ProcCall {
                                            operator: Rc::new(var_expr!("+")),
                                            operands: vec_rc![int_expr!(1), var_expr!("x")]
                                        },
                                        var_expr!("y")
                                    ]
                                }])))
                            })]
                        })))
                    }),
                    ExprOrDef::new_expr(Expr::ProcCall {
                        operator: Rc::new(var_expr!("#temp_var")),
                        operands: vec_rc![int_expr!(1), int_expr!(5)]
                    })
                ]
            }))
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("do"),
                Datum::EmptyList,
                proper_list_datum![bool_datum!(false)],
                proper_list_datum![symbol_datum!("f")],
            ]),
            Ok(ExprOrDef::new_expr(Expr::SimpleLet {
                arg: "#temp_var".into(),
                value: Rc::new(Expr::Undefined),
                body: vec![
                    ExprOrDef::new_expr(Expr::Assignment {
                        variable: "#temp_var".into(),
                        value: Rc::new(Expr::Lambda(Rc::new(ProcData {
                            args: vec![],
                            rest: None,
                            body: vec![ExprOrDef::new_expr(Expr::Conditional {
                                test: Rc::new(bool_expr!(false)),
                                consequent: Rc::new(Expr::Begin(Vec::new())),
                                alternate: Some(Rc::new(Expr::Begin(vec_rc![
                                    Expr::ProcCall {
                                        operator: Rc::new(var_expr!("f")),
                                        operands: vec_rc![]
                                    },
                                    Expr::ProcCall {
                                        operator: Rc::new(var_expr!("#temp_var")),
                                        operands: vec_rc![]
                                    }
                                ])))
                            })]
                        })))
                    }),
                    ExprOrDef::new_expr(Expr::ProcCall {
                        operator: Rc::new(var_expr!("#temp_var")),
                        operands: vec_rc![]
                    })
                ]
            }))
        );
    }

    #[test]
    fn delays() {
        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("delay"),
                proper_list_datum![symbol_datum!("f"), int_datum!(1)]
            ]),
            Ok(ExprOrDef::new_expr(Expr::ProcCall {
                operator: Rc::new(Expr::Variable("make-promise".into())),
                operands: vec_rc![Expr::Lambda(Rc::new(ProcData {
                    args: vec![],
                    rest: None,
                    body: vec![ExprOrDef::new_expr(Expr::ProcCall {
                        operator: Rc::new(var_expr!("f")),
                        operands: vec_rc![int_expr!(1)],
                    })],
                }))],
            })),
        );

        assert_eq!(
            parse(proper_list_datum![symbol_datum!("delay")]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("delay".into())
            })
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("delay"),
                int_datum!(1),
                int_datum!(2)
            ]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("delay".into())
            })
        );
    }

    #[test]
    fn quasiquotes() {
        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("quasiquote"),
                int_datum!(1),
            ]),
            Ok(ExprOrDef::new_expr(Expr::Literal(LiteralKind::Quotation(
                int_datum!(1)
            )))),
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("quasiquote"),
                proper_list_datum![symbol_datum!("+"), int_datum!(1), int_datum!(2)],
            ]),
            Ok(ExprOrDef::new_expr(Expr::ProcCall {
                operator: Rc::new(var_expr!("list")),
                operands: vec_rc![
                    Expr::Literal(LiteralKind::Quotation(symbol_datum!("+"))),
                    Expr::Literal(LiteralKind::Quotation(int_datum!(1))),
                    Expr::Literal(LiteralKind::Quotation(int_datum!(2))),
                ]
            })),
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("quasiquote"),
                proper_list_datum![
                    symbol_datum!("a"),
                    proper_list_datum![
                        symbol_datum!("unquote"),
                        proper_list_datum![symbol_datum!("+"), int_datum!(1), int_datum!(2)]
                    ],
                    proper_list_datum![
                        symbol_datum!("unquote-splicing"),
                        proper_list_datum![
                            symbol_datum!("map"),
                            symbol_datum!("abs"),
                            proper_list_datum![
                                symbol_datum!("quote"),
                                proper_list_datum![int_datum!(4), int_datum!(-5), int_datum!(6)]
                            ],
                        ]
                    ],
                    symbol_datum!("b"),
                ],
            ]),
            Ok(ExprOrDef::new_expr(Expr::ProcCall {
                operator: Rc::new(var_expr!("append")),
                operands: vec_rc![
                    Expr::ProcCall {
                        operator: Rc::new(var_expr!("list")),
                        operands: vec_rc![
                            Expr::Literal(LiteralKind::Quotation(symbol_datum!("a"))),
                            Expr::ProcCall {
                                operator: Rc::new(var_expr!("+")),
                                operands: vec_rc![int_expr!(1), int_expr!(2)],
                            },
                        ]
                    },
                    Expr::ProcCall {
                        operator: Rc::new(var_expr!("map")),
                        operands: vec_rc![
                            var_expr!("abs"),
                            Expr::Literal(LiteralKind::Quotation(proper_list_datum![
                                int_datum!(4),
                                int_datum!(-5),
                                int_datum!(6)
                            ]))
                        ]
                    },
                    Expr::ProcCall {
                        operator: Rc::new(var_expr!("list")),
                        operands: vec_rc![Expr::Literal(LiteralKind::Quotation(symbol_datum!(
                            "b"
                        )))]
                    },
                    Expr::Literal(LiteralKind::Quotation(Datum::EmptyList))
                ]
            }))
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("quasiquote"),
                vector_datum![
                    int_datum!(1),
                    proper_list_datum![
                        symbol_datum!("unquote"),
                        proper_list_datum![symbol_datum!("sqrt"), int_datum!(4)]
                    ],
                    int_datum!(3),
                    int_datum!(4)
                ],
            ]),
            Ok(ExprOrDef::new_expr(Expr::ProcCall {
                operator: Rc::new(var_expr!("vector")),
                operands: vec_rc![
                    Expr::Literal(LiteralKind::Quotation(int_datum!(1))),
                    Expr::ProcCall {
                        operator: Rc::new(var_expr!("sqrt")),
                        operands: vec_rc![int_expr!(4)],
                    },
                    Expr::Literal(LiteralKind::Quotation(int_datum!(3))),
                    Expr::Literal(LiteralKind::Quotation(int_datum!(4))),
                ]
            }))
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("quasiquote"),
                vector_datum![
                    int_datum!(1),
                    proper_list_datum![
                        symbol_datum!("unquote-splicing"),
                        proper_list_datum![symbol_datum!("f"), int_datum!(2)]
                    ],
                ],
            ]),
            Ok(ExprOrDef::new_expr(Expr::ProcCall {
                operator: Rc::new(var_expr!("list->vector")),
                operands: vec_rc![Expr::ProcCall {
                    operator: Rc::new(var_expr!("append")),
                    operands: vec_rc![
                        Expr::ProcCall {
                            operator: Rc::new(var_expr!("list")),
                            operands: vec_rc![Expr::Literal(LiteralKind::Quotation(int_datum!(1)))],
                        },
                        Expr::ProcCall {
                            operator: Rc::new(var_expr!("f")),
                            operands: vec_rc![int_expr!(2)],
                        },
                        Expr::Literal(LiteralKind::Quotation(Datum::EmptyList))
                    ]
                }]
            }))
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("quasiquote"),
                proper_list_datum![
                    symbol_datum!("a"),
                    proper_list_datum![
                        symbol_datum!("quasiquote"),
                        proper_list_datum![
                            symbol_datum!("b"),
                            proper_list_datum![
                                symbol_datum!("unquote"),
                                proper_list_datum![
                                    symbol_datum!("+"),
                                    int_datum!(1),
                                    int_datum!(2)
                                ]
                            ],
                            proper_list_datum![
                                symbol_datum!("unquote"),
                                proper_list_datum![
                                    symbol_datum!("foo"),
                                    proper_list_datum![
                                        symbol_datum!("unquote"),
                                        proper_list_datum![
                                            symbol_datum!("+"),
                                            int_datum!(1),
                                            int_datum!(3)
                                        ]
                                    ],
                                    symbol_datum!("d")
                                ]
                            ],
                            symbol_datum!("e")
                        ]
                    ],
                    symbol_datum!("f")
                ]
            ]),
            Ok(ExprOrDef::new_expr(Expr::ProcCall {
                operator: Rc::new(var_expr!("list")),
                operands: vec_rc![
                    Expr::Literal(LiteralKind::Quotation(symbol_datum!("a"))),
                    Expr::ProcCall {
                        operator: Rc::new(var_expr!("cons")),
                        operands: vec_rc![
                            Expr::Literal(LiteralKind::Quotation(symbol_datum!("quasiquote"))),
                            Expr::ProcCall {
                                operator: Rc::new(var_expr!("list")),
                                operands: vec_rc![Expr::ProcCall {
                                    operator: Rc::new(var_expr!("list")),
                                    operands: vec_rc![
                                        Expr::Literal(LiteralKind::Quotation(symbol_datum!("b"))),
                                        Expr::ProcCall {
                                            operator: Rc::new(var_expr!("cons")),
                                            operands: vec_rc![
                                                Expr::Literal(LiteralKind::Quotation(
                                                    symbol_datum!("unquote")
                                                )),
                                                Expr::ProcCall {
                                                    operator: Rc::new(var_expr!("list")),
                                                    operands: vec_rc![Expr::ProcCall {
                                                        operator: Rc::new(var_expr!("list")),
                                                        operands: vec_rc![
                                                            Expr::Literal(LiteralKind::Quotation(
                                                                symbol_datum!("+")
                                                            )),
                                                            Expr::Literal(LiteralKind::Quotation(
                                                                int_datum!(1)
                                                            )),
                                                            Expr::Literal(LiteralKind::Quotation(
                                                                int_datum!(2)
                                                            ))
                                                        ]
                                                    }]
                                                }
                                            ],
                                        },
                                        Expr::ProcCall {
                                            operator: Rc::new(var_expr!("cons")),
                                            operands: vec_rc![
                                                Expr::Literal(LiteralKind::Quotation(
                                                    symbol_datum!("unquote")
                                                )),
                                                Expr::ProcCall {
                                                    operator: Rc::new(var_expr!("list")),
                                                    operands: vec_rc![Expr::ProcCall {
                                                        operator: Rc::new(var_expr!("list")),
                                                        operands: vec_rc![
                                                            Expr::Literal(LiteralKind::Quotation(
                                                                symbol_datum!("foo")
                                                            )),
                                                            Expr::ProcCall {
                                                                operator: Rc::new(var_expr!("+")),
                                                                operands: vec_rc![
                                                                    int_expr!(1),
                                                                    int_expr!(3)
                                                                ]
                                                            },
                                                            Expr::Literal(LiteralKind::Quotation(
                                                                symbol_datum!("d")
                                                            ))
                                                        ]
                                                    }]
                                                }
                                            ],
                                        },
                                        Expr::Literal(LiteralKind::Quotation(symbol_datum!("e")))
                                    ]
                                }]
                            },
                        ]
                    },
                    Expr::Literal(LiteralKind::Quotation(symbol_datum!("f")))
                ]
            }))
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("quasiquote"),
                improper_list_datum![int_datum!(1), int_datum!(2); int_datum!(3)]
            ]),
            Ok(ExprOrDef::new_expr(Expr::ProcCall {
                operator: Rc::new(var_expr!("append")),
                operands: vec_rc![
                    Expr::ProcCall {
                        operator: Rc::new(var_expr!("list")),
                        operands: vec_rc![
                            Expr::Literal(LiteralKind::Quotation(int_datum!(1))),
                            Expr::Literal(LiteralKind::Quotation(int_datum!(2)))
                        ],
                    },
                    Expr::Literal(LiteralKind::Quotation(int_datum!(3)))
                ]
            }))
        );
    }

    #[test]
    fn unquote_errs() {
        assert_eq!(
            parse(proper_list_datum![symbol_datum!("unquote")]),
            Err(ParserError {
                kind: ParserErrorKind::IllegalUnquote
            })
        );

        assert_eq!(
            parse(proper_list_datum![symbol_datum!("unquote-splicing")]),
            Err(ParserError {
                kind: ParserErrorKind::IllegalUnquoteSplicing
            })
        );

        assert_eq!(
            parse(proper_list_datum![symbol_datum!("unquote"), int_datum!(1)]),
            Err(ParserError {
                kind: ParserErrorKind::IllegalUnquote
            })
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("unquote-splicing"),
                int_datum!(1),
            ]),
            Err(ParserError {
                kind: ParserErrorKind::IllegalUnquoteSplicing
            })
        );
    }

    #[test]
    fn define_syntax_top_level() {
        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("if"),
                proper_list_datum![symbol_datum!("define-syntax")],
                bool_datum!(false)
            ]),
            Err(ParserError {
                kind: ParserErrorKind::IllegalDefineSyntax
            })
        )
    }
}
