use std::error::Error;
use std::fmt;

use crate::datum::*;
use crate::expr::*;

#[derive(Debug, PartialEq)]
pub enum ParserErrorKind {
    BadSyntax(String),
    IllegalEmptyList,
    IllegalDot,
    IllegalVariableName(String),
    IllegalDefine,
    IllegalUnquote,
    IllegalUnquoteSplicing,
    MissingExpression,
}

#[derive(Debug, PartialEq)]
pub struct ParserError {
    kind: ParserErrorKind,
}

impl Error for ParserError {}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            ParserErrorKind::BadSyntax(s) => write!(f, "Bad syntax {s}"),
            ParserErrorKind::IllegalEmptyList => write!(f, "Illegal empty list"),
            ParserErrorKind::IllegalDot => write!(f, "Illegal dot"),
            ParserErrorKind::IllegalVariableName(s) => {
                write!(f, "Illegal variable name {s}")
            }
            ParserErrorKind::IllegalDefine => write!(f, "Definition not allowed here"),
            ParserErrorKind::IllegalUnquote => write!(f, "Unquote not allowed here"),
            ParserErrorKind::IllegalUnquoteSplicing => {
                write!(f, "Unquote-splicing not allowed here")
            }
            ParserErrorKind::MissingExpression => write!(f, "Missing expression"),
        }
    }
}

fn is_keyword(symb: &str) -> bool {
    matches!(
        symb,
        "quote"
            | "lambda"
            | "if"
            | "set!"
            | "begin"
            | "cond"
            | "and"
            | "or"
            | "case"
            | "let"
            | "let*"
            | "letrec"
            | "do"
            | "delay"
            | "quasiquote"
            | "else"
            | "=>"
            | "define"
            | "unquote"
            | "unquote-splicing"
    )
}

fn process_define<'a, I: Iterator<Item = &'a Datum>>(
    var: &Datum,
    mut body: I,
) -> Result<Definition, ParserError> {
    let bs_err = || ParserError {
        kind: ParserErrorKind::BadSyntax("define".to_owned()),
    };
    match var {
        Datum::Simple(SimpleDatum::Symbol(s)) => {
            if is_keyword(s) {
                Err(ParserError {
                    kind: ParserErrorKind::IllegalVariableName(s.clone()),
                })
            } else {
                let expr = parse_expr(body.next().ok_or_else(bs_err)?)?;

                if body.next().is_none() {
                    Ok(Definition::Variable {
                        name: s.clone(),
                        value: Box::new(expr),
                    })
                } else {
                    Err(bs_err())
                }
            }
        }
        Datum::Compound(CompoundDatum::List(list)) => match list {
            ListKind::Proper(forms) | ListKind::Improper(forms, _) => {
                if forms.is_empty() {
                    return Err(ParserError {
                        kind: ParserErrorKind::IllegalEmptyList,
                    });
                }
                let name = match &forms[0] {
                    Datum::Simple(SimpleDatum::Symbol(s)) => s,
                    _ => return Err(bs_err()),
                };
                let args = forms
                    .iter()
                    .skip(1)
                    .map(|d| match d {
                        Datum::Simple(SimpleDatum::Symbol(s)) => {
                            if is_keyword(s) {
                                Err(ParserError {
                                    kind: ParserErrorKind::IllegalVariableName(s.clone()),
                                })
                            } else {
                                Ok(s.clone())
                            }
                        }
                        _ => Err(bs_err()),
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(Definition::Procedure {
                    name: name.clone(),
                    args,
                    rest: if let ListKind::Improper(_, rest) = list {
                        match &**rest {
                            Datum::Simple(SimpleDatum::Symbol(s)) => {
                                if is_keyword(s) {
                                    return Err(ParserError {
                                        kind: ParserErrorKind::IllegalVariableName(s.clone()),
                                    });
                                }
                                Some(s.clone())
                            }
                            _ => {
                                return Err(bs_err());
                            }
                        }
                    } else {
                        None
                    },
                    body: process_body(body)?,
                })
            }
            _ => Err(bs_err()),
        },
        _ => Err(bs_err()),
    }
}

fn process_body<'a, I: Iterator<Item = &'a Datum>>(data: I) -> Result<Body, ParserError> {
    let mut defs: Vec<Definition> = vec![];
    let mut exprs: Vec<Expr> = vec![];
    let mut last_is_expr = false;
    for d in data {
        match parse(d)? {
            ExprOrDef::Definition(def) => {
                defs.push(def);
                last_is_expr = false;
            }
            ExprOrDef::Expr(expr) => {
                exprs.push(expr);
                last_is_expr = true;
            }
            ExprOrDef::MixedBegin(_) => {
                return Err(ParserError {
                    kind: ParserErrorKind::IllegalDefine,
                })
            }
        }
    }
    if last_is_expr {
        Ok(Body { defs, exprs })
    } else {
        Err(ParserError {
            kind: ParserErrorKind::MissingExpression,
        })
    }
}

fn process_qq_template_or_splice(
    datum: &Datum,
    qq_level: usize,
) -> Result<QQTemplateOrSplice, ParserError> {
    if qq_level == 0 {
        Err(ParserError {
            kind: ParserErrorKind::IllegalUnquote,
        })
    } else {
        match datum {
            Datum::Compound(CompoundDatum::List(ListKind::Abbreviation(
                AbbreviationPrefix::UnquoteSplicing,
                arg,
            ))) => Ok(QQTemplateOrSplice::Splice(process_qq_template(
                arg,
                qq_level - 1,
            )?)),
            Datum::Compound(CompoundDatum::List(ListKind::Proper(list))) => match list[..] {
                [Datum::Simple(SimpleDatum::Symbol(ref s)), ref arg] if s == "unquote-splicing" => {
                    Ok(QQTemplateOrSplice::Splice(process_qq_template(
                        arg,
                        qq_level - 1,
                    )?))
                }
                _ => Ok(QQTemplateOrSplice::Template(process_qq_template(
                    datum, qq_level,
                )?)),
            },
            _ => Ok(QQTemplateOrSplice::Template(process_qq_template(
                datum, qq_level,
            )?)),
        }
    }
}

fn process_qq_template(datum: &Datum, qq_level: usize) -> Result<QQTemplate, ParserError> {
    if qq_level == 0 {
        return Ok(QQTemplate::Level0(Box::new(parse_expr(datum)?)));
    }
    Ok(QQTemplate::LevelD(
        qq_level,
        match datum {
            Datum::Simple(_) | Datum::EmptyList => Box::new(QQTemplateData::Datum(datum.clone())),
            Datum::Compound(CompoundDatum::List(list)) => match list {
                ListKind::Proper(list) => match list[0] {
                    Datum::Simple(SimpleDatum::Symbol(ref s))
                        if s == "unquote" || s == "quasiquote" =>
                    {
                        if list.len() != 2 {
                            return Err(ParserError {
                                kind: ParserErrorKind::BadSyntax(s.to_owned()),
                            });
                        }
                        if s == "unquote" {
                            Box::new(QQTemplateData::Unquotation(process_qq_template(
                                &list[1],
                                qq_level - 1,
                            )?))
                        } else {
                            Box::new(QQTemplateData::List(ListQQTemplate::QQ(
                                process_qq_template(&list[1], qq_level + 1)?,
                            )))
                        }
                    }
                    _ => Box::new(QQTemplateData::List(ListQQTemplate::Proper(
                        list.iter()
                            .map(|d| process_qq_template_or_splice(d, qq_level))
                            .collect::<Result<Vec<_>, _>>()?,
                    ))),
                },
                ListKind::Improper(list, last) => {
                    Box::new(QQTemplateData::List(ListQQTemplate::Improper(
                        list.iter()
                            .map(|d| process_qq_template_or_splice(d, qq_level))
                            .collect::<Result<Vec<_>, _>>()?,
                        process_qq_template(last, qq_level)?,
                    )))
                }
                ListKind::Abbreviation(abbr, arg) => match abbr {
                    AbbreviationPrefix::Unquote => Box::new(QQTemplateData::Unquotation(
                        process_qq_template(arg, qq_level - 1)?,
                    )),
                    AbbreviationPrefix::Quote => Box::new(QQTemplateData::List(
                        ListQQTemplate::Quote(process_qq_template(arg, qq_level)?),
                    )),
                    AbbreviationPrefix::Quasiquote => Box::new(QQTemplateData::List(
                        ListQQTemplate::QQ(process_qq_template(arg, qq_level + 1)?),
                    )),
                    AbbreviationPrefix::UnquoteSplicing => {
                        return Err(ParserError {
                            kind: ParserErrorKind::IllegalUnquoteSplicing,
                        });
                    }
                },
            },
            Datum::Compound(CompoundDatum::Vector(vector)) => Box::new(QQTemplateData::Vector(
                vector
                    .iter()
                    .map(|d| process_qq_template_or_splice(d, qq_level))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            Datum::Void => {
                return Err(ParserError {
                    kind: ParserErrorKind::BadSyntax("quasiquote".to_owned()),
                });
            }
        },
    ))
}

fn process_keyword<'a, I: Iterator<Item = &'a Datum>>(
    kw: &str,
    mut operands: I,
) -> Result<ExprOrDef, ParserError> {
    let bs_err = || ParserError {
        kind: ParserErrorKind::BadSyntax(kw.to_owned()),
    };
    if kw == "define" {
        let var = operands.next().ok_or_else(bs_err)?;
        Ok(ExprOrDef::Definition(process_define(var, operands)?))
    } else {
        match kw {
            "quote" => {
                let operand = operands.next().ok_or_else(bs_err)?;
                if operands.next().is_none() {
                    Ok(ExprOrDef::Expr(Expr::Literal(LiteralKind::Quotation(
                        operand.clone(),
                    ))))
                } else {
                    Err(bs_err())
                }
            }
            "lambda" => {
                let formals = operands.next().ok_or_else(bs_err)?;
                match formals {
                    Datum::Compound(CompoundDatum::List(list)) => match list {
                        ListKind::Proper(forms) | ListKind::Improper(forms, _) => {
                            if forms.is_empty() {
                                return Err(ParserError {
                                    kind: ParserErrorKind::IllegalEmptyList,
                                });
                            }
                            let args = forms
                                .iter()
                                .map(|d| match d {
                                    Datum::Simple(SimpleDatum::Symbol(s)) => {
                                        if is_keyword(s) {
                                            Err(ParserError {
                                                kind: ParserErrorKind::IllegalVariableName(
                                                    s.clone(),
                                                ),
                                            })
                                        } else {
                                            Ok(s.clone())
                                        }
                                    }
                                    _ => Err(bs_err()),
                                })
                                .collect::<Result<Vec<_>, _>>()?;

                            Ok(ExprOrDef::Expr(Expr::Lambda {
                                args,
                                rest: if let ListKind::Improper(_, rest) = list {
                                    match &**rest {
                                        Datum::Simple(SimpleDatum::Symbol(s)) => {
                                            if is_keyword(s) {
                                                return Err(ParserError {
                                                    kind: ParserErrorKind::IllegalVariableName(
                                                        s.clone(),
                                                    ),
                                                });
                                            }
                                            Some(s.clone())
                                        }
                                        _ => {
                                            return Err(bs_err());
                                        }
                                    }
                                } else {
                                    None
                                },
                                body: process_body(operands)?,
                            }))
                        }
                        _ => Err(bs_err()),
                    },
                    Datum::Simple(SimpleDatum::Symbol(rest)) => {
                        if is_keyword(rest) {
                            return Err(ParserError {
                                kind: ParserErrorKind::IllegalVariableName(rest.clone()),
                            });
                        }
                        Ok(ExprOrDef::Expr(Expr::Lambda {
                            args: vec![],
                            rest: Some(rest.clone()),
                            body: process_body(operands)?,
                        }))
                    }
                    Datum::EmptyList => Ok(ExprOrDef::Expr(Expr::Lambda {
                        args: vec![],
                        rest: None,
                        body: process_body(operands)?,
                    })),
                    _ => Err(bs_err()),
                }
            }
            "begin" => {
                let mut children = vec![];
                let mut has_def = false;
                let mut has_expr = false;
                for operand in operands {
                    let child = parse(operand)?;
                    match child {
                        ExprOrDef::Definition(_) => {
                            has_def = true;
                        }
                        _ => {
                            has_expr = true;
                        }
                    }
                    children.push(child);
                }
                if has_def && has_expr {
                    Ok(ExprOrDef::MixedBegin(children))
                } else if has_def {
                    Ok(ExprOrDef::Definition(Definition::Begin(
                        children
                            .into_iter()
                            .map(|eod| match eod {
                                ExprOrDef::Definition(d) => d,
                                _ => unreachable!("should not encounter non-definition"),
                            })
                            .collect(),
                    )))
                } else {
                    Ok(ExprOrDef::Expr(Expr::DerivedExpr(DerivedExprKind::Begin(
                        children
                            .into_iter()
                            .map(|eod| match eod {
                                ExprOrDef::Expr(e) => e,
                                _ => unreachable!("should not encounter non-definition"),
                            })
                            .collect(),
                    ))))
                }
            }
            "set!" => {
                let variable = operands.next().ok_or_else(bs_err)?;
                let value = operands.next().ok_or_else(bs_err)?;
                if operands.next().is_none() {
                    Ok(ExprOrDef::Expr(Expr::Assignment {
                        variable: match variable {
                            Datum::Simple(SimpleDatum::Symbol(s)) => s.clone(),
                            _ => {
                                return Err(bs_err());
                            }
                        },
                        value: Box::new(parse_expr(value)?),
                    }))
                } else {
                    Err(bs_err())
                }
            }
            "if" => {
                let test = operands.next().ok_or_else(bs_err)?;
                let consequent = operands.next().ok_or_else(bs_err)?;
                let alternate = operands.next();
                if operands.next().is_none() {
                    Ok(ExprOrDef::Expr(Expr::Conditional {
                        test: Box::new(parse_expr(test)?),
                        consequent: Box::new(parse_expr(consequent)?),
                        alternate: if let Some(a) = alternate {
                            Some(Box::new(parse_expr(a)?))
                        } else {
                            None
                        },
                    }))
                } else {
                    Err(bs_err())
                }
            }
            "cond" => {
                let mut clauses = vec![];
                let mut else_present = false;
                for clause in operands {
                    if else_present {
                        return Err(bs_err());
                    }
                    if let Datum::Compound(CompoundDatum::List(ListKind::Proper(parts))) = clause {
                        if parts.is_empty() {
                            return Err(ParserError {
                                kind: ParserErrorKind::IllegalEmptyList,
                            });
                        }
                        match &parts[0] {
                            Datum::Simple(SimpleDatum::Symbol(s)) if s == "else" => {
                                let seq = parts
                                    .iter()
                                    .skip(1)
                                    .map(parse_expr)
                                    .collect::<Result<Vec<_>, _>>()?;
                                if seq.is_empty() {
                                    return Err(bs_err());
                                }
                                clauses.push(CondClause::Else(seq));
                                else_present = true;
                            }
                            _ => {
                                let test = parse_expr(&parts[0])?;
                                if parts.len() == 1 {
                                    clauses.push(CondClause::Normal(test, vec![]));
                                    continue;
                                }
                                match &parts[1] {
                                    Datum::Simple(SimpleDatum::Symbol(s)) if s == "=>" => {
                                        if parts.len() != 3 {
                                            return Err(bs_err());
                                        }
                                        let recipient = parse_expr(&parts[2])?;
                                        clauses.push(CondClause::Arrow(test, recipient));
                                    }
                                    _ => {
                                        let seq = parts
                                            .iter()
                                            .skip(1)
                                            .map(parse_expr)
                                            .collect::<Result<Vec<_>, _>>()?;
                                        if seq.is_empty() {
                                            return Err(bs_err());
                                        }
                                        clauses.push(CondClause::Normal(test, seq));
                                    }
                                }
                            }
                        }
                    } else {
                        return Err(bs_err());
                    }
                }
                Ok(ExprOrDef::Expr(Expr::DerivedExpr(DerivedExprKind::Cond(
                    clauses,
                ))))
            }
            "else" | "kw" => Err(bs_err()),
            "and" => Ok(ExprOrDef::Expr(Expr::DerivedExpr(DerivedExprKind::And(
                operands.map(parse_expr).collect::<Result<Vec<_>, _>>()?,
            )))),
            "or" => Ok(ExprOrDef::Expr(Expr::DerivedExpr(DerivedExprKind::Or(
                operands.map(parse_expr).collect::<Result<Vec<_>, _>>()?,
            )))),
            "case" => {
                let key = parse_expr(operands.next().ok_or_else(bs_err)?)?;
                let mut clauses = vec![];
                let mut else_present = false;
                for clause in operands {
                    if else_present {
                        return Err(bs_err());
                    }
                    if let Datum::Compound(CompoundDatum::List(ListKind::Proper(parts))) = clause {
                        if parts.is_empty() {
                            return Err(ParserError {
                                kind: ParserErrorKind::IllegalEmptyList,
                            });
                        }
                        match &parts[0] {
                            Datum::Simple(SimpleDatum::Symbol(s)) if s == "else" => {
                                let seq = parts
                                    .iter()
                                    .skip(1)
                                    .map(parse_expr)
                                    .collect::<Result<Vec<_>, _>>()?;
                                if seq.is_empty() {
                                    return Err(bs_err());
                                }
                                clauses.push(CaseClause::Else(seq));
                                else_present = true;
                            }
                            _ => {
                                if let Datum::Compound(CompoundDatum::List(ListKind::Proper(
                                    keys,
                                ))) = &parts[0]
                                {
                                    let seq = parts
                                        .iter()
                                        .skip(1)
                                        .map(parse_expr)
                                        .collect::<Result<Vec<_>, _>>()?;
                                    if seq.is_empty() {
                                        return Err(bs_err());
                                    }
                                    clauses.push(CaseClause::Normal(keys.clone(), seq));
                                } else {
                                    return Err(bs_err());
                                }
                            }
                        }
                    } else {
                        return Err(bs_err());
                    }
                }
                Ok(ExprOrDef::Expr(Expr::DerivedExpr(DerivedExprKind::Case {
                    key: Box::new(key),
                    clauses,
                })))
            }
            "let" | "let*" | "letrec" => {
                let mut first = operands.next().ok_or_else(bs_err)?;
                let mut name: Option<String> = None;
                if let Datum::Simple(SimpleDatum::Symbol(n)) = first {
                    if kw == "let" {
                        first = operands.next().ok_or_else(bs_err)?;
                        name = Some(n.clone());
                    } else {
                        return Err(bs_err());
                    }
                }
                match first {
                    Datum::Compound(CompoundDatum::List(ListKind::Proper(_)))
                    | Datum::EmptyList => {
                        let bindings = if let Datum::Compound(CompoundDatum::List(
                            ListKind::Proper(binding_data),
                        )) = first
                        {
                            binding_data
                                .iter()
                                .map(|binding| {
                                    if let Datum::Compound(CompoundDatum::List(ListKind::Proper(
                                        parts,
                                    ))) = binding
                                    {
                                        if parts.len() != 2 {
                                            return Err(bs_err());
                                        }
                                        if let Datum::Simple(SimpleDatum::Symbol(name)) = &parts[0]
                                        {
                                            Ok((name.clone(), parse_expr(&parts[1])?))
                                        } else {
                                            Err(bs_err())
                                        }
                                    } else {
                                        Err(bs_err())
                                    }
                                })
                                .collect::<Result<Vec<_>, _>>()?
                        } else {
                            vec![]
                        };
                        let body = process_body(operands)?;
                        Ok(ExprOrDef::Expr(Expr::DerivedExpr(DerivedExprKind::Let {
                            kind: match kw {
                                "let" => {
                                    if let Some(name) = name {
                                        LetKind::Named(name)
                                    } else {
                                        LetKind::Normal
                                    }
                                }
                                "let*" => LetKind::Star,
                                "letrec" => LetKind::Rec,
                                _ => unreachable!(),
                            },
                            bindings,
                            body,
                        })))
                    }
                    _ => Err(bs_err()),
                }
            }
            "do" => {
                let first = operands.next().ok_or_else(bs_err)?;
                match first {
                    Datum::Compound(CompoundDatum::List(ListKind::Proper(_)))
                    | Datum::EmptyList => {
                        let specs = if let Datum::Compound(CompoundDatum::List(ListKind::Proper(
                            spec_data,
                        ))) = first
                        {
                            spec_data
                                .iter()
                                .map(|spec| {
                                    if let Datum::Compound(CompoundDatum::List(ListKind::Proper(
                                        parts,
                                    ))) = spec
                                    {
                                        if parts.len() != 2 && parts.len() != 3 {
                                            return Err(bs_err());
                                        }
                                        if let Datum::Simple(SimpleDatum::Symbol(name)) = &parts[0]
                                        {
                                            Ok(IterationSpec {
                                                variable: name.clone(),
                                                init: parse_expr(&parts[1])?,
                                                step: match parts.len() {
                                                    2 => None,
                                                    3 => Some(parse_expr(&parts[2])?),
                                                    _ => unreachable!("parts len should be 2 or 3"),
                                                },
                                            })
                                        } else {
                                            Err(bs_err())
                                        }
                                    } else {
                                        Err(bs_err())
                                    }
                                })
                                .collect::<Result<Vec<_>, _>>()?
                        } else {
                            vec![]
                        };
                        let second = operands.next().ok_or_else(bs_err)?;
                        if let Datum::Compound(CompoundDatum::List(ListKind::Proper(term))) = second
                        {
                            if term.is_empty() {
                                return Err(bs_err());
                            }
                            let test = parse_expr(&term[0])?;
                            let seq = term
                                .iter()
                                .skip(1)
                                .map(parse_expr)
                                .collect::<Result<Vec<_>, _>>()?;
                            let body = operands.map(parse_expr).collect::<Result<Vec<_>, _>>()?;
                            Ok(ExprOrDef::Expr(Expr::DerivedExpr(DerivedExprKind::Do {
                                specs,
                                term: (Box::new(test), seq),
                                body,
                            })))
                        } else {
                            Err(bs_err())
                        }
                    }
                    _ => Err(bs_err()),
                }
            }
            "delay" => {
                let expr = parse_expr(operands.next().ok_or_else(bs_err)?)?;
                if operands.next().is_none() {
                    Ok(ExprOrDef::Expr(Expr::DerivedExpr(DerivedExprKind::Delay(
                        Box::new(expr),
                    ))))
                } else {
                    Err(bs_err())
                }
            }
            "quasiquote" => {
                let template = process_qq_template(operands.next().ok_or_else(bs_err)?, 1)?;
                if operands.next().is_none() {
                    Ok(ExprOrDef::Expr(Expr::Quasiquotation(template)))
                } else {
                    Err(bs_err())
                }
            }
            "unquote" => Err(ParserError {
                kind: ParserErrorKind::IllegalUnquote,
            }),
            "unquote-splicing" => Err(ParserError {
                kind: ParserErrorKind::IllegalUnquoteSplicing,
            }),
            _ => unreachable!("keyword should have been handled"),
        }
    }
}

fn parse_expr(datum: &Datum) -> Result<Expr, ParserError> {
    match parse(datum)? {
        ExprOrDef::Expr(expr) => Ok(expr),
        _ => Err(ParserError {
            kind: ParserErrorKind::IllegalDefine,
        }),
    }
}

pub fn parse(datum: &Datum) -> Result<ExprOrDef, ParserError> {
    match datum {
        Datum::Simple(simple) => match simple {
            SimpleDatum::Boolean(b) => Ok(ExprOrDef::Expr(Expr::Literal(
                LiteralKind::SelfEvaluating(SelfEvaluatingKind::Boolean(*b)),
            ))),
            SimpleDatum::Number(n) => Ok(ExprOrDef::Expr(Expr::Literal(
                LiteralKind::SelfEvaluating(SelfEvaluatingKind::Number(n.clone())),
            ))),
            SimpleDatum::Character(c) => Ok(ExprOrDef::Expr(Expr::Literal(
                LiteralKind::SelfEvaluating(SelfEvaluatingKind::Character(*c)),
            ))),
            SimpleDatum::String(s) => Ok(ExprOrDef::Expr(Expr::Literal(
                LiteralKind::SelfEvaluating(SelfEvaluatingKind::String(s.clone())),
            ))),
            SimpleDatum::Symbol(symb) => match symb.as_str() {
                kw if is_keyword(kw) => Err(ParserError {
                    kind: ParserErrorKind::BadSyntax(kw.to_owned()),
                }),
                _ => Ok(ExprOrDef::Expr(Expr::Variable(symb.clone()))),
            },
        },
        Datum::Compound(compound) => match compound {
            CompoundDatum::List(list) => match list {
                ListKind::Proper(list) => {
                    let first = list.first().ok_or(ParserError {
                        kind: ParserErrorKind::IllegalEmptyList,
                    })?;
                    match first {
                        Datum::Simple(simple) => {
                            let operator = parse_expr(first);
                            let operands = list.iter().skip(1);
                            match operator {
                                Ok(se) => Ok(ExprOrDef::Expr(Expr::ProcCall {
                                    operator: Box::new(se),
                                    operands: operands
                                        .map(parse_expr)
                                        .collect::<Result<Vec<_>, _>>()?,
                                })),
                                Err(_) => {
                                    if let SimpleDatum::Symbol(kw) = simple {
                                        process_keyword(kw, operands)
                                    } else {
                                        unreachable!("keyword should be a symbol")
                                    }
                                }
                            }
                        }
                        Datum::Compound(_) | Datum::Void => {
                            let operator = parse_expr(first)?;
                            let rest = list
                                .iter()
                                .skip(1)
                                .map(parse_expr)
                                .collect::<Result<Vec<_>, _>>()?;
                            Ok(ExprOrDef::Expr(Expr::ProcCall {
                                operator: Box::new(operator),
                                operands: rest,
                            }))
                        }
                        Datum::EmptyList => Err(ParserError {
                            kind: ParserErrorKind::IllegalEmptyList,
                        }),
                    }
                }
                ListKind::Improper(_, _) => Err(ParserError {
                    kind: ParserErrorKind::IllegalDot,
                }),
                ListKind::Abbreviation(abbr, arg) => {
                    let operands = std::iter::once(&**arg);
                    process_keyword(abbr.to_keyword(), operands)
                }
            },
            CompoundDatum::Vector(vector) => {
                let elements = vector
                    .iter()
                    .map(parse_expr)
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(ExprOrDef::Expr(Expr::Literal(LiteralKind::Vector(
                    elements,
                ))))
            }
        },
        Datum::EmptyList => Err(ParserError {
            kind: ParserErrorKind::IllegalEmptyList,
        }),
        Datum::Void => Ok(ExprOrDef::Expr(Expr::Literal(LiteralKind::Void))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::*;

    #[test]
    fn literals() {
        assert_eq!(
            parse(&bool_datum!(true)),
            Ok(ExprOrDef::Expr(bool_expr!(true)))
        );
        assert_eq!(parse(&int_datum!(42)), Ok(ExprOrDef::Expr(int_expr!(42))));
        assert_eq!(
            parse(&char_datum!('c')),
            Ok(ExprOrDef::Expr(char_expr!('c')))
        );
        assert_eq!(
            parse(&str_datum!("foo")),
            Ok(ExprOrDef::Expr(str_expr!("foo")))
        );
    }

    #[test]
    fn proc_calls() {
        assert_eq!(
            parse(&proper_list_datum![symbol_datum!("f")]),
            Ok(ExprOrDef::Expr(Expr::ProcCall {
                operator: Box::new(var_expr!("f")),
                operands: vec![],
            }))
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("g"),
                int_datum!(1),
                int_datum!(2),
            ]),
            Ok(ExprOrDef::Expr(Expr::ProcCall {
                operator: Box::new(var_expr!("g")),
                operands: vec![int_expr!(1), int_expr!(2)],
            }))
        );
    }

    #[test]
    fn illegal_lists() {
        assert_eq!(
            parse(&Datum::EmptyList),
            Err(ParserError {
                kind: ParserErrorKind::IllegalEmptyList,
            })
        );
        assert_eq!(
            parse(&improper_list_datum![int_datum!(1); int_datum!(2)]),
            Err(ParserError {
                kind: ParserErrorKind::IllegalDot,
            })
        );
    }

    #[test]
    fn definitions() {
        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("define"),
                symbol_datum!("x"),
                int_datum!(42),
            ]),
            Ok(ExprOrDef::Definition(Definition::Variable {
                name: "x".to_owned(),
                value: Box::new(int_expr!(42)),
            }))
        );

        assert_eq!(
            parse_expr(&proper_list_datum![
                symbol_datum!("define"),
                symbol_datum!("x"),
                int_datum!(42),
            ]),
            Err(ParserError {
                kind: ParserErrorKind::IllegalDefine,
            })
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("define"),
                proper_list_datum![symbol_datum!("f"), symbol_datum!("x")],
                proper_list_datum![symbol_datum!("define"), symbol_datum!("y"), int_datum!(1)],
                proper_list_datum![symbol_datum!("+"), symbol_datum!("x"), symbol_datum!("y")],
            ]),
            Ok(ExprOrDef::Definition(Definition::Procedure {
                name: "f".to_owned(),
                args: vec!["x".to_owned()],
                rest: None,
                body: Body {
                    defs: vec![Definition::Variable {
                        name: "y".to_owned(),
                        value: Box::new(int_expr!(1)),
                    }],
                    exprs: vec![Expr::ProcCall {
                        operator: Box::new(var_expr!("+")),
                        operands: vec![var_expr!("x"), var_expr!("y")],
                    }],
                }
            }))
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("define"),
                improper_list_datum![
                    symbol_datum!("f"),
                    symbol_datum!("x"),
                    symbol_datum!("y");
                    symbol_datum!("z")
                ],
                symbol_datum!("z"),
            ]),
            Ok(ExprOrDef::Definition(Definition::Procedure {
                name: "f".to_owned(),
                args: vec!["x".to_owned(), "y".to_owned()],
                rest: Some("z".to_owned()),
                body: Body {
                    defs: vec![],
                    exprs: vec![var_expr!("z")],
                }
            }))
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("define"),
                proper_list_datum![symbol_datum!("f"), int_datum!(1),],
                int_datum!(2),
            ]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("define".to_owned()),
            })
        );

        assert_eq!(
            parse(&proper_list_datum![symbol_datum!("define")]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("define".to_owned())
            })
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("define"),
                int_datum!(1),
                int_datum!(2),
            ]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("define".to_owned())
            })
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("define"),
                symbol_datum!("x"),
                int_datum!(1),
                int_datum!(2),
            ]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("define".to_owned())
            })
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("define"),
                symbol_datum!("x"),
                int_datum!(1),
                int_datum!(2),
            ]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("define".to_owned())
            })
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("define"),
                proper_list_datum![symbol_datum!("f"), symbol_datum!("x")],
            ]),
            Err(ParserError {
                kind: ParserErrorKind::MissingExpression
            })
        );

        assert_eq!(
            parse(&proper_list_datum![
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
            parse(&proper_list_datum![
                symbol_datum!("lambda"),
                proper_list_datum![symbol_datum!("x"), symbol_datum!("y")],
                proper_list_datum![symbol_datum!("+"), symbol_datum!("x"), symbol_datum!("y")],
            ]),
            Ok(ExprOrDef::Expr(Expr::Lambda {
                args: vec!["x".to_owned(), "y".to_owned()],
                rest: None,
                body: Body {
                    defs: vec![],
                    exprs: vec![Expr::ProcCall {
                        operator: Box::new(var_expr!("+")),
                        operands: vec![var_expr!("x"), var_expr!("y")],
                    }],
                }
            }))
        );

        assert_eq!(
            parse(&proper_list_datum![
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
            Ok(ExprOrDef::Expr(Expr::Lambda {
                args: vec!["x".to_owned(), "y".to_owned()],
                rest: Some("z".to_owned()),
                body: Body {
                    defs: vec![Definition::Variable {
                        name: "a".to_owned(),
                        value: Box::new(int_expr!(42)),
                    }],
                    exprs: vec![Expr::ProcCall {
                        operator: Box::new(var_expr!("+")),
                        operands: vec![
                            var_expr!("x"),
                            var_expr!("y"),
                            Expr::ProcCall {
                                operator: Box::new(var_expr!("car")),
                                operands: vec![var_expr!("z")],
                            },
                        ],
                    }],
                }
            }))
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("lambda"),
                symbol_datum!("args"),
                int_datum!(1)
            ]),
            Ok(ExprOrDef::Expr(Expr::Lambda {
                args: vec![],
                rest: Some("args".to_owned()),
                body: Body {
                    defs: vec![],
                    exprs: vec![int_expr!(1)],
                }
            }))
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("lambda"),
                Datum::EmptyList,
                int_datum!(1)
            ]),
            Ok(ExprOrDef::Expr(Expr::Lambda {
                args: vec![],
                rest: None,
                body: Body {
                    defs: vec![],
                    exprs: vec![int_expr!(1)],
                }
            }))
        );

        assert_eq!(
            parse(&proper_list_datum![symbol_datum!("lambda")]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("lambda".to_owned())
            })
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("lambda"),
                proper_list_datum![symbol_datum!("x")]
            ]),
            Err(ParserError {
                kind: ParserErrorKind::MissingExpression
            })
        );

        assert_eq!(
            parse(&proper_list_datum![
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
            parse(&proper_list_datum![
                symbol_datum!("quote"),
                proper_list_datum![symbol_datum!("a"), symbol_datum!("b")],
            ]),
            Ok(ExprOrDef::Expr(Expr::Literal(LiteralKind::Quotation(
                proper_list_datum![symbol_datum!("a"), symbol_datum!("b")],
            ))))
        );

        assert_eq!(
            parse(&proper_list_datum![symbol_datum!("quote")]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("quote".to_owned())
            })
        );

        assert_eq!(
            parse(&abbr_list_datum!(
                AbbreviationPrefix::Quote,
                proper_list_datum![symbol_datum!("a"), symbol_datum!("b")]
            )),
            Ok(ExprOrDef::Expr(Expr::Literal(LiteralKind::Quotation(
                proper_list_datum![symbol_datum!("a"), symbol_datum!("b")],
            ))))
        );
    }

    #[test]
    fn assignments() {
        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("set!"),
                symbol_datum!("x"),
                int_datum!(1),
            ]),
            Ok(ExprOrDef::Expr(Expr::Assignment {
                variable: "x".to_owned(),
                value: Box::new(int_expr!(1)),
            }))
        );

        assert_eq!(
            parse(&proper_list_datum![symbol_datum!("set!")]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("set!".to_owned())
            })
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("set!"),
                symbol_datum!("x"),
                int_datum!(1),
                int_datum!(2),
            ]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("set!".to_owned())
            })
        );
    }

    #[test]
    fn begins() {
        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("begin"),
                proper_list_datum![symbol_datum!("define"), symbol_datum!("x"), int_datum!(42)],
                str_datum!("hello"),
                int_datum!(1),
                int_datum!(2),
                int_datum!(3),
            ]),
            Ok(ExprOrDef::MixedBegin(vec![
                ExprOrDef::Definition(Definition::Variable {
                    name: "x".to_owned(),
                    value: Box::new(int_expr!(42))
                }),
                ExprOrDef::Expr(str_expr!("hello")),
                ExprOrDef::Expr(int_expr!(1)),
                ExprOrDef::Expr(int_expr!(2)),
                ExprOrDef::Expr(int_expr!(3)),
            ]))
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("begin"),
                proper_list_datum![symbol_datum!("define"), symbol_datum!("x"), int_datum!(42)],
                proper_list_datum![symbol_datum!("define"), symbol_datum!("y"), int_datum!(43)],
            ]),
            Ok(ExprOrDef::Definition(Definition::Begin(vec![
                Definition::Variable {
                    name: "x".to_owned(),
                    value: Box::new(int_expr!(42)),
                },
                Definition::Variable {
                    name: "y".to_owned(),
                    value: Box::new(int_expr!(43)),
                },
            ])))
        );

        assert_eq!(
            parse(&proper_list_datum![symbol_datum!("begin")]),
            Ok(ExprOrDef::Expr(Expr::DerivedExpr(DerivedExprKind::Begin(
                vec![]
            ))))
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("begin"),
                symbol_datum!("x"),
                symbol_datum!("y"),
                symbol_datum!("z"),
            ]),
            Ok(ExprOrDef::Expr(Expr::DerivedExpr(DerivedExprKind::Begin(
                vec![var_expr!("x"), var_expr!("y"), var_expr!("z"),]
            ))))
        )
    }

    #[test]
    fn conds() {
        assert_eq!(
            parse(&proper_list_datum![
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
            Ok(ExprOrDef::Expr(Expr::DerivedExpr(DerivedExprKind::Cond(
                vec![
                    CondClause::Normal(bool_expr!(false), vec![int_expr!(0)]),
                    CondClause::Normal(
                        Expr::ProcCall {
                            operator: Box::new(var_expr!("=")),
                            operands: vec![int_expr!(1), int_expr!(2)],
                        },
                        vec![int_expr!(3), int_expr!(4)],
                    ),
                    CondClause::Arrow(
                        Expr::ProcCall {
                            operator: Box::new(var_expr!("cons")),
                            operands: vec![int_expr!(5), int_expr!(6)],
                        },
                        var_expr!("car"),
                    ),
                    CondClause::Else(vec![int_expr!(3)]),
                ]
            ))))
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("cond"),
                proper_list_datum![symbol_datum!("else"), int_datum!(0)],
                proper_list_datum![bool_datum!(true), int_datum!(1)],
            ]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("cond".to_owned())
            })
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("cond"),
                proper_list_datum![symbol_datum!("else"), int_datum!(0)],
                proper_list_datum![symbol_datum!("else"), int_datum!(1)],
            ]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("cond".to_owned())
            })
        );
    }

    #[test]
    fn ands_ors() {
        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("and"),
                bool_datum!(true),
                bool_datum!(false),
            ]),
            Ok(ExprOrDef::Expr(Expr::DerivedExpr(DerivedExprKind::And(
                vec![bool_expr!(true), bool_expr!(false),]
            ))))
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("or"),
                bool_datum!(true),
                bool_datum!(false),
            ]),
            Ok(ExprOrDef::Expr(Expr::DerivedExpr(DerivedExprKind::Or(
                vec![bool_expr!(true), bool_expr!(false),]
            ))))
        );

        assert_eq!(
            parse(&proper_list_datum![symbol_datum!("and")]),
            Ok(ExprOrDef::Expr(Expr::DerivedExpr(DerivedExprKind::And(
                vec![]
            ))))
        );

        assert_eq!(
            parse(&proper_list_datum![symbol_datum!("or")]),
            Ok(ExprOrDef::Expr(Expr::DerivedExpr(DerivedExprKind::Or(
                vec![]
            ))))
        );
    }

    #[test]
    fn cases() {
        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("case"),
                int_datum!(42),
                proper_list_datum![proper_list_datum![int_datum!(1)], int_datum!(2)],
                proper_list_datum![
                    proper_list_datum![int_datum!(3), int_datum!(4)],
                    int_datum!(5),
                ],
                proper_list_datum![symbol_datum!("else"), int_datum!(6)],
            ]),
            Ok(ExprOrDef::Expr(Expr::DerivedExpr(DerivedExprKind::Case {
                key: Box::new(int_expr!(42)),
                clauses: vec![
                    CaseClause::Normal(vec![int_datum!(1)], vec![int_expr!(2)]),
                    CaseClause::Normal(vec![int_datum!(3), int_datum!(4)], vec![int_expr!(5)]),
                    CaseClause::Else(vec![int_expr!(6)]),
                ],
            })))
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("case"),
                int_datum!(42),
                proper_list_datum![symbol_datum!("else"), int_datum!(0)],
                proper_list_datum![int_datum!(1), int_datum!(2)],
            ]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("case".to_owned())
            })
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("case"),
                int_datum!(42),
                proper_list_datum![symbol_datum!("else"), int_datum!(0)],
                proper_list_datum![symbol_datum!("else"), int_datum!(1)],
            ]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("case".to_owned())
            })
        );
    }

    #[test]
    fn lets() {
        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("let"),
                Datum::EmptyList,
                int_datum!(0)
            ]),
            Ok(ExprOrDef::Expr(Expr::DerivedExpr(DerivedExprKind::Let {
                kind: LetKind::Normal,
                bindings: vec![],
                body: Body {
                    defs: vec![],
                    exprs: vec![int_expr!(0)],
                },
            })))
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("let"),
                proper_list_datum![proper_list_datum![symbol_datum!("x"), int_datum!(1)]],
                proper_list_datum![symbol_datum!("define"), symbol_datum!("y"), int_datum!(2)],
                proper_list_datum![symbol_datum!("+"), symbol_datum!("x"), symbol_datum!("y")],
            ]),
            Ok(ExprOrDef::Expr(Expr::DerivedExpr(DerivedExprKind::Let {
                kind: LetKind::Normal,
                bindings: vec![("x".to_owned(), int_expr!(1))],
                body: Body {
                    defs: vec![Definition::Variable {
                        name: "y".to_owned(),
                        value: Box::new(int_expr!(2)),
                    }],
                    exprs: vec![Expr::ProcCall {
                        operator: Box::new(var_expr!("+")),
                        operands: vec![var_expr!("x"), var_expr!("y")],
                    }],
                }
            })))
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("let*"),
                proper_list_datum![proper_list_datum![symbol_datum!("x"), int_datum!(1)]],
                symbol_datum!("x"),
            ]),
            Ok(ExprOrDef::Expr(Expr::DerivedExpr(DerivedExprKind::Let {
                kind: LetKind::Star,
                bindings: vec![("x".to_owned(), int_expr!(1))],
                body: Body {
                    defs: vec![],
                    exprs: vec![var_expr!("x")],
                }
            })))
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("letrec"),
                proper_list_datum![proper_list_datum![symbol_datum!("x"), int_datum!(1)]],
                symbol_datum!("x"),
            ]),
            Ok(ExprOrDef::Expr(Expr::DerivedExpr(DerivedExprKind::Let {
                kind: LetKind::Rec,
                bindings: vec![("x".to_owned(), int_expr!(1))],
                body: Body {
                    defs: vec![],
                    exprs: vec![var_expr!("x")],
                }
            })))
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("let"),
                symbol_datum!("foo"),
                proper_list_datum![proper_list_datum![symbol_datum!("x"), int_datum!(1)]],
                proper_list_datum![symbol_datum!("foo"), symbol_datum!("x")],
            ]),
            Ok(ExprOrDef::Expr(Expr::DerivedExpr(DerivedExprKind::Let {
                kind: LetKind::Named("foo".to_owned()),
                bindings: vec![("x".to_owned(), int_expr!(1))],
                body: Body {
                    defs: vec![],
                    exprs: vec![Expr::ProcCall {
                        operator: Box::new(var_expr!("foo")),
                        operands: vec![var_expr!("x")],
                    }],
                }
            })))
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("let*"),
                symbol_datum!("foo"),
                proper_list_datum![proper_list_datum![symbol_datum!("x"), int_datum!(1)]],
                proper_list_datum![symbol_datum!("foo"), symbol_datum!("x")],
            ]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("let*".to_owned())
            })
        );

        assert_eq!(
            parse(&proper_list_datum![symbol_datum!("let")]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("let".to_owned())
            })
        );

        assert_eq!(
            parse(&proper_list_datum![symbol_datum!("let"), int_datum!(1)]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("let".to_owned())
            })
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("let"),
                proper_list_datum![symbol_datum!("x"), int_datum!(1)],
                symbol_datum!("x")
            ]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("let".to_owned())
            })
        );
    }

    #[test]
    fn dos() {
        assert_eq!(
            parse(&proper_list_datum![
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
            Ok(ExprOrDef::Expr(Expr::DerivedExpr(DerivedExprKind::Do {
                specs: vec![
                    IterationSpec {
                        variable: "x".to_owned(),
                        init: int_expr!(1),
                        step: Some(Expr::ProcCall {
                            operator: Box::new(var_expr!("+")),
                            operands: vec![int_expr!(1), var_expr!("x")],
                        }),
                    },
                    IterationSpec {
                        variable: "y".to_owned(),
                        init: int_expr!(5),
                        step: None,
                    },
                ],
                term: (
                    Box::new(Expr::ProcCall {
                        operator: Box::new(var_expr!("=")),
                        operands: vec![var_expr!("x"), var_expr!("y")],
                    }),
                    vec![Expr::ProcCall {
                        operator: Box::new(var_expr!("+")),
                        operands: vec![var_expr!("x"), var_expr!("y")],
                    }],
                ),
                body: vec![]
            })))
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("do"),
                Datum::EmptyList,
                proper_list_datum![bool_datum!(false)],
                proper_list_datum![symbol_datum!("f")],
            ]),
            Ok(ExprOrDef::Expr(Expr::DerivedExpr(DerivedExprKind::Do {
                specs: vec![],
                term: (Box::new(bool_expr!(false)), vec![],),
                body: vec![Expr::ProcCall {
                    operator: Box::new(var_expr!("f")),
                    operands: vec![],
                }],
            })))
        );
    }

    #[test]
    fn delays() {
        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("delay"),
                proper_list_datum![symbol_datum!("f"), int_datum!(1)]
            ]),
            Ok(ExprOrDef::Expr(Expr::DerivedExpr(DerivedExprKind::Delay(
                Box::new(Expr::ProcCall {
                    operator: Box::new(var_expr!("f")),
                    operands: vec![int_expr!(1)],
                })
            ))))
        );

        assert_eq!(
            parse(&proper_list_datum![symbol_datum!("delay")]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("delay".to_owned())
            })
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("delay"),
                int_datum!(1),
                int_datum!(2)
            ]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("delay".to_owned())
            })
        );
    }

    #[test]
    fn quasiquotes() {
        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("quasiquote"),
                int_datum!(1),
            ]),
            Ok(ExprOrDef::Expr(Expr::Quasiquotation(QQTemplate::LevelD(
                1,
                Box::new(QQTemplateData::Datum(int_datum!(1)))
            )))),
        );

        let quoted_p12 = || {
            QQTemplateData::List(ListQQTemplate::Proper(vec![
                QQTemplateOrSplice::Template(QQTemplate::LevelD(
                    1,
                    Box::new(QQTemplateData::Datum(symbol_datum!("+"))),
                )),
                QQTemplateOrSplice::Template(QQTemplate::LevelD(
                    1,
                    Box::new(QQTemplateData::Datum(int_datum!(1))),
                )),
                QQTemplateOrSplice::Template(QQTemplate::LevelD(
                    1,
                    Box::new(QQTemplateData::Datum(int_datum!(2))),
                )),
            ]))
        };
        assert_eq!(
            parse(&abbr_list_datum!(
                AbbreviationPrefix::Quasiquote,
                proper_list_datum![symbol_datum!("+"), int_datum!(1), int_datum!(2)],
            )),
            Ok(ExprOrDef::Expr(Expr::Quasiquotation(QQTemplate::LevelD(
                1,
                Box::new(quoted_p12()),
            )))),
        );

        let with_abbrs = parse(&abbr_list_datum!(
            AbbreviationPrefix::Quasiquote,
            proper_list_datum![
                symbol_datum!("a"),
                abbr_list_datum!(
                    AbbreviationPrefix::Unquote,
                    proper_list_datum![symbol_datum!("+"), int_datum!(1), int_datum!(2)]
                ),
                abbr_list_datum!(
                    AbbreviationPrefix::UnquoteSplicing,
                    proper_list_datum![
                        symbol_datum!("map"),
                        symbol_datum!("abs"),
                        abbr_list_datum!(
                            AbbreviationPrefix::Quote,
                            proper_list_datum![int_datum!(4), int_datum!(-5), int_datum!(6)]
                        ),
                    ]
                ),
                symbol_datum!("b"),
            ],
        ));
        let with_kws = parse(&proper_list_datum![
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
        ]);
        assert_eq!(with_abbrs, with_kws);
        assert_eq!(
            with_abbrs,
            Ok(ExprOrDef::Expr(Expr::Quasiquotation(QQTemplate::LevelD(
                1,
                Box::new(QQTemplateData::List(ListQQTemplate::Proper(vec![
                    QQTemplateOrSplice::Template(QQTemplate::LevelD(
                        1,
                        Box::new(QQTemplateData::Datum(symbol_datum!("a")))
                    )),
                    QQTemplateOrSplice::Template(QQTemplate::LevelD(
                        1,
                        Box::new(QQTemplateData::Unquotation(QQTemplate::Level0(Box::new(
                            Expr::ProcCall {
                                operator: Box::new(var_expr!("+")),
                                operands: vec![int_expr!(1), int_expr!(2)],
                            }
                        ))))
                    )),
                    QQTemplateOrSplice::Splice(QQTemplate::Level0(Box::new(Expr::ProcCall {
                        operator: Box::new(var_expr!("map")),
                        operands: vec![
                            var_expr!("abs"),
                            Expr::Literal(LiteralKind::Quotation(proper_list_datum![
                                int_datum!(4),
                                int_datum!(-5),
                                int_datum!(6)
                            ],))
                        ],
                    }))),
                    QQTemplateOrSplice::Template(QQTemplate::LevelD(
                        1,
                        Box::new(QQTemplateData::Datum(symbol_datum!("b")))
                    )),
                ]))),
            ))))
        );

        assert_eq!(
            parse(&abbr_list_datum!(
                AbbreviationPrefix::Quasiquote,
                vector_datum![
                    int_datum!(1),
                    abbr_list_datum!(
                        AbbreviationPrefix::Unquote,
                        proper_list_datum![symbol_datum!("sqrt"), int_datum!(4)]
                    ),
                    int_datum!(3),
                    int_datum!(4)
                ],
            )),
            Ok(ExprOrDef::Expr(Expr::Quasiquotation(QQTemplate::LevelD(
                1,
                Box::new(QQTemplateData::Vector(vec![
                    QQTemplateOrSplice::Template(QQTemplate::LevelD(
                        1,
                        Box::new(QQTemplateData::Datum(int_datum!(1)))
                    )),
                    QQTemplateOrSplice::Template(QQTemplate::LevelD(
                        1,
                        Box::new(QQTemplateData::Unquotation(QQTemplate::Level0(Box::new(
                            Expr::ProcCall {
                                operator: Box::new(var_expr!("sqrt")),
                                operands: vec![int_expr!(4)],
                            }
                        ))))
                    )),
                    QQTemplateOrSplice::Template(QQTemplate::LevelD(
                        1,
                        Box::new(QQTemplateData::Datum(int_datum!(3)))
                    )),
                    QQTemplateOrSplice::Template(QQTemplate::LevelD(
                        1,
                        Box::new(QQTemplateData::Datum(int_datum!(4)))
                    )),
                ])),
            )))),
        );

        assert_eq!(
            parse(&abbr_list_datum!(
                AbbreviationPrefix::Quasiquote,
                proper_list_datum![
                    symbol_datum!("a"),
                    abbr_list_datum!(
                        AbbreviationPrefix::Quasiquote,
                        proper_list_datum![
                            symbol_datum!("b"),
                            abbr_list_datum!(
                                AbbreviationPrefix::Unquote,
                                proper_list_datum![
                                    symbol_datum!("+"),
                                    int_datum!(1),
                                    int_datum!(2)
                                ]
                            ),
                            abbr_list_datum!(
                                AbbreviationPrefix::Unquote,
                                proper_list_datum![
                                    symbol_datum!("foo"),
                                    abbr_list_datum!(
                                        AbbreviationPrefix::Unquote,
                                        proper_list_datum![
                                            symbol_datum!("+"),
                                            int_datum!(1),
                                            int_datum!(3)
                                        ]
                                    ),
                                    symbol_datum!("d")
                                ]
                            ),
                            symbol_datum!("e")
                        ]
                    ),
                    symbol_datum!("f")
                ]
            )),
            Ok(ExprOrDef::Expr(Expr::Quasiquotation(QQTemplate::LevelD(
                1,
                Box::new(QQTemplateData::List(ListQQTemplate::Proper(vec![
                    QQTemplateOrSplice::Template(QQTemplate::LevelD(
                        1,
                        Box::new(QQTemplateData::Datum(symbol_datum!("a")))
                    )),
                    QQTemplateOrSplice::Template(QQTemplate::LevelD(
                        1,
                        Box::new(QQTemplateData::List(ListQQTemplate::QQ(
                            QQTemplate::LevelD(
                                2,
                                Box::new(QQTemplateData::List(ListQQTemplate::Proper(vec![
                                    QQTemplateOrSplice::Template(QQTemplate::LevelD(
                                        2,
                                        Box::new(QQTemplateData::Datum(symbol_datum!("b")))
                                    )),
                                    QQTemplateOrSplice::Template(QQTemplate::LevelD(
                                        2,
                                        Box::new(QQTemplateData::Unquotation(QQTemplate::LevelD(
                                            1,
                                            Box::new(quoted_p12())
                                        ))),
                                    )),
                                    QQTemplateOrSplice::Template(QQTemplate::LevelD(
                                        2,
                                        Box::new(QQTemplateData::Unquotation(QQTemplate::LevelD(
                                            1,
                                            Box::new(QQTemplateData::List(ListQQTemplate::Proper(
                                                vec![
                                                    QQTemplateOrSplice::Template(
                                                        QQTemplate::LevelD(
                                                            1,
                                                            Box::new(QQTemplateData::Datum(
                                                                symbol_datum!("foo")
                                                            ))
                                                        )
                                                    ),
                                                    QQTemplateOrSplice::Template(
                                                        QQTemplate::LevelD(
                                                            1,
                                                            Box::new(QQTemplateData::Unquotation(
                                                                QQTemplate::Level0(Box::new(
                                                                    Expr::ProcCall {
                                                                        operator: Box::new(
                                                                            var_expr!("+")
                                                                        ),
                                                                        operands: vec![
                                                                            int_expr!(1),
                                                                            int_expr!(3)
                                                                        ],
                                                                    }
                                                                ))
                                                            ))
                                                        )
                                                    ),
                                                    QQTemplateOrSplice::Template(
                                                        QQTemplate::LevelD(
                                                            1,
                                                            Box::new(QQTemplateData::Datum(
                                                                symbol_datum!("d")
                                                            ))
                                                        )
                                                    ),
                                                ]
                                            )))
                                        )))
                                    )),
                                    QQTemplateOrSplice::Template(QQTemplate::LevelD(
                                        2,
                                        Box::new(QQTemplateData::Datum(symbol_datum!("e")))
                                    ))
                                ])))
                            )
                        )))
                    )),
                    QQTemplateOrSplice::Template(QQTemplate::LevelD(
                        1,
                        Box::new(QQTemplateData::Datum(symbol_datum!("f")))
                    ))
                ])))
            ))))
        );

        assert_eq!(
            parse(&abbr_list_datum!(
                AbbreviationPrefix::Quasiquote,
                improper_list_datum![int_datum!(1), int_datum!(2); int_datum!(3)]
            )),
            Ok(ExprOrDef::Expr(Expr::Quasiquotation(QQTemplate::LevelD(
                1,
                Box::new(QQTemplateData::List(ListQQTemplate::Improper(
                    vec![
                        QQTemplateOrSplice::Template(QQTemplate::LevelD(
                            1,
                            Box::new(QQTemplateData::Datum(int_datum!(1)))
                        )),
                        QQTemplateOrSplice::Template(QQTemplate::LevelD(
                            1,
                            Box::new(QQTemplateData::Datum(int_datum!(2)))
                        )),
                    ],
                    QQTemplate::LevelD(1, Box::new(QQTemplateData::Datum(int_datum!(3))))
                )))
            ))))
        );
    }

    #[test]
    fn unquote_errs() {
        assert_eq!(
            parse(&proper_list_datum![symbol_datum!("unquote")]),
            Err(ParserError {
                kind: ParserErrorKind::IllegalUnquote
            })
        );

        assert_eq!(
            parse(&proper_list_datum![symbol_datum!("unquote-splicing")]),
            Err(ParserError {
                kind: ParserErrorKind::IllegalUnquoteSplicing
            })
        );

        assert_eq!(
            parse(&proper_list_datum![symbol_datum!("unquote"), int_datum!(1)]),
            Err(ParserError {
                kind: ParserErrorKind::IllegalUnquote
            })
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("unquote-splicing"),
                int_datum!(1),
            ]),
            Err(ParserError {
                kind: ParserErrorKind::IllegalUnquoteSplicing
            })
        );

        assert_eq!(
            parse(&abbr_list_datum!(
                AbbreviationPrefix::Unquote,
                int_datum!(1),
            )),
            Err(ParserError {
                kind: ParserErrorKind::IllegalUnquote
            })
        );

        assert_eq!(
            parse(&abbr_list_datum!(
                AbbreviationPrefix::UnquoteSplicing,
                int_datum!(1),
            )),
            Err(ParserError {
                kind: ParserErrorKind::IllegalUnquoteSplicing
            })
        );
    }
}
