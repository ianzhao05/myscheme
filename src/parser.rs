use std::error::Error;
use std::fmt;

use crate::datum::*;
use crate::expr::*;

use uuid::Uuid;

fn gen_temp_name() -> String {
    format!("__temp_{}", Uuid::new_v4().simple())
}

#[derive(Debug, PartialEq)]
pub enum ParserErrorKind {
    BadSyntax(String),
    IllegalEmptyList,
    IllegalVector,
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
            ParserErrorKind::IllegalVector => write!(f, "Vector must be quoted"),
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

fn process_proc<I: Iterator<Item = Datum>>(
    list: ListKind,
    body: I,
    named: bool,
) -> Result<(Option<String>, ProcData), ParserError> {
    let bs_err = || ParserError {
        kind: ParserErrorKind::BadSyntax((if named { "define" } else { "lambda" }).to_owned()),
    };
    let process_forms = |forms: Vec<Datum>| -> Result<(Option<String>, Vec<String>), ParserError> {
        let mut name = None;
        let mut fi = forms.into_iter();
        if named {
            let first = fi.next().ok_or(ParserError {
                kind: ParserErrorKind::IllegalEmptyList,
            })?;
            name = match first {
                Datum::Simple(SimpleDatum::Symbol(s)) => Some(s),
                _ => return Err(bs_err()),
            };
        }
        Ok((
            name,
            fi.into_iter()
                .map(|d| match d {
                    Datum::Simple(SimpleDatum::Symbol(s)) => {
                        if is_keyword(&s) {
                            Err(ParserError {
                                kind: ParserErrorKind::IllegalVariableName(s),
                            })
                        } else {
                            Ok(s)
                        }
                    }
                    _ => Err(bs_err()),
                })
                .collect::<Result<Vec<_>, _>>()?,
        ))
    };
    match list {
        ListKind::Proper(forms) => {
            let (name, args) = process_forms(forms)?;
            Ok((
                name,
                ProcData {
                    args,
                    rest: None,
                    body: process_body(body)?,
                },
            ))
        }
        ListKind::Improper(forms, rest) => {
            let (name, args) = process_forms(forms)?;
            Ok((
                name,
                ProcData {
                    args,
                    rest: match *rest {
                        Datum::Simple(SimpleDatum::Symbol(s)) => {
                            if is_keyword(&s) {
                                return Err(ParserError {
                                    kind: ParserErrorKind::IllegalVariableName(s),
                                });
                            }
                            Some(s)
                        }
                        _ => {
                            return Err(bs_err());
                        }
                    },
                    body: process_body(body)?,
                },
            ))
        }
        _ => Err(bs_err()),
    }
}

fn process_define<I: Iterator<Item = Datum>>(
    var: Datum,
    mut body: I,
) -> Result<Definition, ParserError> {
    let bs_err = || ParserError {
        kind: ParserErrorKind::BadSyntax("define".to_owned()),
    };
    match var {
        Datum::Simple(SimpleDatum::Symbol(s)) => {
            if is_keyword(&s) {
                Err(ParserError {
                    kind: ParserErrorKind::IllegalVariableName(s),
                })
            } else {
                let expr = parse_expr(body.next().ok_or_else(bs_err)?)?;

                if body.next().is_none() {
                    Ok(Definition::Variable {
                        name: s,
                        value: Box::new(expr),
                    })
                } else {
                    Err(bs_err())
                }
            }
        }
        Datum::Compound(CompoundDatum::List(list)) => {
            let (name, proc_data) = process_proc(list, body, true)?;
            Ok(Definition::Procedure {
                name: name.expect("Name should be present"),
                data: proc_data,
            })
        }
        _ => Err(bs_err()),
    }
}

fn process_body<I: Iterator<Item = Datum>>(data: I) -> Result<Body, ParserError> {
    let mut eods = Vec::new();
    let mut last_is_expr = false;
    for d in data {
        let eod = parse(d)?;
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
        Ok(Body(eods))
    } else {
        Err(ParserError {
            kind: ParserErrorKind::MissingExpression,
        })
    }
}

fn process_qq_template_or_splice(
    datum: Datum,
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
                *arg,
                qq_level - 1,
            )?)),
            Datum::Compound(CompoundDatum::List(ListKind::Proper(list))) => {
                return match list.first() {
                    Some(Datum::Simple(SimpleDatum::Symbol(s))) if s == "unquote-splicing" => {
                        Ok(QQTemplateOrSplice::Splice(process_qq_template(
                            list.into_iter().nth(1).ok_or(ParserError {
                                kind: ParserErrorKind::IllegalUnquoteSplicing,
                            })?,
                            qq_level - 1,
                        )?))
                    }
                    _ => Ok(QQTemplateOrSplice::Template(process_qq_template(
                        Datum::Compound(CompoundDatum::List(ListKind::Proper(list))),
                        qq_level,
                    )?)),
                };
            }
            _ => Ok(QQTemplateOrSplice::Template(process_qq_template(
                datum, qq_level,
            )?)),
        }
    }
}

fn process_qq_template(datum: Datum, qq_level: usize) -> Result<QQTemplate, ParserError> {
    if qq_level == 0 {
        return Ok(QQTemplate::Level0(Box::new(parse_expr(datum)?)));
    }
    Ok(QQTemplate::LevelD(
        qq_level,
        match datum {
            Datum::Simple(_) | Datum::EmptyList => Box::new(QQTemplateData::Datum(datum.clone())),
            Datum::Compound(CompoundDatum::List(list)) => match list {
                ListKind::Proper(list) => {
                    let mut li = list.into_iter();
                    let first = li.next().ok_or(ParserError {
                        kind: ParserErrorKind::IllegalEmptyList,
                    })?;
                    match first {
                        Datum::Simple(SimpleDatum::Symbol(ref s))
                            if s == "unquote" || s == "quasiquote" =>
                        {
                            let arg = li.next().ok_or_else(|| ParserError {
                                kind: ParserErrorKind::BadSyntax(s.to_owned()),
                            })?;
                            if li.next().is_some() {
                                return Err(ParserError {
                                    kind: ParserErrorKind::BadSyntax(s.to_owned()),
                                });
                            }
                            if s == "unquote" {
                                Box::new(QQTemplateData::Unquotation(process_qq_template(
                                    arg,
                                    qq_level - 1,
                                )?))
                            } else {
                                Box::new(QQTemplateData::List(ListQQTemplate::QQ(
                                    process_qq_template(arg, qq_level + 1)?,
                                )))
                            }
                        }
                        _ => Box::new(QQTemplateData::List(ListQQTemplate::Proper(
                            std::iter::once(first)
                                .chain(li)
                                .map(|d| process_qq_template_or_splice(d, qq_level))
                                .collect::<Result<Vec<_>, _>>()?,
                        ))),
                    }
                }
                ListKind::Improper(list, last) => {
                    Box::new(QQTemplateData::List(ListQQTemplate::Improper(
                        list.into_iter()
                            .map(|d| process_qq_template_or_splice(d, qq_level))
                            .collect::<Result<Vec<_>, _>>()?,
                        process_qq_template(*last, qq_level)?,
                    )))
                }
                ListKind::Abbreviation(abbr, arg) => match abbr {
                    AbbreviationPrefix::Unquote => Box::new(QQTemplateData::Unquotation(
                        process_qq_template(*arg, qq_level - 1)?,
                    )),
                    AbbreviationPrefix::Quote => Box::new(QQTemplateData::List(
                        ListQQTemplate::Quote(process_qq_template(*arg, qq_level)?),
                    )),
                    AbbreviationPrefix::Quasiquote => Box::new(QQTemplateData::List(
                        ListQQTemplate::QQ(process_qq_template(*arg, qq_level + 1)?),
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
                    .into_iter()
                    .map(|d| process_qq_template_or_splice(d, qq_level))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
        },
    ))
}

fn process_keyword<I: DoubleEndedIterator<Item = Datum>>(
    kw: &str,
    mut operands: I,
) -> Result<ExprOrDef, ParserError> {
    let bs_err = || ParserError {
        kind: ParserErrorKind::BadSyntax(kw.to_owned()),
    };
    let process_bindings = |binding_data: Vec<Datum>| -> Result<Vec<(String, Expr)>, ParserError> {
        Ok(binding_data
            .into_iter()
            .map(|binding| {
                if let Datum::Compound(CompoundDatum::List(ListKind::Proper(parts))) = binding {
                    if parts.len() != 2 {
                        return Err(bs_err());
                    }
                    let mut pi = parts.into_iter();
                    if let Datum::Simple(SimpleDatum::Symbol(name)) = pi.next().unwrap() {
                        Ok((name, parse_expr(pi.next().unwrap())?))
                    } else {
                        Err(bs_err())
                    }
                } else {
                    Err(bs_err())
                }
            })
            .collect::<Result<Vec<_>, _>>()?)
    };
    match kw {
        "define" => {
            let var = operands.next().ok_or_else(bs_err)?;
            Ok(ExprOrDef::Definition(process_define(var, operands)?))
        }
        "quote" => {
            let operand = operands.next().ok_or_else(bs_err)?;
            if operands.next().is_none() {
                Ok(ExprOrDef::Expr(Expr::Literal(LiteralKind::Quotation(
                    operand,
                ))))
            } else {
                Err(bs_err())
            }
        }
        "lambda" => {
            let formals = operands.next().ok_or_else(bs_err)?;
            match formals {
                Datum::Compound(CompoundDatum::List(list)) => {
                    let (_, args) = process_proc(list, operands, false)?;
                    Ok(ExprOrDef::Expr(Expr::Lambda(args)))
                }
                Datum::Simple(SimpleDatum::Symbol(rest)) => {
                    if is_keyword(&rest) {
                        return Err(ParserError {
                            kind: ParserErrorKind::IllegalVariableName(rest),
                        });
                    }
                    Ok(ExprOrDef::Expr(Expr::Lambda(ProcData {
                        args: vec![],
                        rest: Some(rest),
                        body: process_body(operands)?,
                    })))
                }
                Datum::EmptyList => Ok(ExprOrDef::Expr(Expr::Lambda(ProcData {
                    args: vec![],
                    rest: None,
                    body: process_body(operands)?,
                }))),
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
                Ok(ExprOrDef::Expr(Expr::Begin(
                    children
                        .into_iter()
                        .map(|eod| match eod {
                            ExprOrDef::Expr(e) => e,
                            _ => unreachable!("should not encounter non-definition"),
                        })
                        .collect(),
                )))
            }
        }
        "set!" => {
            let variable = operands.next().ok_or_else(bs_err)?;
            let value = operands.next().ok_or_else(bs_err)?;
            if operands.next().is_none() {
                Ok(ExprOrDef::Expr(Expr::Assignment {
                    variable: match variable {
                        Datum::Simple(SimpleDatum::Symbol(s)) => s,
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
            let mut acc: Option<Expr> = None;
            for clause in operands.rev() {
                if let Datum::Compound(CompoundDatum::List(ListKind::Proper(parts))) = clause {
                    let mut pi = parts.into_iter();
                    let first = pi.next().ok_or(ParserError {
                        kind: ParserErrorKind::IllegalEmptyList,
                    })?;
                    match first {
                        Datum::Simple(SimpleDatum::Symbol(s)) if s == "else" => {
                            if let Some(_) = acc {
                                return Err(bs_err());
                            }
                            let seq = pi.map(parse_expr).collect::<Result<Vec<_>, _>>()?;
                            if seq.is_empty() {
                                return Err(bs_err());
                            }
                            acc = Some(if seq.len() == 1 {
                                seq.into_iter().next().unwrap()
                            } else {
                                Expr::Begin(seq)
                            });
                        }
                        _ => {
                            let test = parse_expr(first)?;
                            let second = pi.next();
                            if second.is_none() {
                                match acc {
                                    Some(a) => {
                                        acc = Some(Expr::Conditional {
                                            test: Box::new(test),
                                            consequent: Box::new(a),
                                            alternate: None,
                                        })
                                    }
                                    None => {
                                        acc = Some(test);
                                    }
                                }
                                continue;
                            }
                            match second.unwrap() {
                                Datum::Simple(SimpleDatum::Symbol(ref s)) if s == "=>" => {
                                    let third = pi.next().ok_or_else(bs_err)?;
                                    if pi.next().is_some() {
                                        return Err(bs_err());
                                    }
                                    let recipient = parse_expr(third)?;
                                    let temp = gen_temp_name();
                                    acc = Some(Expr::SimpleLet {
                                        arg: temp.clone(),
                                        value: Box::new(test),
                                        body: Box::new(Expr::Conditional {
                                            test: Box::new(Expr::Variable(temp.clone())),
                                            consequent: Box::new(Expr::ProcCall {
                                                operator: Box::new(recipient),
                                                operands: vec![Expr::Variable(temp)],
                                            }),
                                            alternate: match acc {
                                                Some(a) => Some(Box::new(a)),
                                                None => None,
                                            },
                                        }),
                                    })
                                }
                                second => {
                                    let seq = std::iter::once(second)
                                        .chain(pi)
                                        .map(parse_expr)
                                        .collect::<Result<Vec<_>, _>>()?;
                                    if seq.is_empty() {
                                        return Err(bs_err());
                                    }
                                    acc = Some(Expr::Conditional {
                                        test: Box::new(test),
                                        consequent: Box::new(if seq.len() == 1 {
                                            seq.into_iter().next().unwrap()
                                        } else {
                                            Expr::Begin(seq)
                                        }),
                                        alternate: match acc {
                                            Some(a) => Some(Box::new(a)),
                                            None => None,
                                        },
                                    })
                                }
                            }
                        }
                    }
                } else {
                    return Err(bs_err());
                }
            }
            match acc {
                Some(a) => Ok(ExprOrDef::Expr(a)),
                None => Err(bs_err()),
            }
        }
        "else" => Err(bs_err()),
        "and" => {
            let ops = operands
                .rev()
                .map(parse_expr)
                .collect::<Result<Vec<_>, _>>()?;
            Ok(ExprOrDef::Expr(match ops.len() {
                0 => Expr::Literal(LiteralKind::SelfEvaluating(SelfEvaluatingKind::Boolean(
                    true,
                ))),
                _ => {
                    let mut oi = ops.into_iter();
                    let mut acc = oi.next().unwrap();
                    for op in oi {
                        acc = Expr::Conditional {
                            test: Box::new(op),
                            consequent: Box::new(acc),
                            alternate: Some(Box::new(Expr::Literal(LiteralKind::SelfEvaluating(
                                SelfEvaluatingKind::Boolean(false),
                            )))),
                        };
                    }
                    acc
                }
            }))
        }
        "or" => {
            let ops = operands
                .rev()
                .map(parse_expr)
                .collect::<Result<Vec<_>, _>>()?;
            Ok(ExprOrDef::Expr(match ops.len() {
                0 => Expr::Literal(LiteralKind::SelfEvaluating(SelfEvaluatingKind::Boolean(
                    false,
                ))),
                _ => {
                    let mut oi = ops.into_iter();
                    let mut acc = oi.next().unwrap();
                    for op in oi {
                        let temp = gen_temp_name();
                        acc = Expr::SimpleLet {
                            arg: temp.clone(),
                            value: Box::new(op),
                            body: Box::new(Expr::Conditional {
                                test: Box::new(Expr::Variable(temp.clone())),
                                consequent: Box::new(Expr::Variable(temp)),
                                alternate: Some(Box::new(acc)),
                            }),
                        };
                    }
                    acc
                }
            }))
        }
        "case" => {
            let key = parse_expr(operands.next().ok_or_else(bs_err)?)?;
            let temp = gen_temp_name();
            let mut acc: Option<Expr> = None;
            for clause in operands.rev() {
                if let Datum::Compound(CompoundDatum::List(ListKind::Proper(parts))) = clause {
                    let mut pi = parts.into_iter();
                    let first = pi.next().ok_or(ParserError {
                        kind: ParserErrorKind::IllegalEmptyList,
                    })?;
                    let seq = pi.map(parse_expr).collect::<Result<Vec<_>, _>>()?;
                    if seq.is_empty() {
                        return Err(bs_err());
                    }
                    match first {
                        Datum::Simple(SimpleDatum::Symbol(s)) if s == "else" => {
                            if let Some(_) = acc {
                                return Err(bs_err());
                            }
                            acc = Some(if seq.len() == 1 {
                                seq.into_iter().next().unwrap()
                            } else {
                                Expr::Begin(seq)
                            });
                        }
                        Datum::Compound(CompoundDatum::List(ListKind::Proper(_))) => {
                            acc = Some(Expr::Conditional {
                                test: Box::new(Expr::ProcCall {
                                    operator: Box::new(Expr::Variable("memv".to_owned())),
                                    operands: vec![
                                        Expr::Variable(temp.clone()),
                                        Expr::Literal(LiteralKind::Quotation(first)),
                                    ],
                                }),
                                consequent: Box::new(if seq.len() == 1 {
                                    seq.into_iter().next().unwrap()
                                } else {
                                    Expr::Begin(seq)
                                }),
                                alternate: match acc {
                                    Some(a) => Some(Box::new(a)),
                                    None => None,
                                },
                            });
                        }
                        _ => {
                            return Err(bs_err());
                        }
                    }
                }
            }
            match acc {
                Some(a) => Ok(ExprOrDef::Expr(Expr::SimpleLet {
                    arg: temp,
                    value: Box::new(key),
                    body: Box::new(a),
                })),
                None => Err(bs_err()),
            }
        }
        "let" => {
            let mut first = operands.next().ok_or_else(bs_err)?;
            let name: Option<String> = if let Datum::Simple(SimpleDatum::Symbol(n)) = first {
                first = operands.next().ok_or_else(bs_err)?;
                Some(n)
            } else {
                None
            };
            let (args, vals) = match first {
                Datum::Compound(CompoundDatum::List(ListKind::Proper(binding_data))) => {
                    process_bindings(binding_data)?.into_iter().unzip()
                }
                Datum::EmptyList => (vec![], vec![]),
                _ => return Err(bs_err()),
            };
            let body = process_body(operands)?;
            match name {
                Some(n) => Ok(ExprOrDef::Expr(Expr::ProcCall {
                    operator: Box::new(Expr::Lambda(ProcData {
                        args: vec![n.clone()],
                        rest: None,
                        body: Body(vec![
                            ExprOrDef::Expr(Expr::Assignment {
                                variable: n.clone(),
                                value: Box::new(Expr::Lambda(ProcData {
                                    args,
                                    rest: None,
                                    body,
                                })),
                            }),
                            ExprOrDef::Expr(Expr::ProcCall {
                                operator: Box::new(Expr::Variable(n)),
                                operands: vals,
                            }),
                        ]),
                    })),
                    operands: vec![Expr::Undefined],
                })),
                None => Ok(ExprOrDef::Expr(Expr::ProcCall {
                    operator: Box::new(Expr::Lambda(ProcData {
                        args,
                        rest: None,
                        body,
                    })),
                    operands: vals,
                })),
            }
        }
        "let*" => {
            let first = operands.next().ok_or_else(bs_err)?;
            let bindings = match first {
                Datum::Compound(CompoundDatum::List(ListKind::Proper(binding_data))) => {
                    process_bindings(binding_data)?
                }
                Datum::EmptyList => vec![],
                _ => return Err(bs_err()),
            };
            let mut body = Some(process_body(operands)?);
            if bindings.is_empty() {
                return Ok(ExprOrDef::Expr(Expr::ProcCall {
                    operator: Box::new(Expr::Lambda(ProcData {
                        args: vec![],
                        rest: None,
                        body: body.take().unwrap(),
                    })),
                    operands: vec![],
                }));
            }
            let mut acc: Option<Expr> = None;
            for (arg, val) in bindings.into_iter().rev() {
                acc = Some(Expr::ProcCall {
                    operator: Box::new(Expr::Lambda(ProcData {
                        args: vec![arg],
                        rest: None,
                        body: match acc {
                            Some(a) => Body(vec![ExprOrDef::Expr(a)]),
                            None => body.take().unwrap(),
                        },
                    })),
                    operands: vec![val],
                });
            }
            Ok(ExprOrDef::Expr(acc.unwrap()))
        }
        "letrec" => {
            let first = operands.next().ok_or_else(bs_err)?;
            let bindings = match first {
                Datum::Compound(CompoundDatum::List(ListKind::Proper(binding_data))) => {
                    process_bindings(binding_data)?
                }
                Datum::EmptyList => vec![],
                _ => return Err(bs_err()),
            };
            let mut body = process_body(operands)?;
            if bindings.is_empty() {
                return Ok(ExprOrDef::Expr(Expr::ProcCall {
                    operator: Box::new(Expr::Lambda(ProcData {
                        args: vec![],
                        rest: None,
                        body,
                    })),
                    operands: vec![],
                }));
            }
            let (args, vals): (Vec<_>, Vec<_>) = bindings.into_iter().unzip();
            let temps: Vec<_> = args.iter().map(|_| gen_temp_name()).collect();
            let setters = Body(
                args.iter()
                    .cloned()
                    .zip(temps.iter().cloned())
                    .map(|(arg, temp)| {
                        ExprOrDef::Expr(Expr::Assignment {
                            variable: arg,
                            value: Box::new(Expr::Variable(temp)),
                        })
                    })
                    .collect(),
            );
            body.0.insert(
                0,
                ExprOrDef::Expr(Expr::ProcCall {
                    operator: Box::new(Expr::Lambda(ProcData {
                        args: temps,
                        rest: None,
                        body: setters,
                    })),
                    operands: vals,
                }),
            );
            let operands = vec![Expr::Undefined; args.len()];
            Ok(ExprOrDef::Expr(Expr::ProcCall {
                operator: Box::new(Expr::Lambda(ProcData {
                    args,
                    rest: None,
                    body,
                })),
                operands,
            }))
        }
        "do" => {
            let first = operands.next().ok_or_else(bs_err)?;
            match first {
                Datum::Compound(CompoundDatum::List(ListKind::Proper(_))) | Datum::EmptyList => {
                    let mut vars = vec![];
                    let mut inits = vec![];
                    let mut steps = vec![];
                    if let Datum::Compound(CompoundDatum::List(ListKind::Proper(spec_data))) = first
                    {
                        for spec in spec_data {
                            if let Datum::Compound(CompoundDatum::List(ListKind::Proper(parts))) =
                                spec
                            {
                                if parts.len() != 2 && parts.len() != 3 {
                                    return Err(bs_err());
                                }
                                let mut pi = parts.into_iter();
                                if let Datum::Simple(SimpleDatum::Symbol(name)) = pi.next().unwrap()
                                {
                                    inits.push(parse_expr(pi.next().unwrap())?);
                                    let step = pi
                                        .next()
                                        .map(parse_expr)
                                        .transpose()?
                                        .unwrap_or_else(|| Expr::Variable(name.clone()));
                                    vars.push(name);
                                    steps.push(step);
                                } else {
                                    return Err(bs_err());
                                }
                            } else {
                                return Err(bs_err());
                            }
                        }
                    }
                    let second = operands.next().ok_or_else(bs_err)?;
                    if let Datum::Compound(CompoundDatum::List(ListKind::Proper(term))) = second {
                        let mut ei = term.into_iter().map(parse_expr);
                        let test = ei.next().ok_or_else(bs_err)??;
                        let seq = ei.collect::<Result<Vec<_>, _>>()?;
                        let mut body = operands.map(parse_expr).collect::<Result<Vec<_>, _>>()?;
                        let temp = gen_temp_name();
                        body.push(Expr::ProcCall {
                            operator: Box::new(Expr::Variable(temp.clone())),
                            operands: steps,
                        });
                        Ok(ExprOrDef::Expr(Expr::ProcCall {
                            operator: Box::new(Expr::Lambda(ProcData {
                                args: vec![temp.clone()],
                                rest: None,
                                body: Body(vec![
                                    ExprOrDef::Expr(Expr::Assignment {
                                        variable: temp.clone(),
                                        value: Box::new(Expr::Lambda(ProcData {
                                            args: vars,
                                            rest: None,
                                            body: Body(vec![ExprOrDef::Expr(Expr::Conditional {
                                                test: Box::new(test),
                                                consequent: Box::new(Expr::Begin(seq)),
                                                alternate: Some(Box::new(Expr::Begin(body))),
                                            })]),
                                        })),
                                    }),
                                    ExprOrDef::Expr(Expr::ProcCall {
                                        operator: Box::new(Expr::Variable(temp)),
                                        operands: inits,
                                    }),
                                ]),
                            })),
                            operands: vec![Expr::Undefined],
                        }))
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
                Ok(ExprOrDef::Expr(Expr::ProcCall {
                    operator: Box::new(Expr::Variable("make-promise".to_owned())),
                    operands: vec![Expr::Lambda(ProcData {
                        args: vec![],
                        rest: None,
                        body: Body(vec![ExprOrDef::Expr(expr)]),
                    })],
                }))
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

fn parse_expr(datum: Datum) -> Result<Expr, ParserError> {
    match parse(datum)? {
        ExprOrDef::Expr(expr) => Ok(expr),
        _ => Err(ParserError {
            kind: ParserErrorKind::IllegalDefine,
        }),
    }
}

pub fn parse(datum: Datum) -> Result<ExprOrDef, ParserError> {
    match datum {
        Datum::Simple(simple) => match simple {
            SimpleDatum::Boolean(b) => Ok(ExprOrDef::Expr(Expr::Literal(
                LiteralKind::SelfEvaluating(SelfEvaluatingKind::Boolean(b)),
            ))),
            SimpleDatum::Number(n) => Ok(ExprOrDef::Expr(Expr::Literal(
                LiteralKind::SelfEvaluating(SelfEvaluatingKind::Number(n)),
            ))),
            SimpleDatum::Character(c) => Ok(ExprOrDef::Expr(Expr::Literal(
                LiteralKind::SelfEvaluating(SelfEvaluatingKind::Character(c)),
            ))),
            SimpleDatum::String(s) => Ok(ExprOrDef::Expr(Expr::Literal(
                LiteralKind::SelfEvaluating(SelfEvaluatingKind::String(s)),
            ))),
            SimpleDatum::Symbol(symb) => match &symb {
                kw if is_keyword(kw) => Err(ParserError {
                    kind: ParserErrorKind::BadSyntax(kw.to_owned()),
                }),
                _ => Ok(ExprOrDef::Expr(Expr::Variable(symb))),
            },
        },
        Datum::Compound(compound) => match compound {
            CompoundDatum::List(list) => match list {
                ListKind::Proper(list) => {
                    let mut li = list.into_iter();
                    let first = li.next().ok_or(ParserError {
                        kind: ParserErrorKind::IllegalEmptyList,
                    })?;
                    match &first {
                        Datum::Simple(SimpleDatum::Symbol(kw)) if is_keyword(kw) => {
                            process_keyword(kw, li)
                        }
                        _ => {
                            let operator = parse_expr(first)?;
                            let rest = li.map(parse_expr).collect::<Result<Vec<_>, _>>()?;
                            Ok(ExprOrDef::Expr(Expr::ProcCall {
                                operator: Box::new(operator),
                                operands: rest,
                            }))
                        }
                    }
                }
                ListKind::Improper(_, _) => Err(ParserError {
                    kind: ParserErrorKind::IllegalDot,
                }),
                ListKind::Abbreviation(abbr, arg) => {
                    let operands = std::iter::once(*arg);
                    process_keyword(abbr.to_keyword(), operands)
                }
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

    #[test]
    fn literals() {
        assert_eq!(
            parse(bool_datum!(true)),
            Ok(ExprOrDef::Expr(bool_expr!(true)))
        );
        assert_eq!(parse(int_datum!(42)), Ok(ExprOrDef::Expr(int_expr!(42))));
        assert_eq!(
            parse(char_datum!('c')),
            Ok(ExprOrDef::Expr(char_expr!('c')))
        );
        assert_eq!(
            parse(str_datum!("foo")),
            Ok(ExprOrDef::Expr(str_expr!("foo")))
        );
    }

    #[test]
    fn proc_calls() {
        assert_eq!(
            parse(proper_list_datum![symbol_datum!("f")]),
            Ok(ExprOrDef::Expr(Expr::ProcCall {
                operator: Box::new(var_expr!("f")),
                operands: vec![],
            }))
        );

        assert_eq!(
            parse(proper_list_datum![
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
                kind: ParserErrorKind::IllegalDot,
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
            Ok(ExprOrDef::Definition(Definition::Variable {
                name: "x".to_owned(),
                value: Box::new(int_expr!(42)),
            }))
        );

        assert_eq!(
            parse_expr(proper_list_datum![
                symbol_datum!("define"),
                symbol_datum!("x"),
                int_datum!(42),
            ]),
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
            Ok(ExprOrDef::Definition(Definition::Procedure {
                name: "f".to_owned(),
                data: ProcData {
                    args: vec!["x".to_owned()],
                    rest: None,
                    body: Body(vec![
                        ExprOrDef::Definition(Definition::Variable {
                            name: "y".to_owned(),
                            value: Box::new(int_expr!(1)),
                        }),
                        ExprOrDef::Expr(Expr::ProcCall {
                            operator: Box::new(var_expr!("+")),
                            operands: vec![var_expr!("x"), var_expr!("y")],
                        }),
                    ]),
                }
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
            Ok(ExprOrDef::Definition(Definition::Procedure {
                name: "f".to_owned(),
                data: ProcData {
                    args: vec!["x".to_owned(), "y".to_owned()],
                    rest: Some("z".to_owned()),
                    body: Body(vec![ExprOrDef::Expr(var_expr!("z"))]),
                }
            }))
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("define"),
                proper_list_datum![symbol_datum!("f"), int_datum!(1),],
                int_datum!(2),
            ]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("define".to_owned()),
            })
        );

        assert_eq!(
            parse(proper_list_datum![symbol_datum!("define")]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("define".to_owned())
            })
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("define"),
                int_datum!(1),
                int_datum!(2),
            ]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("define".to_owned())
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
                kind: ParserErrorKind::BadSyntax("define".to_owned())
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
                kind: ParserErrorKind::BadSyntax("define".to_owned())
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
            Ok(ExprOrDef::Expr(Expr::Lambda(ProcData {
                args: vec!["x".to_owned(), "y".to_owned()],
                rest: None,
                body: Body(vec![ExprOrDef::Expr(Expr::ProcCall {
                    operator: Box::new(var_expr!("+")),
                    operands: vec![var_expr!("x"), var_expr!("y")],
                })]),
            })))
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
            Ok(ExprOrDef::Expr(Expr::Lambda(ProcData {
                args: vec!["x".to_owned(), "y".to_owned()],
                rest: Some("z".to_owned()),
                body: Body(vec![
                    ExprOrDef::Definition(Definition::Variable {
                        name: "a".to_owned(),
                        value: Box::new(int_expr!(42)),
                    }),
                    ExprOrDef::Expr(Expr::ProcCall {
                        operator: Box::new(var_expr!("+")),
                        operands: vec![
                            var_expr!("x"),
                            var_expr!("y"),
                            Expr::ProcCall {
                                operator: Box::new(var_expr!("car")),
                                operands: vec![var_expr!("z")],
                            }
                        ],
                    })
                ]),
            })))
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("lambda"),
                symbol_datum!("args"),
                int_datum!(1)
            ]),
            Ok(ExprOrDef::Expr(Expr::Lambda(ProcData {
                args: vec![],
                rest: Some("args".to_owned()),
                body: Body(vec![ExprOrDef::Expr(int_expr!(1))]),
            })))
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("lambda"),
                Datum::EmptyList,
                int_datum!(1)
            ]),
            Ok(ExprOrDef::Expr(Expr::Lambda(ProcData {
                args: vec![],
                rest: None,
                body: Body(vec![ExprOrDef::Expr(int_expr!(1))]),
            })))
        );

        assert_eq!(
            parse(proper_list_datum![symbol_datum!("lambda")]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("lambda".to_owned())
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
            Ok(ExprOrDef::Expr(Expr::Literal(LiteralKind::Quotation(
                proper_list_datum![symbol_datum!("a"), symbol_datum!("b")],
            ))))
        );

        assert_eq!(
            parse(proper_list_datum![symbol_datum!("quote")]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("quote".to_owned())
            })
        );

        assert_eq!(
            parse(abbr_list_datum!(
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
            parse(proper_list_datum![
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
            parse(proper_list_datum![symbol_datum!("set!")]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("set!".to_owned())
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
                kind: ParserErrorKind::BadSyntax("set!".to_owned())
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
            Ok(ExprOrDef::Expr(Expr::Conditional {
                test: Box::new(int_expr!(1)),
                consequent: Box::new(int_expr!(2)),
                alternate: Some(Box::new(int_expr!(3))),
            }))
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("if"),
                int_datum!(1),
                int_datum!(2),
            ]),
            Ok(ExprOrDef::Expr(Expr::Conditional {
                test: Box::new(int_expr!(1)),
                consequent: Box::new(int_expr!(2)),
                alternate: None,
            }))
        );

        assert_eq!(
            parse(proper_list_datum![symbol_datum!("if")]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("if".to_owned())
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
                kind: ParserErrorKind::BadSyntax("if".to_owned())
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
            parse(proper_list_datum![
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
            parse(proper_list_datum![symbol_datum!("begin")]),
            Ok(ExprOrDef::Expr(Expr::Begin(vec![])))
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("begin"),
                symbol_datum!("x"),
                symbol_datum!("y"),
                symbol_datum!("z"),
            ]),
            Ok(ExprOrDef::Expr(Expr::Begin(vec![
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
            Ok(ExprOrDef::Expr(Expr::Conditional {
                test: Box::new(bool_expr!(false)),
                consequent: Box::new(int_expr!(0)),
                alternate: Some(Box::new(Expr::Conditional {
                    test: Box::new(Expr::ProcCall {
                        operator: Box::new(var_expr!("=")),
                        operands: vec![int_expr!(1), int_expr!(2)]
                    }),
                    consequent: Box::new(Expr::Begin(vec![int_expr!(3), int_expr!(4)])),
                    alternate: Some(Box::new(Expr::SimpleLet {
                        arg: "__temp_var".to_owned(),
                        value: Box::new(Expr::ProcCall {
                            operator: Box::new(var_expr!("cons")),
                            operands: vec![int_expr!(5), int_expr!(6)]
                        }),
                        body: Box::new(Expr::Conditional {
                            test: Box::new(var_expr!("__temp_var")),
                            consequent: Box::new(Expr::ProcCall {
                                operator: Box::new(var_expr!("car")),
                                operands: vec![var_expr!("__temp_var")]
                            }),
                            alternate: Some(Box::new(int_expr!(3))),
                        })
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
                kind: ParserErrorKind::BadSyntax("cond".to_owned())
            })
        );

        assert_eq!(
            parse(proper_list_datum![
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
            parse(proper_list_datum![symbol_datum!("and")]),
            Ok(ExprOrDef::Expr(bool_expr!(true)))
        );

        assert_eq!(
            parse(proper_list_datum![symbol_datum!("and"), int_datum!(1),]),
            Ok(ExprOrDef::Expr(int_expr!(1)))
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("and"),
                bool_datum!(true),
                int_datum!(1),
            ]),
            Ok(ExprOrDef::Expr(Expr::Conditional {
                test: Box::new(bool_expr!(true)),
                consequent: Box::new(int_expr!(1)),
                alternate: Some(Box::new(bool_expr!(false))),
            }))
        );

        assert_eq!(
            parse(proper_list_datum![symbol_datum!("or"), int_datum!(1),]),
            Ok(ExprOrDef::Expr(int_expr!(1)))
        );

        assert_eq!(
            parse(proper_list_datum![symbol_datum!("or")]),
            Ok(ExprOrDef::Expr(bool_expr!(false)))
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("or"),
                int_datum!(1),
                bool_datum!(true),
            ]),
            Ok(ExprOrDef::Expr(Expr::SimpleLet {
                arg: "__temp_var".to_owned(),
                value: Box::new(int_expr!(1)),
                body: Box::new(Expr::Conditional {
                    test: Box::new(var_expr!("__temp_var")),
                    consequent: Box::new(var_expr!("__temp_var")),
                    alternate: Some(Box::new(bool_expr!(true))),
                })
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
            Ok(ExprOrDef::Expr(Expr::SimpleLet {
                arg: "__temp_var".to_owned(),
                value: Box::new(int_expr!(42)),
                body: Box::new(Expr::Conditional {
                    test: Box::new(Expr::ProcCall {
                        operator: Box::new(var_expr!("memv")),
                        operands: vec![
                            var_expr!("__temp_var"),
                            Expr::Literal(LiteralKind::Quotation(proper_list_datum![int_datum!(
                                1
                            )]))
                        ]
                    }),
                    consequent: Box::new(int_expr!(2)),
                    alternate: Some(Box::new(Expr::Conditional {
                        test: Box::new(Expr::ProcCall {
                            operator: Box::new(var_expr!("memv")),
                            operands: vec![
                                var_expr!("__temp_var"),
                                Expr::Literal(LiteralKind::Quotation(proper_list_datum![
                                    int_datum!(3),
                                    int_datum!(4)
                                ]))
                            ]
                        }),
                        consequent: Box::new(int_expr!(5)),
                        alternate: Some(Box::new(int_expr!(6))),
                    })),
                })
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
                kind: ParserErrorKind::BadSyntax("case".to_owned())
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
                kind: ParserErrorKind::BadSyntax("case".to_owned())
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
            Ok(ExprOrDef::Expr(Expr::ProcCall {
                operator: Box::new(Expr::Lambda(ProcData {
                    args: vec![],
                    rest: None,
                    body: Body(vec![ExprOrDef::Expr(int_expr!(0))]),
                })),
                operands: vec![]
            }))
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("let"),
                proper_list_datum![proper_list_datum![symbol_datum!("x"), int_datum!(1)]],
                proper_list_datum![symbol_datum!("define"), symbol_datum!("y"), int_datum!(2)],
                proper_list_datum![symbol_datum!("+"), symbol_datum!("x"), symbol_datum!("y")],
            ]),
            Ok(ExprOrDef::Expr(Expr::ProcCall {
                operator: Box::new(Expr::Lambda(ProcData {
                    args: vec!["x".to_owned()],
                    rest: None,
                    body: Body(vec![
                        ExprOrDef::Definition(Definition::Variable {
                            name: "y".to_owned(),
                            value: Box::new(int_expr!(2)),
                        }),
                        ExprOrDef::Expr(Expr::ProcCall {
                            operator: Box::new(var_expr!("+")),
                            operands: vec![var_expr!("x"), var_expr!("y")],
                        }),
                    ]),
                })),
                operands: vec![int_expr!(1)],
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
            Ok(ExprOrDef::Expr(Expr::ProcCall {
                operator: Box::new(Expr::Lambda(ProcData {
                    args: vec!["x".to_owned()],
                    rest: None,
                    body: Body(vec![ExprOrDef::Expr(Expr::ProcCall {
                        operator: Box::new(Expr::Lambda(ProcData {
                            args: vec!["y".to_owned()],
                            rest: None,
                            body: Body(vec![ExprOrDef::Expr(var_expr!("y"))]),
                        })),
                        operands: vec![var_expr!("x")],
                    })]),
                })),
                operands: vec![int_expr!(1)],
            }))
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("letrec"),
                proper_list_datum![proper_list_datum![symbol_datum!("x"), int_datum!(1)]],
                symbol_datum!("x"),
            ]),
            Ok(ExprOrDef::Expr(Expr::ProcCall {
                operator: Box::new(Expr::Lambda(ProcData {
                    args: vec!["x".to_owned()],
                    rest: None,
                    body: Body(vec![
                        ExprOrDef::Expr(Expr::ProcCall {
                            operator: Box::new(Expr::Lambda(ProcData {
                                args: vec!["__temp_var".to_owned()],
                                rest: None,
                                body: Body(vec![ExprOrDef::Expr(Expr::Assignment {
                                    variable: "x".to_owned(),
                                    value: Box::new(var_expr!("__temp_var"))
                                })])
                            })),
                            operands: vec![int_expr!(1)]
                        }),
                        ExprOrDef::Expr(var_expr!("x")),
                    ]),
                })),
                operands: vec![Expr::Undefined],
            }))
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("let"),
                symbol_datum!("foo"),
                proper_list_datum![proper_list_datum![symbol_datum!("x"), int_datum!(1)]],
                proper_list_datum![symbol_datum!("foo"), symbol_datum!("x")],
            ]),
            Ok(ExprOrDef::Expr(Expr::ProcCall {
                operator: Box::new(Expr::Lambda(ProcData {
                    args: vec!["foo".to_owned()],
                    rest: None,
                    body: Body(vec![
                        ExprOrDef::Expr(Expr::Assignment {
                            variable: "foo".to_owned(),
                            value: Box::new(Expr::Lambda(ProcData {
                                args: vec!["x".to_owned()],
                                rest: None,
                                body: Body(vec![ExprOrDef::Expr(Expr::ProcCall {
                                    operator: Box::new(var_expr!("foo")),
                                    operands: vec![var_expr!("x")]
                                })])
                            }))
                        }),
                        ExprOrDef::Expr(Expr::ProcCall {
                            operator: Box::new(var_expr!("foo")),
                            operands: vec![int_expr!(1)]
                        })
                    ])
                })),
                operands: vec![Expr::Undefined]
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
                kind: ParserErrorKind::BadSyntax("let*".to_owned())
            })
        );

        assert_eq!(
            parse(proper_list_datum![symbol_datum!("let")]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("let".to_owned())
            })
        );

        assert_eq!(
            parse(proper_list_datum![symbol_datum!("let"), int_datum!(1)]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("let".to_owned())
            })
        );

        assert_eq!(
            parse(proper_list_datum![
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
            Ok(ExprOrDef::Expr(Expr::ProcCall {
                operator: Box::new(Expr::Lambda(ProcData {
                    args: vec!["__temp_var".to_owned()],
                    rest: None,
                    body: Body(vec![
                        ExprOrDef::Expr(Expr::Assignment {
                            variable: "__temp_var".to_owned(),
                            value: Box::new(Expr::Lambda(ProcData {
                                args: vec!["x".to_owned(), "y".to_owned()],
                                rest: None,
                                body: Body(vec![ExprOrDef::Expr(Expr::Conditional {
                                    test: Box::new(Expr::ProcCall {
                                        operator: Box::new(var_expr!("=")),
                                        operands: vec![var_expr!("x"), var_expr!("y")]
                                    }),
                                    consequent: Box::new(Expr::Begin(vec![Expr::ProcCall {
                                        operator: Box::new(var_expr!("+")),
                                        operands: vec![var_expr!("x"), var_expr!("y")]
                                    }])),
                                    alternate: Some(Box::new(Expr::Begin(vec![Expr::ProcCall {
                                        operator: Box::new(var_expr!("__temp_var")),
                                        operands: vec![
                                            Expr::ProcCall {
                                                operator: Box::new(var_expr!("+")),
                                                operands: vec![int_expr!(1), var_expr!("x")]
                                            },
                                            var_expr!("y")
                                        ]
                                    }])))
                                })])
                            }))
                        }),
                        ExprOrDef::Expr(Expr::ProcCall {
                            operator: Box::new(var_expr!("__temp_var")),
                            operands: vec![int_expr!(1), int_expr!(5)]
                        })
                    ])
                })),
                operands: vec![Expr::Undefined]
            }))
        );

        assert_eq!(
            parse(proper_list_datum![
                symbol_datum!("do"),
                Datum::EmptyList,
                proper_list_datum![bool_datum!(false)],
                proper_list_datum![symbol_datum!("f")],
            ]),
            Ok(ExprOrDef::Expr(Expr::ProcCall {
                operator: Box::new(Expr::Lambda(ProcData {
                    args: vec!["__temp_var".to_owned()],
                    rest: None,
                    body: Body(vec![
                        ExprOrDef::Expr(Expr::Assignment {
                            variable: "__temp_var".to_owned(),
                            value: Box::new(Expr::Lambda(ProcData {
                                args: vec![],
                                rest: None,
                                body: Body(vec![ExprOrDef::Expr(Expr::Conditional {
                                    test: Box::new(bool_expr!(false)),
                                    consequent: Box::new(Expr::Begin(vec![])),
                                    alternate: Some(Box::new(Expr::Begin(vec![
                                        Expr::ProcCall {
                                            operator: Box::new(var_expr!("f")),
                                            operands: vec![]
                                        },
                                        Expr::ProcCall {
                                            operator: Box::new(var_expr!("__temp_var")),
                                            operands: vec![]
                                        }
                                    ])))
                                })])
                            }))
                        }),
                        ExprOrDef::Expr(Expr::ProcCall {
                            operator: Box::new(var_expr!("__temp_var")),
                            operands: vec![]
                        })
                    ])
                })),
                operands: vec![Expr::Undefined]
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
            Ok(ExprOrDef::Expr(Expr::ProcCall {
                operator: Box::new(Expr::Variable("make-promise".to_owned())),
                operands: vec![Expr::Lambda(ProcData {
                    args: vec![],
                    rest: None,
                    body: Body(vec![ExprOrDef::Expr(Expr::ProcCall {
                        operator: Box::new(var_expr!("f")),
                        operands: vec![int_expr!(1)],
                    })]),
                })],
            })),
        );

        assert_eq!(
            parse(proper_list_datum![symbol_datum!("delay")]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("delay".to_owned())
            })
        );

        assert_eq!(
            parse(proper_list_datum![
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
            parse(proper_list_datum![
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
            parse(abbr_list_datum!(
                AbbreviationPrefix::Quasiquote,
                proper_list_datum![symbol_datum!("+"), int_datum!(1), int_datum!(2)],
            )),
            Ok(ExprOrDef::Expr(Expr::Quasiquotation(QQTemplate::LevelD(
                1,
                Box::new(quoted_p12()),
            )))),
        );

        let with_abbrs = parse(abbr_list_datum!(
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
        let with_kws = parse(proper_list_datum![
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
            parse(abbr_list_datum!(
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
            parse(abbr_list_datum!(
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
            parse(abbr_list_datum!(
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

        assert_eq!(
            parse(abbr_list_datum!(AbbreviationPrefix::Unquote, int_datum!(1),)),
            Err(ParserError {
                kind: ParserErrorKind::IllegalUnquote
            })
        );

        assert_eq!(
            parse(abbr_list_datum!(
                AbbreviationPrefix::UnquoteSplicing,
                int_datum!(1),
            )),
            Err(ParserError {
                kind: ParserErrorKind::IllegalUnquoteSplicing
            })
        );
    }
}
