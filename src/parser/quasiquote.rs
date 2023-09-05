use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ListType {
    Proper,
    Improper,
    Vector,
}

fn process_qq_list(
    mut li: impl Iterator<Item = Datum>,
    qq_level: u32,
    list_type: ListType,
) -> Result<(Vec<Rc<Expr>>, bool), ParserError> {
    let mut parts = vec![];
    let mut curr = vec![];
    let mut to_push = None;
    while let Some(el) = li.next() {
        match el {
            Datum::Compound(CompoundDatum::List(ListKind::Proper(l)))
                if matches!(
                    l.first(),
                    Some(Datum::Simple(SimpleDatum::Symbol(s)))
                        if *s == "unquote-splicing".into()
                ) && qq_level == 0 =>
            {
                if !curr.is_empty() {
                    parts.push(Rc::new(Expr::ProcCall {
                        operator: Rc::new(Expr::Variable("list".into())),
                        operands: curr,
                    }));
                    curr = vec![];
                }
                let mut us = l.into_iter().skip(1);
                let arg = us.next().ok_or_else(|| ParserError {
                    kind: ParserErrorKind::BadSyntax("unquote-splicing".into()),
                })?;
                if us.next().is_some() {
                    return Err(ParserError {
                        kind: ParserErrorKind::BadSyntax("unquote-splicing".into()),
                    });
                }
                parts.push(parse_expr(arg)?);
            }
            _ => {
                if let Datum::Simple(SimpleDatum::Symbol(s)) = &el {
                    let (is_unquote, is_unquote_splicing, is_quasiquote) = (
                        *s == "unquote".into(),
                        *s == "unquote-splicing".into(),
                        *s == "quasiquote".into(),
                    );
                    if is_unquote || is_unquote_splicing || is_quasiquote {
                        match list_type {
                            ListType::Proper => {
                                to_push = Some(process_qq(
                                    Datum::Compound(CompoundDatum::List(ListKind::Proper(
                                        std::iter::once(el).chain(li).collect(),
                                    ))),
                                    qq_level,
                                )?);
                                break;
                            }
                            ListType::Improper => {
                                if qq_level == 0 && !is_quasiquote {
                                    return Err(ParserError {
                                        kind: ParserErrorKind::BadSyntax(*s),
                                    });
                                }
                            }
                            ListType::Vector => {}
                        }
                    }
                }
                curr.push(process_qq(el, qq_level)?);
            }
        }
    }
    if !curr.is_empty() {
        parts.push(Rc::new(Expr::ProcCall {
            operator: Rc::new(Expr::Variable(
                (if list_type == ListType::Vector && parts.is_empty() {
                    "vector"
                } else {
                    "list"
                })
                .into(),
            )),
            operands: curr,
        }));
    }
    if let Some(to_push) = to_push {
        parts.push(to_push);
        Ok((parts, true))
    } else {
        Ok((parts, false))
    }
}

pub(super) fn process_qq(datum: Datum, qq_level: u32) -> Result<Rc<Expr>, ParserError> {
    match datum {
        Datum::Simple(_) | Datum::EmptyList => {
            Ok(Rc::new(Expr::Literal(LiteralKind::Quotation(datum))))
        }
        Datum::Compound(CompoundDatum::List(list)) => match list {
            ListKind::Proper(list) => {
                if let Some(Datum::Simple(SimpleDatum::Symbol(s))) = list.first() {
                    let (is_unquote, is_unquote_splicing, is_quasiquote) = (
                        *s == "unquote".into(),
                        *s == "unquote-splicing".into(),
                        *s == "quasiquote".into(),
                    );
                    if is_unquote || is_unquote_splicing || is_quasiquote {
                        let s = *s;
                        let mut li = list.into_iter().skip(1);
                        if qq_level == 0 && !is_quasiquote {
                            let arg = li.next().ok_or(ParserError {
                                kind: ParserErrorKind::BadSyntax(s),
                            })?;
                            if li.next().is_some() {
                                return Err(ParserError {
                                    kind: ParserErrorKind::BadSyntax(s),
                                });
                            }
                            if is_unquote_splicing {
                                return Err(ParserError {
                                    kind: ParserErrorKind::IllegalUnquoteSplicing,
                                });
                            }
                            return parse_expr(arg);
                        }
                        let args: Vec<_> = li.collect();
                        let valid = args.len() == 1;
                        return Ok(Rc::new(Expr::ProcCall {
                            operator: Rc::new(Expr::Variable("cons".into())),
                            operands: vec![
                                Rc::new(Expr::Literal(LiteralKind::Quotation(Datum::Simple(
                                    SimpleDatum::Symbol(s),
                                )))),
                                process_qq(
                                    Datum::Compound(CompoundDatum::List(ListKind::Proper(args))),
                                    if !valid {
                                        qq_level
                                    } else if is_quasiquote {
                                        qq_level + 1
                                    } else {
                                        qq_level - 1
                                    },
                                )?,
                            ],
                        }));
                    }
                }
                let (mut parts, is_improper) =
                    process_qq_list(list.into_iter(), qq_level, ListType::Proper)?;
                if parts.len() == 1 {
                    Ok(parts.into_iter().next().unwrap())
                } else {
                    if !is_improper {
                        parts.push(Rc::new(Expr::Literal(LiteralKind::Quotation(
                            Datum::EmptyList,
                        ))));
                    }
                    Ok(Rc::new(Expr::ProcCall {
                        operator: Rc::new(Expr::Variable("append".into())),
                        operands: parts,
                    }))
                }
            }
            ListKind::Improper(list, last) => {
                let (mut parts, _) =
                    process_qq_list(list.into_iter(), qq_level, ListType::Improper)?;
                parts.push(process_qq(*last, qq_level)?);
                Ok(Rc::new(Expr::ProcCall {
                    operator: Rc::new(Expr::Variable("append".into())),
                    operands: parts,
                }))
            }
        },
        Datum::Compound(CompoundDatum::Vector(vector)) => {
            let (mut parts, _) = process_qq_list(vector.into_iter(), qq_level, ListType::Vector)?;
            if parts.len() == 1 {
                Ok(parts.into_iter().next().unwrap())
            } else {
                parts.push(Rc::new(Expr::Literal(LiteralKind::Quotation(
                    Datum::EmptyList,
                ))));
                Ok(Rc::new(Expr::ProcCall {
                    operator: Rc::new(Expr::Variable("list->vector".into())),
                    operands: vec![Rc::new(Expr::ProcCall {
                        operator: Rc::new(Expr::Variable("append".into())),
                        operands: parts,
                    })],
                }))
            }
        }
    }
}
