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
) -> Result<Expr, ParserError> {
    match var {
        Datum::Simple(SimpleDatum::Symbol(s)) => {
            if is_keyword(s) {
                Err(ParserError {
                    kind: ParserErrorKind::IllegalVariableName(s.to_owned()),
                })
            } else {
                let expr = parse(body.next().ok_or(ParserError {
                    kind: ParserErrorKind::BadSyntax("define".to_owned()),
                })?)?;

                if body.next().is_none() {
                    Ok(Expr::Definition(Definition::Variable {
                        name: s.to_owned(),
                        value: Box::new(expr),
                    }))
                } else {
                    Err(ParserError {
                        kind: ParserErrorKind::BadSyntax("define".to_owned()),
                    })
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
                    _ => {
                        return Err(ParserError {
                            kind: ParserErrorKind::BadSyntax("define".to_owned()),
                        })
                    }
                };
                let args = forms[1..]
                    .iter()
                    .map(|d| match d {
                        Datum::Simple(SimpleDatum::Symbol(s)) => {
                            if is_keyword(s) {
                                Err(ParserError {
                                    kind: ParserErrorKind::IllegalVariableName(s.to_owned()),
                                })
                            } else {
                                Ok(s.to_owned())
                            }
                        }
                        _ => Err(ParserError {
                            kind: ParserErrorKind::BadSyntax("define".to_owned()),
                        }),
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(Expr::Definition(Definition::Procedure {
                    name: name.to_owned(),
                    args,
                    rest: if let ListKind::Improper(_, rest) = list {
                        match &**rest {
                            Datum::Simple(SimpleDatum::Symbol(s)) => {
                                if is_keyword(s) {
                                    return Err(ParserError {
                                        kind: ParserErrorKind::IllegalVariableName(s.to_owned()),
                                    });
                                }
                                Some(s.to_owned())
                            }
                            _ => {
                                return Err(ParserError {
                                    kind: ParserErrorKind::BadSyntax("define".to_owned()),
                                });
                            }
                        }
                    } else {
                        None
                    },
                    body: process_body(body)?,
                }))
            }
            _ => Err(ParserError {
                kind: ParserErrorKind::BadSyntax("define".to_owned()),
            }),
        },
        _ => Err(ParserError {
            kind: ParserErrorKind::BadSyntax("define".to_owned()),
        }),
    }
}

fn process_body<'a, I: Iterator<Item = &'a Datum>>(data: I) -> Result<Body, ParserError> {
    let mut defs: Vec<Definition> = vec![];
    let mut exprs: Vec<Expr> = vec![];
    let mut last_is_expr = false;
    for d in data {
        match parse(d)? {
            Expr::Definition(def) => {
                defs.push(def);
                last_is_expr = false;
            }
            expr => {
                exprs.push(expr);
                last_is_expr = true;
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

fn process_keyword<'a, I: Iterator<Item = &'a Datum>>(
    kw: &str,
    mut operands: I,
) -> Result<Expr, ParserError> {
    match kw {
        "define" => {
            let var = operands.next().ok_or(ParserError {
                kind: ParserErrorKind::BadSyntax("define".to_owned()),
            })?;
            process_define(var, operands)
        }
        "quote" => {
            let operand = operands.next().ok_or(ParserError {
                kind: ParserErrorKind::BadSyntax(kw.to_owned()),
            })?;
            if operands.next().is_none() {
                Ok(Expr::Literal(LiteralKind::Quotation(operand.clone())))
            } else {
                Err(ParserError {
                    kind: ParserErrorKind::BadSyntax(kw.to_owned()),
                })
            }
        }
        "lambda" => {
            let formals = operands.next().ok_or(ParserError {
                kind: ParserErrorKind::BadSyntax(kw.to_owned()),
            })?;
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
                                                s.to_owned(),
                                            ),
                                        })
                                    } else {
                                        Ok(s.to_owned())
                                    }
                                }
                                _ => Err(ParserError {
                                    kind: ParserErrorKind::BadSyntax("lambda".to_owned()),
                                }),
                            })
                            .collect::<Result<Vec<_>, _>>()?;

                        Ok(Expr::Lambda {
                            args,
                            rest: if let ListKind::Improper(_, rest) = list {
                                match &**rest {
                                    Datum::Simple(SimpleDatum::Symbol(s)) => {
                                        if is_keyword(s) {
                                            return Err(ParserError {
                                                kind: ParserErrorKind::IllegalVariableName(
                                                    s.to_owned(),
                                                ),
                                            });
                                        }
                                        Some(s.to_owned())
                                    }
                                    _ => {
                                        return Err(ParserError {
                                            kind: ParserErrorKind::BadSyntax("lambda".to_owned()),
                                        });
                                    }
                                }
                            } else {
                                None
                            },
                            body: process_body(operands)?,
                        })
                    }
                    _ => Err(ParserError {
                        kind: ParserErrorKind::BadSyntax("define".to_owned()),
                    }),
                },
                Datum::Simple(SimpleDatum::Symbol(rest)) => {
                    if is_keyword(rest) {
                        return Err(ParserError {
                            kind: ParserErrorKind::IllegalVariableName(rest.to_owned()),
                        });
                    }
                    Ok(Expr::Lambda {
                        args: vec![],
                        rest: Some(rest.to_owned()),
                        body: process_body(operands)?,
                    })
                }
                Datum::EmptyList => Ok(Expr::Lambda {
                    args: vec![],
                    rest: None,
                    body: process_body(operands)?,
                }),
                _ => Err(ParserError {
                    kind: ParserErrorKind::BadSyntax(kw.to_owned()),
                }),
            }
        }
        "begin" => {
            let mut children = vec![];
            let mut has_def = false;
            let mut has_expr = false;
            for operand in operands {
                let child = parse(operand)?;
                match child {
                    Expr::Definition(_) => {
                        has_def = true;
                    }
                    _ => {
                        has_expr = true;
                    }
                }
                children.push(child);
            }
            if has_def && has_expr {
                Ok(Expr::MixedBegin(children))
            } else if has_def {
                Ok(Expr::Definition(Definition::Begin(
                    children
                        .into_iter()
                        .map(|e| match e {
                            Expr::Definition(d) => d,
                            _ => unreachable!("should not encounter non-definition"),
                        })
                        .collect(),
                )))
            } else {
                Ok(Expr::DerivedExpr(DerivedExprKind::Begin(children)))
            }
        }
        "set!" => {
            let variable = operands.next().ok_or(ParserError {
                kind: ParserErrorKind::BadSyntax(kw.to_owned()),
            })?;
            let value = operands.next().ok_or(ParserError {
                kind: ParserErrorKind::BadSyntax(kw.to_owned()),
            })?;
            if operands.next().is_none() {
                Ok(Expr::Assignment {
                    variable: match variable {
                        Datum::Simple(SimpleDatum::Symbol(s)) => s.to_owned(),
                        _ => {
                            return Err(ParserError {
                                kind: ParserErrorKind::BadSyntax(kw.to_owned()),
                            });
                        }
                    },
                    value: Box::new(parse(value)?),
                })
            } else {
                Err(ParserError {
                    kind: ParserErrorKind::BadSyntax(kw.to_owned()),
                })
            }
        }
        "if" => {
            let test = operands.next().ok_or(ParserError {
                kind: ParserErrorKind::BadSyntax(kw.to_owned()),
            })?;
            let consequent = operands.next().ok_or(ParserError {
                kind: ParserErrorKind::BadSyntax(kw.to_owned()),
            })?;
            let alternate = operands.next();
            if operands.next().is_none() {
                Ok(Expr::Conditional {
                    test: Box::new(parse(test)?),
                    consequent: Box::new(parse(consequent)?),
                    alternate: if let Some(a) = alternate {
                        Some(Box::new(parse(a)?))
                    } else {
                        None
                    },
                })
            } else {
                Err(ParserError {
                    kind: ParserErrorKind::BadSyntax(kw.to_owned()),
                })
            }
        }
        "cond" => {
            let mut clauses = vec![];
            let mut else_present = false;
            for clause in operands {
                if let Datum::Compound(CompoundDatum::List(ListKind::Proper(parts))) = clause {
                    if parts.is_empty() {
                        return Err(ParserError {
                            kind: ParserErrorKind::IllegalEmptyList,
                        });
                    }
                    match &parts[0] {
                        Datum::Simple(SimpleDatum::Symbol(s)) if s == "else" => {
                            if parts.len() < 2 {
                                return Err(ParserError {
                                    kind: ParserErrorKind::BadSyntax(kw.to_owned()),
                                });
                            }
                            let seq = parts
                                .iter()
                                .skip(1)
                                .map(parse)
                                .collect::<Result<Vec<_>, _>>()?;
                            clauses.push(CondClause::Else(seq));
                            else_present = true;
                        }
                        _ => {
                            if else_present {
                                return Err(ParserError {
                                    kind: ParserErrorKind::BadSyntax(kw.to_owned()),
                                });
                            }
                            let test = parse(&parts[0])?;
                            if parts.len() == 1 {
                                clauses.push(CondClause::Normal(test, vec![]));
                                continue;
                            }
                            match &parts[1] {
                                Datum::Simple(SimpleDatum::Symbol(s)) if s == "=>" => {
                                    if parts.len() != 3 {
                                        return Err(ParserError {
                                            kind: ParserErrorKind::BadSyntax(kw.to_owned()),
                                        });
                                    }
                                    let recipient = parse(&parts[2])?;
                                    clauses.push(CondClause::Arrow(test, recipient));
                                }
                                _ => {
                                    let seq = parts
                                        .iter()
                                        .skip(1)
                                        .map(parse)
                                        .collect::<Result<Vec<_>, _>>()?;
                                    clauses.push(CondClause::Normal(test, seq));
                                }
                            }
                        }
                    }
                } else {
                    return Err(ParserError {
                        kind: ParserErrorKind::BadSyntax(kw.to_owned()),
                    });
                }
            }
            Ok(Expr::DerivedExpr(DerivedExprKind::Cond(clauses)))
        }
        "else" | "kw" => Err(ParserError {
            kind: ParserErrorKind::BadSyntax(kw.to_owned()),
        }),
        "and" => Ok(Expr::DerivedExpr(DerivedExprKind::And(
            operands.map(parse).collect::<Result<Vec<_>, _>>()?,
        ))),
        "or" => Ok(Expr::DerivedExpr(DerivedExprKind::Or(
            operands.map(parse).collect::<Result<Vec<_>, _>>()?,
        ))),
        "case" => {
            todo!()
        }
        "let" => {
            todo!()
        }
        "let*" => {
            todo!()
        }
        "letrec" => {
            todo!()
        }
        "do" => {
            todo!()
        }
        "delay" => {
            todo!()
        }
        "quasiquote" => {
            todo!()
        }
        _ => todo!(),
    }
}

pub fn parse(datum: &Datum) -> Result<Expr, ParserError> {
    match datum {
        Datum::Simple(simple) => match simple {
            SimpleDatum::Boolean(b) => Ok(Expr::Literal(LiteralKind::SelfEvaluating(
                SelfEvaluatingKind::Boolean(*b),
            ))),
            SimpleDatum::Number(n) => Ok(Expr::Literal(LiteralKind::SelfEvaluating(
                SelfEvaluatingKind::Number(n.clone()),
            ))),
            SimpleDatum::Character(c) => Ok(Expr::Literal(LiteralKind::SelfEvaluating(
                SelfEvaluatingKind::Character(*c),
            ))),
            SimpleDatum::String(s) => Ok(Expr::Literal(LiteralKind::SelfEvaluating(
                SelfEvaluatingKind::String(s.clone()),
            ))),
            SimpleDatum::Symbol(symb) => match symb.as_str() {
                kw if is_keyword(kw) => Err(ParserError {
                    kind: ParserErrorKind::BadSyntax(kw.to_owned()),
                }),
                _ => Ok(Expr::Variable(symb.clone())),
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
                            let operator = parse(first);
                            let operands = list.iter().skip(1);
                            match operator {
                                Ok(se) => Ok(Expr::ProcCall {
                                    operator: Box::new(se),
                                    operands: operands.map(parse).collect::<Result<Vec<_>, _>>()?,
                                }),
                                Err(_) => {
                                    if let SimpleDatum::Symbol(kw) = simple {
                                        process_keyword(kw, operands)
                                    } else {
                                        unreachable!("keyword should be a symbol")
                                    }
                                }
                            }
                        }
                        Datum::Compound(_) => {
                            let operator = parse(first)?;
                            let rest = list
                                .iter()
                                .skip(1)
                                .map(parse)
                                .collect::<Result<Vec<_>, _>>()?;
                            Ok(Expr::ProcCall {
                                operator: Box::new(operator),
                                operands: rest,
                            })
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
                    match abbr {
                        AbbreviationPrefix::Quote => process_keyword("quote", operands),
                        AbbreviationPrefix::Quasiquote => process_keyword("quasiquote", operands),
                        AbbreviationPrefix::Unquote => process_keyword("unquote", operands),
                        AbbreviationPrefix::UnquoteSplicing => {
                            process_keyword("unquote-splicing", operands)
                        }
                    }
                }
            },
            CompoundDatum::Vector(vector) => {
                let elements = vector.iter().map(parse).collect::<Result<Vec<_>, _>>()?;
                Ok(Expr::Literal(LiteralKind::Vector(elements)))
            }
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
        assert_eq!(parse(&bool_datum!(true)), Ok(bool_expr!(true)));
        assert_eq!(parse(&int_datum!(42)), Ok(int_expr!(42)));
        assert_eq!(parse(&char_datum!('c')), Ok(char_expr!('c')));
        assert_eq!(parse(&str_datum!("foo")), Ok(str_expr!("foo")));
    }

    #[test]
    fn proc_calls() {
        assert_eq!(
            parse(&proper_list_datum![symbol_datum!("f")]),
            Ok(Expr::ProcCall {
                operator: Box::new(var_expr!("f")),
                operands: vec![],
            })
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("g"),
                int_datum!(1),
                int_datum!(2),
            ]),
            Ok(Expr::ProcCall {
                operator: Box::new(var_expr!("g")),
                operands: vec![int_expr!(1), int_expr!(2)],
            })
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
            Ok(Expr::Definition(Definition::Variable {
                name: "x".to_owned(),
                value: Box::new(int_expr!(42)),
            }))
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("define"),
                proper_list_datum![symbol_datum!("f"), symbol_datum!("x")],
                proper_list_datum![symbol_datum!("define"), symbol_datum!("y"), int_datum!(1)],
                proper_list_datum![symbol_datum!("+"), symbol_datum!("x"), symbol_datum!("y")],
            ]),
            Ok(Expr::Definition(Definition::Procedure {
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
            Ok(Expr::Definition(Definition::Procedure {
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
            Ok(Expr::Lambda {
                args: vec!["x".to_owned(), "y".to_owned()],
                rest: None,
                body: Body {
                    defs: vec![],
                    exprs: vec![Expr::ProcCall {
                        operator: Box::new(var_expr!("+")),
                        operands: vec![var_expr!("x"), var_expr!("y")],
                    }],
                }
            })
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
            Ok(Expr::Lambda {
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
            })
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("lambda"),
                symbol_datum!("args"),
                int_datum!(1)
            ]),
            Ok(Expr::Lambda {
                args: vec![],
                rest: Some("args".to_owned()),
                body: Body {
                    defs: vec![],
                    exprs: vec![int_expr!(1)],
                }
            })
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("lambda"),
                Datum::EmptyList,
                int_datum!(1)
            ]),
            Ok(Expr::Lambda {
                args: vec![],
                rest: None,
                body: Body {
                    defs: vec![],
                    exprs: vec![int_expr!(1)],
                }
            })
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
            Ok(Expr::Literal(LiteralKind::Quotation(proper_list_datum![
                symbol_datum!("a"),
                symbol_datum!("b")
            ],)))
        );

        assert_eq!(
            parse(&proper_list_datum![symbol_datum!("quote")]),
            Err(ParserError {
                kind: ParserErrorKind::BadSyntax("quote".to_owned())
            })
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
            Ok(Expr::Assignment {
                variable: "x".to_owned(),
                value: Box::new(int_expr!(1)),
            })
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
            Ok(Expr::MixedBegin(vec![
                Expr::Definition(Definition::Variable {
                    name: "x".to_owned(),
                    value: Box::new(int_expr!(42))
                }),
                str_expr!("hello"),
                int_expr!(1),
                int_expr!(2),
                int_expr!(3),
            ]))
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("begin"),
                proper_list_datum![symbol_datum!("define"), symbol_datum!("x"), int_datum!(42)],
                proper_list_datum![symbol_datum!("define"), symbol_datum!("y"), int_datum!(43)],
            ]),
            Ok(Expr::Definition(Definition::Begin(vec![
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
            Ok(Expr::DerivedExpr(DerivedExprKind::Begin(vec![])))
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("begin"),
                symbol_datum!("x"),
                symbol_datum!("y"),
                symbol_datum!("z"),
            ]),
            Ok(Expr::DerivedExpr(DerivedExprKind::Begin(vec![
                var_expr!("x"),
                var_expr!("y"),
                var_expr!("z"),
            ])))
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
            Ok(Expr::DerivedExpr(DerivedExprKind::Cond(vec![
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
            ])))
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
    }

    #[test]
    fn ands_ors() {
        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("and"),
                bool_datum!(true),
                bool_datum!(false),
            ]),
            Ok(Expr::DerivedExpr(DerivedExprKind::And(vec![
                bool_expr!(true),
                bool_expr!(false),
            ])))
        );

        assert_eq!(
            parse(&proper_list_datum![
                symbol_datum!("or"),
                bool_datum!(true),
                bool_datum!(false),
            ]),
            Ok(Expr::DerivedExpr(DerivedExprKind::Or(vec![
                bool_expr!(true),
                bool_expr!(false),
            ])))
        );

        assert_eq!(
            parse(&proper_list_datum![symbol_datum!("and")]),
            Ok(Expr::DerivedExpr(DerivedExprKind::And(vec![])))
        );

        assert_eq!(
            parse(&proper_list_datum![symbol_datum!("or")]),
            Ok(Expr::DerivedExpr(DerivedExprKind::Or(vec![])))
        );
    }
}
