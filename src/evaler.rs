use std::error::Error;
use std::fmt;
use std::{cell::RefCell, rc::Rc};

use crate::datum::SimpleDatum;
use crate::env::Env;
use crate::expr::*;
use crate::object::Object;
use crate::proc::{Call, Procedure, UserDefined};

#[derive(Debug, PartialEq)]
pub enum EvalErrorKind {
    ZeroDivision,
    UndefinedVariable(String),
    ContractViolation {
        expected: String,
        got: Object,
    },
    NotAProcedure(Object),
    DuplicateArg(String),
    ArityMismatch {
        expected: usize,
        got: usize,
        rest: bool,
    },
}

#[derive(Debug, PartialEq)]
pub struct EvalError {
    kind: EvalErrorKind,
}

impl EvalError {
    pub fn new(kind: EvalErrorKind) -> Self {
        Self { kind }
    }
}

impl Error for EvalError {}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            EvalErrorKind::ZeroDivision => write!(f, "Division by zero"),
            EvalErrorKind::UndefinedVariable(s) => write!(f, "Undefined variable: {s}"),
            EvalErrorKind::ContractViolation { expected, got } => {
                write!(f, "Contract violation: expected {expected}, got {got:?}")
            }
            EvalErrorKind::NotAProcedure(o) => write!(f, "Not a procedure: {o:?}"),
            EvalErrorKind::DuplicateArg(s) => write!(f, "Duplicate argument name: {s}"),
            EvalErrorKind::ArityMismatch {
                expected,
                got,
                rest,
            } => {
                let expected = if *rest {
                    format!("at least {}", expected)
                } else {
                    expected.to_string()
                };
                write!(f, "Arity mismatch: expected {expected}, got {got}")
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum EvalResult {
    Expr(Object),
    Def,
}

fn eval_expr(expr: &Expr, env: Rc<RefCell<Env>>) -> Result<Object, EvalError> {
    match expr {
        Expr::Variable(v) => match env.borrow().get(v) {
            Some(obj) => Ok(obj),
            None => Err(EvalError {
                kind: EvalErrorKind::UndefinedVariable(v.clone()),
            }),
        },
        Expr::Literal(l) => Ok(match l {
            LiteralKind::Quotation(d) => Object::from(d.clone()),
            LiteralKind::SelfEvaluating(s) => Object::Atom(match s {
                SelfEvaluatingKind::Boolean(b) => SimpleDatum::Boolean(*b),
                SelfEvaluatingKind::Character(c) => SimpleDatum::Character(*c),
                SelfEvaluatingKind::Number(n) => SimpleDatum::Number(n.clone()),
                SelfEvaluatingKind::String(s) => SimpleDatum::String(s.clone()),
            }),
        }),
        Expr::Lambda(data) => Ok(Object::Procedure(Rc::new(Procedure::UserDefined(
            UserDefined::new(data.clone(), env)?,
        )))),
        Expr::ProcCall { operator, operands } => {
            let operator = eval_expr(operator, env.clone())?;
            if let Object::Procedure(proc) = operator {
                let operands = operands
                    .iter()
                    .map(|op| eval_expr(op, env.clone()))
                    .collect::<Result<Vec<_>, _>>()?;
                proc.call(&operands)
            } else {
                Err(EvalError {
                    kind: EvalErrorKind::ContractViolation {
                        expected: "procedure".to_owned(),
                        got: operator,
                    },
                })
            }
        }
        Expr::Conditional {
            test,
            consequent,
            alternate,
        } => {
            let test = eval_expr(test, env.clone())?;
            match test {
                Object::Atom(SimpleDatum::Boolean(false)) => match alternate {
                    Some(alternate) => eval_expr(alternate, env),
                    None => Ok(Object::Void),
                },
                _ => eval_expr(consequent, env),
            }
        }
        _ => todo!(),
    }
}

fn eval_def(def: &Definition, env: Rc<RefCell<Env>>) -> Result<(), EvalError> {
    match def {
        Definition::Variable { name, value } => {
            let res = eval_expr(value, env.clone())?;
            env.borrow_mut().insert(name, res);
            Ok(())
        }
        Definition::Procedure { name, data } => {
            let proc = Object::Procedure(Rc::new(Procedure::UserDefined(UserDefined::new(
                data.clone(),
                env.clone(),
            )?)));
            env.borrow_mut().insert(name, proc);
            Ok(())
        }
        Definition::Begin(_) => todo!(),
    }
}

pub fn eval_body(body: &Body, env: Rc<RefCell<Env>>) -> Result<Object, EvalError> {
    let mut res = Object::Void;
    for eod in &body.0 {
        res = match eval(eod, env.clone())? {
            EvalResult::Expr(obj) => obj,
            EvalResult::Def => Object::Void,
        };
    }
    Ok(res)
}

pub fn eval(eod: &ExprOrDef, env: Rc<RefCell<Env>>) -> Result<EvalResult, EvalError> {
    match eod {
        ExprOrDef::Expr(expr) => Ok(EvalResult::Expr(eval_expr(expr, env)?)),
        ExprOrDef::Definition(def) => {
            eval_def(def, env)?;
            Ok(EvalResult::Def)
        }
        ExprOrDef::MixedBegin(_) => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::datum::AbbreviationPrefix;
    use crate::test_util::*;

    #[test]
    fn atoms() {
        let env = Rc::new(RefCell::new(Env::new(None)));

        assert_eq!(
            eval(&ExprOrDef::Expr(int_expr!(1)), env.clone()),
            Ok(EvalResult::Expr(atom_obj!(int_datum!(1))))
        );

        assert_eq!(
            eval(&ExprOrDef::Expr(bool_expr!(true)), env),
            Ok(EvalResult::Expr(atom_obj!(bool_datum!(true))))
        );
    }

    #[test]
    fn quotes() {
        let env = Rc::new(RefCell::new(Env::new(None)));

        assert_eq!(
            eval(
                &ExprOrDef::Expr(Expr::Literal(LiteralKind::Quotation(int_datum!(1)))),
                env.clone()
            ),
            Ok(EvalResult::Expr(atom_obj!(int_datum!(1))))
        );

        assert_eq!(
            eval(
                &ExprOrDef::Expr(Expr::Literal(LiteralKind::Quotation(symbol_datum!("a")))),
                env.clone()
            ),
            Ok(EvalResult::Expr(atom_obj!(symbol_datum!("a"))))
        );

        let d = proper_list_datum![
            proper_list_datum![symbol_datum!("a"), symbol_datum!("b")],
            improper_list_datum![
                symbol_datum!("c"),
                symbol_datum!("d");
                symbol_datum!("e")
            ],
            abbr_list_datum!(AbbreviationPrefix::Quote, symbol_datum!("f")),
            vector_datum![symbol_datum!("g"), symbol_datum!("h")]
        ];
        assert_eq!(
            eval(
                &ExprOrDef::Expr(Expr::Literal(LiteralKind::Quotation(d.clone()))),
                env
            ),
            Ok(EvalResult::Expr(Object::from(d)))
        );
    }

    #[test]
    fn primitive_calls() {
        let env = Rc::new(RefCell::new(Env::primitives()));

        assert_eq!(
            eval(
                &ExprOrDef::Expr(Expr::ProcCall {
                    operator: Box::new(var_expr!("+")),
                    operands: vec![int_expr!(1), int_expr!(2)]
                }),
                env.clone()
            ),
            Ok(EvalResult::Expr(atom_obj!(int_datum!(3))))
        );

        assert_eq!(
            eval(
                &ExprOrDef::Expr(Expr::ProcCall {
                    operator: Box::new(var_expr!("+")),
                    operands: vec![
                        int_expr!(1),
                        Expr::ProcCall {
                            operator: Box::new(var_expr!("+")),
                            operands: vec![int_expr!(2), int_expr!(3)]
                        }
                    ]
                }),
                env
            ),
            Ok(EvalResult::Expr(atom_obj!(int_datum!(6))))
        );
    }

    #[test]
    fn simple_defines() {
        let env = Rc::new(RefCell::new(Env::new(None)));

        assert_eq!(
            eval(&ExprOrDef::Expr(var_expr!("a")), env.clone()),
            Err(EvalError {
                kind: EvalErrorKind::UndefinedVariable("a".to_owned())
            })
        );

        assert_eq!(
            eval(
                &ExprOrDef::Definition(Definition::Variable {
                    name: "a".to_owned(),
                    value: Box::new(int_expr!(1))
                }),
                env.clone()
            ),
            Ok(EvalResult::Def)
        );

        assert_eq!(
            eval(&ExprOrDef::Expr(var_expr!("a")), env),
            Ok(EvalResult::Expr(atom_obj!(int_datum!(1))))
        );
    }

    #[test]
    fn proc_defines() {
        let env = Rc::new(RefCell::new(Env::new(None)));

        assert_eq!(
            eval(
                &ExprOrDef::Definition(Definition::Procedure {
                    name: "f".to_owned(),
                    data: ProcData {
                        args: vec!["x".to_owned()],
                        rest: None,
                        body: Body(vec![ExprOrDef::Expr(var_expr!("x"))]),
                    }
                }),
                env.clone()
            ),
            Ok(EvalResult::Def)
        );
        assert_eq!(
            eval(
                &ExprOrDef::Expr(Expr::ProcCall {
                    operator: Box::new(var_expr!("f")),
                    operands: vec![int_expr!(1)]
                }),
                env.clone()
            ),
            Ok(EvalResult::Expr(atom_obj!(int_datum!(1))))
        );
        assert_eq!(
            eval(
                &ExprOrDef::Expr(Expr::ProcCall {
                    operator: Box::new(var_expr!("f")),
                    operands: vec![int_expr!(1), int_expr!(2)]
                }),
                env.clone()
            ),
            Err(EvalError {
                kind: EvalErrorKind::ArityMismatch {
                    expected: 1,
                    got: 2,
                    rest: false
                }
            })
        );

        assert_eq!(
            eval(
                &ExprOrDef::Definition(Definition::Variable {
                    name: "g".to_owned(),
                    value: Box::new(Expr::Lambda(ProcData {
                        args: vec!["x".to_owned(), "y".to_owned()],
                        rest: None,
                        body: Body(vec![ExprOrDef::Expr(var_expr!("y"))]),
                    }))
                }),
                env.clone()
            ),
            Ok(EvalResult::Def)
        );
        assert_eq!(
            eval(
                &ExprOrDef::Expr(Expr::ProcCall {
                    operator: Box::new(var_expr!("g")),
                    operands: vec![int_expr!(1)]
                }),
                env.clone()
            ),
            Err(EvalError {
                kind: EvalErrorKind::ArityMismatch {
                    expected: 2,
                    got: 1,
                    rest: false
                }
            })
        );
        assert_eq!(
            eval(
                &ExprOrDef::Expr(Expr::ProcCall {
                    operator: Box::new(var_expr!("g")),
                    operands: vec![int_expr!(1), int_expr!(2)]
                }),
                env
            ),
            Ok(EvalResult::Expr(atom_obj!(int_datum!(2))))
        );
    }
}
