use std::error::Error;
use std::fmt;
use std::{cell::RefCell, rc::Rc};

use crate::datum::SimpleDatum;
use crate::env::Env;
use crate::expr::*;
use crate::object::Object;

#[derive(Debug, PartialEq)]
pub enum EvalErrorKind {
    ZeroDivision,
    UndefinedVariable(String),
}

#[derive(Debug, PartialEq)]
pub struct EvalError {
    kind: EvalErrorKind,
}

impl Error for EvalError {}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            EvalErrorKind::ZeroDivision => write!(f, "Division by zero"),
            EvalErrorKind::UndefinedVariable(s) => write!(f, "Undefined variable: {s}"),
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
        _ => todo!(),
    }
}

fn eval_def(def: &Definition, env: Rc<RefCell<Env>>) -> Result<(), EvalError> {
    match def {
        Definition::Variable { name, value } => {
            let res = eval_expr(value, env.clone())?;
            env.borrow_mut().set(name, res);
            Ok(())
        }
        _ => todo!(),
    }
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
}
