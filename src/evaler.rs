use std::error::Error;
use std::fmt;
use std::{cell::RefCell, rc::Rc};

use crate::datum::SimpleDatum;
use crate::env::Env;
use crate::expr::*;
use crate::object::{Object, ObjectRef};
use crate::proc::{Procedure, UserDefined};
use crate::trampoline::{trampoline, Bouncer, BouncerFn};

#[derive(Debug, PartialEq)]
pub enum EvalErrorKind {
    ZeroDivision,
    UndefinedVariable(String),
    ContractViolation {
        expected: String,
        got: ObjectRef,
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
    Expr(ObjectRef),
    Def,
}

impl EvalResult {
    pub fn unwrap_expr(self) -> ObjectRef {
        match self {
            EvalResult::Expr(o) => o,
            _ => panic!("Expected an expression"),
        }
    }
}

pub fn eval_expr<'a>(expr: &'a Expr, env: Rc<RefCell<Env>>, k: BouncerFn<'a>) -> Bouncer<'a> {
    match expr {
        Expr::Variable(v) => {
            let benv = env.borrow();
            let res = match benv.get(v) {
                Some(obj) => Ok(obj),
                None => Err(EvalError {
                    kind: EvalErrorKind::UndefinedVariable(v.clone()),
                }),
            };
            drop(benv);
            k(res)
        }
        Expr::Literal(l) => k(Ok(match l {
            LiteralKind::Quotation(d) => ObjectRef::from(d.clone()),
            LiteralKind::SelfEvaluating(s) => ObjectRef::new(Object::Atom(match s {
                SelfEvaluatingKind::Boolean(b) => SimpleDatum::Boolean(*b),
                SelfEvaluatingKind::Character(c) => SimpleDatum::Character(*c),
                SelfEvaluatingKind::Number(n) => SimpleDatum::Number(n.clone()),
                SelfEvaluatingKind::String(s) => SimpleDatum::String(s.clone()),
            })),
        })),
        Expr::Lambda(data) => k(match UserDefined::new(data.clone(), env) {
            Ok(proc) => Ok(ObjectRef::new(Object::Procedure(Procedure::UserDefined(
                proc,
            )))),
            Err(e) => Err(e),
        }),
        Expr::ProcCall { operator, operands } => {
            Bouncer::BounceCall(operator, operands, env, k, Vec::new())
        }
        Expr::Conditional {
            test,
            consequent,
            alternate,
        } => eval_expr(
            test,
            env.clone(),
            Box::new(move |obj| match obj {
                Ok(obj) => {
                    let res = match obj.try_deref() {
                        Some(obj) => match *obj {
                            Object::Atom(SimpleDatum::Boolean(false)) => match alternate {
                                Some(alternate) => alternate,
                                None => return k(Ok(ObjectRef::Void)),
                            },
                            _ => consequent,
                        },
                        None => match alternate {
                            Some(alternate) => alternate,
                            None => return k(Ok(ObjectRef::Void)),
                        },
                    };
                    eval_expr(res, env.clone(), k)
                }
                e => k(e),
            }),
        ),
        Expr::Assignment { variable, value } => eval_expr(
            value,
            env.clone(),
            Box::new(move |val| match val {
                Ok(obj) => {
                    println!("Setting {} to {:?}", variable, obj);
                    let mut benv = env.borrow_mut();
                    let res = match benv.set(variable, obj) {
                        true => Ok(ObjectRef::Void),
                        false => Err(EvalError {
                            kind: EvalErrorKind::UndefinedVariable(variable.clone()),
                        }),
                    };
                    drop(benv);
                    k(res)
                }
                e => k(e),
            }),
        ),
        _ => todo!(),
    }
}

pub fn eval_proc_call<'a>(
    proc: &'a Expr,
    exprs: &'a [Expr],
    env: Rc<RefCell<Env>>,
    k: BouncerFn<'a>,
    mut acc: Vec<ObjectRef>,
) -> Bouncer<'a> {
    match exprs.split_first() {
        Some((first, rest)) => Bouncer::Bounce(
            first,
            env.clone(),
            Box::new(move |obj| match obj {
                Ok(obj) => {
                    acc.push(obj);
                    Bouncer::BounceCall(proc, rest, env.clone(), k, acc)
                }
                e => k(e),
            }),
        ),
        None => Bouncer::Bounce(
            proc,
            env,
            Box::new(move |objres| match objres {
                Ok(objref) => {
                    let err = || EvalError {
                        kind: EvalErrorKind::ContractViolation {
                            expected: "procedure".to_owned(),
                            got: objref.clone(),
                        },
                    };
                    match &objref {
                        ObjectRef::Object(obj) => match &**obj {
                            Object::Procedure(_) => {
                                // p.call(acc, k, objref.pin())
                                unsafe {
                                    let p_ptr = Rc::as_ptr(obj);
                                    match &*p_ptr {
                                        Object::Procedure(p) => p.call(acc, k, objref.pin()),
                                        _ => unreachable!(),
                                    }
                                }
                            }
                            _ => k(Err(err())),
                        },
                        _ => k(Err(err())),
                    }
                }
                e => k(e),
            }),
        ),
    }
}

pub fn eval_def(def: &Definition, env: Rc<RefCell<Env>>) -> Result<(), EvalError> {
    match def {
        Definition::Variable { name, value } => {
            let res = trampoline(eval_expr(value, env.clone(), Box::new(Bouncer::Land)))?;
            env.borrow_mut().insert(name, res);
            Ok(())
        }
        Definition::Procedure { name, data } => {
            let proc = Object::Procedure(Procedure::UserDefined(UserDefined::new(
                data.clone(),
                env.clone(),
            )?));
            env.borrow_mut().insert(name, ObjectRef::new(proc));
            Ok(())
        }
        Definition::Begin(defs) => {
            for def in defs {
                eval_def(def, env.clone())?;
            }
            Ok(())
        }
    }
}

pub fn eval_body(body: &Body, env: Rc<RefCell<Env>>) -> Result<ObjectRef, EvalError> {
    let mut res = ObjectRef::Void;
    for eod in &body.0 {
        res = match eval(eod, env.clone())? {
            EvalResult::Expr(obj) => obj,
            EvalResult::Def => ObjectRef::Void,
        };
    }
    Ok(res)
}

pub fn eval(eod: &ExprOrDef, env: Rc<RefCell<Env>>) -> Result<EvalResult, EvalError> {
    match eod {
        ExprOrDef::Expr(expr) => Ok(EvalResult::Expr(trampoline(Bouncer::Bounce(
            expr,
            env,
            Box::new(Bouncer::Land),
        ))?)),
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
            Ok(EvalResult::Expr(ObjectRef::new(atom_obj!(int_datum!(1)))))
        );

        assert_eq!(
            eval(&ExprOrDef::Expr(bool_expr!(true)), env),
            Ok(EvalResult::Expr(ObjectRef::new(atom_obj!(bool_datum!(
                true
            )))))
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
            Ok(EvalResult::Expr(ObjectRef::new(atom_obj!(int_datum!(1)))))
        );

        assert_eq!(
            eval(
                &ExprOrDef::Expr(Expr::Literal(LiteralKind::Quotation(symbol_datum!("a")))),
                env.clone()
            ),
            Ok(EvalResult::Expr(ObjectRef::new(atom_obj!(symbol_datum!(
                "a"
            )))))
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
            *eval(
                &ExprOrDef::Expr(Expr::Literal(LiteralKind::Quotation(d.clone()))),
                env
            )
            .unwrap()
            .unwrap_expr(),
            *ObjectRef::from(d)
        );
    }

    #[test]
    fn primitive_calls() {
        let env = Env::primitives();

        assert_eq!(
            eval(
                &ExprOrDef::Expr(Expr::ProcCall {
                    operator: Box::new(var_expr!("+")),
                    operands: vec![int_expr!(1), int_expr!(2)]
                }),
                env.clone()
            ),
            Ok(EvalResult::Expr(ObjectRef::new(atom_obj!(int_datum!(3)))))
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
            Ok(EvalResult::Expr(ObjectRef::new(atom_obj!(int_datum!(6)))))
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
            eval(&ExprOrDef::Expr(var_expr!("a")), env.clone()),
            Ok(EvalResult::Expr(ObjectRef::new(atom_obj!(int_datum!(1)))))
        );

        assert_eq!(
            eval(
                &ExprOrDef::Definition(Definition::Begin(vec![
                    Definition::Variable {
                        name: "b".to_owned(),
                        value: Box::new(int_expr!(2))
                    },
                    Definition::Variable {
                        name: "c".to_owned(),
                        value: Box::new(int_expr!(3))
                    },
                ])),
                env.clone()
            ),
            Ok(EvalResult::Def)
        );
        assert_eq!(
            eval(&ExprOrDef::Expr(var_expr!("b")), env.clone()),
            Ok(EvalResult::Expr(ObjectRef::new(atom_obj!(int_datum!(2)))))
        );
        assert_eq!(
            eval(&ExprOrDef::Expr(var_expr!("c")), env),
            Ok(EvalResult::Expr(ObjectRef::new(atom_obj!(int_datum!(3)))))
        );
    }

    #[test]
    fn assignments() {
        let env = Rc::new(RefCell::new(Env::new(None)));
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
            eval(
                &ExprOrDef::Expr(Expr::Assignment {
                    variable: "a".to_owned(),
                    value: Box::new(int_expr!(2))
                }),
                env.clone()
            ),
            Ok(EvalResult::Expr(ObjectRef::Void))
        );
        assert_eq!(
            eval(&ExprOrDef::Expr(var_expr!("a")), env),
            Ok(EvalResult::Expr(ObjectRef::new(atom_obj!(int_datum!(2)))))
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
            Ok(EvalResult::Expr(ObjectRef::new(atom_obj!(int_datum!(1)))))
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
            Ok(EvalResult::Expr(ObjectRef::new(atom_obj!(int_datum!(2)))))
        );
    }
}
