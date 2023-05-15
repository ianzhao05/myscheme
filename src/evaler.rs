use std::error::Error;
use std::fmt;
use std::{cell::RefCell, rc::Rc};

use crate::cont::{Acc, Cont, Frame, State};
use crate::datum::SimpleDatum;
use crate::env::Env;
use crate::expr::*;
use crate::object::{Object, ObjectRef};
use crate::primitives::ReadError;
use crate::proc::{Call, Procedure, UserDefined};
use crate::trampoline::{trampoline, Bouncer};

#[derive(Debug, PartialEq)]
pub enum EvalErrorKind {
    ZeroDivision,
    UndefinedVariable(String),
    ContractViolation {
        expected: String,
        got: ObjectRef,
    },
    NotAProcedure(ObjectRef),
    DuplicateArg(String),
    ArityMismatch {
        expected: usize,
        got: usize,
        rest: bool,
    },
    IndexOutOfBounds {
        index: usize,
        len: usize,
    },
    IOError,
    ReadError(ReadError),
    ClosedPort,
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
                write!(f, "Contract violation: expected {expected}, got {got:#}")
            }
            EvalErrorKind::NotAProcedure(o) => write!(f, "Not a procedure: {o:#}"),
            EvalErrorKind::DuplicateArg(s) => write!(f, "Duplicate argument name: {s}"),
            EvalErrorKind::ArityMismatch {
                expected,
                got,
                rest,
            } => {
                let expected = if *rest {
                    format!("expected at least {expected}, ")
                } else if *expected != usize::MAX {
                    format!("expected {expected}, ")
                } else {
                    String::new()
                };
                write!(f, "Arity mismatch: {expected}got {got}")
            }
            EvalErrorKind::IndexOutOfBounds { index, len } => {
                write!(f, "Index out of bounds: {index} >= {len}")
            }
            EvalErrorKind::IOError => write!(f, "IO error"),
            EvalErrorKind::ReadError(e) => write!(f, "read: {e}"),
            EvalErrorKind::ClosedPort => write!(f, "Attempted read or write from closed port"),
        }
    }
}

pub fn eval_expr(state: State) -> Bouncer {
    match state {
        State {
            acc: Acc::Expr(expr),
            cont,
            env,
            rib,
            stack,
        } => match &*expr {
            Expr::Variable(v) => {
                let res = match env.borrow().get(v) {
                    Some(obj) => match obj {
                        ObjectRef::Undefined => {
                            Err(EvalError::new(EvalErrorKind::UndefinedVariable(v.clone())))
                        }
                        _ => Ok(obj),
                    },
                    None => Err(EvalError::new(EvalErrorKind::UndefinedVariable(v.clone()))),
                };
                Bouncer::Bounce(State {
                    acc: Acc::Obj(res),
                    cont,
                    env,
                    rib,
                    stack,
                })
            }
            Expr::Literal(l) => Bouncer::Bounce(State {
                acc: Acc::Obj(Ok(match l {
                    LiteralKind::Quotation(d) => ObjectRef::from(d.clone()),
                    LiteralKind::SelfEvaluating(s) => ObjectRef::new(Object::Atom(match s {
                        SelfEvaluatingKind::Boolean(b) => SimpleDatum::Boolean(*b),
                        SelfEvaluatingKind::Character(c) => SimpleDatum::Character(*c),
                        SelfEvaluatingKind::Number(n) => SimpleDatum::Number(n.clone()),
                        SelfEvaluatingKind::String(s) => SimpleDatum::String(s.clone()),
                    })),
                })),
                cont,
                env,
                rib,
                stack,
            }),
            Expr::ProcCall { operator, operands } => {
                let tail = cont.is_tail();
                let (acc, ncont) = match operands.first() {
                    Some(first) => {
                        let mut bcont = Rc::new(Cont::Proc {
                            operator: operator.clone(),
                        });
                        for next in operands.iter().skip(1).rev().cloned() {
                            bcont = Rc::new(Cont::Argument { next, cont: bcont });
                        }
                        (Acc::Expr(first.clone()), bcont)
                    }
                    None => (Acc::Expr(operator.clone()), Rc::new(Cont::Apply)),
                };
                Bouncer::Bounce(State {
                    acc,
                    cont: ncont,
                    env: env.clone(),
                    rib: Vec::new(),
                    stack: if tail {
                        stack
                    } else {
                        Some(Rc::new(Frame {
                            cont,
                            env,
                            rib,
                            next: stack,
                        }))
                    },
                })
            }
            Expr::Lambda(data) => Bouncer::Bounce(State {
                acc: Acc::Obj(match UserDefined::new(data.clone(), env.clone()) {
                    Ok(proc) => Ok(ObjectRef::new(Object::Procedure(Procedure::UserDefined(
                        proc,
                    )))),
                    Err(e) => Err(e),
                }),
                cont,
                env,
                rib,
                stack,
            }),
            Expr::Conditional {
                test,
                consequent,
                alternate,
            } => Bouncer::Bounce(State {
                acc: Acc::Expr(test.clone()),
                cont: Rc::new(Cont::Conditional {
                    consequent: consequent.clone(),
                    alternate: alternate.clone(),
                    cont,
                }),
                env,
                rib,
                stack,
            }),
            Expr::Assignment { variable, value } => Bouncer::Bounce(State {
                acc: Acc::Expr(value.clone()),
                cont: Rc::new(Cont::Assignment {
                    variable: variable.clone(),
                    cont,
                }),
                env,
                rib,
                stack,
            }),
            Expr::Begin(seq) => match seq.first() {
                Some(first) => {
                    let mut bcont = cont;
                    for next in seq.iter().skip(1).rev().cloned() {
                        bcont = Rc::new(Cont::Begin { next, cont: bcont });
                    }
                    Bouncer::Bounce(State {
                        acc: Acc::Expr(first.clone()),
                        cont: bcont,
                        env,
                        rib,
                        stack,
                    })
                }
                None => Bouncer::Bounce(State {
                    acc: Acc::Obj(Ok(ObjectRef::Void)),
                    cont,
                    env,
                    rib,
                    stack,
                }),
            },
            Expr::SimpleLet { arg, value, body } => Bouncer::Bounce(State {
                acc: Acc::Expr(value.clone()),
                cont: Rc::new(Cont::SimpleLet {
                    arg: arg.clone(),
                    body: body.clone(),
                    cont,
                }),
                env,
                rib,
                stack,
            }),
            Expr::Quasiquotation(_) => todo!(),
            Expr::Undefined => Bouncer::Bounce(State {
                acc: Acc::Obj(Ok(ObjectRef::Undefined)),
                cont,
                env,
                rib,
                stack,
            }),
        },
        State {
            acc: Acc::Obj(obj),
            cont,
            env,
            mut rib,
            stack,
        } => match obj {
            Ok(obj) => match &*cont {
                Cont::Return => match stack {
                    Some(frame) => Bouncer::Bounce(State {
                        acc: Acc::Obj(Ok(obj)),
                        cont: frame.cont.clone(),
                        env: frame.env.clone(),
                        rib: frame.rib.clone(),
                        stack: frame.next.clone(),
                    }),
                    None => Bouncer::Land(Ok(obj)),
                },
                Cont::Apply => match obj.try_deref() {
                    Some(o) => match o {
                        Object::Procedure(p) => p.call(State {
                            acc: Acc::Obj(Ok(ObjectRef::Undefined)),
                            cont,
                            env,
                            rib,
                            stack,
                        }),
                        _ => Bouncer::Land(Err(EvalError::new(EvalErrorKind::NotAProcedure(obj)))),
                    },
                    None => Bouncer::Land(Err(EvalError::new(EvalErrorKind::NotAProcedure(obj)))),
                },
                Cont::Proc { operator } => {
                    rib.push(obj);
                    Bouncer::Bounce(State {
                        acc: Acc::Expr(operator.clone()),
                        cont: Rc::new(Cont::Apply),
                        env,
                        rib,
                        stack,
                    })
                }
                Cont::Argument { next, cont } => {
                    rib.push(obj);
                    Bouncer::Bounce(State {
                        acc: Acc::Expr(next.clone()),
                        cont: cont.clone(),
                        env,
                        rib,
                        stack,
                    })
                }
                Cont::SimpleLet { arg, body, cont } => {
                    env.borrow_mut().insert(arg, obj);
                    Bouncer::Bounce(State {
                        acc: Acc::Expr(body.clone()),
                        cont: cont.clone(),
                        env,
                        rib,
                        stack,
                    })
                }
                Cont::Conditional {
                    consequent,
                    alternate,
                    cont,
                } => {
                    let test = match obj.try_deref() {
                        Some(o) => match o {
                            Object::Atom(SimpleDatum::Boolean(false)) => false,
                            _ => true,
                        },
                        None => true,
                    };
                    if test {
                        Bouncer::Bounce(State {
                            acc: Acc::Expr(consequent.clone()),
                            cont: cont.clone(),
                            env,
                            rib,
                            stack,
                        })
                    } else {
                        match alternate {
                            Some(alternate) => Bouncer::Bounce(State {
                                acc: Acc::Expr(alternate.clone()),
                                cont: cont.clone(),
                                env,
                                rib,
                                stack,
                            }),
                            None => Bouncer::Bounce(State {
                                acc: Acc::Obj(Ok(ObjectRef::Void)),
                                cont: cont.clone(),
                                env,
                                rib,
                                stack,
                            }),
                        }
                    }
                }
                Cont::Assignment { variable, cont } => {
                    if !env.borrow_mut().set(variable, obj) {
                        Bouncer::Land(Err(EvalError::new(EvalErrorKind::UndefinedVariable(
                            variable.clone(),
                        ))))
                    } else {
                        Bouncer::Bounce(State {
                            acc: Acc::Obj(Ok(ObjectRef::Void)),
                            cont: cont.clone(),
                            env,
                            rib,
                            stack,
                        })
                    }
                }
                Cont::Begin { next, cont } => Bouncer::Bounce(State {
                    acc: Acc::Expr(next.clone()),
                    cont: cont.clone(),
                    env,
                    rib,
                    stack,
                }),
                Cont::Define { name, cont } => {
                    env.borrow_mut().insert(name, obj);
                    Bouncer::Bounce(State {
                        acc: Acc::Obj(Ok(ObjectRef::Void)),
                        cont: cont.clone(),
                        env,
                        rib,
                        stack,
                    })
                }
                Cont::DefineProc { name, data, cont } => {
                    let proc = Object::Procedure(Procedure::UserDefined(
                        match UserDefined::new(data.clone(), env.clone()) {
                            Ok(p) => p,
                            Err(e) => return Bouncer::Land(Err(e)),
                        },
                    ));
                    env.borrow_mut().insert(name, ObjectRef::new(proc));
                    Bouncer::Bounce(State {
                        acc: Acc::Obj(Ok(ObjectRef::Void)),
                        cont: cont.clone(),
                        env,
                        rib,
                        stack,
                    })
                }
            },
            Err(e) => Bouncer::Land(Err(e)),
        },
    }
}

fn eval_def(def: Rc<Definition>, env: Rc<RefCell<Env>>) -> Result<ObjectRef, EvalError> {
    match &*def {
        Definition::Variable { name, value } => trampoline(Bouncer::Bounce(State::new(
            Acc::Expr(value.clone()),
            env.clone(),
            Some(Rc::new(Cont::Define {
                name: name.clone(),
                cont: Rc::new(Cont::Return),
            })),
        ))),
        Definition::Procedure { name, data } => {
            let proc = Object::Procedure(Procedure::UserDefined(UserDefined::new(
                data.clone(),
                env.clone(),
            )?));
            env.borrow_mut().insert(name, ObjectRef::new(proc));
            Ok(ObjectRef::Void)
        }
        Definition::Begin(defs) => trampoline(Bouncer::Bounce(State::new(
            Acc::Obj(Ok(ObjectRef::Undefined)),
            env.clone(),
            Some(Cont::from_defs(defs, None)),
        ))),
    }
}

pub fn eval(eod: ExprOrDef, env: Rc<RefCell<Env>>) -> Result<ObjectRef, EvalError> {
    match eod {
        ExprOrDef::Expr(expr) => Ok(trampoline(Bouncer::Bounce(State::new(
            Acc::Expr(expr),
            env,
            None,
        )))?),
        ExprOrDef::Definition(def) => eval_def(def, env),
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
            eval(ExprOrDef::new_expr(int_expr!(1)), env.clone()),
            Ok(ObjectRef::new(atom_obj!(int_datum!(1))))
        );

        assert_eq!(
            eval(ExprOrDef::new_expr(bool_expr!(true)), env),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(true))))
        );
    }

    #[test]
    fn quotes() {
        let env = Rc::new(RefCell::new(Env::new(None)));

        assert_eq!(
            eval(
                ExprOrDef::new_expr(Expr::Literal(LiteralKind::Quotation(int_datum!(1)))),
                env.clone()
            ),
            Ok(ObjectRef::new(atom_obj!(int_datum!(1))))
        );

        assert_eq!(
            eval(
                ExprOrDef::new_expr(Expr::Literal(LiteralKind::Quotation(symbol_datum!("a")))),
                env.clone()
            ),
            Ok(ObjectRef::new(atom_obj!(symbol_datum!("a"))))
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
                ExprOrDef::new_expr(Expr::Literal(LiteralKind::Quotation(d.clone()))),
                env
            )
            .unwrap(),
            *ObjectRef::from(d)
        );
    }

    #[test]
    fn primitive_calls() {
        let env = Env::primitives();

        assert_eq!(
            eval(
                ExprOrDef::new_expr(Expr::ProcCall {
                    operator: Rc::new(var_expr!("+")),
                    operands: vec_rc![int_expr!(1), int_expr!(2)]
                }),
                env.clone()
            ),
            Ok(ObjectRef::new(atom_obj!(int_datum!(3))))
        );

        assert_eq!(
            eval(
                ExprOrDef::new_expr(Expr::ProcCall {
                    operator: Rc::new(var_expr!("+")),
                    operands: vec_rc![
                        int_expr!(1),
                        Expr::ProcCall {
                            operator: Rc::new(var_expr!("+")),
                            operands: vec_rc![int_expr!(2), int_expr!(3)]
                        }
                    ]
                }),
                env
            ),
            Ok(ObjectRef::new(atom_obj!(int_datum!(6))))
        );
    }

    #[test]
    fn simple_defines() {
        let env = Rc::new(RefCell::new(Env::new(None)));

        assert_eq!(
            eval(ExprOrDef::new_expr(var_expr!("a")), env.clone()),
            Err(EvalError::new(EvalErrorKind::UndefinedVariable(
                "a".to_owned()
            )))
        );
        assert_eq!(
            eval(
                ExprOrDef::new_def(Definition::Variable {
                    name: "a".to_owned(),
                    value: Rc::new(int_expr!(1))
                }),
                env.clone()
            ),
            Ok(ObjectRef::Void)
        );
        assert_eq!(
            eval(ExprOrDef::new_expr(var_expr!("a")), env.clone()),
            Ok(ObjectRef::new(atom_obj!(int_datum!(1))))
        );

        assert_eq!(
            eval(
                ExprOrDef::new_def(Definition::Begin(vec_rc![
                    Definition::Variable {
                        name: "b".to_owned(),
                        value: Rc::new(int_expr!(2))
                    },
                    Definition::Variable {
                        name: "c".to_owned(),
                        value: Rc::new(int_expr!(3))
                    },
                ])),
                env.clone()
            ),
            Ok(ObjectRef::Void)
        );
        assert_eq!(
            eval(ExprOrDef::new_expr(var_expr!("b")), env.clone()),
            Ok(ObjectRef::new(atom_obj!(int_datum!(2))))
        );
        assert_eq!(
            eval(ExprOrDef::new_expr(var_expr!("c")), env),
            Ok(ObjectRef::new(atom_obj!(int_datum!(3))))
        );
    }

    #[test]
    fn assignments() {
        let env = Rc::new(RefCell::new(Env::new(None)));
        assert_eq!(
            eval(
                ExprOrDef::new_def(Definition::Variable {
                    name: "a".to_owned(),
                    value: Rc::new(int_expr!(1))
                }),
                env.clone()
            ),
            Ok(ObjectRef::Void)
        );
        assert_eq!(
            eval(
                ExprOrDef::new_expr(Expr::Assignment {
                    variable: "a".to_owned(),
                    value: Rc::new(int_expr!(2))
                }),
                env.clone()
            ),
            Ok(ObjectRef::Void)
        );
        assert_eq!(
            eval(ExprOrDef::new_expr(var_expr!("a")), env),
            Ok(ObjectRef::new(atom_obj!(int_datum!(2))))
        );
    }

    #[test]
    fn proc_defines() {
        let env = Rc::new(RefCell::new(Env::new(None)));

        assert_eq!(
            eval(
                ExprOrDef::new_def(Definition::Procedure {
                    name: "f".to_owned(),
                    data: Rc::new(ProcData {
                        args: vec!["x".to_owned()],
                        rest: None,
                        body: vec![ExprOrDef::new_expr(var_expr!("x"))],
                    })
                }),
                env.clone()
            ),
            Ok(ObjectRef::Void)
        );
        assert_eq!(
            eval(
                ExprOrDef::new_expr(Expr::ProcCall {
                    operator: Rc::new(var_expr!("f")),
                    operands: vec_rc![int_expr!(1)]
                }),
                env.clone()
            ),
            Ok(ObjectRef::new(atom_obj!(int_datum!(1))))
        );
        assert_eq!(
            eval(
                ExprOrDef::new_expr(Expr::ProcCall {
                    operator: Rc::new(var_expr!("f")),
                    operands: vec_rc![int_expr!(1), int_expr!(2)]
                }),
                env.clone()
            ),
            Err(EvalError::new(EvalErrorKind::ArityMismatch {
                expected: 1,
                got: 2,
                rest: false
            }))
        );

        assert_eq!(
            eval(
                ExprOrDef::new_def(Definition::Variable {
                    name: "g".to_owned(),
                    value: Rc::new(Expr::Lambda(Rc::new(ProcData {
                        args: vec!["x".to_owned(), "y".to_owned()],
                        rest: None,
                        body: vec![ExprOrDef::new_expr(var_expr!("y"))],
                    })))
                }),
                env.clone()
            ),
            Ok(ObjectRef::Void)
        );
        assert_eq!(
            eval(
                ExprOrDef::new_expr(Expr::ProcCall {
                    operator: Rc::new(var_expr!("g")),
                    operands: vec_rc![int_expr!(1)]
                }),
                env.clone()
            ),
            Err(EvalError::new(EvalErrorKind::ArityMismatch {
                expected: 2,
                got: 1,
                rest: false
            }))
        );
        assert_eq!(
            eval(
                ExprOrDef::new_expr(Expr::ProcCall {
                    operator: Rc::new(var_expr!("g")),
                    operands: vec_rc![int_expr!(1), int_expr!(2)]
                }),
                env
            ),
            Ok(ObjectRef::new(atom_obj!(int_datum!(2))))
        );
    }

    #[test]
    fn begins() {
        let env = Rc::new(RefCell::new(Env::new(None)));

        assert_eq!(
            eval(
                ExprOrDef::new_expr(Expr::Begin(vec_rc![
                    int_expr!(1),
                    int_expr!(2),
                    int_expr!(3)
                ])),
                env.clone()
            ),
            Ok(ObjectRef::new(atom_obj!(int_datum!(3))))
        );

        assert_eq!(
            eval(ExprOrDef::new_expr(Expr::Begin(Vec::new())), env),
            Ok(ObjectRef::Void)
        );
    }
}
