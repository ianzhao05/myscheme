use std::error::Error;
use std::fmt;
use std::rc::Rc;

use crate::cont::{Acc, Cont, Frame, State, Wind, WindsOp};
use crate::datum::SimpleDatum;
use crate::env::Env;
use crate::expr::*;
use crate::interner::Symbol;
use crate::object::{Object, ObjectRef};
use crate::parser::ParserError;
use crate::port::ReadError;
use crate::proc::{Call, Procedure, UserDefined};
use crate::trampoline::{trampoline, Bouncer};

#[derive(Debug, PartialEq)]
pub enum EvalErrorKind {
    ZeroDivision,
    UndefinedVariable(Symbol),
    ContractViolation {
        expected: &'static str,
        got: ObjectRef,
    },
    NotAProcedure(ObjectRef),
    DuplicateArg(Symbol),
    ArityMismatch {
        expected: usize,
        max_expected: usize,
        got: usize,
    },
    IndexOutOfBounds {
        index: usize,
        len: usize,
    },
    ReadError(ReadError),
    ParserError(ParserError),
    IOError,
    ClosedPort,
    InexactNonDecimalFormat,
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
                max_expected,
                got,
            } => {
                write!(f, "Arity mismatch: ")?;
                if *max_expected == usize::MAX {
                    write!(f, "expected at least {expected}, ")?;
                } else if expected == max_expected {
                    write!(f, "expected {expected}, ")?;
                } else {
                    write!(f, "expected between {expected} and {max_expected}, ")?;
                }
                write!(f, "got {got}")
            }
            EvalErrorKind::IndexOutOfBounds { index, len } => {
                write!(f, "Index out of bounds: {index} >= {len}")
            }
            EvalErrorKind::ReadError(e) => write!(f, "read: {e}"),
            EvalErrorKind::ParserError(e) => write!(f, "parse: {e}"),
            EvalErrorKind::IOError => write!(f, "IO error"),
            EvalErrorKind::ClosedPort => write!(f, "Attempted read or write from closed port"),
            EvalErrorKind::InexactNonDecimalFormat => {
                write!(f, "Inexact numbers must be printed in decimal")
            }
        }
    }
}

pub(crate) fn eval_expr(state: State) -> Bouncer {
    match state {
        State {
            acc: Acc::Expr(expr),
            cont,
            env,
            rib,
            stack,
            winds,
        } => {
            match &*expr {
                Expr::Variable(v) => {
                    let res = match env.get(*v) {
                        Some(obj) => match obj {
                            ObjectRef::Undefined => {
                                Err(EvalError::new(EvalErrorKind::UndefinedVariable(*v)))
                            }
                            _ => Ok(obj),
                        },
                        None => Err(EvalError::new(EvalErrorKind::UndefinedVariable(*v))),
                    };
                    Bouncer::Bounce(State {
                        acc: Acc::Obj(res),
                        cont,
                        env,
                        rib,
                        stack,
                        winds,
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
                    winds,
                }),
                Expr::ProcCall { operator, operands } => {
                    let tail = cont.is_tail();
                    let (acc, ncont) = match operands.split_first() {
                        Some((first, rest)) => (
                            Acc::Expr(first.clone()),
                            rest.iter().cloned().rfold(
                                Rc::new(Cont::Proc {
                                    operator: operator.clone(),
                                }),
                                |cont, next| Rc::new(Cont::Argument { next, cont }),
                            ),
                        ),
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
                        winds,
                    })
                }
                Expr::Lambda(data) => Bouncer::Bounce(State {
                    acc: Acc::Obj(UserDefined::new(data.clone(), env.clone()).map(|proc| {
                        ObjectRef::new(Object::Procedure(Procedure::UserDefined(proc)))
                    })),
                    cont,
                    env,
                    rib,
                    stack,
                    winds,
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
                    winds,
                }),
                Expr::Assignment { variable, value } => Bouncer::Bounce(State {
                    acc: Acc::Expr(value.clone()),
                    cont: Rc::new(Cont::Assignment {
                        variable: *variable,
                        cont,
                    }),
                    env,
                    rib,
                    stack,
                    winds,
                }),
                Expr::Begin(seq) => match seq.split_first() {
                    Some((first, rest)) => Bouncer::Bounce(State {
                        acc: Acc::Expr(first.clone()),
                        cont: rest
                            .iter()
                            .cloned()
                            .rfold(cont, |cont, next| Rc::new(Cont::Begin { next, cont })),
                        env,
                        rib,
                        stack,
                        winds,
                    }),
                    None => Bouncer::Bounce(State {
                        acc: Acc::Obj(Ok(ObjectRef::Void)),
                        cont,
                        env,
                        rib,
                        stack,
                        winds,
                    }),
                },
                Expr::SimpleLet { arg, value, body } => Bouncer::Bounce(State {
                    acc: Acc::Expr(value.clone()),
                    cont: Rc::new(Cont::SimpleLet {
                        arg: *arg,
                        cont: Cont::from_body(body, Some(cont)),
                    }),
                    env,
                    rib,
                    stack,
                    winds,
                }),
                Expr::Undefined => Bouncer::Bounce(State {
                    acc: Acc::Obj(Ok(ObjectRef::Undefined)),
                    cont,
                    env,
                    rib,
                    stack,
                    winds,
                }),
            }
        }
        State {
            acc: Acc::Obj(obj),
            cont,
            env,
            mut rib,
            stack,
            winds,
        } => match obj {
            Ok(obj) => match &*cont {
                Cont::Return => match stack {
                    Some(frame) => Bouncer::Bounce(State {
                        acc: Acc::Obj(Ok(obj)),
                        cont: frame.cont.clone(),
                        env: frame.env.clone(),
                        rib: frame.rib.clone(),
                        stack: frame.next.clone(),
                        winds,
                    }),
                    None => Bouncer::Land(Ok(obj)),
                },
                Cont::ReturnVal { val } => match stack {
                    Some(frame) => Bouncer::Bounce(State {
                        acc: Acc::Obj(Ok(val.clone())),
                        cont: frame.cont.clone(),
                        env: frame.env.clone(),
                        rib: frame.rib.clone(),
                        stack: frame.next.clone(),
                        winds,
                    }),
                    None => Bouncer::Land(Ok(val.clone())),
                },
                Cont::Apply => match obj.try_deref() {
                    Some(Object::Procedure(p)) => p.call(State {
                        acc: Acc::Obj(Ok(ObjectRef::Undefined)),
                        cont,
                        env,
                        rib,
                        stack,
                        winds,
                    }),
                    _ => Bouncer::Land(Err(EvalError::new(EvalErrorKind::NotAProcedure(obj)))),
                },
                Cont::Proc { operator } => {
                    rib.push(obj);
                    Bouncer::Bounce(State {
                        acc: Acc::Expr(operator.clone()),
                        cont: Rc::new(Cont::Apply),
                        env,
                        rib,
                        stack,
                        winds,
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
                        winds,
                    })
                }
                Cont::SimpleLet { arg, cont } => {
                    env.insert(*arg, obj);
                    Bouncer::Bounce(State {
                        acc: Acc::Obj(Ok(ObjectRef::Undefined)),
                        cont: cont.clone(),
                        env,
                        rib,
                        stack,
                        winds,
                    })
                }
                Cont::Conditional {
                    consequent,
                    alternate,
                    cont,
                } => {
                    let test = !matches!(
                        obj.try_deref(),
                        Some(Object::Atom(SimpleDatum::Boolean(false))),
                    );
                    if test {
                        Bouncer::Bounce(State {
                            acc: Acc::Expr(consequent.clone()),
                            cont: cont.clone(),
                            env,
                            rib,
                            stack,
                            winds,
                        })
                    } else {
                        match alternate {
                            Some(alternate) => Bouncer::Bounce(State {
                                acc: Acc::Expr(alternate.clone()),
                                cont: cont.clone(),
                                env,
                                rib,
                                stack,
                                winds,
                            }),
                            None => Bouncer::Bounce(State {
                                acc: Acc::Obj(Ok(ObjectRef::Void)),
                                cont: cont.clone(),
                                env,
                                rib,
                                stack,
                                winds,
                            }),
                        }
                    }
                }
                Cont::Assignment { variable, cont } => {
                    if env.set(*variable, obj) {
                        Bouncer::Bounce(State {
                            acc: Acc::Obj(Ok(ObjectRef::Void)),
                            cont: cont.clone(),
                            env,
                            rib,
                            stack,
                            winds,
                        })
                    } else {
                        Bouncer::Land(Err(EvalError::new(EvalErrorKind::UndefinedVariable(
                            *variable,
                        ))))
                    }
                }
                Cont::Begin { next, cont } => Bouncer::Bounce(State {
                    acc: Acc::Expr(next.clone()),
                    cont: cont.clone(),
                    env,
                    rib,
                    stack,
                    winds,
                }),
                Cont::Define { name, cont } => {
                    env.insert(*name, obj);
                    Bouncer::Bounce(State {
                        acc: Acc::Obj(Ok(ObjectRef::Void)),
                        cont: cont.clone(),
                        env,
                        rib,
                        stack,
                        winds,
                    })
                }
                Cont::DefineProc { name, data, cont } => {
                    let proc = Object::Procedure(Procedure::UserDefined(
                        match UserDefined::new(data.clone(), env.clone()) {
                            Ok(p) => p,
                            Err(e) => return Bouncer::Land(Err(e)),
                        },
                    ));
                    env.insert(*name, ObjectRef::new(proc));
                    Bouncer::Bounce(State {
                        acc: Acc::Obj(Ok(ObjectRef::Void)),
                        cont: cont.clone(),
                        env,
                        rib,
                        stack,
                        winds,
                    })
                }
                Cont::DoWinds { from, to, cont } => {
                    let (ncont, nwinds) = match (from, to) {
                        (None, Some(sto)) => (
                            Rc::new(Cont::DoWinds {
                                from: None,
                                to: sto.next.clone(),
                                cont: Rc::new(Cont::ApplyThunk {
                                    thunk: sto.in_thunk.clone(),
                                    cont: Rc::new(Cont::WindsOp {
                                        op: WindsOp::Set(to.clone()),
                                        cont: cont.clone(),
                                    }),
                                }),
                            }),
                            from.clone(),
                        ),
                        (Some(sfrom), None) => (
                            Rc::new(Cont::ApplyThunk {
                                thunk: sfrom.out_thunk.clone(),
                                cont: Rc::new(Cont::DoWinds {
                                    from: sfrom.next.clone(),
                                    to: None,
                                    cont: Rc::new(Cont::WindsOp {
                                        op: WindsOp::Set(to.clone()),
                                        cont: cont.clone(),
                                    }),
                                }),
                            }),
                            from.clone(),
                        ),
                        (Some(sfrom), Some(sto)) if !Rc::ptr_eq(sfrom, sto) => (
                            Rc::new(Cont::ApplyThunk {
                                thunk: sfrom.out_thunk.clone(),
                                cont: Rc::new(Cont::DoWinds {
                                    from: sfrom.next.clone(),
                                    to: sto.next.clone(),
                                    cont: Rc::new(Cont::ApplyThunk {
                                        thunk: sto.in_thunk.clone(),
                                        cont: Rc::new(Cont::WindsOp {
                                            op: WindsOp::Set(to.clone()),
                                            cont: cont.clone(),
                                        }),
                                    }),
                                }),
                            }),
                            from.clone(),
                        ),
                        _ => (cont.clone(), to.clone()),
                    };
                    Bouncer::Bounce(State {
                        acc: Acc::Obj(Ok(obj)),
                        cont: ncont,
                        env,
                        rib,
                        stack,
                        winds: nwinds,
                    })
                }
                Cont::WindsOp { op, cont } => Bouncer::Bounce(State {
                    acc: Acc::Obj(Ok(obj)),
                    cont: cont.clone(),
                    env,
                    rib,
                    stack,
                    winds: match op {
                        WindsOp::Set(winds) => winds.clone(),
                        WindsOp::Push(in_thunk, out_thunk) => Some(Rc::new(Wind {
                            in_thunk: in_thunk.clone(),
                            out_thunk: out_thunk.clone(),
                            next: winds.clone(),
                        })),
                        WindsOp::Pop => match winds {
                            Some(winds) => winds.next.clone(),
                            None => panic!("Attempted to pop empty winds"),
                        },
                    },
                }),
                Cont::ApplyThunk { thunk, cont } => {
                    let tail = cont.is_tail();
                    Bouncer::Bounce(State {
                        acc: Acc::Obj(Ok(thunk.clone())),
                        cont: Rc::new(Cont::Apply),
                        env: env.clone(),
                        rib: Vec::new(),
                        stack: if tail {
                            stack
                        } else {
                            Some(Rc::new(Frame {
                                cont: cont.clone(),
                                env,
                                rib,
                                next: stack,
                            }))
                        },
                        winds,
                    })
                }
            },
            Err(e) => Bouncer::Land(Err(e)),
        },
    }
}

fn eval_def(def: Rc<Definition>, env: Rc<Env>) -> Result<ObjectRef, EvalError> {
    match &*def {
        Definition::Variable { name, value } => trampoline(Bouncer::Bounce(State::new(
            Acc::Expr(value.clone()),
            env,
            Some(Rc::new(Cont::Define {
                name: *name,
                cont: Rc::new(Cont::Return),
            })),
        ))),
        Definition::Procedure { name, data } => {
            let proc = Object::Procedure(Procedure::UserDefined(UserDefined::new(
                data.clone(),
                env.clone(),
            )?));
            env.insert(*name, ObjectRef::new(proc));
            Ok(ObjectRef::Void)
        }
        Definition::Begin(defs) => trampoline(Bouncer::Bounce(State::new(
            Acc::Obj(Ok(ObjectRef::Undefined)),
            env,
            Some(Cont::from_defs(defs, None)),
        ))),
    }
}

pub fn eval(eod: ExprOrDef, env: Rc<Env>) -> Result<ObjectRef, EvalError> {
    match eod {
        ExprOrDef::Expr(expr) => Ok(trampoline(Bouncer::Bounce(State::new(
            Acc::Expr(expr),
            env,
            None,
        )))?),
        ExprOrDef::Definition(def) => eval_def(def, env),
        ExprOrDef::MixedBegin(eods) => Ok(trampoline(Bouncer::Bounce(State::new(
            Acc::Obj(Ok(ObjectRef::Undefined)),
            env,
            Some(Cont::from_body(&eods, None)),
        )))?),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::*;

    #[test]
    fn atoms() {
        let env = Env::new_empty(None);

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
        let env = Env::new_empty(None);

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
        let env = Env::new_empty(None);

        assert_eq!(
            eval(ExprOrDef::new_expr(var_expr!("a")), env.clone()),
            Err(EvalError::new(EvalErrorKind::UndefinedVariable("a".into())))
        );
        assert_eq!(
            eval(
                ExprOrDef::new_def(Definition::Variable {
                    name: "a".into(),
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
                        name: "b".into(),
                        value: Rc::new(int_expr!(2))
                    },
                    Definition::Variable {
                        name: "c".into(),
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
        let env = Env::new_empty(None);
        assert_eq!(
            eval(
                ExprOrDef::new_def(Definition::Variable {
                    name: "a".into(),
                    value: Rc::new(int_expr!(1))
                }),
                env.clone()
            ),
            Ok(ObjectRef::Void)
        );
        assert_eq!(
            eval(
                ExprOrDef::new_expr(Expr::Assignment {
                    variable: "a".into(),
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
        let env = Env::new_empty(None);

        assert_eq!(
            eval(
                ExprOrDef::new_def(Definition::Procedure {
                    name: "f".into(),
                    data: Rc::new(ProcData {
                        args: vec!["x".into()],
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
                max_expected: 1,
                got: 2,
            }))
        );

        assert_eq!(
            eval(
                ExprOrDef::new_def(Definition::Variable {
                    name: "g".into(),
                    value: Rc::new(Expr::Lambda(Rc::new(ProcData {
                        args: vec!["x".into(), "y".into()],
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
                max_expected: 2,
                got: 1,
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
        let env = Env::new_empty(None);

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
