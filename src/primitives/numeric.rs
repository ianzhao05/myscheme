use std::cmp::Ordering;
use std::collections::HashMap;

use num::{Integer, Signed, ToPrimitive};

use crate::datum::SimpleDatum;
use crate::evaler::{EvalError, EvalErrorKind};
use crate::number::*;
use crate::object::{Object, ObjectRef};
use crate::primitives::utils::int_cv;

use super::utils::{ensure_arity, num_cv, PrimitiveMap};

fn integer(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 1);

    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Boolean(
        match &args[0] {
            ObjectRef::Object(o) => match &**o {
                Object::Atom(SimpleDatum::Number(n)) => n.is_integer(),
                _ => false,
            },
            _ => false,
        },
    ))))
}

fn num_map(
    args: &[ObjectRef],
    f: impl FnOnce(&Number) -> SimpleDatum,
) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 1);

    Ok(ObjectRef::new(Object::Atom(
        match args[0].try_deref_or(num_cv)? {
            Object::Atom(SimpleDatum::Number(n)) => f(&n),
            _ => Err(num_cv(&args[0]))?,
        },
    )))
}

fn add(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    let mut sum = Number::Real(RealKind::Integer(0.into()));
    for arg in args {
        match arg.try_deref_or(num_cv)? {
            Object::Atom(SimpleDatum::Number(n)) => sum += n.clone(),
            _ => return Err(num_cv(arg)),
        }
    }
    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Number(sum))))
}

fn sub(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 1, usize::MAX);

    let first = match args[0].try_deref_or(num_cv)? {
        Object::Atom(SimpleDatum::Number(n)) => n,
        _ => return Err(num_cv(&args[0])),
    };
    if args.len() == 1 {
        return Ok(ObjectRef::new(Object::Atom(SimpleDatum::Number(
            -first.clone(),
        ))));
    }
    let mut res = first.clone();
    for arg in &args[1..] {
        match arg.try_deref_or(num_cv)? {
            Object::Atom(SimpleDatum::Number(n)) => res -= n.clone(),
            _ => return Err(num_cv(arg)),
        }
    }
    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Number(res))))
}

fn mul(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    let mut prod = Number::Real(RealKind::Integer(1.into()));
    for arg in args {
        match arg.try_deref_or(num_cv)? {
            Object::Atom(SimpleDatum::Number(n)) => prod *= n.clone(),
            _ => return Err(num_cv(arg)),
        }
    }
    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Number(prod))))
}

fn div(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 1, usize::MAX);

    let first = match args[0].try_deref_or(num_cv)? {
        Object::Atom(SimpleDatum::Number(n)) => n,
        _ => return Err(num_cv(&args[0])),
    };
    let zero = Number::Real(RealKind::Integer(0.into()));
    if args.len() == 1 {
        if Number::eq_val(first, &zero) {
            return Err(EvalError::new(EvalErrorKind::ZeroDivision));
        }
        return Ok(ObjectRef::new(Object::Atom(SimpleDatum::Number(
            Number::Real(RealKind::Integer(1.into())) / first.clone(),
        ))));
    }
    let mut res = first.clone();
    for arg in &args[1..] {
        match arg.try_deref_or(num_cv)? {
            Object::Atom(SimpleDatum::Number(n)) => {
                if Number::eq_val(n, &zero) {
                    return Err(EvalError::new(EvalErrorKind::ZeroDivision));
                }
                res /= n.clone()
            }
            _ => return Err(num_cv(arg)),
        }
    }
    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Number(res))))
}

fn eq(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 1, usize::MAX);

    let first = match args[0].try_deref_or(num_cv)? {
        Object::Atom(SimpleDatum::Number(n)) => n,
        _ => return Err(num_cv(&args[0])),
    };
    for arg in &args[1..] {
        match arg.try_deref_or(num_cv)? {
            Object::Atom(SimpleDatum::Number(n)) => {
                if !Number::eq_val(first, n) {
                    return Ok(ObjectRef::new(Object::Atom(SimpleDatum::Boolean(false))));
                }
            }
            _ => return Err(num_cv(arg)),
        }
    }
    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Boolean(true))))
}

fn cmp(args: &[ObjectRef], ord: Ordering, strict: bool) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 1, usize::MAX);

    for w in args.windows(2) {
        match w[0].try_deref_or(num_cv)? {
            Object::Atom(SimpleDatum::Number(Number::Real(n1))) => {
                match w[1].try_deref_or(num_cv)? {
                    Object::Atom(SimpleDatum::Number(Number::Real(n2))) => {
                        if strict && n1.partial_cmp(n2).unwrap() != ord
                            || !strict && n1.partial_cmp(n2).unwrap() == ord.reverse()
                        {
                            return Ok(ObjectRef::new(Object::Atom(SimpleDatum::Boolean(false))));
                        }
                    }
                    _ => return Err(num_cv(&w[1])),
                }
            }
            _ => return Err(num_cv(&w[0])),
        }
    }
    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Boolean(true))))
}

fn maxmin(args: &[ObjectRef], max: bool) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 1, usize::MAX);

    let first = match args[0].try_deref_or(num_cv)? {
        Object::Atom(SimpleDatum::Number(n)) => n,
        _ => return Err(num_cv(&args[0])),
    };
    let mut res = first.clone();
    for arg in &args[1..] {
        match arg.try_deref_or(num_cv)? {
            Object::Atom(SimpleDatum::Number(n)) => {
                res = if max {
                    res.max(n.clone())
                } else {
                    res.min(n.clone())
                }
            }
            _ => return Err(num_cv(arg)),
        }
    }
    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Number(res))))
}

enum IntOp {
    Quotient,
    Remainder,
    Modulo,
}

fn ints_op(args: &[ObjectRef], op: IntOp) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 2);

    let mut it = args.iter().map(|arg| match arg.try_deref_or(int_cv)? {
        Object::Atom(SimpleDatum::Number(n)) if n.is_integer() => Ok(n),
        _ => Err(int_cv(arg)),
    });
    let n1 = it.next().unwrap()?;
    let n2 = it.next().unwrap()?;
    let inexact = !n1.is_exact() || !n2.is_exact();
    let i1 = n1.to_integer().expect("should be integer");
    let i2 = n2.to_integer().expect("should be integer");
    let res = match op {
        IntOp::Quotient => i1 / i2,
        IntOp::Remainder => i1 % i2,
        IntOp::Modulo => i1.mod_floor(&i2),
    };
    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Number(
        Number::Real(if inexact {
            RealKind::Real(res.to_f64().unwrap_or(f64::NAN))
        } else {
            RealKind::Integer(res)
        }),
    ))))
}

fn gcd_lcm(args: &[ObjectRef], lcm: bool) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 1, usize::MAX);

    let mut it = args.iter().map(|arg| match arg.try_deref_or(int_cv)? {
        Object::Atom(SimpleDatum::Number(n)) if n.is_integer() => Ok(n),
        _ => Err(int_cv(arg)),
    });
    let first = it.next().unwrap()?;
    let mut inexact = !first.is_exact();
    let mut res = first.to_integer().expect("should be integer").abs();
    for arg in it {
        let n = arg?;
        inexact = inexact || !n.is_exact();
        let i = n.to_integer().expect("should be integer");
        res = if lcm { res.lcm(&i) } else { res.gcd(&i) };
    }
    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Number(
        Number::Real(if inexact {
            RealKind::Real(res.to_f64().unwrap_or(f64::NAN))
        } else {
            RealKind::Integer(res)
        }),
    ))))
}

pub fn primitives() -> PrimitiveMap {
    let mut m: PrimitiveMap = HashMap::new();
    m.insert("integer?", integer);
    m.insert("+", add);
    m.insert("-", sub);
    m.insert("*", mul);
    m.insert("/", div);
    m.insert("=", eq);
    m.insert("<", |args| cmp(args, Ordering::Less, true));
    m.insert(">", |args| cmp(args, Ordering::Greater, true));
    m.insert("<=", |args| cmp(args, Ordering::Less, false));
    m.insert(">=", |args| cmp(args, Ordering::Greater, false));
    m.insert("max", |args| maxmin(args, true));
    m.insert("min", |args| maxmin(args, false));
    m.insert("exact?", |args| {
        num_map(args, |n| SimpleDatum::Boolean(n.is_exact()))
    });
    m.insert("inexact?", |args| {
        num_map(args, |n| SimpleDatum::Boolean(!n.is_exact()))
    });
    m.insert("zero?", |args| {
        num_map(args, |n| SimpleDatum::Boolean(n.is_zero()))
    });
    m.insert("positive?", |args| {
        num_map(args, |n| SimpleDatum::Boolean(n.is_positive()))
    });
    m.insert("negative?", |args| {
        num_map(args, |n| SimpleDatum::Boolean(n.is_negative()))
    });
    m.insert("abs", |args| {
        num_map(args, |n| SimpleDatum::Number(n.abs()))
    });
    m.insert("floor", |args| {
        num_map(args, |n| SimpleDatum::Number(n.floor()))
    });
    m.insert("ceiling", |args| {
        num_map(args, |n| SimpleDatum::Number(n.ceil()))
    });
    m.insert("truncate", |args| {
        num_map(args, |n| SimpleDatum::Number(n.trunc()))
    });
    m.insert("round", |args| {
        num_map(args, |n| SimpleDatum::Number(n.round()))
    });
    m.insert("quotient", |args| ints_op(args, IntOp::Quotient));
    m.insert("remainder", |args| ints_op(args, IntOp::Remainder));
    m.insert("modulo", |args| ints_op(args, IntOp::Modulo));
    m.insert("gcd", |args| gcd_lcm(args, false));
    m.insert("lcm", |args| gcd_lcm(args, true));
    m
}

pub const PRELUDE: &str = "
(define (even? n) (= (remainder n 2) 0))
(define (odd? n) (= (remainder n 2) 1))
";

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::*;

    #[test]
    fn predicates() {
        assert_eq!(
            integer(&[ObjectRef::new(atom_obj!(int_datum!(1)))]),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(true))))
        );
        assert_eq!(
            integer(&[ObjectRef::new(atom_obj!(rational_datum!(2, 1)))]),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(true))))
        );
        assert_eq!(
            integer(&[ObjectRef::new(atom_obj!(real_datum!(1.0)))]),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(true))))
        );
        assert_eq!(
            integer(&[ObjectRef::new(atom_obj!(rational_datum!(1, 2)))]),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(false))))
        );
        assert_eq!(
            integer(&[ObjectRef::new(atom_obj!(real_datum!(0.5)))]),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(false))))
        );
    }

    #[test]
    fn arithmetic() {
        assert_eq!(add(&[]), Ok(ObjectRef::new(atom_obj!(int_datum!(0)))));
        assert_eq!(
            add(&[
                ObjectRef::new(atom_obj!(int_datum!(1))),
                ObjectRef::new(atom_obj!(int_datum!(2))),
                ObjectRef::new(atom_obj!(int_datum!(3)))
            ]),
            Ok(ObjectRef::new(atom_obj!(int_datum!(6))))
        );
        assert_eq!(
            add(&[
                ObjectRef::new(atom_obj!(int_datum!(1))),
                ObjectRef::new(atom_obj!(rational_datum!(1, 2)))
            ]),
            Ok(ObjectRef::new(atom_obj!(rational_datum!(3, 2))))
        );
        assert_eq!(
            add(&[ObjectRef::new(atom_obj!(bool_datum!(true)))]),
            Err(num_cv(&ObjectRef::new(atom_obj!(bool_datum!(true)))))
        );
        assert_eq!(
            add(&[ObjectRef::EmptyList]),
            Err(num_cv(&ObjectRef::EmptyList))
        );

        assert_eq!(
            sub(&[]),
            Err(EvalError::new(EvalErrorKind::ArityMismatch {
                expected: 1,
                max_expected: usize::MAX,
                got: 0,
            }))
        );
        assert_eq!(
            sub(&[ObjectRef::new(atom_obj!(int_datum!(1)))]),
            Ok(ObjectRef::new(atom_obj!(int_datum!(-1))))
        );
        assert_eq!(
            sub(&[
                ObjectRef::new(atom_obj!(int_datum!(1))),
                ObjectRef::new(atom_obj!(int_datum!(2))),
                ObjectRef::new(atom_obj!(int_datum!(3)))
            ]),
            Ok(ObjectRef::new(atom_obj!(int_datum!(-4))))
        );

        assert_eq!(mul(&[]), Ok(ObjectRef::new(atom_obj!(int_datum!(1)))));
        assert_eq!(
            mul(&[
                ObjectRef::new(atom_obj!(int_datum!(1))),
                ObjectRef::new(atom_obj!(int_datum!(2))),
                ObjectRef::new(atom_obj!(int_datum!(3)))
            ]),
            Ok(ObjectRef::new(atom_obj!(int_datum!(6))))
        );

        assert_eq!(
            div(&[]),
            Err(EvalError::new(EvalErrorKind::ArityMismatch {
                expected: 1,
                max_expected: usize::MAX,
                got: 0,
            }))
        );
        assert_eq!(
            div(&[ObjectRef::new(atom_obj!(int_datum!(2)))]),
            Ok(ObjectRef::new(atom_obj!(rational_datum!(1, 2))))
        );
        assert_eq!(
            div(&[
                ObjectRef::new(atom_obj!(int_datum!(1))),
                ObjectRef::new(atom_obj!(int_datum!(2))),
                ObjectRef::new(atom_obj!(int_datum!(3)))
            ]),
            Ok(ObjectRef::new(atom_obj!(rational_datum!(1, 6))))
        );

        assert_eq!(
            ints_op(
                &[
                    ObjectRef::new(atom_obj!(int_datum!(15))),
                    ObjectRef::new(atom_obj!(int_datum!(4)))
                ],
                IntOp::Quotient
            ),
            Ok(ObjectRef::new(atom_obj!(int_datum!(3))))
        );
        assert_eq!(
            ints_op(
                &[
                    ObjectRef::new(atom_obj!(int_datum!(20))),
                    ObjectRef::new(atom_obj!(real_datum!(3.0)))
                ],
                IntOp::Remainder
            ),
            Ok(ObjectRef::new(atom_obj!(real_datum!(2.0))))
        );
        assert_eq!(
            ints_op(
                &[
                    ObjectRef::new(atom_obj!(int_datum!(20))),
                    ObjectRef::new(atom_obj!(int_datum!(-3)))
                ],
                IntOp::Modulo
            ),
            Ok(ObjectRef::new(atom_obj!(int_datum!(-1))))
        );

        assert_eq!(
            gcd_lcm(
                &[
                    ObjectRef::new(atom_obj!(int_datum!(20))),
                    ObjectRef::new(atom_obj!(int_datum!(30))),
                    ObjectRef::new(atom_obj!(int_datum!(40)))
                ],
                false
            ),
            Ok(ObjectRef::new(atom_obj!(int_datum!(10))))
        );

        assert_eq!(
            gcd_lcm(
                &[
                    ObjectRef::new(atom_obj!(int_datum!(20))),
                    ObjectRef::new(atom_obj!(int_datum!(30))),
                    ObjectRef::new(atom_obj!(int_datum!(40)))
                ],
                true
            ),
            Ok(ObjectRef::new(atom_obj!(int_datum!(120))))
        );
    }

    #[test]
    fn comparison() {
        assert_eq!(
            eq(&[]),
            Err(EvalError::new(EvalErrorKind::ArityMismatch {
                expected: 1,
                max_expected: usize::MAX,
                got: 0,
            }))
        );
        assert_eq!(
            eq(&[
                ObjectRef::new(atom_obj!(int_datum!(1))),
                ObjectRef::new(atom_obj!(int_datum!(1)))
            ]),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(true))))
        );
        assert_eq!(
            eq(&[
                ObjectRef::new(atom_obj!(int_datum!(1))),
                ObjectRef::new(atom_obj!(int_datum!(2)))
            ]),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(false))))
        );
        assert_eq!(
            eq(&[
                ObjectRef::new(atom_obj!(int_datum!(1))),
                ObjectRef::new(atom_obj!(int_datum!(1))),
                ObjectRef::new(atom_obj!(int_datum!(1)))
            ]),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(true))))
        );
        assert_eq!(
            eq(&[
                ObjectRef::new(atom_obj!(int_datum!(1))),
                ObjectRef::new(atom_obj!(int_datum!(2))),
                ObjectRef::new(atom_obj!(int_datum!(1)))
            ]),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(false))))
        );

        assert_eq!(
            cmp(
                &[
                    ObjectRef::new(atom_obj!(int_datum!(1))),
                    ObjectRef::new(atom_obj!(int_datum!(2))),
                    ObjectRef::new(atom_obj!(int_datum!(3)))
                ],
                Ordering::Less,
                true
            ),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(true))))
        );
        assert_eq!(
            cmp(
                &[
                    ObjectRef::new(atom_obj!(int_datum!(1))),
                    ObjectRef::new(atom_obj!(int_datum!(2))),
                    ObjectRef::new(atom_obj!(int_datum!(2)))
                ],
                Ordering::Less,
                true
            ),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(false))))
        );
        assert_eq!(
            cmp(
                &[
                    ObjectRef::new(atom_obj!(int_datum!(1))),
                    ObjectRef::new(atom_obj!(int_datum!(2))),
                    ObjectRef::new(atom_obj!(int_datum!(2)))
                ],
                Ordering::Less,
                false
            ),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(true))))
        );
        assert_eq!(
            cmp(
                &[
                    ObjectRef::new(atom_obj!(int_datum!(3))),
                    ObjectRef::new(atom_obj!(int_datum!(2))),
                    ObjectRef::new(atom_obj!(int_datum!(1)))
                ],
                Ordering::Greater,
                false
            ),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(true))))
        );
        assert_eq!(
            cmp(
                &[
                    ObjectRef::new(atom_obj!(int_datum!(3))),
                    ObjectRef::new(atom_obj!(int_datum!(2))),
                    ObjectRef::new(atom_obj!(int_datum!(2)))
                ],
                Ordering::Greater,
                true
            ),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(false))))
        );
        assert_eq!(
            cmp(
                &[
                    ObjectRef::new(atom_obj!(int_datum!(3))),
                    ObjectRef::new(atom_obj!(int_datum!(2))),
                    ObjectRef::new(atom_obj!(int_datum!(2)))
                ],
                Ordering::Greater,
                false
            ),
            Ok(ObjectRef::new(atom_obj!(bool_datum!(true))))
        );

        assert_eq!(
            maxmin(
                &[
                    ObjectRef::new(atom_obj!(int_datum!(1))),
                    ObjectRef::new(atom_obj!(int_datum!(2))),
                    ObjectRef::new(atom_obj!(int_datum!(3)))
                ],
                true
            ),
            Ok(ObjectRef::new(atom_obj!(int_datum!(3))))
        );

        assert_eq!(
            maxmin(
                &[
                    ObjectRef::new(atom_obj!(int_datum!(1))),
                    ObjectRef::new(atom_obj!(int_datum!(2))),
                    ObjectRef::new(atom_obj!(int_datum!(3)))
                ],
                false
            ),
            Ok(ObjectRef::new(atom_obj!(int_datum!(1))))
        );

        assert_eq!(
            maxmin(
                &[
                    ObjectRef::new(atom_obj!(int_datum!(2))),
                    ObjectRef::new(atom_obj!(real_datum!(1.5))),
                ],
                true
            ),
            Ok(ObjectRef::new(atom_obj!(real_datum!(2.0))))
        );
    }
}
