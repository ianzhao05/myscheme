use std::cmp::Ordering;
use std::collections::HashMap;

use num::{BigInt, BigRational, Integer, One, Signed, ToPrimitive, Zero};

use crate::datum::SimpleDatum;
use crate::evaler::{EvalError, EvalErrorKind};
use crate::number::*;
use crate::object::{Object, ObjectRef};

use super::utils::{ensure_arity, get_int, get_num, PrimitiveMap};

fn integer(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 1);

    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Boolean(
        get_int(&args[0]).is_ok(),
    ))))
}

fn num_map(
    args: &[ObjectRef],
    f: impl FnOnce(&Number) -> SimpleDatum,
) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 1);

    Ok(ObjectRef::new(Object::Atom(f(get_num(&args[0])?))))
}

fn add(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    let mut sum = Number::Real(RealKind::Integer(0.into()));
    for arg in args {
        sum += get_num(arg)?;
    }
    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Number(sum))))
}

fn sub(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 1, usize::MAX);

    let first = get_num(&args[0])?;
    if args.len() == 1 {
        return Ok(ObjectRef::new(Object::Atom(SimpleDatum::Number(
            -first.clone(),
        ))));
    }
    let mut res = first.clone();
    for arg in &args[1..] {
        res -= get_num(arg)?;
    }
    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Number(res))))
}

fn mul(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    let mut prod = Number::Real(RealKind::Integer(1.into()));
    for arg in args {
        prod *= get_num(arg)?;
    }
    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Number(prod))))
}

fn div(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 1, usize::MAX);

    let first = get_num(&args[0])?;
    if args.len() == 1 {
        if first.is_zero() {
            return Err(EvalError::new(EvalErrorKind::ZeroDivision));
        }
        return Ok(ObjectRef::new(Object::Atom(SimpleDatum::Number(
            &Number::Real(RealKind::Integer(1.into())) / first,
        ))));
    }
    let mut res = first.clone();
    for arg in &args[1..] {
        let n = get_num(arg)?;
        if n.is_zero() {
            return Err(EvalError::new(EvalErrorKind::ZeroDivision));
        }
        res /= n;
    }
    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Number(res))))
}

fn eq(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 1, usize::MAX);

    let first = get_num(&args[0])?;
    for arg in &args[1..] {
        if !Number::eq_val(first, get_num(arg)?) {
            return Ok(ObjectRef::new(Object::Atom(SimpleDatum::Boolean(false))));
        }
    }
    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Boolean(true))))
}

fn cmp(args: &[ObjectRef], ord: Ordering, strict: bool) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 1, usize::MAX);

    for w in args.windows(2) {
        let n1 = get_num(&w[0])?;
        let n2 = get_num(&w[1])?;
        match (n1, n2) {
            (Number::Real(n1), Number::Real(n2)) => {
                if strict && n1.partial_cmp(n2).unwrap() != ord
                    || !strict && n1.partial_cmp(n2).unwrap() == ord.reverse()
                {
                    return Ok(ObjectRef::new(Object::Atom(SimpleDatum::Boolean(false))));
                }
            }
        }
    }
    Ok(ObjectRef::new(Object::Atom(SimpleDatum::Boolean(true))))
}

fn maxmin(args: &[ObjectRef], max: bool) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 1, usize::MAX);

    let first = get_num(&args[0])?;
    let mut res = first.clone();
    for arg in &args[1..] {
        let n = get_num(arg)?;
        res = if max {
            res.max(n.clone())
        } else {
            res.min(n.clone())
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

    let n1 = get_int(&args[0])?;
    let n2 = get_int(&args[1])?;
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

    let first = get_num(&args[0])?;
    let mut inexact = !first.is_exact();
    let mut res = first.to_integer().expect("should be integer").abs();
    for arg in &args[1..] {
        let n = get_num(arg)?;
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

fn rationalize(args: &[ObjectRef]) -> Result<ObjectRef, EvalError> {
    ensure_arity!(args, 2);

    let x = get_num(&args[0])?;
    let y = get_num(&args[1])?;
    let inexact = !x.is_exact() || !y.is_exact();

    let x = match x.to_exact_rational() {
        Some(r) => r,
        None => return Ok(args[0].clone()),
    };
    let y = match y.abs().to_exact_rational() {
        Some(r) => r,
        None => {
            return Ok(ObjectRef::new(Object::Atom(SimpleDatum::Number(
                Number::Real(RealKind::Real(0.0)),
            ))))
        }
    };

    let mut lo = &x - &y;
    let mut hi = x + y;
    let neg = hi.is_negative();
    if neg {
        (lo, hi) = (-hi, -lo);
    } else if !lo.is_positive() {
        return Ok(ObjectRef::new(Object::Atom(SimpleDatum::Number(
            Number::Real(if inexact {
                RealKind::Real(0.0)
            } else {
                RealKind::Integer(BigInt::zero())
            }),
        ))));
    }

    // Continued fraction algorithm
    // Credit: https://stackoverflow.com/a/65189151/377022
    let (mut s, mut t, mut u, mut v) = (
        lo.numer().clone(),
        lo.denom().clone(),
        hi.numer().clone(),
        hi.denom().clone(),
    );
    let (mut a, mut b, mut c, mut d) =
        (BigInt::one(), BigInt::zero(), BigInt::zero(), BigInt::one());
    loop {
        let q = (&s - 1) / &t;
        (s, t, u, v) = (v.clone(), &u - &q * v, t.clone(), &s - &q * t);
        (a, b, c, d) = (b.clone() + &q * &a, a, d.clone() + &q * &c, c);
        if t >= s {
            let mut res = BigRational::new(a + b, c + d);
            if neg {
                res = -res;
            }
            return Ok(ObjectRef::new(Object::Atom(SimpleDatum::Number(
                Number::Real(if inexact {
                    RealKind::Real(res.to_f64().unwrap_or(f64::NAN))
                } else {
                    RealKind::Rational(res)
                }),
            ))));
        }
    }
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
    m.insert("numerator", |args| {
        num_map(args, |n| SimpleDatum::Number(n.numerator()))
    });
    m.insert("denominator", |args| {
        num_map(args, |n| SimpleDatum::Number(n.denominator()))
    });
    m.insert("exact->inexact", |args| {
        num_map(args, |n| SimpleDatum::Number(n.to_inexact()))
    });
    m.insert("inexact->exact", |args| {
        num_map(args, |n| SimpleDatum::Number(n.to_exact()))
    });
    m.insert("exp", |args| {
        num_map(args, |n| SimpleDatum::Number(n.map_inexact(f64::exp)))
    });
    m.insert("log", |args| {
        num_map(args, |n| SimpleDatum::Number(n.map_inexact(f64::ln)))
    });
    m.insert("sin", |args| {
        num_map(args, |n| SimpleDatum::Number(n.map_inexact(f64::sin)))
    });
    m.insert("cos", |args| {
        num_map(args, |n| SimpleDatum::Number(n.map_inexact(f64::cos)))
    });
    m.insert("tan", |args| {
        num_map(args, |n| SimpleDatum::Number(n.map_inexact(f64::tan)))
    });
    m.insert("asin", |args| {
        num_map(args, |n| SimpleDatum::Number(n.map_inexact(f64::asin)))
    });
    m.insert("acos", |args| {
        num_map(args, |n| SimpleDatum::Number(n.map_inexact(f64::acos)))
    });
    m.insert("quotient", |args| ints_op(args, IntOp::Quotient));
    m.insert("remainder", |args| ints_op(args, IntOp::Remainder));
    m.insert("modulo", |args| ints_op(args, IntOp::Modulo));
    m.insert("gcd", |args| gcd_lcm(args, false));
    m.insert("lcm", |args| gcd_lcm(args, true));
    m.insert("rationalize", rationalize);
    m
}

pub const PRELUDE: &str = "
(define (even? n) (= (remainder n 2) 0))
(define (odd? n) (= (remainder n 2) 1))
";

#[cfg(test)]
mod tests {
    use super::*;
    use crate::primitives::utils::num_cv;
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

    #[test]
    fn test_rationalize() {
        assert_eq!(
            rationalize(&[
                ObjectRef::new(atom_obj!(rational_datum!(3, 10))),
                ObjectRef::new(atom_obj!(rational_datum!(1, 10)))
            ]),
            Ok(ObjectRef::new(atom_obj!(rational_datum!(1, 3))))
        );

        assert_eq!(
            rationalize(&[
                ObjectRef::new(atom_obj!(rational_datum!(-3, 10))),
                ObjectRef::new(atom_obj!(rational_datum!(1, 10)))
            ]),
            Ok(ObjectRef::new(atom_obj!(rational_datum!(-1, 3))))
        );

        assert_eq!(
            rationalize(&[
                ObjectRef::new(atom_obj!(real_datum!(0.2))),
                ObjectRef::new(atom_obj!(real_datum!(0.3)))
            ]),
            Ok(ObjectRef::new(atom_obj!(real_datum!(0.0))))
        );
    }
}
