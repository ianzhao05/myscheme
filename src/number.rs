use std::error::Error;
use std::fmt;
use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign};
use std::str::FromStr;

use num::FromPrimitive;
use num::{bigint::ToBigInt, BigInt, BigRational, Complex, Signed, ToPrimitive, Zero};

#[derive(Debug, Clone)]
pub enum RealKind {
    Real(f64),
    Rational(BigRational),
    Integer(BigInt),
}

impl RealKind {
    pub fn eq_val(this: &Self, other: &Self) -> bool {
        match (this, other) {
            (RealKind::Real(a), _) => match other {
                RealKind::Real(b) => a == b,
                RealKind::Rational(b) => a == &b.to_f64().unwrap_or(f64::NAN),
                RealKind::Integer(b) => a == &b.to_f64().unwrap_or(f64::NAN),
            },
            (_, RealKind::Real(b)) => match this {
                RealKind::Real(a) => a == b,
                RealKind::Rational(a) => &a.to_f64().unwrap_or(f64::NAN) == b,
                RealKind::Integer(a) => &a.to_f64().unwrap_or(f64::NAN) == b,
            },
            _ => this == other,
        }
    }

    pub fn is_zero(&self) -> bool {
        match self {
            RealKind::Real(r) => r.is_zero(),
            RealKind::Rational(r) => r.is_zero(),
            RealKind::Integer(i) => i.is_zero(),
        }
    }

    pub fn is_negative(&self) -> bool {
        match self {
            RealKind::Real(r) => r.is_negative() && !r.is_zero(),
            RealKind::Rational(r) => r.is_negative(),
            RealKind::Integer(i) => i.is_negative(),
        }
    }

    pub fn is_positive(&self) -> bool {
        match self {
            RealKind::Real(r) => r.is_positive() && !r.is_zero(),
            RealKind::Rational(r) => r.is_positive(),
            RealKind::Integer(i) => i.is_positive(),
        }
    }

    pub fn is_exact(&self) -> bool {
        match self {
            RealKind::Real(_) => false,
            RealKind::Rational(_) => true,
            RealKind::Integer(_) => true,
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            RealKind::Real(r) => r.fract() == 0.0,
            RealKind::Rational(r) => r.is_integer(),
            RealKind::Integer(_) => true,
        }
    }

    pub fn abs(&self) -> Self {
        match self {
            RealKind::Real(r) => RealKind::Real(r.abs()),
            RealKind::Rational(r) => RealKind::Rational(r.abs()),
            RealKind::Integer(i) => RealKind::Integer(i.abs()),
        }
    }

    fn floor(&self) -> RealKind {
        match self {
            RealKind::Real(r) => RealKind::Real(r.floor()),
            RealKind::Rational(r) => RealKind::Rational(r.floor()),
            RealKind::Integer(i) => RealKind::Integer(i.clone()),
        }
    }

    fn ceil(&self) -> RealKind {
        match self {
            RealKind::Real(r) => RealKind::Real(r.ceil()),
            RealKind::Rational(r) => RealKind::Rational(r.ceil()),
            RealKind::Integer(i) => RealKind::Integer(i.clone()),
        }
    }

    fn round(&self) -> RealKind {
        match self {
            RealKind::Real(r) => RealKind::Real(r.round()),
            RealKind::Rational(r) => RealKind::Rational(r.round()),
            RealKind::Integer(i) => RealKind::Integer(i.clone()),
        }
    }

    fn trunc(&self) -> RealKind {
        match self {
            RealKind::Real(r) => RealKind::Real(r.trunc()),
            RealKind::Rational(r) => RealKind::Rational(r.trunc()),
            RealKind::Integer(i) => RealKind::Integer(i.clone()),
        }
    }

    pub fn max(self, other: Self) -> Self {
        match (self, other) {
            (RealKind::Real(a), RealKind::Real(b)) => RealKind::Real(a.max(b)),
            (RealKind::Rational(a), RealKind::Rational(b)) => RealKind::Rational(a.max(b)),
            (RealKind::Integer(a), RealKind::Integer(b)) => RealKind::Integer(a.max(b)),
            (RealKind::Rational(a), RealKind::Integer(b)) => {
                RealKind::Rational(a.max(BigRational::from(b)))
            }
            (RealKind::Integer(a), RealKind::Rational(b)) => {
                RealKind::Rational(BigRational::from(a).max(b))
            }
            (RealKind::Real(a), RealKind::Rational(b)) => {
                RealKind::Real(a.max(b.to_f64().unwrap_or(f64::NAN)))
            }
            (RealKind::Rational(a), RealKind::Real(b)) => {
                RealKind::Real(a.to_f64().unwrap_or(f64::NAN).max(b))
            }
            (RealKind::Real(a), RealKind::Integer(b)) => {
                RealKind::Real(a.max(b.to_f64().unwrap_or(f64::NAN)))
            }
            (RealKind::Integer(a), RealKind::Real(b)) => {
                RealKind::Real(a.to_f64().unwrap_or(f64::NAN).max(b))
            }
        }
    }

    pub fn min(self, other: Self) -> Self {
        match (self, other) {
            (RealKind::Real(a), RealKind::Real(b)) => RealKind::Real(a.min(b)),
            (RealKind::Rational(a), RealKind::Rational(b)) => RealKind::Rational(a.min(b)),
            (RealKind::Integer(a), RealKind::Integer(b)) => RealKind::Integer(a.min(b)),
            (RealKind::Rational(a), RealKind::Integer(b)) => {
                RealKind::Rational(a.min(BigRational::from(b)))
            }
            (RealKind::Integer(a), RealKind::Rational(b)) => {
                RealKind::Rational(BigRational::from(a).min(b))
            }
            (RealKind::Real(a), RealKind::Rational(b)) => {
                RealKind::Real(a.min(b.to_f64().unwrap_or(f64::NAN)))
            }
            (RealKind::Rational(a), RealKind::Real(b)) => {
                RealKind::Real(a.to_f64().unwrap_or(f64::NAN).min(b))
            }
            (RealKind::Real(a), RealKind::Integer(b)) => {
                RealKind::Real(a.min(b.to_f64().unwrap_or(f64::NAN)))
            }
            (RealKind::Integer(a), RealKind::Real(b)) => {
                RealKind::Real(a.to_f64().unwrap_or(f64::NAN).min(b))
            }
        }
    }

    pub fn to_exact(&self) -> Self {
        match self {
            RealKind::Real(r) => match BigRational::from_f64(*r) {
                Some(r) => RealKind::Rational(r),
                None => RealKind::Real(*r),
            },
            _ => self.clone(),
        }
    }

    pub fn to_exact_rational(&self) -> Option<BigRational> {
        match self {
            RealKind::Real(r) => BigRational::from_f64(*r),
            RealKind::Rational(r) => Some(r.clone()),
            RealKind::Integer(i) => Some(BigRational::from_integer(i.clone())),
        }
    }

    pub fn to_inexact(&self) -> Self {
        RealKind::Real(self.to_inexact_impl())
    }

    fn to_inexact_impl(&self) -> f64 {
        match self {
            RealKind::Real(r) => *r,
            RealKind::Rational(r) => r.to_f64().unwrap_or(f64::NAN),
            RealKind::Integer(i) => i.to_f64().unwrap_or(f64::NAN),
        }
    }

    fn map_inexact<F: FnOnce(f64) -> f64>(&self, f: F) -> RealKind {
        RealKind::Real(f(self.to_inexact_impl()))
    }

    pub fn to_integer(&self) -> Option<BigInt> {
        match self {
            RealKind::Real(r) => {
                if r.fract() == 0.0 {
                    r.to_bigint()
                } else {
                    None
                }
            }
            RealKind::Rational(r) => {
                if r.is_integer() {
                    Some(r.numer().clone())
                } else {
                    None
                }
            }
            RealKind::Integer(i) => Some(i.clone()),
        }
    }

    pub fn numerator(&self) -> Self {
        match self {
            RealKind::Real(r) => RealKind::Real(
                BigRational::from_f64(*r)
                    .map(|r| r.numer().to_f64())
                    .flatten()
                    .unwrap_or(f64::NAN),
            ),
            RealKind::Rational(r) => RealKind::Integer(r.numer().clone()),
            RealKind::Integer(i) => RealKind::Integer(i.clone()),
        }
    }

    pub fn denominator(&self) -> Self {
        if self.is_zero() {
            return match self {
                RealKind::Real(_) => RealKind::Real(1.0),
                _ => RealKind::Integer(1.into()),
            };
        }
        match self {
            RealKind::Real(r) => RealKind::Real(
                BigRational::from_f64(*r)
                    .map(|r| r.denom().to_f64())
                    .flatten()
                    .unwrap_or(f64::NAN),
            ),
            RealKind::Rational(r) => RealKind::Integer(r.denom().clone()),
            RealKind::Integer(_) => RealKind::Integer(BigInt::from(1)),
        }
    }
}

impl PartialEq for RealKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (RealKind::Real(a), RealKind::Real(b)) => a == b,
            (RealKind::Rational(a), RealKind::Rational(b)) => a == b,
            (RealKind::Integer(a), RealKind::Integer(b)) => a == b,
            (RealKind::Rational(a), RealKind::Integer(b)) => a.is_integer() && a.numer() == b,
            (RealKind::Integer(b), RealKind::Rational(a)) => a.is_integer() && a.numer() == b,
            _ => false,
        }
    }
}

impl PartialOrd for RealKind {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (RealKind::Real(a), b) => match b {
                RealKind::Real(b) => a.partial_cmp(b),
                RealKind::Rational(b) => a.partial_cmp(&b.to_f64().unwrap_or(f64::NAN)),
                RealKind::Integer(b) => a.partial_cmp(&b.to_f64().unwrap_or(f64::NAN)),
            },
            (RealKind::Rational(a), RealKind::Rational(b)) => a.partial_cmp(b),
            (RealKind::Integer(a), RealKind::Integer(b)) => a.partial_cmp(b),
            (RealKind::Integer(a), RealKind::Rational(b)) => {
                BigRational::from(a.clone()).partial_cmp(b)
            }
            _ => other.partial_cmp(self).map(|o| o.reverse()),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]

pub enum ComplexKind {
    Real(Complex<f64>),
    Rational(Complex<BigRational>),
    Integer(Complex<BigInt>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Number {
    // unused
    // Complex(ComplexKind),
    Real(RealKind),
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Number::Real(r) => match r {
                RealKind::Real(r) => r.fmt(f),
                RealKind::Rational(r) => r.fmt(f),
                RealKind::Integer(r) => r.fmt(f),
            },
        }
    }
}

impl Number {
    pub fn eq_val(this: &Self, other: &Self) -> bool {
        match (this, other) {
            (Number::Real(a), Number::Real(b)) => RealKind::eq_val(a, b),
        }
    }

    pub fn is_exact(&self) -> bool {
        match self {
            Number::Real(r) => r.is_exact(),
        }
    }

    pub fn is_zero(&self) -> bool {
        match self {
            Number::Real(r) => r.is_zero(),
        }
    }

    pub fn is_negative(&self) -> bool {
        match self {
            Number::Real(r) => r.is_negative(),
        }
    }

    pub fn is_positive(&self) -> bool {
        match self {
            Number::Real(r) => r.is_positive(),
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            Number::Real(r) => r.is_integer(),
        }
    }

    pub fn abs(&self) -> Self {
        match self {
            Number::Real(r) => Number::Real(r.abs()),
        }
    }

    pub fn floor(&self) -> Self {
        match self {
            Number::Real(r) => Number::Real(r.floor()),
        }
    }

    pub fn ceil(&self) -> Self {
        match self {
            Number::Real(r) => Number::Real(r.ceil()),
        }
    }

    pub fn round(&self) -> Self {
        match self {
            Number::Real(r) => Number::Real(r.round()),
        }
    }

    pub fn trunc(&self) -> Self {
        match self {
            Number::Real(r) => Number::Real(r.trunc()),
        }
    }

    pub fn max(self, other: Self) -> Self {
        match (self, other) {
            (Number::Real(a), Number::Real(b)) => Number::Real(a.max(b)),
        }
    }

    pub fn min(self, other: Self) -> Self {
        match (self, other) {
            (Number::Real(a), Number::Real(b)) => Number::Real(a.min(b)),
        }
    }

    pub fn to_exact(&self) -> Self {
        match self {
            Number::Real(r) => Number::Real(r.to_exact()),
        }
    }

    pub fn to_exact_rational(&self) -> Option<BigRational> {
        match self {
            Number::Real(r) => r.to_exact_rational(),
        }
    }

    pub fn to_inexact(&self) -> Self {
        match self {
            Number::Real(r) => Number::Real(r.to_inexact()),
        }
    }

    pub fn to_integer(&self) -> Option<BigInt> {
        match self {
            Number::Real(r) => r.to_integer(),
        }
    }

    pub fn map_inexact<F: FnOnce(f64) -> f64>(&self, f: F) -> Self {
        match self {
            Number::Real(r) => Number::Real(r.map_inexact(f)),
        }
    }

    pub fn numerator(&self) -> Self {
        match self {
            Number::Real(r) => Number::Real(r.numerator()),
        }
    }

    pub fn denominator(&self) -> Self {
        match self {
            Number::Real(r) => Number::Real(r.denominator()),
        }
    }
}

impl Add<&Number> for &Number {
    type Output = Number;

    fn add(self, rhs: &Number) -> Self::Output {
        match (self, rhs) {
            (Number::Real(a), Number::Real(b)) => match (a, b) {
                (RealKind::Real(a), RealKind::Real(b)) => Number::Real(RealKind::Real(a + b)),
                (RealKind::Rational(a), RealKind::Rational(b)) => {
                    Number::Real(RealKind::Rational(a + b))
                }
                (RealKind::Integer(a), RealKind::Integer(b)) => {
                    Number::Real(RealKind::Integer(a + b))
                }
                (RealKind::Rational(a), RealKind::Integer(b)) => {
                    Number::Real(RealKind::Rational(a + BigRational::from_integer(b.clone())))
                }
                (RealKind::Integer(a), RealKind::Rational(b)) => {
                    Number::Real(RealKind::Rational(BigRational::from_integer(a.clone()) + b))
                }
                (RealKind::Real(a), RealKind::Rational(b)) => {
                    Number::Real(RealKind::Real(a + b.to_f64().unwrap_or(f64::NAN)))
                }
                (RealKind::Rational(a), RealKind::Real(b)) => {
                    Number::Real(RealKind::Real(a.to_f64().unwrap_or(f64::NAN) + b))
                }
                (RealKind::Real(a), RealKind::Integer(b)) => {
                    Number::Real(RealKind::Real(a + b.to_f64().unwrap_or(f64::NAN)))
                }
                (RealKind::Integer(a), RealKind::Real(b)) => {
                    Number::Real(RealKind::Real(a.to_f64().unwrap_or(f64::NAN) + b))
                }
            },
        }
    }
}

impl AddAssign<&Number> for Number {
    fn add_assign(&mut self, rhs: &Number) {
        *self = &*self + rhs;
    }
}

impl Sub<&Number> for &Number {
    type Output = Number;

    fn sub(self, rhs: &Number) -> Self::Output {
        match (self, rhs) {
            (Number::Real(a), Number::Real(b)) => match (a, b) {
                (RealKind::Real(a), RealKind::Real(b)) => Number::Real(RealKind::Real(a - b)),
                (RealKind::Rational(a), RealKind::Rational(b)) => {
                    Number::Real(RealKind::Rational(a - b))
                }
                (RealKind::Integer(a), RealKind::Integer(b)) => {
                    Number::Real(RealKind::Integer(a - b))
                }
                (RealKind::Rational(a), RealKind::Integer(b)) => {
                    Number::Real(RealKind::Rational(a - BigRational::from_integer(b.clone())))
                }
                (RealKind::Integer(a), RealKind::Rational(b)) => {
                    Number::Real(RealKind::Rational(BigRational::from_integer(a.clone()) - b))
                }
                (RealKind::Real(a), RealKind::Rational(b)) => {
                    Number::Real(RealKind::Real(a - b.to_f64().unwrap_or(f64::NAN)))
                }
                (RealKind::Rational(a), RealKind::Real(b)) => {
                    Number::Real(RealKind::Real(a.to_f64().unwrap_or(f64::NAN) - b))
                }
                (RealKind::Real(a), RealKind::Integer(b)) => {
                    Number::Real(RealKind::Real(a - b.to_f64().unwrap_or(f64::NAN)))
                }
                (RealKind::Integer(a), RealKind::Real(b)) => {
                    Number::Real(RealKind::Real(a.to_f64().unwrap_or(f64::NAN) - b))
                }
            },
        }
    }
}

impl SubAssign<&Number> for Number {
    fn sub_assign(&mut self, rhs: &Number) {
        *self = &*self - rhs;
    }
}

impl Neg for Number {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Number::Real(a) => match a {
                RealKind::Real(a) => Number::Real(RealKind::Real(-a)),
                RealKind::Rational(a) => Number::Real(RealKind::Rational(-a)),
                RealKind::Integer(a) => Number::Real(RealKind::Integer(-a)),
            },
        }
    }
}

impl Mul<&Number> for &Number {
    type Output = Number;

    fn mul(self, rhs: &Number) -> Self::Output {
        match (self, rhs) {
            (Number::Real(a), Number::Real(b)) => match (a, b) {
                (RealKind::Real(a), RealKind::Real(b)) => Number::Real(RealKind::Real(a * b)),
                (RealKind::Rational(a), RealKind::Rational(b)) => {
                    Number::Real(RealKind::Rational(a * b))
                }
                (RealKind::Integer(a), RealKind::Integer(b)) => {
                    Number::Real(RealKind::Integer(a * b))
                }
                (RealKind::Rational(a), RealKind::Integer(b)) => {
                    Number::Real(RealKind::Rational(a * BigRational::from_integer(b.clone())))
                }
                (RealKind::Integer(a), RealKind::Rational(b)) => {
                    Number::Real(RealKind::Rational(BigRational::from_integer(a.clone()) * b))
                }
                (RealKind::Real(a), RealKind::Rational(b)) => {
                    Number::Real(RealKind::Real(a * b.to_f64().unwrap_or(f64::NAN)))
                }
                (RealKind::Rational(a), RealKind::Real(b)) => {
                    Number::Real(RealKind::Real(a.to_f64().unwrap_or(f64::NAN) * b))
                }
                (RealKind::Real(a), RealKind::Integer(b)) => {
                    Number::Real(RealKind::Real(a * b.to_f64().unwrap_or(f64::NAN)))
                }
                (RealKind::Integer(a), RealKind::Real(b)) => {
                    Number::Real(RealKind::Real(a.to_f64().unwrap_or(f64::NAN) * b))
                }
            },
        }
    }
}

impl MulAssign<&Number> for Number {
    fn mul_assign(&mut self, rhs: &Number) {
        *self = &*self * rhs;
    }
}

impl Div<&Number> for &Number {
    type Output = Number;

    fn div(self, rhs: &Number) -> Self::Output {
        match (self, rhs) {
            (Number::Real(a), Number::Real(b)) => match (a, b) {
                (RealKind::Real(a), RealKind::Real(b)) => Number::Real(RealKind::Real(a / b)),
                (RealKind::Rational(a), RealKind::Rational(b)) => {
                    Number::Real(RealKind::Rational(a / b))
                }
                (RealKind::Integer(a), RealKind::Integer(b)) => {
                    Number::Real(RealKind::Rational(BigRational::new(a.clone(), b.clone())))
                }
                (RealKind::Rational(a), RealKind::Integer(b)) => {
                    Number::Real(RealKind::Rational(a / BigRational::from_integer(b.clone())))
                }
                (RealKind::Integer(a), RealKind::Rational(b)) => {
                    Number::Real(RealKind::Rational(BigRational::from_integer(a.clone()) / b))
                }
                (RealKind::Real(a), RealKind::Rational(b)) => {
                    Number::Real(RealKind::Real(a / b.to_f64().unwrap_or(1.0)))
                }
                (RealKind::Rational(a), RealKind::Real(b)) => {
                    Number::Real(RealKind::Real(a.to_f64().unwrap_or(f64::NAN) / b))
                }
                (RealKind::Real(a), RealKind::Integer(b)) => {
                    Number::Real(RealKind::Real(a / b.to_f64().unwrap_or(1.0)))
                }
                (RealKind::Integer(a), RealKind::Real(b)) => {
                    Number::Real(RealKind::Real(a.to_f64().unwrap_or(f64::NAN) / b))
                }
            },
        }
    }
}

impl DivAssign<&Number> for Number {
    fn div_assign(&mut self, rhs: &Number) {
        *self = &*self / rhs;
    }
}

impl FromStr for Number {
    type Err = ParseNumberError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // TODO: implement complex numbers and other bases
        if s.contains('/') {
            Ok(Number::Real(RealKind::Rational(
                BigRational::from_str(s).map_err(|e| ParseNumberError {
                    message: e.to_string(),
                })?,
            )))
        } else if s.contains(&['.', 'e', 'E']) {
            Ok(Number::Real(RealKind::Real(f64::from_str(s).map_err(
                |e| ParseNumberError {
                    message: e.to_string(),
                },
            )?)))
        } else {
            Ok(Number::Real(RealKind::Integer(
                BigInt::from_str(s).map_err(|e| ParseNumberError {
                    message: e.to_string(),
                })?,
            )))
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ParseNumberError {
    message: String,
}

impl Error for ParseNumberError {}

impl fmt::Display for ParseNumberError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_number() {
        assert_eq!("1".parse(), Ok(Number::Real(RealKind::Integer(1.into()))));
        assert_eq!("1.0".parse(), Ok(Number::Real(RealKind::Real(1.0))));
        assert_eq!(
            "1/2".parse(),
            Ok(Number::Real(RealKind::Rational(BigRational::new(
                1.into(),
                2.into()
            ))))
        );
        assert_eq!("-2.3".parse(), Ok(Number::Real(RealKind::Real(-2.3))));
        assert_eq!(
            "-4/5".parse(),
            Ok(Number::Real(RealKind::Rational(BigRational::new(
                (-4).into(),
                5.into()
            ))))
        );
        assert_eq!("1e2".parse(), Ok(Number::Real(RealKind::Real(1e2))));
    }

    #[test]
    fn parse_number_error() {
        assert!(Number::from_str("1.0.0").is_err());
        assert!(Number::from_str("1/0").is_err());
        assert!(Number::from_str("1.0/2").is_err());
    }

    #[test]
    fn predicates() {
        assert!(Number::Real(RealKind::Integer(1.into())).is_exact());
        assert!(Number::Real(RealKind::Rational(BigRational::new(1.into(), 2.into()))).is_exact());
        assert!(!Number::Real(RealKind::Real(1.0)).is_exact());

        assert!(Number::Real(RealKind::Integer(0.into())).is_zero());
        assert!(!Number::Real(RealKind::Integer(1.into())).is_zero());

        assert!(Number::Real(RealKind::Integer((-1).into())).is_negative());
        assert!(!Number::Real(RealKind::Integer(0.into())).is_negative());

        assert!(Number::Real(RealKind::Integer(1.into())).is_positive());
        assert!(!Number::Real(RealKind::Integer(0.into())).is_positive());

        assert!(Number::Real(RealKind::Integer(1.into())).is_integer());
        assert!(
            !Number::Real(RealKind::Rational(BigRational::new(1.into(), 2.into()))).is_integer()
        );
        assert!(
            Number::Real(RealKind::Rational(BigRational::new(2.into(), 1.into()))).is_integer()
        );
        assert!(Number::Real(RealKind::Real(1.0)).is_integer());
        assert!(!Number::Real(RealKind::Real(1.5)).is_integer());
    }

    #[test]
    fn comparison() {
        assert_eq!(
            Number::Real(RealKind::Real(2.0)),
            Number::Real(RealKind::Real(2.0))
        );
        assert_eq!(
            Number::Real(RealKind::Integer(2.into())),
            Number::Real(RealKind::Integer(2.into())),
        );
        assert_eq!(
            Number::Real(RealKind::Rational(BigRational::new(2.into(), 1.into()))),
            Number::Real(RealKind::Rational(BigRational::new(2.into(), 1.into())))
        );
        assert_eq!(
            Number::Real(RealKind::Integer(2.into())),
            Number::Real(RealKind::Rational(BigRational::new(2.into(), 1.into())))
        );
        assert_ne!(
            Number::Real(RealKind::Real(1.0)),
            Number::Real(RealKind::Integer(1.into()))
        );
        assert!(Number::eq_val(
            &Number::Real(RealKind::Real(1.0)),
            &Number::Real(RealKind::Integer(1.into()))
        ));
        assert_ne!(
            Number::Real(RealKind::Real(0.5)),
            Number::Real(RealKind::Rational(BigRational::new(1.into(), 2.into())))
        );
        assert!(Number::eq_val(
            &Number::Real(RealKind::Real(0.5)),
            &Number::Real(RealKind::Rational(BigRational::new(1.into(), 2.into())))
        ));

        assert!(
            RealKind::Rational(BigRational::new(1.into(), 2.into())) < RealKind::Integer(1.into())
        );
        assert!(RealKind::Integer(1.into()) > RealKind::Real(0.5));
        assert!(RealKind::Real(0.25) < RealKind::Rational(BigRational::new(1.into(), 2.into())));
    }

    #[test]
    fn operations() {
        assert_eq!(
            Number::Real(RealKind::Integer(5.into())).abs(),
            Number::Real(RealKind::Integer(5.into()))
        );
        assert_eq!(
            Number::Real(RealKind::Integer((-6).into())).abs(),
            Number::Real(RealKind::Integer(6.into()))
        );
        assert_eq!(
            Number::Real(RealKind::Rational(BigRational::new((-5).into(), 2.into()))).abs(),
            Number::Real(RealKind::Rational(BigRational::new(5.into(), 2.into())))
        );
        assert_eq!(
            Number::Real(RealKind::Real(-8.0)).abs(),
            Number::Real(RealKind::Real(8.0))
        );

        assert_eq!(
            Number::Real(RealKind::Real(1.6)).round(),
            Number::Real(RealKind::Real(2.0.into()))
        );
        assert_eq!(
            Number::Real(RealKind::Real(1.4)).round(),
            Number::Real(RealKind::Real(1.0.into()))
        );
        assert_eq!(
            Number::Real(RealKind::Rational(BigRational::new(8.into(), 5.into()))).floor(),
            Number::Real(RealKind::Integer(1.into()))
        );
        assert_eq!(
            Number::Real(RealKind::Rational(BigRational::new((-8).into(), 3.into()))).ceil(),
            Number::Real(RealKind::Integer((-2).into()))
        );
        assert_eq!(
            Number::Real(RealKind::Real(1.1)).trunc(),
            Number::Real(RealKind::Real(1.0.into()))
        );
    }
}
