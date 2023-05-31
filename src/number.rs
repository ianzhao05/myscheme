use std::error::Error;
use std::fmt;
use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign};
use std::str::FromStr;

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

    pub fn to_integer(&self) -> Option<BigInt> {
        match self {
            Number::Real(r) => r.to_integer(),
        }
    }
}

impl Add for Number {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
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
                    Number::Real(RealKind::Rational(a + BigRational::from(b)))
                }
                (RealKind::Integer(a), RealKind::Rational(b)) => {
                    Number::Real(RealKind::Rational(BigRational::from(a) + b))
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

impl AddAssign for Number {
    fn add_assign(&mut self, rhs: Self) {
        *self = self.clone() + rhs;
    }
}

impl Sub for Number {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
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
                    Number::Real(RealKind::Rational(a - BigRational::from(b)))
                }
                (RealKind::Integer(a), RealKind::Rational(b)) => {
                    Number::Real(RealKind::Rational(BigRational::from(a) - b))
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

impl SubAssign for Number {
    fn sub_assign(&mut self, rhs: Self) {
        *self = self.clone() - rhs;
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

impl Mul for Number {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
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
                    Number::Real(RealKind::Rational(a * BigRational::from(b)))
                }
                (RealKind::Integer(a), RealKind::Rational(b)) => {
                    Number::Real(RealKind::Rational(BigRational::from(a) * b))
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

impl MulAssign for Number {
    fn mul_assign(&mut self, rhs: Self) {
        *self = self.clone() * rhs;
    }
}

impl Div for Number {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Real(a), Number::Real(b)) => match (a, b) {
                (RealKind::Real(a), RealKind::Real(b)) => Number::Real(RealKind::Real(a / b)),
                (RealKind::Rational(a), RealKind::Rational(b)) => {
                    Number::Real(RealKind::Rational(a / b))
                }
                (RealKind::Integer(a), RealKind::Integer(b)) => {
                    Number::Real(RealKind::Rational(BigRational::new(a, b)))
                }
                (RealKind::Rational(a), RealKind::Integer(b)) => {
                    Number::Real(RealKind::Rational(a / BigRational::from(b)))
                }
                (RealKind::Integer(a), RealKind::Rational(b)) => {
                    Number::Real(RealKind::Rational(BigRational::from(a) / b))
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

impl DivAssign for Number {
    fn div_assign(&mut self, rhs: Self) {
        *self = self.clone() / rhs;
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
}
