use std::error::Error;
use std::fmt;
use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign};
use std::str::FromStr;

use num::{
    bigint::ToBigInt, BigInt, BigRational, FromPrimitive, Integer, Num, Signed, ToPrimitive, Zero,
};

fn perfect_root(n: &BigInt, exp: u32) -> Option<BigInt> {
    let t = n.nth_root(exp);
    if t.pow(exp) == *n {
        Some(t)
    } else {
        None
    }
}

#[derive(Debug, Clone)]
pub enum RealKind {
    Real(f64),
    Rational(BigRational),
    Integer(BigInt),
}

impl fmt::Display for RealKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RealKind::Real(r) => {
                let mag = r.abs();
                if mag != 0.0 && !(1e-16..=1e16).contains(&mag) {
                    write!(f, "{r:e}")
                } else if r.fract() == 0.0 {
                    r.fmt(f)?;
                    f.write_str(".0")
                } else {
                    r.fmt(f)
                }
            }
            RealKind::Rational(r) => r.fmt(f),
            RealKind::Integer(i) => i.fmt(f),
        }
    }
}

impl fmt::Binary for RealKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RealKind::Real(_) => panic!("Inexact numbers must be printed in decimal"),
            RealKind::Rational(r) => r.fmt(f),
            RealKind::Integer(i) => i.fmt(f),
        }
    }
}

impl fmt::Octal for RealKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RealKind::Real(_) => panic!("Inexact numbers must be printed in decimal"),
            RealKind::Rational(r) => r.fmt(f),
            RealKind::Integer(i) => i.fmt(f),
        }
    }
}

impl fmt::LowerHex for RealKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RealKind::Real(_) => panic!("Inexact numbers must be printed in decimal"),
            RealKind::Rational(r) => r.fmt(f),
            RealKind::Integer(i) => i.fmt(f),
        }
    }
}

impl fmt::UpperHex for RealKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RealKind::Real(_) => panic!("Inexact numbers must be printed in decimal"),
            RealKind::Rational(r) => r.fmt(f),
            RealKind::Integer(i) => i.fmt(f),
        }
    }
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
            RealKind::Rational(_) | RealKind::Integer(_) => true,
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
        RealKind::Real(self.to_inexact_raw())
    }

    pub fn to_inexact_raw(&self) -> f64 {
        match self {
            RealKind::Real(r) => *r,
            RealKind::Rational(r) => r.to_f64().unwrap_or(f64::NAN),
            RealKind::Integer(i) => i.to_f64().unwrap_or(f64::NAN),
        }
    }

    fn map_inexact<F: FnOnce(f64) -> f64>(&self, f: F) -> RealKind {
        RealKind::Real(f(self.to_inexact_raw()))
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
                    .and_then(|r| r.numer().to_f64())
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
                    .and_then(|r| r.denom().to_f64())
                    .unwrap_or(f64::NAN),
            ),
            RealKind::Rational(r) => RealKind::Integer(r.denom().clone()),
            RealKind::Integer(_) => RealKind::Integer(BigInt::from(1)),
        }
    }

    fn sqrt(&self) -> RealKind {
        if self.is_negative() {
            return RealKind::Real(f64::NAN);
        }
        match self {
            RealKind::Real(r) => RealKind::Real(r.sqrt()),
            RealKind::Rational(r) => {
                if let (Some(n), Some(d)) = (perfect_root(r.numer(), 2), perfect_root(r.denom(), 2))
                {
                    RealKind::Rational(BigRational::new(n, d))
                } else {
                    RealKind::Real(r.to_f64().unwrap_or(f64::NAN).sqrt())
                }
            }
            RealKind::Integer(i) => {
                if let Some(s) = perfect_root(i, 2) {
                    RealKind::Integer(s)
                } else {
                    RealKind::Real(i.to_f64().unwrap_or(f64::NAN).sqrt())
                }
            }
        }
    }

    pub fn pow(&self, exp: &Self) -> RealKind {
        match exp {
            RealKind::Real(r) => RealKind::Real(self.to_inexact_raw().powf(*r)),
            RealKind::Rational(r) => {
                let (Some(numer), Some(denom)) = (r.numer().to_i32(), r.denom().to_u32()) else {
                    return RealKind::Real(f64::NAN);
                };
                if denom.is_even() && self.is_negative() {
                    return RealKind::Real(f64::NAN);
                }
                match self {
                    RealKind::Rational(b) => {
                        if let (Some(n), Some(d)) = (
                            perfect_root(b.numer(), denom),
                            perfect_root(b.denom(), denom),
                        ) {
                            if numer.is_negative() && n.is_zero() {
                                RealKind::Real(f64::INFINITY)
                            } else {
                                RealKind::Rational(BigRational::new(n, d).pow(numer))
                            }
                        } else {
                            RealKind::Real(
                                b.to_f64()
                                    .unwrap_or(f64::NAN)
                                    .powf(r.to_f64().unwrap_or(f64::NAN)),
                            )
                        }
                    }
                    RealKind::Integer(b) => {
                        if let Some(s) = perfect_root(b, denom) {
                            if numer.is_negative() {
                                if s.is_zero() {
                                    RealKind::Real(f64::INFINITY)
                                } else {
                                    RealKind::Rational(BigRational::from_integer(s).pow(numer))
                                }
                            } else {
                                RealKind::Integer(s.pow(numer as u32))
                            }
                        } else {
                            RealKind::Real(
                                b.to_f64()
                                    .unwrap_or(f64::NAN)
                                    .powf(r.to_f64().unwrap_or(f64::NAN)),
                            )
                        }
                    }
                    RealKind::Real(b) => RealKind::Real(b.powf(r.to_f64().unwrap_or(f64::NAN))),
                }
            }
            RealKind::Integer(i) => {
                let Some(exp) = i.to_i32() else {
                    return RealKind::Real(f64::NAN);
                };
                if exp.is_negative() {
                    if self.is_zero() {
                        return RealKind::Real(f64::INFINITY);
                    }
                    match self {
                        RealKind::Rational(b) => RealKind::Rational(b.pow(exp)),
                        RealKind::Integer(b) => {
                            RealKind::Rational(BigRational::new(1.into(), b.pow(-exp as u32)))
                        }
                        RealKind::Real(b) => RealKind::Real(b.powi(exp)),
                    }
                } else {
                    match self {
                        RealKind::Rational(b) => RealKind::Rational(b.pow(exp)),
                        RealKind::Integer(b) => RealKind::Integer(b.pow(exp as u32)),
                        RealKind::Real(b) => RealKind::Real(b.powi(exp)),
                    }
                }
            }
        }
    }
}

impl PartialEq for RealKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (RealKind::Real(a), RealKind::Real(b)) => a == b,
            (RealKind::Rational(a), RealKind::Rational(b)) => a == b,
            (RealKind::Integer(a), RealKind::Integer(b)) => a == b,
            (RealKind::Rational(a), RealKind::Integer(b))
            | (RealKind::Integer(b), RealKind::Rational(a)) => a.is_integer() && a.numer() == b,
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
            _ => other.partial_cmp(self).map(std::cmp::Ordering::reverse),
        }
    }
}

// TODO: implement complex numbers
// #[derive(Debug, PartialEq, Clone)]
// pub enum ComplexKind {
//     Real(Complex<f64>),
//     Rational(Complex<BigRational>),
//     Integer(Complex<BigInt>),
// }

#[derive(Debug, PartialEq, Clone)]
pub enum Number {
    // unused
    // Complex(ComplexKind),
    Real(RealKind),
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Number::Real(r) => r.fmt(f),
        }
    }
}

impl fmt::Binary for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Number::Real(r) => r.fmt(f),
        }
    }
}

impl fmt::Octal for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Number::Real(r) => r.fmt(f),
        }
    }
}

impl fmt::LowerHex for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Number::Real(r) => r.fmt(f),
        }
    }
}

impl fmt::UpperHex for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Number::Real(r) => r.fmt(f),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ParseNumberError {
    message: String,
}

impl ParseNumberError {
    pub fn new(message: impl fmt::Display) -> Self {
        Self {
            message: message.to_string(),
        }
    }
}

impl Error for ParseNumberError {}

impl fmt::Display for ParseNumberError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl FromStr for Number {
    type Err = ParseNumberError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Number::from_str_default_radix(s, 10)
    }
}

impl Number {
    pub fn from_str_default_radix(s: &str, mut radix: u32) -> Result<Self, ParseNumberError> {
        let chars = s.as_bytes();
        let mut inexact = None;
        for i in 0..2 {
            match chars.get(i * 2) {
                Some(b'#') => match chars.get(i * 2 + 1) {
                    Some(b'b') => radix = 2,
                    Some(b'o') => radix = 8,
                    Some(b'd') => radix = 10,
                    Some(b'x') => radix = 16,
                    Some(b'e') => inexact = Some(false),
                    Some(b'i') => inexact = Some(true),
                    Some(c) => {
                        return Err(ParseNumberError {
                            message: format!("Invalid prefix: {c}"),
                        })
                    }
                    None => {
                        return Err(ParseNumberError {
                            message: "Expected prefix".to_owned(),
                        })
                    }
                },
                _ => return Number::from_str_radix(&s[(i * 2)..], radix, inexact),
            }
        }
        Number::from_str_radix(&s[4..], radix, inexact)
    }

    pub fn from_str_radix(
        s: &str,
        radix: u32,
        inexact: Option<bool>,
    ) -> Result<Self, ParseNumberError> {
        let has_slash = s.contains('/');
        let has_decimal = s.contains('.');
        let has_exp = s.contains(['e', 'E', 's', 'S', 'f', 'F', 'd', 'D', 'l', 'L']);
        let inexact = match inexact {
            Some(i) => i,
            None => {
                if radix == 10 {
                    has_decimal || has_exp
                } else if has_decimal {
                    return Err(ParseNumberError {
                        message: "Decimal only allowed with radix 10".to_owned(),
                    });
                } else {
                    false
                }
            }
        };
        if inexact {
            if has_slash {
                if has_decimal {
                    return Err(ParseNumberError {
                        message: "Decimal not allowed in rational".to_owned(),
                    });
                }
                if radix == 10 && has_exp {
                    return Err(ParseNumberError {
                        message: "Exponent not allowed in rational".to_owned(),
                    });
                }
                let (num, den) = s.split_once('/').unwrap();
                return Ok(Number::Real(RealKind::Real(
                    f64::from_str_radix(num, radix).map_err(ParseNumberError::new)?
                        / f64::from_str_radix(den, radix).map_err(ParseNumberError::new)?,
                )));
            }
            return Ok(Number::Real(RealKind::Real(
                f64::from_str_radix(
                    &s.replace(['s', 'S', 'f', 'F', 'd', 'D', 'l', 'L'], "e"),
                    radix,
                )
                .map_err(ParseNumberError::new)?,
            )));
        }
        if radix == 10 {
            if has_exp {
                let mut esplit = s.splitn(2, &['e', 'E', 's', 'S', 'f', 'F', 'd', 'D', 'l', 'L']);
                let num = esplit.next().unwrap();
                let exp = esplit
                    .next()
                    .unwrap()
                    .parse::<i32>()
                    .map_err(ParseNumberError::new)?;
                return Number::parse_exact_dec(num, exp);
            } else if has_decimal {
                return Number::parse_exact_dec(s, 0);
            }
        }
        if has_slash {
            Ok(Number::Real(RealKind::Rational(
                BigRational::from_str_radix(s, radix).map_err(ParseNumberError::new)?,
            )))
        } else {
            Ok(Number::Real(RealKind::Integer(
                BigInt::from_str_radix(s, radix).map_err(ParseNumberError::new)?,
            )))
        }
    }

    fn parse_exact_dec(s: &str, exp: i32) -> Result<Self, ParseNumberError> {
        let mut split = s.splitn(2, '.');
        let int_part = split.next().unwrap().to_owned();
        let dec_part = split.next().unwrap_or("");
        let exp = (dec_part.len() as i32) - exp;
        if exp < 0 {
            Ok(Number::Real(RealKind::Integer(
                BigInt::from_str(&(int_part + dec_part)).map_err(ParseNumberError::new)?
                    * BigInt::from(10).pow((-exp) as u32),
            )))
        } else {
            Ok(Number::Real(RealKind::Rational(BigRational::new(
                BigInt::from_str(&(int_part + dec_part)).map_err(ParseNumberError::new)?,
                BigInt::from(10).pow(exp as u32),
            ))))
        }
    }

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

    pub fn to_inexact_raw(&self) -> f64 {
        match self {
            Number::Real(r) => r.to_inexact_raw(),
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

    pub fn sqrt(&self) -> Self {
        match self {
            Number::Real(r) => Number::Real(r.sqrt()),
        }
    }

    pub fn pow(&self, exp: &Self) -> Self {
        match (self, exp) {
            (Number::Real(a), Number::Real(b)) => Number::Real(a.pow(b)),
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
        assert_eq!(
            "#xDEADBEEF".parse(),
            Ok(Number::Real(RealKind::Integer(0xdeadbeef_u32.into())))
        );
        assert_eq!(
            "#o12345670".parse(),
            Ok(Number::Real(RealKind::Integer(0o12345670_u32.into())))
        );
        assert_eq!(
            "#b110010".parse(),
            Ok(Number::Real(RealKind::Integer(0b110010_u32.into())))
        );
        assert_eq!(
            "#e1e2".parse(),
            Ok(Number::Real(RealKind::Integer(100.into())))
        );
        assert_eq!("#i#x8/10".parse(), Ok(Number::Real(RealKind::Real(0.5))));
        assert_eq!(
            "#e1.2345".parse(),
            Ok(Number::Real(RealKind::Rational(BigRational::new(
                12345.into(),
                10000.into()
            ))))
        );
        assert_eq!(
            "#e1.2L3".parse(),
            Ok(Number::Real(RealKind::Integer(1200.into())))
        );
        assert_eq!("6f0".parse(), Ok(Number::Real(RealKind::Real(6.0))));
    }

    #[test]
    fn parse_number_error() {
        assert!(Number::from_str("1.0.0").is_err());
        assert!(Number::from_str("1/0").is_err());
        assert!(Number::from_str("1.0/2").is_err());
        assert!(Number::from_str("99/1e2").is_err());
        assert!(Number::from_str("1e2.0").is_err());
        assert!(Number::from_str("#x123.abc").is_err());
        assert!(Number::from_str("#q123").is_err());
        assert!(Number::from_str("#x123e-1").is_err());
        assert!(Number::from_str("#b2").is_err());
        assert!(Number::from_str("#i").is_err());
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

        assert_eq!(
            Number::Real(RealKind::Integer(4.into())).sqrt(),
            Number::Real(RealKind::Integer(2.into()))
        );
        assert_eq!(
            Number::Real(RealKind::Real(4.0)).sqrt(),
            Number::Real(RealKind::Real(2.0.into()))
        );
        assert_eq!(
            Number::Real(RealKind::Rational(BigRational::new(4.into(), 9.into()))).sqrt(),
            Number::Real(RealKind::Rational(BigRational::new(2.into(), 3.into())))
        );
        assert!(!Number::Real(RealKind::Integer(5.into())).sqrt().is_exact());
        assert!(
            !Number::Real(RealKind::Rational(BigRational::new(1.into(), 3.into())))
                .sqrt()
                .is_exact()
        );

        assert_eq!(
            Number::Real(RealKind::Integer(2.into()))
                .pow(&Number::Real(RealKind::Integer(3.into()))),
            Number::Real(RealKind::Integer(8.into()))
        );
        assert_eq!(
            Number::Real(RealKind::Integer(2.into()))
                .pow(&Number::Real(RealKind::Integer(0.into()))),
            Number::Real(RealKind::Integer(1.into()))
        );
        assert_eq!(
            Number::Real(RealKind::Integer(2.into()))
                .pow(&Number::Real(RealKind::Integer((-2).into()))),
            Number::Real(RealKind::Rational(BigRational::new(1.into(), 4.into())))
        );

        assert_eq!(
            Number::Real(RealKind::Integer(2.into())).pow(&Number::Real(RealKind::Real(3.0))),
            Number::Real(RealKind::Real(8.0.into()))
        );
        assert_eq!(
            Number::Real(RealKind::Real(2.0)).pow(&Number::Real(RealKind::Integer(3.into()))),
            Number::Real(RealKind::Real(8.0.into()))
        );

        assert_eq!(
            Number::Real(RealKind::Integer(8.into())).pow(&Number::Real(RealKind::Rational(
                BigRational::new(2.into(), 3.into())
            ))),
            Number::Real(RealKind::Integer(4.into()))
        );
        assert_eq!(
            Number::Real(RealKind::Rational(BigRational::new(
                (-32).into(),
                243.into()
            )))
            .pow(&Number::Real(RealKind::Rational(BigRational::new(
                (-3).into(),
                5.into()
            )))),
            Number::Real(RealKind::Rational(BigRational::new((-27).into(), 8.into())))
        );
    }
}
