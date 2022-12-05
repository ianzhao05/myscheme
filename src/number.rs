use std::error::Error;
use std::fmt;
use std::str::FromStr;

use num::{BigInt, BigRational, Complex};

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum RealKind {
    Real(f64),
    Rational(BigRational),
    Integer(BigInt),
}

#[derive(Debug, PartialEq, Clone)]

pub enum ComplexKind {
    Real(Complex<f64>),
    Rational(Complex<BigRational>),
    Integer(Complex<BigInt>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Number {
    Complex(ComplexKind), // unused
    Real(RealKind),
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

#[derive(Debug)]
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
        assert_eq!(
            Number::from_str("1").unwrap(),
            Number::Real(RealKind::Integer(BigInt::from(1)))
        );
        assert_eq!(
            Number::from_str("1.0").unwrap(),
            Number::Real(RealKind::Real(1.0))
        );
        assert_eq!(
            Number::from_str("1/2").unwrap(),
            Number::Real(RealKind::Rational(BigRational::new(
                BigInt::from(1),
                BigInt::from(2)
            )))
        );
        assert_eq!(
            Number::from_str("-2.3").unwrap(),
            Number::Real(RealKind::Real(-2.3))
        );
        assert_eq!(
            Number::from_str("-4/5").unwrap(),
            Number::Real(RealKind::Rational(BigRational::new(
                BigInt::from(-4),
                BigInt::from(5)
            )))
        );
        assert_eq!(
            Number::from_str("1e2").unwrap(),
            Number::Real(RealKind::Real(1e2))
        );
    }

    #[test]
    fn parse_number_error() {
        assert!(Number::from_str("1.0.0").is_err());
        assert!(Number::from_str("1/0").is_err());
        assert!(Number::from_str("1.0/2").is_err());
    }
}
