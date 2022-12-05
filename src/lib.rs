pub mod datum;
pub mod lexer;
pub mod number;
pub mod read;
pub mod repl;

#[cfg(test)]
pub mod test_util {
    use super::datum::*;
    use super::number::*;

    pub fn integer_datum(n: i64) -> Datum {
        Datum::Simple(SimpleDatumKind::Number(Number::Real(RealKind::Integer(
            num::BigInt::from(n),
        ))))
    }
}
