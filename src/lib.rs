pub mod cont;
pub mod datum;
pub mod env;
pub mod err;
pub mod evaler;
pub mod expr;
pub mod interpret;
pub mod lexer;
pub mod number;
pub mod object;
pub mod parser;
pub mod primitives;
pub mod proc;
pub mod reader;
pub mod trampoline;

pub use crate::interpret::{eval_str, repl};

#[cfg(test)]
pub mod test_util {
    macro_rules! tokenize {
        ($e:expr) => {
            $crate::lexer::Lexer::new($e).collect::<Result<Vec<_>, _>>()
        };
    }
    pub(crate) use tokenize;

    macro_rules! int_datum {
        ($n:expr) => {
            $crate::datum::Datum::Simple($crate::datum::SimpleDatum::Number(
                $crate::number::Number::Real($crate::number::RealKind::Integer(num::BigInt::from(
                    $n,
                ))),
            ))
        };
    }
    pub(crate) use int_datum;

    macro_rules! rational_datum {
        ($n:expr,$d:expr) => {
            $crate::datum::Datum::Simple($crate::datum::SimpleDatum::Number(
                $crate::number::Number::Real($crate::number::RealKind::Rational(
                    num::BigRational::new(num::BigInt::from($n), num::BigInt::from($d)),
                )),
            ))
        };
    }
    pub(crate) use rational_datum;

    macro_rules! real_datum {
        ($n:expr) => {
            $crate::datum::Datum::Simple($crate::datum::SimpleDatum::Number(
                $crate::number::Number::Real($crate::number::RealKind::Real($n)),
            ))
        };
    }
    pub(crate) use real_datum;

    macro_rules! bool_datum {
        ($b:expr) => {
            $crate::datum::Datum::Simple($crate::datum::SimpleDatum::Boolean($b))
        };
    }
    pub(crate) use bool_datum;

    macro_rules! char_datum {
        ($c:expr) => {
            $crate::datum::Datum::Simple($crate::datum::SimpleDatum::Character($c))
        };
    }
    pub(crate) use char_datum;

    macro_rules! str_datum {
        ($s:expr) => {
            $crate::datum::Datum::Simple($crate::datum::SimpleDatum::String($s.to_owned()))
        };
    }
    pub(crate) use str_datum;

    macro_rules! symbol_datum {
        ($s:expr) => {
            $crate::datum::Datum::Simple($crate::datum::SimpleDatum::Symbol($s.to_owned()))
        };
    }
    pub(crate) use symbol_datum;

    macro_rules! proper_list_datum {
        ($($d:expr),* $(,)?) => {
            $crate::datum::Datum::Compound($crate::datum::CompoundDatum::List(
                $crate::datum::ListKind::Proper(
                    vec![$($d),*],
                )))
        };
    }
    pub(crate) use proper_list_datum;

    macro_rules! improper_list_datum {
        ($($d:expr),* ; $l:expr $(,)?) => {
            $crate::datum::Datum::Compound($crate::datum::CompoundDatum::List(
                $crate::datum::ListKind::Improper(
                    vec![$($d),*],
                    Box::new($l),
                )))
        };
    }
    pub(crate) use improper_list_datum;

    macro_rules! abbr_list_datum {
        ($k:expr, $d:expr $(,)?) => {
            $crate::datum::Datum::Compound($crate::datum::CompoundDatum::List(
                $crate::datum::ListKind::Abbreviation($k, Box::new($d)),
            ))
        };
    }
    pub(crate) use abbr_list_datum;

    macro_rules! vector_datum {
        ($($d:expr),* $(,)?) => {
            $crate::datum::Datum::Compound($crate::datum::CompoundDatum::Vector(
                vec![$($d),*],
            ))
        };
    }
    pub(crate) use vector_datum;

    macro_rules! int_expr {
        ($n:expr) => {
            $crate::expr::Expr::Literal($crate::expr::LiteralKind::SelfEvaluating(
                $crate::expr::SelfEvaluatingKind::Number($crate::number::Number::Real(
                    $crate::number::RealKind::Integer(num::BigInt::from($n)),
                )),
            ))
        };
    }
    pub(crate) use int_expr;

    macro_rules! bool_expr {
        ($b:expr) => {
            $crate::expr::Expr::Literal($crate::expr::LiteralKind::SelfEvaluating(
                $crate::expr::SelfEvaluatingKind::Boolean($b),
            ))
        };
    }
    pub(crate) use bool_expr;

    macro_rules! char_expr {
        ($c:expr) => {
            $crate::expr::Expr::Literal($crate::expr::LiteralKind::SelfEvaluating(
                $crate::expr::SelfEvaluatingKind::Character($c),
            ))
        };
    }
    pub(crate) use char_expr;

    macro_rules! str_expr {
        ($s:expr) => {
            $crate::expr::Expr::Literal($crate::expr::LiteralKind::SelfEvaluating(
                $crate::expr::SelfEvaluatingKind::String($s.to_owned()),
            ))
        };
    }
    pub(crate) use str_expr;

    macro_rules! var_expr {
        ($s:expr) => {
            $crate::expr::Expr::Variable($s.to_owned())
        };
    }
    pub(crate) use var_expr;

    macro_rules! atom_obj {
        ($d:expr) => {
            match $d {
                $crate::datum::Datum::Simple(s) => $crate::object::Object::Atom(s),
                _ => panic!("Expected simple datum, got {:?}", $d),
            }
        };
    }
    pub(crate) use atom_obj;

    macro_rules! vec_rc {
        ($($e:expr),* $(,)?) => {
            vec![$($e.into()),*]
        };
    }
    pub(crate) use vec_rc;
}
