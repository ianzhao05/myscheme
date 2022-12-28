mod eq;
mod list;
mod numeric;

use std::collections::HashMap;

use lazy_static::lazy_static;

use crate::evaler::EvalError;
use crate::expr::ExprOrDef;
use crate::object::{Object, ObjectRef};
use crate::parser::parse;
use crate::proc::{Primitive, Procedure};
use crate::reader::Reader;
use crate::tokenize;

pub type PrimitiveMap = HashMap<&'static str, fn(&[ObjectRef]) -> Result<ObjectRef, EvalError>>;

fn merge(maps: &[PrimitiveMap]) -> PrimitiveMap {
    let mut m: PrimitiveMap = HashMap::new();
    for n in maps {
        m.extend(n);
    }
    m
}

pub fn primitives() -> HashMap<String, ObjectRef> {
    merge(&[numeric::primitives(), eq::primitives(), list::primitives()])
        .into_iter()
        .map(|(k, v)| {
            (
                k.to_owned(),
                ObjectRef::new(Object::Procedure(Procedure::Primitive(Primitive::new(
                    k, v,
                )))),
            )
        })
        .collect()
}

pub fn prelude() -> &'static [ExprOrDef] {
    lazy_static! {
        static ref PRELUDE: Vec<ExprOrDef> = {
            let prelude = vec![numeric::PRELUDE, eq::PRELUDE, list::PRELUDE].join("");
            Reader::new(tokenize!(&prelude).expect("Error lexing prelude").iter())
                .collect::<Result<Vec<_>, _>>()
                .expect("Error reading prelude")
                .iter()
                .map(parse)
                .collect::<Result<Vec<_>, _>>()
                .expect("Error parsing prelude")
        };
    }
    &PRELUDE
}
