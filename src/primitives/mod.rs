mod eq;
mod list;
mod numeric;

use std::collections::HashMap;

use lazy_static::lazy_static;

use crate::evaler::EvalError;
use crate::expr::ExprOrDef;
use crate::interpret::parse_str;
use crate::object::{Object, ObjectRef};
use crate::proc::{Primitive, Procedure};

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
        static ref PRELUDE: Vec<ExprOrDef> =
            parse_str(&vec![numeric::PRELUDE, eq::PRELUDE, list::PRELUDE].join(""))
                .expect("Prelude should parse without error");
    }
    &PRELUDE
}
