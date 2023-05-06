mod control;
mod delay;
mod eq;
mod list;
mod numeric;
mod pred;

use std::collections::HashMap;

use crate::evaler::EvalError;
use crate::expr::ExprOrDef;
use crate::interpret::parse_str;
use crate::object::{Object, ObjectRef};
use crate::proc::{Primitive, PrimitiveFunc, Procedure};

pub type PrimitiveMap = HashMap<&'static str, fn(&[ObjectRef]) -> Result<ObjectRef, EvalError>>;

fn merge(maps: &[PrimitiveMap]) -> PrimitiveMap {
    let mut m: PrimitiveMap = HashMap::new();
    for n in maps {
        m.extend(n);
    }
    m
}

pub fn primitives() -> HashMap<String, ObjectRef> {
    merge(&[
        numeric::primitives(),
        eq::primitives(),
        list::primitives(),
        pred::primitives(),
    ])
    .into_iter()
    .map(|(k, v)| {
        (
            k.to_owned(),
            ObjectRef::new(Object::Procedure(Procedure::Primitive(Primitive::new(
                k,
                PrimitiveFunc::Args(v),
            )))),
        )
    })
    .chain(control::primitives().into_iter().map(|(k, v)| {
        (
            k.to_owned(),
            ObjectRef::new(Object::Procedure(Procedure::Primitive(Primitive::new(
                k,
                PrimitiveFunc::State(v),
            )))),
        )
    }))
    .collect()
}

thread_local! {
    pub static PRELUDE: Vec<ExprOrDef> = parse_str(
        &vec![
            numeric::PRELUDE,
            list::PRELUDE,
            delay::PRELUDE,
            pred::PRELUDE,
            control::PRELUDE,
        ]
        .join(""),
    )
    .expect("Prelude should parse without error");
}
