mod control;
mod delay;
mod eq;
mod io;
mod list;
mod numeric;
mod pred;
mod string;
mod symbol;
mod vector;

mod utils;

use std::collections::HashMap;

use crate::expr::ExprOrDef;
use crate::interner::Symbol;
use crate::interpret::parse_str;
use crate::object::{Object, ObjectRef};
use crate::proc::{Primitive, PrimitiveFunc, Procedure};

pub fn primitives() -> HashMap<Symbol, ObjectRef> {
    utils::merge(&[
        numeric::primitives(),
        eq::primitives(),
        list::primitives(),
        pred::primitives(),
        vector::primitives(),
        io::primitives(),
        string::primitives(),
        symbol::primitives(),
    ])
    .into_iter()
    .map(|(k, v)| {
        (
            k.into(),
            ObjectRef::new(Object::Procedure(Procedure::Primitive(Primitive::new(
                k,
                PrimitiveFunc::Args(v),
            )))),
        )
    })
    .chain(control::primitives().into_iter().map(|(k, v)| {
        (
            k.into(),
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
            eq::PRELUDE,
            list::PRELUDE,
            delay::PRELUDE,
            pred::PRELUDE,
            control::PRELUDE,
            vector::PRELUDE,
            io::PRELUDE,
            string::PRELUDE,
        ]
        .join(""),
    )
    .expect("Prelude should parse without error");
}
