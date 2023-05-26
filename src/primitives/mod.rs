mod char;
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

use once_cell::sync::Lazy;

use crate::interner::Symbol;
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
        char::primitives(),
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

pub static PRELUDE: Lazy<String> = Lazy::new(|| {
    [
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
    .join("")
});
