mod char;
mod control;
mod eq;
mod eval;
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

use crate::env::EnvBinding;
use crate::interner::Symbol;
use crate::object::{Object, ObjectRef};
use crate::proc::{Primitive, PrimitiveFunc, Procedure};

pub(crate) fn primitives() -> HashMap<Symbol, EnvBinding> {
    [
        numeric::primitives(),
        eq::primitives(),
        list::primitives(),
        pred::primitives(),
        vector::primitives(),
        io::primitives(),
        string::primitives(),
        symbol::primitives(),
        char::primitives(),
        eval::primitives(),
    ]
    .into_iter()
    .flat_map(|m| {
        m.into_iter().map(|(k, v)| {
            (
                k.into(),
                EnvBinding::Variable(ObjectRef::new(Object::Procedure(Procedure::Primitive(
                    Primitive::new(k, PrimitiveFunc::Args(v)),
                )))),
            )
        })
    })
    .chain(
        [control::cprimitives(), eval::cprimitives()]
            .into_iter()
            .flat_map(|m| {
                m.into_iter().map(|(k, v)| {
                    (
                        k.into(),
                        EnvBinding::Variable(ObjectRef::new(Object::Procedure(
                            Procedure::Primitive(Primitive::new(k, PrimitiveFunc::State(v))),
                        ))),
                    )
                })
            }),
    )
    .collect()
}

pub(crate) static PRELUDE: Lazy<String> = Lazy::new(|| {
    [
        numeric::PRELUDE,
        eq::PRELUDE,
        list::PRELUDE,
        pred::PRELUDE,
        control::PRELUDE,
        vector::PRELUDE,
        io::PRELUDE,
        string::PRELUDE,
    ]
    .join("")
});
