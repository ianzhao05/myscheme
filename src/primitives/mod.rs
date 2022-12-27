mod numeric;

use std::{collections::HashMap, rc::Rc};

use crate::evaler::EvalError;
use crate::object::Object;
use crate::proc::{Primitive, Procedure};

use self::numeric::numeric_primitives;

pub type PrimitiveMap = HashMap<&'static str, fn(&[Object]) -> Result<Object, EvalError>>;

fn merge(maps: &[PrimitiveMap]) -> PrimitiveMap {
    let mut m: PrimitiveMap = HashMap::new();
    for n in maps {
        m.extend(n);
    }
    m
}

pub fn primitives() -> HashMap<String, Object> {
    merge(&[numeric_primitives()])
        .into_iter()
        .map(|(k, v)| {
            (
                k.to_owned(),
                Object::Procedure(Rc::new(Procedure::Primitive(Primitive::new(k, v)))),
            )
        })
        .collect()
}
