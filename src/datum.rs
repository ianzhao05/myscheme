use std::cell::RefCell;
use std::fmt;

use crate::interner::Symbol;
use crate::number::Number;
use crate::object::{Object, ObjectRef};

#[derive(Debug, PartialEq, Clone)]
pub enum SimpleDatum {
    Boolean(bool),
    Number(Number),
    Character(char),
    String(RefCell<String>),
    Symbol(Symbol),
}

impl fmt::Display for SimpleDatum {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SimpleDatum::Boolean(b) => {
                if *b {
                    f.write_str("#t")
                } else {
                    f.write_str("#f")
                }
            }
            SimpleDatum::Number(n) => n.fmt(f),
            SimpleDatum::Character(c) => {
                if f.alternate() {
                    match c {
                        '\n' => f.write_str("#\\newline"),
                        ' ' => f.write_str("#\\space"),
                        _ => write!(f, "#\\{c}"),
                    }
                } else {
                    c.fmt(f)
                }
            }
            SimpleDatum::String(s) => {
                if f.alternate() {
                    write!(f, "{:?}", s.borrow())
                } else {
                    s.borrow().fmt(f)
                }
            }
            SimpleDatum::Symbol(s) => s.fmt(f),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ListKind {
    Proper(Vec<Datum>),
    Improper(Vec<Datum>, Box<Datum>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum CompoundDatum {
    List(ListKind),
    Vector(Vec<Datum>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Datum {
    Simple(SimpleDatum),
    Compound(CompoundDatum),
    EmptyList,
}

impl TryFrom<ObjectRef> for Datum {
    type Error = ();

    fn try_from(obj: ObjectRef) -> Result<Self, Self::Error> {
        match obj {
            ObjectRef::Object(o) => match &*o {
                Object::Atom(a) => Ok(Datum::Simple(a.clone())),
                Object::Pair(p) => Ok(Datum::Compound(CompoundDatum::List({
                    let mut acc = vec![];
                    let mut cur = p.clone();
                    loop {
                        cur = {
                            let b = cur.borrow();
                            acc.push(b.0.clone().try_into()?);
                            match &b.1 {
                                ObjectRef::Object(o) => match &**o {
                                    Object::Pair(p) => p.clone(),
                                    _ => {
                                        break ListKind::Improper(
                                            acc,
                                            Box::new(b.1.clone().try_into()?),
                                        );
                                    }
                                },
                                ObjectRef::EmptyList => {
                                    break ListKind::Proper(acc);
                                }
                                _ => return Err(()),
                            }
                        }
                    }
                }))),
                Object::Vector(v) => Ok(Datum::Compound(CompoundDatum::Vector(
                    v.borrow()
                        .iter()
                        .cloned()
                        .map(Datum::try_from)
                        .collect::<Result<_, _>>()?,
                ))),
                _ => Err(()),
            },
            ObjectRef::EmptyList => Ok(Datum::EmptyList),
            _ => Err(()),
        }
    }
}
