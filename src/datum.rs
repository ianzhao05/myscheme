use std::cell::RefCell;
use std::fmt;

use crate::interner::Symbol;
use crate::number::Number;

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
