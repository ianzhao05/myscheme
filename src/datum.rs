use crate::number::Number;

#[derive(Debug, PartialEq)]
pub enum SimpleDatumKind {
    Boolean(bool),
    Number(Number),
    Character(char),
    String(String),
    Symbol(String),
}

#[derive(Debug, PartialEq)]
pub enum AbbreviationPrefix {
    Quote,
    Quasiquote,
    Unquote,
    UnquoteSplicing,
}

#[derive(Debug, PartialEq)]
pub enum ListKind {
    Proper(Vec<Datum>),
    Improper(Vec<Datum>, Box<Datum>),
    Abbreviation(AbbreviationPrefix, Box<Datum>),
}

#[derive(Debug, PartialEq)]
pub enum CompoundDatumKind {
    List(ListKind),
    Vector(Vec<Datum>),
}

#[derive(Debug, PartialEq)]
pub enum Datum {
    Simple(SimpleDatumKind),
    Compound(CompoundDatumKind),
    EmptyList,
}
