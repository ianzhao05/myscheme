use crate::number::Number;

#[derive(Debug, PartialEq, Clone)]
pub enum SimpleDatum {
    Boolean(bool),
    Number(Number),
    Character(char),
    String(String),
    Symbol(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum AbbreviationPrefix {
    Quote,
    Quasiquote,
    Unquote,
    UnquoteSplicing,
}

impl AbbreviationPrefix {
    pub fn to_keyword(&self) -> &str {
        match self {
            AbbreviationPrefix::Quote => "quote",
            AbbreviationPrefix::Quasiquote => "quasiquote",
            AbbreviationPrefix::Unquote => "unquote",
            AbbreviationPrefix::UnquoteSplicing => "unquote-splicing",
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ListKind {
    Proper(Vec<Datum>),
    Improper(Vec<Datum>, Box<Datum>),
    Abbreviation(AbbreviationPrefix, Box<Datum>),
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
