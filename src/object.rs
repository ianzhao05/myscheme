use std::cell::RefCell;
use std::rc::Rc;

use crate::datum::*;
use crate::proc::Procedure;

#[derive(Debug, PartialEq, Clone)]
pub struct Pair(Rc<RefCell<Object>>, Rc<RefCell<Object>>);

impl Pair {
    pub fn new(car: Object, cdr: Object) -> Self {
        Self(Rc::new(RefCell::new(car)), Rc::new(RefCell::new(cdr)))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Atom(SimpleDatum),
    Pair(Rc<RefCell<Pair>>),
    Vector(Rc<RefCell<Vec<Object>>>),
    Procedure(Rc<Procedure>),
    EmptyList,
    Void,
}

impl From<Datum> for Object {
    fn from(d: Datum) -> Self {
        match d {
            Datum::Simple(s) => Object::Atom(s),
            Datum::Compound(c) => match c {
                CompoundDatum::List(l) => match l {
                    ListKind::Proper(v) => v
                        .into_iter()
                        .rev()
                        .map(Object::from)
                        .fold(Object::EmptyList, |a, b| {
                            Object::Pair(Rc::new(RefCell::new(Pair::new(b, a))))
                        }),
                    ListKind::Improper(v, b) => v
                        .into_iter()
                        .rev()
                        .map(Object::from)
                        .fold(Object::from(*b), |a, b| {
                            Object::Pair(Rc::new(RefCell::new(Pair::new(b, a))))
                        }),
                    ListKind::Abbreviation(p, b) => Object::Pair(Rc::new(RefCell::new(Pair::new(
                        Object::Atom(SimpleDatum::Symbol(p.to_keyword().to_string())),
                        Object::Pair(Rc::new(RefCell::new(Pair::new(
                            Object::from(*b),
                            Object::EmptyList,
                        )))),
                    )))),
                },
                CompoundDatum::Vector(v) => Object::Vector(Rc::new(RefCell::new(
                    v.into_iter().map(Object::from).collect(),
                ))),
            },
            Datum::EmptyList => Object::EmptyList,
        }
    }
}

impl Object {
    pub fn eqv(this: &Self, other: &Self) -> bool {
        // eq? and eqv?
        match (this, other) {
            (Object::Atom(a), Object::Atom(b)) => a == b,
            (Object::Pair(a), Object::Pair(b)) => Rc::ptr_eq(a, b),
            (Object::Vector(a), Object::Vector(b)) => Rc::ptr_eq(a, b),
            (Object::Procedure(a), Object::Procedure(b)) => Rc::ptr_eq(a, b),
            (Object::EmptyList, Object::EmptyList) => true,
            (Object::Void, Object::Void) => true,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::*;

    #[test]
    fn datum_conversion() {
        assert_eq!(
            Object::from(bool_datum!(true)),
            Object::Atom(SimpleDatum::Boolean(true))
        );

        assert_eq!(Object::from(Datum::EmptyList), Object::EmptyList);

        assert_eq!(
            Object::from(proper_list_datum![
                symbol_datum!("a"),
                symbol_datum!("b"),
                symbol_datum!("c")
            ]),
            pair_obj!(
                atom_obj!(symbol_datum!("a")),
                pair_obj!(
                    atom_obj!(symbol_datum!("b")),
                    pair_obj!(atom_obj!(symbol_datum!("c")), Object::EmptyList)
                )
            )
        );

        assert_eq!(
            Object::from(improper_list_datum![
                symbol_datum!("a"),
                symbol_datum!("b");
                symbol_datum!("c")
            ]),
            pair_obj!(
                atom_obj!(symbol_datum!("a")),
                pair_obj!(atom_obj!(symbol_datum!("b")), atom_obj!(symbol_datum!("c")))
            )
        );

        assert_eq!(
            Object::from(abbr_list_datum!(
                AbbreviationPrefix::Quote,
                symbol_datum!("a")
            )),
            pair_obj!(
                atom_obj!(symbol_datum!("quote")),
                pair_obj!(atom_obj!(symbol_datum!("a")), Object::EmptyList)
            )
        );
    }

    #[test]
    fn nested_conversion() {
        assert_eq!(
            Object::from(proper_list_datum![
                proper_list_datum![symbol_datum!("a"), symbol_datum!("b")],
                improper_list_datum![
                    symbol_datum!("c"),
                    symbol_datum!("d");
                    symbol_datum!("e")
                ],
                abbr_list_datum!(AbbreviationPrefix::Quote, symbol_datum!("f")),
                vector_datum![symbol_datum!("g"), symbol_datum!("h")]
            ]),
            pair_obj!(
                pair_obj!(
                    atom_obj!(symbol_datum!("a")),
                    pair_obj!(atom_obj!(symbol_datum!("b")), Object::EmptyList)
                ),
                pair_obj!(
                    pair_obj!(
                        atom_obj!(symbol_datum!("c")),
                        pair_obj!(atom_obj!(symbol_datum!("d")), atom_obj!(symbol_datum!("e")))
                    ),
                    pair_obj!(
                        pair_obj!(
                            atom_obj!(symbol_datum!("quote")),
                            pair_obj!(atom_obj!(symbol_datum!("f")), Object::EmptyList)
                        ),
                        pair_obj!(
                            vector_obj!(
                                atom_obj!(symbol_datum!("g")),
                                atom_obj!(symbol_datum!("h"))
                            ),
                            Object::EmptyList
                        )
                    )
                )
            )
        );
    }

    #[test]
    fn equivalence() {
        let o1 = Object::from(int_datum!(1));
        let o2 = Object::from(int_datum!(1));
        assert_eq!(o1, o2);
        assert!(Object::eqv(&o1, &o2));

        let o1 = Object::from(proper_list_datum![
            symbol_datum!("a"),
            symbol_datum!("b"),
            symbol_datum!("c")
        ]);
        assert!(Object::eqv(&o1, &o1));
        let o2 = Object::from(proper_list_datum![
            symbol_datum!("a"),
            symbol_datum!("b"),
            symbol_datum!("c")
        ]);
        assert_eq!(o1, o2);
        assert!(!Object::eqv(&o1, &o2));

        let o3 = Object::from(proper_list_datum![
            symbol_datum!("a"),
            symbol_datum!("b"),
            symbol_datum!("d")
        ]);
        let o4 = Object::from(improper_list_datum![
            symbol_datum!("a"),
            symbol_datum!("b");
            symbol_datum!("c")
        ]);
        assert_ne!(o1, o3);
        assert_ne!(o1, o4);
        assert!(!Object::eqv(&o1, &o3));
        assert!(!Object::eqv(&o1, &o4));
    }
}
