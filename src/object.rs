use core::ops::Deref;
use std::cell::RefCell;
use std::pin::Pin;
use std::rc::Rc;

use crate::datum::*;
use crate::proc::Procedure;

#[derive(Debug, Clone)]
pub enum ObjectRef {
    Object(Rc<Object>),
    EmptyList,
    Void,
}

impl ObjectRef {
    pub fn new(o: Object) -> Self {
        Self::Object(Rc::new(o))
    }

    pub fn new_pair(car: ObjectRef, cdr: ObjectRef) -> Self {
        Self::new(Object::Pair(RefCell::new((car, cdr))))
    }

    pub fn equal(this: &Self, other: &Self) -> bool {
        match (this, other) {
            (ObjectRef::Object(o1), ObjectRef::Object(o2)) => o1 == o2,
            (ObjectRef::EmptyList, ObjectRef::EmptyList) => true,
            (ObjectRef::Void, ObjectRef::Void) => true,
            _ => false,
        }
    }

    pub fn try_deref(&self) -> Option<&Object> {
        match self {
            ObjectRef::Object(o) => Some(o),
            _ => None,
        }
    }

    pub fn try_deref_or<E, F: FnOnce(&Self) -> E>(&self, err: F) -> Result<&Object, E> {
        match self {
            ObjectRef::Object(o) => Ok(o),
            _ => Err(err(self)),
        }
    }

    pub fn pin(self) -> Pin<Rc<Object>> {
        match self {
            ObjectRef::Object(o) => Pin::new(o),
            _ => panic!("Cannot pin {:?}", self),
        }
    }
}

impl Deref for ObjectRef {
    type Target = Object;

    fn deref(&self) -> &Self::Target {
        match self {
            ObjectRef::Object(o) => o,
            _ => panic!("Cannot deref {:?}", self),
        }
    }
}

impl PartialEq for ObjectRef {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ObjectRef::Object(o1), ObjectRef::Object(o2)) => match (&**o1, &**o2) {
                (Object::Atom(a), Object::Atom(b)) => a == b,
                (Object::Pair(_), Object::Pair(_)) => Rc::ptr_eq(o1, o2),
                (Object::Vector(_), Object::Vector(_)) => Rc::ptr_eq(o1, o2),
                (Object::Procedure(_), Object::Procedure(_)) => Rc::ptr_eq(o1, o2),
                _ => false,
            },
            (ObjectRef::EmptyList, ObjectRef::EmptyList) => true,
            (ObjectRef::Void, ObjectRef::Void) => true,
            _ => false,
        }
    }
}

impl From<Datum> for ObjectRef {
    fn from(d: Datum) -> Self {
        match d {
            Datum::Simple(s) => ObjectRef::new(Object::Atom(s)),
            Datum::Compound(c) => match c {
                CompoundDatum::List(l) => match l {
                    ListKind::Proper(v) => v
                        .into_iter()
                        .rev()
                        .map(ObjectRef::from)
                        .fold(ObjectRef::EmptyList, |a, b| ObjectRef::new_pair(b, a)),
                    ListKind::Improper(v, b) => v
                        .into_iter()
                        .rev()
                        .map(ObjectRef::from)
                        .fold(ObjectRef::from(*b), |a, b| ObjectRef::new_pair(b, a)),
                    ListKind::Abbreviation(p, b) => ObjectRef::new_pair(
                        ObjectRef::new(Object::Atom(SimpleDatum::Symbol(
                            p.to_keyword().to_string(),
                        ))),
                        ObjectRef::new_pair(ObjectRef::from(*b), ObjectRef::EmptyList),
                    ),
                },
                CompoundDatum::Vector(v) => ObjectRef::new(Object::Vector(RefCell::new(
                    v.into_iter().map(ObjectRef::from).collect(),
                ))),
            },
            Datum::EmptyList => ObjectRef::EmptyList,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Object {
    Atom(SimpleDatum),
    Pair(RefCell<(ObjectRef, ObjectRef)>),
    Vector(RefCell<Vec<ObjectRef>>),
    Procedure(Procedure),
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Object::Atom(a), Object::Atom(b)) => a == b,
            (Object::Pair(p1), Object::Pair(p2)) => {
                let b1 = p1.borrow();
                let b2 = p2.borrow();
                ObjectRef::equal(&b1.0, &b2.0) && ObjectRef::equal(&b1.1, &b2.1)
            }
            (Object::Vector(v1), Object::Vector(v2)) => {
                let b1 = &*v1.borrow();
                let b2 = &*v2.borrow();
                b1.len() == b2.len() && b1.iter().zip(b2).all(|(a, b)| ObjectRef::equal(&*a, &*b))
            }
            (Object::Procedure(p1), Object::Procedure(p2)) => p1 == p2,
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
            *ObjectRef::from(bool_datum!(true)),
            Object::Atom(SimpleDatum::Boolean(true))
        );

        assert_eq!(ObjectRef::from(Datum::EmptyList), ObjectRef::EmptyList);

        assert_eq!(
            *ObjectRef::from(proper_list_datum![
                symbol_datum!("a"),
                symbol_datum!("b"),
                symbol_datum!("c")
            ]),
            *ObjectRef::new_pair(
                ObjectRef::new(atom_obj!(symbol_datum!("a"))),
                ObjectRef::new_pair(
                    ObjectRef::new(atom_obj!(symbol_datum!("b"))),
                    ObjectRef::new_pair(
                        ObjectRef::new(atom_obj!(symbol_datum!("c"))),
                        ObjectRef::EmptyList
                    )
                )
            )
        );

        assert_eq!(
            *ObjectRef::from(improper_list_datum![
                symbol_datum!("a"),
                symbol_datum!("b");
                symbol_datum!("c")
            ]),
            *ObjectRef::new_pair(
                ObjectRef::new(atom_obj!(symbol_datum!("a"))),
                ObjectRef::new_pair(
                    ObjectRef::new(atom_obj!(symbol_datum!("b"))),
                    ObjectRef::new(atom_obj!(symbol_datum!("c")))
                )
            )
        );

        assert_eq!(
            *ObjectRef::from(abbr_list_datum!(
                AbbreviationPrefix::Quote,
                symbol_datum!("a")
            )),
            *ObjectRef::new_pair(
                ObjectRef::new(atom_obj!(symbol_datum!("quote"))),
                ObjectRef::new_pair(
                    ObjectRef::new(atom_obj!(symbol_datum!("a"))),
                    ObjectRef::EmptyList
                )
            )
        );
    }

    #[test]
    fn nested_conversion() {
        assert_eq!(
            *ObjectRef::from(proper_list_datum![
                proper_list_datum![symbol_datum!("a"), symbol_datum!("b")],
                improper_list_datum![
                    symbol_datum!("c"),
                    symbol_datum!("d");
                    symbol_datum!("e")
                ],
                abbr_list_datum!(AbbreviationPrefix::Quote, symbol_datum!("f")),
                vector_datum![symbol_datum!("g"), symbol_datum!("h")]
            ]),
            *ObjectRef::new_pair(
                ObjectRef::new_pair(
                    ObjectRef::new(atom_obj!(symbol_datum!("a"))),
                    ObjectRef::new_pair(
                        ObjectRef::new(atom_obj!(symbol_datum!("b"))),
                        ObjectRef::EmptyList
                    )
                ),
                ObjectRef::new_pair(
                    ObjectRef::new_pair(
                        ObjectRef::new(atom_obj!(symbol_datum!("c"))),
                        ObjectRef::new_pair(
                            ObjectRef::new(atom_obj!(symbol_datum!("d"))),
                            ObjectRef::new(atom_obj!(symbol_datum!("e")))
                        )
                    ),
                    ObjectRef::new_pair(
                        ObjectRef::new_pair(
                            ObjectRef::new(atom_obj!(symbol_datum!("quote"))),
                            ObjectRef::new_pair(
                                ObjectRef::new(atom_obj!(symbol_datum!("f"))),
                                ObjectRef::EmptyList
                            )
                        ),
                        ObjectRef::new_pair(
                            ObjectRef::new(Object::Vector(RefCell::new(vec![
                                ObjectRef::new(atom_obj!(symbol_datum!("g"))),
                                ObjectRef::new(atom_obj!(symbol_datum!("h")))
                            ]))),
                            ObjectRef::EmptyList
                        )
                    )
                )
            )
        );
    }

    #[test]
    fn equivalence() {
        let o1 = ObjectRef::from(int_datum!(1));
        let o2 = ObjectRef::from(int_datum!(1));
        assert_eq!(o1, o2);
        assert!(ObjectRef::equal(&o1, &o2));

        let o1 = ObjectRef::from(proper_list_datum![
            symbol_datum!("a"),
            symbol_datum!("b"),
            symbol_datum!("c")
        ]);
        assert_eq!(o1, o1);
        let o2 = ObjectRef::from(proper_list_datum![
            symbol_datum!("a"),
            symbol_datum!("b"),
            symbol_datum!("c")
        ]);
        assert_ne!(o1, o2);
        assert!(ObjectRef::equal(&o1, &o2));

        let o3 = ObjectRef::from(proper_list_datum![
            symbol_datum!("a"),
            symbol_datum!("b"),
            symbol_datum!("d")
        ]);
        let o4 = ObjectRef::from(improper_list_datum![
            symbol_datum!("a"),
            symbol_datum!("b");
            symbol_datum!("c")
        ]);
        assert_ne!(o1, o3);
        assert_ne!(o1, o4);
        assert!(!ObjectRef::equal(&o1, &o3));
        assert!(!ObjectRef::equal(&o1, &o4));
    }
}
