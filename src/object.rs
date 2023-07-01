use core::ops::Deref;
use std::cell::RefCell;
use std::fmt::{self, Write};
use std::rc::Rc;

use crate::datum::*;
use crate::port::Port;
use crate::proc::Procedure;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum EnvSpec {
    SchemeReport,
    Null,
}

#[derive(Debug, Clone, Default)]
pub enum ObjectRef {
    Object(Rc<Object>),
    EnvSpec(EnvSpec),
    #[default]
    Undefined,
    EmptyList,
    Void,
    Eof,
}

impl ObjectRef {
    pub fn new(o: Object) -> Self {
        Self::Object(Rc::new(o))
    }

    pub fn new_pair(car: ObjectRef, cdr: ObjectRef) -> Self {
        Self::new(Object::Pair(RefCell::new((car, cdr))))
    }

    pub fn new_vector(v: Vec<ObjectRef>) -> Self {
        Self::new(Object::Vector(RefCell::new(v)))
    }

    pub fn new_string(s: String) -> Self {
        Self::new(Object::Atom(SimpleDatum::String(RefCell::new(s))))
    }

    pub fn equal(this: &Self, other: &Self) -> bool {
        match (this, other) {
            (ObjectRef::Object(o1), ObjectRef::Object(o2)) => o1 == o2,
            (ObjectRef::EmptyList, ObjectRef::EmptyList)
            | (ObjectRef::Void, ObjectRef::Void)
            | (ObjectRef::Eof, ObjectRef::Eof) => true,
            (ObjectRef::EnvSpec(a), ObjectRef::EnvSpec(b)) => a == b,
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
}

impl Deref for ObjectRef {
    type Target = Object;

    fn deref(&self) -> &Self::Target {
        match self {
            ObjectRef::Object(o) => o,
            _ => panic!("Cannot deref {self:?}"),
        }
    }
}

impl PartialEq for ObjectRef {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ObjectRef::Object(o1), ObjectRef::Object(o2)) => match (&**o1, &**o2) {
                (Object::Atom(SimpleDatum::String(_)), Object::Atom(SimpleDatum::String(_))) => {
                    Rc::ptr_eq(o1, o2)
                }
                (Object::Atom(a), Object::Atom(b)) => a == b,
                (Object::Pair(_), Object::Pair(_))
                | (Object::Vector(_), Object::Vector(_))
                | (Object::Procedure(_), Object::Procedure(_))
                | (Object::Port(_), Object::Port(_)) => Rc::ptr_eq(o1, o2),
                _ => false,
            },
            (ObjectRef::EmptyList, ObjectRef::EmptyList)
            | (ObjectRef::Void, ObjectRef::Void)
            | (ObjectRef::Eof, ObjectRef::Eof) => true,
            (ObjectRef::EnvSpec(a), ObjectRef::EnvSpec(b)) => a == b,
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
                        .map(ObjectRef::from)
                        .rfold(ObjectRef::EmptyList, |a, b| ObjectRef::new_pair(b, a)),
                    ListKind::Improper(v, b) => v
                        .into_iter()
                        .map(ObjectRef::from)
                        .rfold(ObjectRef::from(*b), |a, b| ObjectRef::new_pair(b, a)),
                },
                CompoundDatum::Vector(v) => ObjectRef::new(Object::Vector(RefCell::new(
                    v.into_iter().map(ObjectRef::from).collect(),
                ))),
            },
            Datum::EmptyList => ObjectRef::EmptyList,
        }
    }
}

impl fmt::Display for ObjectRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ObjectRef::Object(o) => match &**o {
                Object::Atom(a) => a.fmt(f),
                Object::Pair(p) => {
                    let mut first = true;
                    let mut cur = p.clone();
                    loop {
                        cur = {
                            if first {
                                first = false;
                                f.write_char('(')?;
                            } else {
                                f.write_char(' ')?;
                            }
                            let b = cur.borrow();
                            b.0.fmt(f)?;
                            match &b.1 {
                                ObjectRef::Object(o) => match &**o {
                                    Object::Pair(p) => p.clone(),
                                    _ => {
                                        f.write_str(" . ")?;
                                        b.1.fmt(f)?;
                                        break f.write_char(')');
                                    }
                                },
                                ObjectRef::EmptyList => break f.write_char(')'),
                                uov => {
                                    f.write_str(" . ")?;
                                    uov.fmt(f)?;
                                    break f.write_char(')');
                                }
                            }
                        }
                    }
                }
                Object::Vector(v) => {
                    let b = v.borrow();
                    f.write_str("#(")?;
                    for (i, o) in b.iter().enumerate() {
                        if i > 0 {
                            f.write_char(' ')?;
                        }
                        o.fmt(f)?;
                    }
                    f.write_char(')')
                }
                Object::Procedure(p) => p.fmt(f),
                Object::Port(p) => p.fmt(f),
            },
            ObjectRef::EmptyList => f.write_str("()"),
            ObjectRef::Undefined => panic!("Undefined value should not be encountered"),
            ObjectRef::Void => f.write_str("#<void>"),
            ObjectRef::Eof => f.write_str("#<eof>"),
            ObjectRef::EnvSpec(_) => f.write_str("#<environment>"),
        }
    }
}

#[derive(Debug)]
pub enum Object {
    Atom(SimpleDatum),
    Pair(RefCell<(ObjectRef, ObjectRef)>),
    Vector(RefCell<Vec<ObjectRef>>),
    Procedure(Procedure),
    Port(Port),
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
                b1.len() == b2.len() && b1.iter().zip(b2).all(|(a, b)| ObjectRef::equal(a, b))
            }
            (Object::Procedure(p1), Object::Procedure(p2)) => p1 == p2,
            _ => false,
        }
    }
}

impl Drop for Object {
    fn drop(&mut self) {
        use std::mem::take;

        let mut st = Vec::new();
        match self {
            Object::Pair(p) => {
                let mut b = p.borrow_mut();
                st.push(take(&mut b.0));
                st.push(take(&mut b.1));
            }
            Object::Vector(v) => {
                st.extend(v.borrow_mut().iter_mut().map(take));
            }
            _ => {}
        }
        while let Some(o) = st.pop() {
            if let ObjectRef::Object(o) = o {
                match &Rc::try_unwrap(o) {
                    Ok(Object::Pair(p)) => {
                        let mut b = p.borrow_mut();
                        st.push(take(&mut b.0));
                        st.push(take(&mut b.1));
                    }
                    Ok(Object::Vector(v)) => {
                        st.extend(v.borrow_mut().iter_mut().map(take));
                    }
                    _ => {}
                }
            }
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
            *ObjectRef::from(proper_list_datum![
                symbol_datum!("quote"),
                symbol_datum!("a")
            ]),
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
                proper_list_datum![symbol_datum!("quote"), symbol_datum!("f")],
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

    #[test]
    fn display() {
        assert_eq!(ObjectRef::from(int_datum!(1)).to_string(), "1");

        assert_eq!(ObjectRef::from(bool_datum!(true)).to_string(), "#t");

        assert_eq!(ObjectRef::from(char_datum!('a')).to_string(), "a");

        assert_eq!(ObjectRef::from(str_datum!("abc")).to_string(), "abc");

        assert_eq!(
            ObjectRef::from(proper_list_datum![
                int_datum!(1),
                int_datum!(2),
                int_datum!(3)
            ])
            .to_string(),
            "(1 2 3)"
        );

        assert_eq!(
            ObjectRef::from(improper_list_datum![
                int_datum!(1),
                proper_list_datum!(int_datum!(2), int_datum!(3));
                int_datum!(4)])
            .to_string(),
            "(1 (2 3) . 4)"
        );

        assert_eq!(
            ObjectRef::from(vector_datum![int_datum!(1), int_datum!(2), int_datum!(3)]).to_string(),
            "#(1 2 3)"
        );
    }
}
