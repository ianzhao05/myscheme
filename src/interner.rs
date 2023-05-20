use std::collections::HashMap;
use std::fmt;
use std::sync::Mutex;

use lazy_static::lazy_static;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Symbol(usize);

struct Interner {
    map: HashMap<String, Symbol>,
    strings: Vec<String>,
}

impl Interner {
    fn new() -> Self {
        Self {
            map: HashMap::new(),
            strings: Vec::new(),
        }
    }

    fn intern(&mut self, s: String) -> Symbol {
        match self.map.get(&s) {
            Some(&sym) => sym,
            None => {
                let sym = Symbol(self.strings.len());
                self.map.insert(s.clone(), sym);
                self.strings.push(s);
                sym
            }
        }
    }

    fn intern_ref(&mut self, s: &str) -> Symbol {
        match self.map.get(s) {
            Some(&sym) => sym,
            None => {
                let sym = Symbol(self.strings.len());
                self.map.insert(s.to_owned(), sym);
                self.strings.push(s.to_owned());
                sym
            }
        }
    }

    fn resolve(&self, sym: Symbol) -> &str {
        &self.strings[sym.0]
    }
}

lazy_static! {
    static ref INTERNER: Mutex<Interner> = Mutex::new(Interner::new());
}

impl From<String> for Symbol {
    fn from(s: String) -> Self {
        INTERNER.lock().unwrap().intern(s)
    }
}

impl From<&str> for Symbol {
    fn from(s: &str) -> Self {
        INTERNER.lock().unwrap().intern_ref(s)
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(INTERNER.lock().unwrap().resolve(*self))
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Symbol")
            .field("index", &self.0)
            .field("str", &INTERNER.lock().unwrap().resolve(*self))
            .finish()
    }
}
