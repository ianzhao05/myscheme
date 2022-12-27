use std::sync::atomic::{AtomicUsize, Ordering};
use std::{cell::RefCell, rc::Rc};

use crate::env::Env;
use crate::expr::ProcData;

static NEXT_ID: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug, Clone)]
pub struct Procedure {
    id: usize,
    data: ProcData,
    env: Rc<RefCell<Env>>,
}

impl Procedure {
    pub fn new(data: ProcData, env: Rc<RefCell<Env>>) -> Self {
        Self {
            id: NEXT_ID.fetch_add(1, Ordering::Relaxed),
            data,
            env,
        }
    }
}

impl PartialEq for Procedure {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
