use std::io::{self, Write};
use std::{cell::RefCell, rc::Rc};

use crate::env::Env;
use crate::err::SchemeError;
use crate::evaler::eval;
use crate::expr::ExprOrDef;
use crate::lexer::Lexer;
use crate::object::ObjectRef;
use crate::parser::parse;
use crate::reader::Reader;

fn until_err<T, E>(err: &mut &mut Result<(), SchemeError>, item: Result<T, E>) -> Option<T>
where
    E: std::convert::Into<SchemeError>,
{
    match item {
        Ok(item) => Some(item),
        Err(e) => {
            **err = Err(e.into());
            None
        }
    }
}

pub fn parse_str(s: &str) -> Result<Vec<ExprOrDef>, SchemeError> {
    let (mut le, mut re) = (Ok(()), Ok(()));
    let tokens = Lexer::new(s).scan(&mut le, until_err);
    let data = Reader::new(tokens).scan(&mut re, until_err);
    let exprs = data
        .map(parse)
        .collect::<Result<Vec<_>, _>>()
        .map_err(Into::into);
    le?;
    re?;
    exprs
}

pub fn eval_str(s: &str, env: Rc<RefCell<Env>>) -> Result<Vec<ObjectRef>, SchemeError> {
    let (mut le, mut re, mut pe) = (Ok(()), Ok(()), Ok(()));
    let tokens = Lexer::new(s).scan(&mut le, until_err);
    let data = Reader::new(tokens).scan(&mut re, until_err);
    let exprs = data.map(parse).scan(&mut pe, until_err);
    let res = exprs
        .map(|expr| eval(expr, env.clone()).map_err(Into::into))
        .collect();
    le?;
    re?;
    pe?;
    res
}

pub fn repl() {
    let env = Env::primitives();
    loop {
        let mut line = String::new();
        print!(">>> ");
        io::stdout().flush().unwrap();
        if io::stdin().read_line(&mut line).unwrap() == 0 {
            break;
        }
        match eval_str(&line, env.clone()) {
            Ok(res) => {
                for r in res {
                    match r {
                        ObjectRef::Void => (),
                        _ => println!("{r:?}"),
                    }
                }
            }
            Err(e) => {
                println!("{e}");
            }
        }
    }
}
