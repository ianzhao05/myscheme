use std::io;
use std::io::Write;

use crate::env::Env;
use crate::evaler::{eval, EvalResult};
use crate::object::ObjectRef;
use crate::parser::parse;
use crate::reader::Reader;
use crate::tokenize;

pub fn run() {
    let env = Env::primitives();
    loop {
        let mut line = String::new();
        print!(">>> ");
        io::stdout().flush().unwrap();
        if io::stdin().read_line(&mut line).unwrap() == 0 {
            break;
        }
        let tokens = match tokenize!(&line) {
            Ok(t) => t,
            Err(e) => {
                println!("{e}");
                continue;
            }
        };
        let data = match Reader::new(tokens.into_iter()).collect::<Result<Vec<_>, _>>() {
            Ok(d) => d,
            Err(e) => {
                println!("{e}");
                continue;
            }
        };
        let exprs = match data.into_iter().map(parse).collect::<Result<Vec<_>, _>>() {
            Ok(e) => e,
            Err(e) => {
                println!("{e}");
                continue;
            }
        };
        for expr in exprs {
            let res = eval(&expr, env.clone());
            match res {
                Ok(EvalResult::Expr(o)) => match o {
                    ObjectRef::Void => (),
                    _ => println!("{o:?}"),
                },
                Ok(EvalResult::Def) => (),
                Err(e) => {
                    println!("{e}");
                    break;
                }
            }
        }
    }
}
