use std::io::{self, Write};

use crate::env::CombinedEnv;
use crate::err::SchemeError;
use crate::evaler::eval;
use crate::lexer::{Lexer, SexpReader, Token};
use crate::object::ObjectRef;
use crate::parser::parse_top_level;
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

pub fn eval_str(s: &str, (env, syn_env): &CombinedEnv) -> Result<Vec<ObjectRef>, SchemeError> {
    let (mut le, mut re, mut pe) = (Ok(()), Ok(()), Ok(()));
    let tokens = Lexer::new(s).scan(&mut le, until_err);
    let data = Reader::new(tokens).scan(&mut re, until_err);
    let exprs = data
        .flat_map(|datum| parse_top_level(datum, syn_env))
        .scan(&mut pe, until_err);
    let res = exprs
        .map(|expr| eval(expr, env.clone()).map_err(Into::into))
        .collect();
    le?;
    re?;
    pe?;
    res
}

pub fn eval_tokens(
    tokens: Vec<Token>,
    (env, syn_env): &CombinedEnv,
) -> Result<Vec<ObjectRef>, SchemeError> {
    let (mut re, mut pe) = (Ok(()), Ok(()));
    let data = Reader::new(tokens.into_iter()).scan(&mut re, until_err);
    let exprs = data
        .flat_map(|datum| parse_top_level(datum, syn_env))
        .scan(&mut pe, until_err);
    let res = exprs
        .map(|expr| eval(expr, env.clone()).map_err(Into::into))
        .collect();
    re?;
    pe?;
    res
}

pub fn write_results(res: Result<Vec<ObjectRef>, SchemeError>) {
    match res {
        Ok(objs) => {
            for o in objs {
                match o {
                    ObjectRef::Void => (),
                    _ => println!("{o:#}"),
                }
            }
        }
        Err(e) => {
            println!("{e}");
        }
    }
}

pub fn repl(env: &CombinedEnv) -> std::io::Result<()> {
    let mut sreader = SexpReader::new(String::new());
    loop {
        if sreader.buf.is_empty() {
            print!("> ");
        } else {
            print!("| ");
        }
        io::stdout().flush()?;
        if io::stdin().read_line(&mut sreader.buf)? == 0 {
            break;
        }
        match sreader.try_tokenize(false) {
            Ok(ready) => {
                if ready {
                    write_results(eval_tokens(sreader.take_tokens(), env));
                }
            }
            Err(e) => write_results(Err(SchemeError::Lexer(e))),
        }
    }
    Ok(())
}
