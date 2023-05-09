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

pub fn until_err<T, E>(err: &mut &mut Result<(), SchemeError>, item: Result<T, E>) -> Option<T>
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

fn is_ready(s: &str) -> bool {
    let mut open_count = 0;
    let mut in_string = false;
    let mut in_comment = false;
    let mut prev = '\0';
    let mut pprev = '\0';
    let mut esc = false;
    for c in s.chars() {
        match c {
            '\n' => in_comment = false,
            _ if in_comment => (),
            '(' => {
                if !in_string && pprev != '#' && prev != '\\' {
                    open_count += 1;
                }
            }
            ')' => {
                if !in_string && pprev != '#' && prev != '\\' {
                    if open_count == 0 {
                        return true;
                    } else {
                        open_count -= 1;
                    }
                }
            }
            '"' => {
                if prev != '\\' || !esc {
                    in_string = !in_string;
                }
            }
            ';' => {
                if !in_string && pprev != '#' && prev != '\\' {
                    in_comment = true;
                }
            }
            _ => (),
        }
        pprev = prev;
        prev = c;
        esc = !esc && c == '\\';
    }
    open_count == 0 && in_string == false
}

pub fn repl() -> std::io::Result<()> {
    let env = Env::primitives();
    let mut line = String::new();
    loop {
        if line.is_empty() {
            print!("> ");
        } else {
            print!("| ");
        }
        io::stdout().flush()?;
        if io::stdin().read_line(&mut line)? == 0 {
            break;
        }
        if is_ready(&line) {
            write_results(eval_str(&line, env.clone()));
            line.clear();
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::is_ready;

    #[test]
    fn is_ready_unclosed_paren() {
        assert!(!is_ready("(+ 1 2"));
        assert!(is_ready("(+ 1 2)"));
        assert!(is_ready("(+ 1 2))"));
        assert!(!is_ready("()(()()"));
        assert!(is_ready("()(()())"));
        assert!(is_ready(r#" "(" "#));
        assert!(!is_ready(r#" ( ")" "#));
    }

    #[test]
    fn is_ready_unclosed_string() {
        assert!(!is_ready(r#" "hello "#));
        assert!(is_ready(r#" "hello" "#));
        assert!(!is_ready(r#" "hello\" "#));
        assert!(is_ready(r#" "hello\"" "#));
        assert!(!is_ready(r#" "hello\\"" "#));
        assert!(is_ready(r#" "hello\\\"" "#));
        assert!(!is_ready(r#" "hello\\\\"" "#));
    }

    #[test]
    fn is_ready_comments() {
        assert!(!is_ready("( ; )))"));
        assert!(is_ready(r#" " ; " "#));
        assert!(is_ready("( ; )\n )"));
    }

    #[test]
    fn is_ready_chars() {
        assert!(is_ready(r"#\("));
        assert!(!is_ready(r"(#\)"));
        assert!(is_ready(r#" #\" "#));
        assert!(!is_ready(r##" "#\" "##));
        assert!(is_ready(r"( #\; )"));
    }
}
