use std::env;
use std::io;
use std::path::Path;
use std::process::ExitCode;
use std::rc::Rc;

use myscheme::env::Env;

fn run_file<P: AsRef<Path>>(path: P, env: Rc<Env>, interactive: bool) -> io::Result<()> {
    let s = std::fs::read_to_string(path)?;
    myscheme::write_results(myscheme::eval_str(&s, env.clone()));
    if interactive {
        myscheme::repl(env)
    } else {
        Ok(())
    }
}

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    let res = match args.len() {
        1 => myscheme::repl(Env::primitives()),
        2 if &args[1] != "-i" => run_file(&args[1], Env::primitives(), false),
        3 if &args[1] == "-i" => run_file(&args[2], Env::primitives(), true),
        _ => {
            eprintln!("Usage: {} [-i] [file]", args[0]);
            return ExitCode::FAILURE;
        }
    };
    match res {
        Ok(_) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("Error: {e}");
            ExitCode::FAILURE
        }
    }
}
