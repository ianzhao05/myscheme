use std::env;
use std::io;
use std::path::Path;
use std::process::ExitCode;

use myscheme::env::{primitive_env, CombinedEnv};

fn run_file<P: AsRef<Path>>(path: P, env: &CombinedEnv, interactive: bool) -> io::Result<()> {
    let s = std::fs::read_to_string(path)?;
    myscheme::write_results(myscheme::eval_str(&s, env));
    if interactive {
        myscheme::repl(env)
    } else {
        Ok(())
    }
}

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    let res = match args.len() {
        1 => myscheme::repl(&primitive_env()),
        2 if &args[1] != "-i" => run_file(&args[1], &primitive_env(), false),
        3 if &args[1] == "-i" => run_file(&args[2], &primitive_env(), true),
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
