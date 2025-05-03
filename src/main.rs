use std::env;
use std::io;
use std::path::Path;
use std::process::ExitCode;
use std::rc::Rc;

use myscheme::env::Env;
use myscheme::parser::syn_env::SynEnv;

fn run_file<P: AsRef<Path>>(
    path: P,
    env: &Rc<Env>,
    syn_env: &Rc<SynEnv>,
    interactive: bool,
) -> io::Result<()> {
    let s = std::fs::read_to_string(path)?;
    myscheme::write_results(myscheme::eval_str(&s, env, syn_env));
    if interactive {
        myscheme::repl(env, syn_env)
    } else {
        Ok(())
    }
}

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    let res = match args.len() {
        1 => myscheme::repl(&Env::primitives(), &SynEnv::builtin()),
        2 if &args[1] != "-i" => run_file(
            &args[1],
            &Env::primitives(),
            &SynEnv::builtin(),
            false,
        ),
        3 if &args[1] == "-i" => {
            run_file(&args[2], &Env::primitives(), &SynEnv::builtin(), true)
        }
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
