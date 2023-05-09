use myscheme::env::Env;
use std::env;
use std::process::ExitCode;

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    match args.len() {
        1 => match myscheme::repl() {
            Ok(_) => ExitCode::SUCCESS,
            Err(e) => {
                eprintln!("Error: {}", e);
                ExitCode::FAILURE
            }
        },
        2 => {
            let file = std::fs::read_to_string(&args[1]);
            match file {
                Ok(s) => {
                    myscheme::write_results(myscheme::eval_str(&s, Env::primitives()));
                    ExitCode::SUCCESS
                }
                Err(e) => {
                    eprintln!("Error: {}", e);
                    ExitCode::FAILURE
                }
            }
        }
        _ => {
            eprintln!("Usage: {} [file]", args[0]);
            ExitCode::FAILURE
        }
    }
}
