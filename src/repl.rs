use std::io;
use std::io::Write;

use crate::read::Reader;
use crate::tokenize;

pub fn run() {
    loop {
        let mut line = String::new();
        print!(">>> ");
        io::stdout().flush().unwrap();
        io::stdin().read_line(&mut line).unwrap();
        let tokens = tokenize!(&line);
        if let Err(e) = tokens {
            println!("{e}");
            continue;
        }
        let data = Reader::new(tokens.unwrap().iter()).collect::<Result<Vec<_>, _>>();
        println!("{data:?}");
    }
}
