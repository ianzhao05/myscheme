use std::io;
use std::io::Write;

use crate::tokenize;

pub fn run() {
    loop {
        let mut line = String::new();
        print!(">>> ");
        io::stdout().flush().unwrap();
        io::stdin().read_line(&mut line).unwrap();
        let tokens = tokenize!(&line);
        println!("{tokens:?}");
    }
}
