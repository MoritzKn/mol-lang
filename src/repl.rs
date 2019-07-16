use std::io::{self, BufRead, Write};

use crate::interpreter;
use crate::parser;

pub fn start() {
    let stdin = io::stdin();

    print!("> ");
    io::stdout().flush().unwrap();

    for line in stdin.lock().lines() {
        match parser::parse_string(&line.unwrap()) {
            Ok(program) => match interpreter::exec(program) {
                Ok(result) => {
                    if let Some(value) = result {
                        println!("{:?}", value);
                    }
                }
                Err(error) => {
                    println!("Runtime error {:?}", error);
                }
            },
            Err(error) => {
                println!("{}", error);
            }
        }

        print!("> ");
        io::stdout().flush().unwrap();
    }
}
