use rustyline::error::ReadlineError;
use rustyline::Editor;

use crate::interpreter;
use crate::parser;

pub fn start() {
    let mut rl = Editor::<()>::new();

    loop {
        match rl.readline("> ") {
            Ok(line) => {
                match parser::parse_string(&line) {
                    Ok(program) => match interpreter::exec(program) {
                        Ok(result) => {
                            if let Some(value) = result {
                                println!("{:?}", value);
                            }
                        }
                        Err(error) => println!("{:?}", error),
                    },
                    Err(error) => println!("{}", error),
                }

                rl.add_history_entry(line);
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
}
