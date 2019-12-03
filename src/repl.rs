use rustyline::error::ReadlineError;
use rustyline::Editor;

use crate::interpreter;
use crate::parser;

pub fn start() {
    let mut rl = Editor::<()>::new();
    let mut context = interpreter::Context::new();

    loop {
        match rl.readline("> ") {
            Ok(line) => {
                match parser::parse_string(&line) {
                    Ok(program) => match interpreter::exec_with_context(program, &mut context) {
                        Ok(interpreter::Value::Void) => (),
                        Ok(value) => {
                            use colored::*;
                            use interpreter::Value::*;

                            let text = value.print();
                            let text = match value {
                                Number(_) | Boolean(_) => text.yellow(),
                                String(_) => text.green(),
                                Function(_) | NativeFunction(_) => text.blue(),
                                _ => text.normal(),
                            };
                            println!("{}", text)
                        }
                        Err(throw) => println!("Uncaught {}", throw),
                    },
                    Err(error) => {
                        println!("  {: >1$}", "^", error.column);
                        println!();
                        println!("Syntax {}", error);
                        println!();
                    }
                }

                rl.add_history_entry(line);
            }
            Err(ReadlineError::Interrupted) => break,
            Err(ReadlineError::Eof) => break,
            Err(err) => println!("Error: {:?}", err),
        }
    }
}
