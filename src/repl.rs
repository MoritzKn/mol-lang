use rustyline::error::ReadlineError;
use rustyline::Editor;

use crate::interpreter;
use crate::parser;

pub fn start() {
    let mut rl = Editor::<()>::new();
    let mut context = interpreter::Context::new();
    let mut multiline: Option<String> = Option::None;

    loop {
        let promt = match multiline {
            None => "> ",
            Some(_) => "  ",
        };

        match rl.readline(promt) {
            Ok(line) => {
                let line_len = line.len();

                let input = match multiline {
                    Some(prefix) => format!("{}\n{}", prefix, line),
                    None => line,
                };

                multiline = None;

                if line_len == 0 {
                    if input.len() > 0 {
                        multiline = Some(input)
                    }
                    // No need to parse empty string and eval to void
                    continue;
                }

                match parser::parse_string(&input) {
                    Ok(program) => match interpreter::exec_with_context(program, &mut context) {
                        Ok(value) => println!("{}", value.print(0)),
                        Err(throw) => println!("Uncaught {}", throw),
                    },
                    Err(error) => {
                        // If parsing error is an unexpected end of input
                        // TOOD: We can not assume that "line" has no newlines. Instead should check last line length explitily
                        if error.column > line_len {
                            multiline = Some(input);
                            continue;
                        }

                        println!("  {: >1$}", "^", error.column);
                        println!();
                        println!("Syntax {}", error);
                        println!();
                    }
                }

                rl.add_history_entry(input);
            }
            Err(ReadlineError::Interrupted) => break,
            Err(ReadlineError::Eof) => break,
            Err(err) => println!("Error: {:?}", err),
        }
    }
}
