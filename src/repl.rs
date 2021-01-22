use std::env;

use log;

use rustyline::error::ReadlineError;
use rustyline::Editor;

use crate::interpreter;
use crate::parser;

pub fn start() {
    let history_file = env::var("HOME")
        .map(|dir| format!("{}/.mol_repl_history", dir))
        .ok();

    let mut rl = Editor::<()>::new();
    let mut context = interpreter::Context::new();
    let mut multiline: Option<String> = Option::None;

    if let Some(Err(err)) = history_file
        .as_ref()
        .map(|hisotry| rl.load_history(hisotry))
    {
        log::info!("No previous history: {}", err);
    }

    loop {
        let promt = match multiline {
            None => "> ",
            Some(_) => "  ",
        };

        match rl.readline(promt) {
            Ok(line) => {
                let line_len = line.len();
                let org_line = line.clone();

                let input = match multiline {
                    Some(prefix) => format!("{}\n{}", prefix, line),
                    None => line,
                };

                multiline = None;

                if line_len == 0 {
                    if !input.is_empty() {
                        multiline = Some(input)
                    }
                    // No need to parse empty string and eval to void
                    continue;
                }

                rl.add_history_entry(org_line);

                match parser::parse_string(&input) {
                    Ok(program) => match interpreter::exec_with_context(&program, &mut context) {
                        Ok(value) => println!("{}", value.print(0)),
                        Err(throw) => println!("Uncaught {}", throw),
                    },
                    Err(error) => {
                        // If parsing error is an unexpected end of input
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
            }
            Err(ReadlineError::Interrupted) => break,
            Err(ReadlineError::Eof) => break,
            Err(err) => println!("Error: {:?}", err),
        }
    }

    if let Some(Err(err)) = history_file
        .as_ref()
        .map(|hisotry| rl.save_history(&hisotry))
    {
        eprintln!("Failed to save history: {}", err)
    }
}
