use std::env;
use std::sync::{Arc, Mutex};

use rustyline::completion::{Completer, Pair};
use rustyline::error::ReadlineError;
use rustyline::{CompletionType, Config, Editor};
use rustyline_derive::{Helper, Highlighter, Hinter, Validator};

use crate::interpreter;
use crate::parser;

#[derive(Helper, Highlighter, Hinter, Validator)]
struct RlHelper {
    context: Arc<Mutex<interpreter::Context>>,
}

impl Completer for RlHelper {
    type Candidate = Pair;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &rustyline::Context<'_>,
    ) -> rustyline::Result<(usize, Vec<Self::Candidate>)> {
        let prefix = &line[..pos];

        // Split off last word
        let parts = prefix.split(" ").collect::<Vec<&str>>();
        let prefix = if parts.len() > 1 {
            *parts.last().unwrap()
        } else {
            prefix
        };

        // Split off last bind
        let parts = prefix.split(":").collect::<Vec<&str>>();
        let prefix = if parts.len() > 1 {
            *parts.last().unwrap()
        } else {
            prefix
        };

        let parts = prefix.split(".").collect::<Vec<&str>>();

        // Case 1: no member access
        if parts.len() == 1 {
            let context = self.context.lock().unwrap();
            let vars = context.list_vars();
            let prefix_len = prefix.len();

            let candidates = vars
                .into_iter()
                .flat_map(|vars_in_scope| {
                    vars_in_scope
                        .into_iter()
                        .filter(|var| prefix_len <= var.len() && &var[..prefix_len] == prefix)
                        .map(|var| Pair {
                            display: var.clone(),
                            replacement: var,
                        })
                })
                .collect::<Vec<Pair>>();

            return Ok((pos - prefix_len, candidates));
        }

        // Case 2: member access
        if parts.len() > 1 {
            let expr = parts[..(parts.len() - 1)].join(".");

            let ast = parser::parse_string(&expr).ok();
            let result = ast.and_then(|ast| {
                interpreter::exec_with_context(&ast, &mut self.context.lock().unwrap()).ok()
            });

            if let Some(value) = result {
                if let interpreter::Value::Map(map) = value {
                    let prefix = parts.last().unwrap();
                    let prefix_len = prefix.len();

                    let candidates = map
                        .keys()
                        .filter(|var| prefix_len <= var.len() && &var[..prefix_len] == *prefix)
                        .map(|key| Pair {
                            display: key.clone(),
                            replacement: key.clone(),
                        })
                        .collect::<Vec<Pair>>();

                    return Ok((pos - prefix_len, candidates));
                }
            }
        }

        Ok((0, vec![]))
    }
}

pub fn start() {
    let history_file = env::var("HOME")
        .map(|dir| format!("{}/.mol_repl_history", dir))
        .ok();

    let context = Arc::new(Mutex::new(interpreter::Context::new()));

    let mut rl = Editor::<RlHelper>::with_config(
        Config::builder()
            .completion_type(CompletionType::List)
            .build(),
    );
    let mut multiline: Option<String> = Option::None;

    rl.set_helper(Some(RlHelper {
        context: context.clone(),
    }));

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
                    Ok(program) => {
                        match interpreter::exec_with_context(&program, &mut context.lock().unwrap())
                        {
                            Ok(value) => println!("{}", value.print(0)),
                            Err(throw) => println!("Uncaught {}", throw),
                        }
                    }
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
