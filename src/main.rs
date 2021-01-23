extern crate clap;
extern crate colored;
extern crate env_logger;
extern crate log;
extern crate rustyline;
extern crate rustyline_derive;

mod ast;
mod interpreter;
mod parser;
mod repl;
mod stdlib;
mod utils;

use clap::{App, Arg};
use std::fs;

type Cli = clap::ArgMatches<'static>;

fn init_cli() -> Cli {
    App::new("The Mol Programming Language")
        .version(env!("CARGO_PKG_VERSION"))
        .author("Moritz Kneilmann <moritz.kneilmann@gmx.de>")
        .about("Client, server & build chain all in one code base")
        .arg(
            Arg::with_name("files")
                .value_name("FILE")
                .takes_value(true)
                .help("Run file")
                .conflicts_with("eval")
                .conflicts_with("print")
                .multiple(true),
        )
        .arg(
            Arg::with_name("result")
                .short("r")
                .long("result")
                .help("Print results")
                .requires("files")
                .multiple(false),
        )
        .arg(
            Arg::with_name("check")
                .long("check")
                .short("c")
                .help("Syntax check script without executing")
                .requires("files")
                .multiple(false),
        )
        .arg(
            Arg::with_name("print")
                .takes_value(true)
                .value_name("SCRIPT")
                .long("print")
                .short("p")
                .help("Evaluate script and print result")
                .conflicts_with("files")
                .conflicts_with("check")
                .conflicts_with("result")
                .conflicts_with("eval")
                .multiple(true),
        )
        .arg(
            Arg::with_name("eval")
                .takes_value(true)
                .value_name("SCRIPT")
                .long("eval")
                .short("e")
                .help("Evaluate script")
                .conflicts_with("check")
                .conflicts_with("files")
                .conflicts_with("result")
                .conflicts_with("print")
                .multiple(true),
        )
        .get_matches()
}

fn exec_file(file: &str, cli: &Cli) {
    match fs::read_to_string(file) {
        Err(error) => {
            eprintln!("{}: '{}'", error, file);
            std::process::exit(1);
        }

        Ok(content) => match parser::parse_string(&content) {
            Err(error) => {
                eprintln!("Syntax {}", error);
                std::process::exit(1);
            }

            Ok(program) => {
                if cli.args.contains_key("check") {
                    return;
                }

                match interpreter::exec(&program) {
                    Err(error) => {
                        eprintln!("Uncaught {}", error);
                        std::process::exit(1);
                    }

                    Ok(result) => {
                        if cli.args.contains_key("result") {
                            println!("{}", result.print(0))
                        }
                    }
                }
            }
        },
    }
}

fn exec_string(string: &str, print: bool) {
    match parser::parse_string(string) {
        Err(error) => {
            eprintln!("Syntax {}", error);
            std::process::exit(1);
        }

        Ok(program) => match interpreter::exec(&program) {
            Err(error) => {
                eprintln!("Uncaught {}", error);
                std::process::exit(1);
            }

            Ok(result) => {
                if print {
                    println!("{}", result.print(0))
                }
            }
        },
    }
}

fn main() {
    env_logger::init();

    let cli = init_cli();

    if let Some(files) = cli.values_of("files") {
        for file in files {
            exec_file(file, &cli);
        }
    } else if let Some(scripts) = cli.values_of("eval") {
        for script in scripts {
            exec_string(script, false);
        }
    } else if let Some(scripts) = cli.values_of("print") {
        for script in scripts {
            exec_string(script, true);
        }
    } else {
        repl::start();
    }
}
