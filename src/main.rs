extern crate clap;
extern crate colored;
extern crate rustyline;

mod ast;
mod codegen;
mod interpreter;
mod parser;
mod repl;
mod stdlib;
mod utils;

use clap::{App, Arg};
use std::fs;

type Cli = clap::ArgMatches<'static>;

fn init_cli() -> Cli {
    App::new("The Mol Programming Panguage")
        .version(env!("CARGO_PKG_VERSION"))
        .author("Moritz Kneilmann <moritz.kneilmann@gmx.de>")
        .about("Client, server & build chain all in one code base")
        .arg(
            Arg::with_name("generate")
                .long("generate")
                .help("Generate JavaScript"),
        )
        .arg(
            Arg::with_name("result")
                .long("result")
                .help("Print result of execution"),
        )
        .arg(
            Arg::with_name("files")
                .value_name("FILE")
                .takes_value(true)
                .multiple(true)
                .help("Specifies source files"),
        )
        .get_matches()
}

fn process_file(file: &str, cli: &Cli) {
    let gen = cli.is_present("generate");

    match fs::read_to_string(file) {
        Err(error) => {
            println!("{}: '{}'", error, file);
            std::process::exit(1);
        }

        Ok(content) => match parser::parse_string(&content) {
            Err(error) => {
                println!("Syntax {}", error);
                std::process::exit(1);
            }

            Ok(program) => {
                if gen {
                    let output = codegen::generate_js(&program);
                    println!("{}", output);
                } else {
                    match interpreter::exec(program) {
                        Err(error) => {
                            println!("Uncaught {}", error);
                            std::process::exit(1);
                        }

                        Ok(result) => {
                            if cli.is_present("result") {
                                println!("{}", result)
                            }
                        }
                    }
                }
            }
        },
    }
}

fn main() {
    let cli = init_cli();

    if let Some(files) = cli.values_of("files") {
        for file in files {
            process_file(file, &cli);
        }
    } else {
        repl::start();
    }
}
