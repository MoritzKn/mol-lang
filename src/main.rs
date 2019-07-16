extern crate rustyline;

mod ast;
mod interpreter;
mod parser;
mod repl;

fn main() {
    repl::start();
}
