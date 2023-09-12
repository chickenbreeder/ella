mod error;
mod eval;
mod expr;
mod lexer;
mod parser;
mod stmt;
mod token;

use std::{env, process};

use eval::Interpreter;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: ella <Expression>");
        process::exit(1);
    }

    let interpreter = Interpreter::new();

    match interpreter.eval(&args[1]) {
        Err(why) => {
            eprintln!("{why:?}");
        }
        Ok(None) => println!("None"),
        Ok(Some(value)) => println!("{value:?}"),
    }
}
