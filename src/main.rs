mod error;
mod eval;
mod expr;
mod lexer;
mod parser;
mod stmt;
mod token;

use std::{env, path::Path, process};

use eval::Interpreter;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: ella <FILE>");
        process::exit(1);
    }

    let file_path = Path::new(&args[1]);

    if !file_path.exists() {
        eprintln!("File {file_path:?} does not exist");
        process::exit(1);
    }

    let src = std::fs::read_to_string(file_path).expect("Failed to read file");

    let mut interpreter = Interpreter::new();
    match interpreter.eval(&src) {
        Err(why) => {
            eprintln!("{why:?}");
        }
        Ok(()) => (),
    }
}
