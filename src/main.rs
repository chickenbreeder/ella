mod codegen;
mod error;
mod runtime;
mod syntax;

use std::{env, path::Path, process, time::Instant};

use runtime::eval::Interpreter;

fn main() {
    env_logger::init();

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

    let now = Instant::now();

    let mut interpreter = Interpreter::new();

    if let Err(why) = interpreter.eval(&src) {
        eprintln!("{why:?}");
    }

    let duration = now.elapsed();
    log::debug!("Duration: {duration:.2?}");
}
