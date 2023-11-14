mod cli;
mod codegen;
mod error;
mod runtime;
mod syntax;

use std::{
    fs::File,
    io::Write,
    path::{Path, PathBuf},
    process,
    time::Instant,
};

use runtime::eval::Interpreter;

use crate::{cli::Command, codegen::compile::Compiler};

fn eval_file(path: &Path) {
    if !path.exists() {
        eprintln!("File {path:?} does not exist");
        process::exit(1);
    }

    let src = std::fs::read_to_string(path).expect("Failed to read file");

    let now = Instant::now();
    let mut interpreter = Interpreter::new();

    if let Err(why) = interpreter.eval(&src) {
        eprintln!("{why:?}");
    }

    let duration = now.elapsed();
    log::debug!("Duration: {duration:.2?}");
}

fn compile_file(input: PathBuf, output: Option<PathBuf>, print_bytes: bool, generate_wat: bool) {
    if !input.exists() {
        eprintln!("File {input:?} does not exist");
        process::exit(1);
    }

    let src = std::fs::read_to_string(&input).expect("Failed to read file");

    let mut compiler = Compiler::from_src(&src);

    let bytes = match compiler.compile() {
        Ok(bytes) => bytes,
        Err(why) => {
            eprintln!("Failed to compile file: {why:?}");
            process::exit(1);
        }
    };

    if print_bytes {
        println!("{bytes:X?}");
    }

    let out_file = match output {
        Some(p) => p,
        None => {
            let mut buf = input;
            buf.set_extension("wasm");
            buf
        }
    };

    if generate_wat {
        let wat = wasmprinter::print_bytes(&bytes).expect("Failed to print bytes as WAT");
        let mut f =
            File::create(&out_file).expect(&format!("Failed to create output file {out_file:?}"));
        f.write_all(wat.as_bytes())
            .expect("Failed to write to output file");
    } else {
        let mut f =
            File::create(&out_file).expect(&format!("Failed to create output file {out_file:?}"));
        f.write_all(&bytes).expect("Failed to write to output file");
    }
}

fn main() {
    use clap::Parser;

    env_logger::init();
    let cli = cli::Cli::parse();

    match cli.command {
        Command::Compile {
            file,
            output,
            print_bytes,
            wat,
        } => compile_file(file, output, print_bytes, wat),
        Command::Eval { file } => eval_file(&file),
    }
}
