mod cli;
mod codegen;
mod error;
mod runtime;
mod syntax;

use std::{
    fs::File,
    io::{self, Write},
    path::{Path, PathBuf},
    process,
    time::Instant,
};

use cli::OutputFormat;
use error::ErrorKind;
use runtime::eval::Interpreter;

use crate::{cli::Command, codegen::compile::Compiler};

fn eval_file(path: &Path) -> io::Result<()> {
    if !path.exists() {
        eprintln!("File {path:?} does not exist");
        process::exit(1);
    }

    let src = std::fs::read_to_string(path)?;

    let now = Instant::now();
    let mut interpreter = Interpreter::new();

    if let Err(why) = interpreter.eval(&src) {
        eprintln!("{why:?}");
    }

    let duration = now.elapsed();
    log::debug!("Duration: {duration:.2?}");

    Ok(())
}

fn compile_file(input: PathBuf, output: Option<PathBuf>, format: OutputFormat) -> io::Result<()> {
    if !input.exists() {
        eprintln!("File {input:?} does not exist");
        process::exit(1);
    }

    let src = std::fs::read_to_string(&input).expect("Failed to read file");

    let mut compiler = Compiler::from_src(&src);

    let bytes = match compiler.compile() {
        Ok(bytes) => bytes,
        Err(err) => match err {
            ErrorKind::ParseError(why) => {
                eprintln!("error on line ?: {why}");
                process::exit(1);
            }
            other => {
                eprintln!("{other:?}");
                process::exit(1);
            }
        },
    };

    let mut out_file = match output {
        Some(p) => p,
        None => {
            let mut buf = input;
            buf
        }
    };

    match format {
        OutputFormat::WASM => {
            out_file.set_extension("wasm");

            let mut f = File::create(&out_file)?;
            f.write_all(&bytes)?;
        }
        OutputFormat::WAT => {
            out_file.set_extension("wat");

            let wat = wasmprinter::print_bytes(&bytes).expect("Failed to print bytes as WAT");

            let mut f = File::create(&out_file)?;
            f.write_all(wat.as_bytes())?;
        }
    }

    Ok(())
}

fn main() -> io::Result<()> {
    use clap::Parser;

    env_logger::init();
    let cli = cli::Cli::parse();

    match cli.command {
        Command::Compile {
            file,
            output,
            format,
        } => compile_file(file, output, format),
        Command::Eval { file } => eval_file(&file),
    }
}
