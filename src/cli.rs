#[derive(clap::Parser, Debug)]
#[clap(about, long_about = None)]
pub(crate) struct Cli {
    #[command(subcommand)]
    pub command: Command,
}

#[derive(clap::ValueEnum, Clone, Debug)]
pub(crate) enum OutputFormat {
    Wasm,
    Wat,
}

#[derive(clap::Subcommand, Debug)]
pub(crate) enum Command {
    /// Compile a file to WASM
    Compile {
        file: std::path::PathBuf,

        /// The file name of the generated WASM output
        #[arg(short, long)]
        output: Option<std::path::PathBuf>,

        /// Specifies the output format
        #[arg(short, long)]
        #[clap(value_enum, default_value_t = OutputFormat::Wasm)]
        format: OutputFormat,
    },

    /// Evaluate the generated AST
    Eval { file: std::path::PathBuf },
}
