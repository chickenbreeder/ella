#[derive(clap::Parser, Debug)]
#[clap(about, long_about = None)]
pub(crate) struct Cli {
    #[command(subcommand)]
    pub command: Command,
}

#[derive(clap::Subcommand, Debug)]
pub(crate) enum Command {
    /// Compile a file to WASM
    Compile {
        file: std::path::PathBuf,

        /// The file name of the generated WASM output
        #[arg(short, long)]
        output: Option<std::path::PathBuf>,

        /// Print the generated WASM to stdout
        #[arg(long, action=clap::ArgAction::SetTrue)]
        print_bytes: bool,
    },

    /// Evaluate the generated AST
    Eval { file: std::path::PathBuf },
}
