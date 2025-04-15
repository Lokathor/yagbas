#![allow(unused)]
#![allow(clippy::type_complexity)]

use clap::{Args, Parser, Subcommand};
use yagbas::FileData;

#[test]
fn verify_cli() {
  use clap::CommandFactory;
  Cli::command().debug_assert()
}

#[derive(Parser, Debug, Clone)]
#[command(version, about)]
pub struct Cli {
  #[command(subcommand)]
  command: Commands,
}

#[derive(Subcommand, Debug, Clone)]
pub enum Commands {
  /// Prints all tokens within the source files given.
  Tokens(TokensArgs),
}

#[derive(Args, Debug, Clone)]
pub struct TokensArgs {
  /// One or more source files to print tokens for.
  pub files: Vec<String>,
}

pub fn main() {
  let cli = Cli::parse();
  match cli.command {
    Commands::Tokens(args) => do_tokens(args),
  }
}

pub fn do_tokens(args: TokensArgs) {
  let load_results = args.files.iter().map(|f| FileData::load(f));
  for r in load_results {
    match r {
      Ok(_) => todo!(),
      Err(_) => todo!(),
    }
  }
}
