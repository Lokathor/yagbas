#![allow(unused)]
#![allow(clippy::type_complexity)]

use std::{
  collections::HashMap,
  path::{Path, PathBuf},
};

use clap::{Args, Parser, Subcommand};
use yagbas::{
  src_files::{FileSpan, FileSpanned, SrcFile, SrcID},
  str_id::StrID,
  token::Token,
  token_tree::TokenTree,
};

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
  Tokenize(TokenizeArgs),
}

#[derive(Args, Debug, Clone)]
pub struct TokenizeArgs {
  /// One or more source files to tokenize.
  pub files: Vec<String>,
}

pub fn main() {
  let cli = Cli::parse();
  match cli.command {
    Commands::Tokenize(args) => do_tokenize(args),
  }
}

pub fn do_tokenize(args: TokenizeArgs) {
  for filename in &args.files {
    let src_file = match SrcFile::read_from_path(&filename) {
      Ok(src_file) => src_file,
      Err(io_error) => {
        eprintln!("{filename}: File IO Error: {io_error}");
        continue;
      }
    };
    let tokens: Vec<FileSpanned<Token>> = src_file.iter_tokens().collect();
    println!("{filename}: {tokens:?}");
  }
}
