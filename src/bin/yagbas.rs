#![allow(unused)]
#![allow(clippy::type_complexity)]

use std::{
  collections::HashMap,
  path::{Path, PathBuf},
};

use clap::{Args, Parser, Subcommand};
use yagbas::{
  item::{parse_token_trees_to_items, FnDecl, Item, Statement},
  src_files::{FileSpan, FileSpanned, SrcFile, SrcID, TokenTreeOutput},
  str_id::StrID,
  token::Token,
  token_tree::{parse_tokens_to_token_trees, TokenTree},
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
  TokenTrees(TokenTreesArgs),
}

#[derive(Args, Debug, Clone)]
pub struct TokenizeArgs {
  /// One or more source files to tokenize.
  pub files: Vec<String>,
}

#[derive(Args, Debug, Clone)]
pub struct TokenTreesArgs {
  /// One or more source files to make token trees for.
  pub files: Vec<String>,
}

pub fn main() {
  let cli = Cli::parse();
  match cli.command {
    Commands::Tokenize(args) => do_tokenize(args),
    Commands::TokenTrees(args) => do_token_trees(args),
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

pub fn do_token_trees(args: TokenTreesArgs) {
  for filename in &args.files {
    let src_file = match SrcFile::read_from_path(&filename) {
      Ok(src_file) => src_file,
      Err(io_error) => {
        eprintln!("{filename}: File IO Error: {io_error}");
        continue;
      }
    };
    let TokenTreeOutput { trees, errors } = src_file.parse_token_trees();
    println!("{filename}: {trees:?}");
    println!("{filename} ERRORS: {errors:?}");
  }
}
