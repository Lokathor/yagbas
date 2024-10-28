#![allow(unused)]
#![allow(clippy::type_complexity)]

use clap::{Args, Parser, Subcommand};
use std::{
  collections::HashMap,
  path::{Path, PathBuf},
};
use yagbas::{
  parsing::token_tree_p,
  src_files::{
    FileSpan, FileSpanned, ItemParseResult, SrcFile, SrcID,
    TokenTreeParseResult,
  },
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
  /// Prints all token trees within the source files given.
  Trees(TreesArgs),
  /// Prints all items within the source files given.
  Items(ItemsArgs),
}

#[derive(Args, Debug, Clone)]
pub struct TokenizeArgs {
  /// One or more source files to tokenize.
  pub files: Vec<String>,
}

#[derive(Args, Debug, Clone)]
pub struct TreesArgs {
  /// One or more source files to make into token trees.
  pub files: Vec<String>,
}

#[derive(Args, Debug, Clone)]
pub struct ItemsArgs {
  /// One or more source files to make into items.
  pub files: Vec<String>,
}

pub fn main() {
  let cli = Cli::parse();
  match cli.command {
    Commands::Tokenize(args) => do_tokenize(args),
    Commands::Trees(args) => do_trees(args),
    Commands::Items(args) => do_items(args),
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

pub fn do_trees(args: TreesArgs) {
  for filename in &args.files {
    let src_file = match SrcFile::read_from_path(&filename) {
      Ok(src_file) => src_file,
      Err(io_error) => {
        eprintln!("{filename}: File IO Error: {io_error}");
        continue;
      }
    };
    let TokenTreeParseResult { trees, tree_errors } =
      src_file.parse_token_trees();
    println!("{filename}: {trees:?}");
    if !tree_errors.is_empty() {
      println!("{filename}: ERRORS: {tree_errors:?}");
    }
  }
}

pub fn do_items(args: ItemsArgs) {
  for filename in &args.files {
    let src_file = match SrcFile::read_from_path(&filename) {
      Ok(src_file) => src_file,
      Err(io_error) => {
        eprintln!("{filename}: File IO Error: {io_error}");
        continue;
      }
    };
    let ItemParseResult { items, item_errors } = src_file.parse_items();
    println!("=ITEMS {filename}: {items:?}");
    if !item_errors.is_empty() {
      println!("=ITEMS {filename}: ERRORS: {item_errors:?}");
    }
  }
}
