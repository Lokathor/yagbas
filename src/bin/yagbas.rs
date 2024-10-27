#![allow(unused)]
#![allow(clippy::type_complexity)]

use clap::{Args, Parser, Subcommand};
use std::{
  collections::HashMap,
  path::{Path, PathBuf},
};
use yagbas::{
  parsing::token_tree_p,
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
  /// Prints all token trees within the source files given.
  Trees(TreesArgs),
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

pub fn main() {
  let cli = Cli::parse();
  match cli.command {
    Commands::Tokenize(args) => do_tokenize(args),
    Commands::Trees(args) => do_trees(args),
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
    let tokens: Vec<FileSpanned<Token>> = src_file.iter_tokens().collect();
    if tokens.is_empty() {
      println!("{filename}: no tokens present");
      continue;
    }
    use chumsky::{
      input::{BorrowInput, Input},
      prelude::*,
    };
    let last_span = tokens.last().map(|token| token._span).unwrap();
    let end_span = FileSpan { start: last_span.end, ..last_span };
    let recover_strategy =
      via_parser(any().repeated().at_least(1).to(TokenTree::TreeError));
    let (opt_output, errors) = token_tree_p()
      .recover_with(recover_strategy)
      .map_with(|token_tree, ex| FileSpanned::new(token_tree, ex.span()))
      .repeated()
      .collect()
      .parse(Input::map(&tokens[..], end_span, |fs| (&fs._payload, &fs._span)))
      .into_output_errors();
    let output: Vec<FileSpanned<TokenTree>> = opt_output.unwrap_or_default();
    println!("{filename}: {output:?}");
    if !errors.is_empty() {
      println!("{filename}: ERRORS: {errors:?}");
    }
  }
}
