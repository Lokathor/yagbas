#![allow(unused_imports)]
#![allow(unused_mut)]
#![allow(clippy::type_complexity)]

use clap::{Args, Parser, Subcommand};
use rayon::prelude::*;
use std::{
  collections::HashMap,
  path::PathBuf,
  process::{ExitCode, exit},
};
use yagbas::{
  AstItem, FileData, YagError, YagParserState, items_of, log_error,
  log_error_iter, print_any_errors, tokens_of, trees_of,
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
  /// Build a yagbas program.
  Build(BuildArgs),
}

#[derive(Args, Debug, Clone)]
pub struct BuildArgs {
  /// Show the parsed tokens.
  #[arg(long)]
  pub show_tokens: bool,
  /// Show the parsed token trees.
  #[arg(long)]
  pub show_trees: bool,
  /// Show the parsed items.
  #[arg(long)]
  pub show_items: bool,
  /// One or more source files to process.
  pub files: Vec<PathBuf>,
}

pub fn main() -> ExitCode {
  match Cli::parse().command {
    Commands::Build(build_args) => do_build(build_args),
  }
}

#[allow(unused)]
pub fn do_build(build_args: BuildArgs) -> ExitCode {
  // load + parse multi-threaded with rayon.
  let load_parse_data: Vec<_> =
    build_args.files.par_iter().map(load_parse(&build_args)).collect();

  if print_any_errors() { ExitCode::FAILURE } else { ExitCode::SUCCESS }
}

/// This makes a closure that can load and parse the items from a given file.
///
/// Each file will run this closure once, possibly on separate threads.
fn load_parse(build_args: &BuildArgs) -> impl Fn(&PathBuf) -> Vec<AstItem> {
  move |path_buf: &PathBuf| {
    let load_result = FileData::load(path_buf);
    let file_data = match load_result {
      Err(io_error) => {
        log_error(YagError::IO(path_buf.clone(), format!("{io_error}")));
        return Vec::new();
      }
      Ok(file_data) => file_data,
    };

    let tokens = tokens_of(file_data.content());
    if build_args.show_tokens {
      println!("{p} TOKENS: {tokens:?}", p = path_buf.display());
    }

    let (trees, tree_errors) = trees_of(&tokens);
    if build_args.show_trees {
      println!("{p} TREES: {trees:?}", p = path_buf.display());
    }
    log_error_iter(
      tree_errors
        .into_iter()
        .map(|e| YagError::TokenTreeParseError(file_data.id(), e.into_owned())),
    );

    let yag_state =
      YagParserState { file_id: file_data.id(), source: file_data.content() };
    let (items, item_errors) = items_of(&trees, yag_state);
    if build_args.show_items {
      println!("{p} ITEMS: {items:?}", p = path_buf.display());
    }
    log_error_iter(
      item_errors
        .into_iter()
        .map(|e| YagError::ItemParseError(file_data.id(), e.into_owned())),
    );

    items
  }
}
