#![allow(unused)]
#![allow(clippy::type_complexity)]

use std::process::{ExitCode, exit};

use clap::{Args, Parser, Subcommand};
use yagbas::{FileData, items_of, tokens_of, trees_of};

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
  /// Prints all token trees within the source files given.
  Trees(TreesArgs),
  /// Prints all items within the source files given.
  Items(ItemsArgs),
}

#[derive(Args, Debug, Clone)]
pub struct TokensArgs {
  /// One or more source files to print tokens for.
  pub files: Vec<String>,
}

#[derive(Args, Debug, Clone)]
pub struct TreesArgs {
  /// One or more source files to print tokens for.
  pub files: Vec<String>,
}

#[derive(Args, Debug, Clone)]
pub struct ItemsArgs {
  /// One or more source files to print tokens for.
  pub files: Vec<String>,
}

pub fn main() {
  let cli = Cli::parse();
  let had_errors: bool = match cli.command {
    Commands::Tokens(args) => do_tokens(args),
    Commands::Trees(args) => do_trees(args),
    Commands::Items(args) => do_items(args),
  };
  if had_errors {
    exit(1);
  }
}

pub fn do_tokens(args: TokensArgs) -> bool {
  let mut had_error = false;
  let load_results = args.files.iter().map(|f| FileData::load(f));
  for r in load_results {
    match r {
      Err(io_error) => {
        eprintln!("IO Error: {io_error}");
        had_error = true;
      }
      Ok(data) => {
        let path = data.path().display();
        let tokens = tokens_of(data.content());
        println!("{path}> TOKENS {tokens:?}");
      }
    }
  }
  had_error
}

pub fn do_trees(args: TreesArgs) -> bool {
  let mut had_error = false;
  let load_results = args.files.iter().map(|f| FileData::load(f));
  for r in load_results {
    match r {
      Err(io_error) => {
        eprintln!("IO Error: {io_error}");
        had_error = true;
      }
      Ok(data) => {
        let path = data.path().display();
        let (trees, errors) = trees_of(data.content());
        if !errors.is_empty() {
          eprintln!("{path}> ERRORS {errors:?}");
          had_error = true;
        }
        if !trees.is_empty() {
          println!("{path}> TREES {trees:?}");
        }
      }
    }
  }
  had_error
}

pub fn do_items(args: ItemsArgs) -> bool {
  let mut had_error = false;
  let load_results = args.files.iter().filter_map(|f| {
    FileData::load(f).map_err(|e| eprintln!("`{f}`> IO Error> {e}")).ok()
  });
  for data in load_results {
    let path = data.path().display();
    let (items, errors) = items_of(data);
    if !errors.is_empty() {
      eprintln!("{path}> ERRORS {errors:?}");
      had_error = true;
    }
    if !items.is_empty() {
      println!("{path}> ITEMS {items:?}");
    }
  }
  had_error
}
