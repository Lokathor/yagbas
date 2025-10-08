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
  Ast, FileData, Item, S, YagError, items_of,
  separate_ast_statements_into_blocks, tac_blocks_from_expr_blocks, tokens_of,
  trees_of,
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
  /// One or more source files to process.
  pub files: Vec<PathBuf>,
}

pub fn main() -> ExitCode {
  let cli = Cli::parse();
  match cli.command {
    Commands::Build(build_args) => do_build(build_args),
  }
}

pub fn do_build(build_args: BuildArgs) -> ExitCode {
  let mut err_bucket: Vec<YagError> = Vec::new();

  let v: Vec<_> = build_args
    .files
    .par_iter()
    .map(|path_buf| {
      FileData::load(path_buf)
        .map(items_of)
        .map_err(|ioe| YagError::IO(path_buf.clone(), ioe))
    })
    .collect();
  let mut ast = Ast::default();
  for res in v {
    match res {
      Ok((items, parse_errors)) => {
        for S(item, _) in items {
          if let Some(name) = item.get_name()
            && let Some(_old_def) = ast.items.insert(name, item)
          {
            // todo: multiple definition error
          }
        }
        for parse_error in parse_errors {
          err_bucket.push(YagError::AstParseError(parse_error));
        }
      }
      Err(yag_error) => err_bucket.push(yag_error),
    }
  }
  dbg!(&ast);

  if err_bucket.is_empty() {
    ExitCode::SUCCESS
  } else {
    print_errors(&err_bucket);
    ExitCode::FAILURE
  }
}

fn print_errors(err_bucket: &[YagError]) {
  eprintln!("{err_bucket:?}");
}
