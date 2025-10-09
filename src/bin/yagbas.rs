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
  Ast, AstParseError, FileData, Item, S, YagError, items_of,
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
    .map(load_parse(build_args.show_tokens, build_args.show_trees))
    .collect();
  let mut ast = Ast::default();
  for (items, errs) in v {
    err_bucket.extend(errs);
    for S(item, _) in items {
      if let Some(name) = item.get_name()
        && let Some(_old_def) = ast.items.insert(name, item)
      {
        // todo: multiple definition error
      }
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
  for err in err_bucket {
    eprintln!("{err:?}");
  }
}

fn load_parse(
  show_tokens: bool, show_trees: bool,
) -> impl Fn(&PathBuf) -> (Vec<S<Item>>, Vec<YagError>) {
  move |path_buf: &PathBuf| {
    let mut err_bucket = Vec::new();

    let load_result = FileData::load(path_buf);
    let file_data = match load_result {
      Err(io_error) => {
        err_bucket.push(YagError::IO(path_buf.clone(), io_error));
        return (Vec::new(), err_bucket);
      }
      Ok(file_data) => file_data,
    };

    let tokens = tokens_of(file_data.content());
    if show_tokens {
      println!("{p} TOKENS: {tokens:?}", p = path_buf.display());
    }

    let trees = trees_of(&tokens, file_data.id(), &mut err_bucket);
    if show_trees {
      println!("{p} TREES: {trees:?}", p = path_buf.display());
    }

    todo!();
  }
}
