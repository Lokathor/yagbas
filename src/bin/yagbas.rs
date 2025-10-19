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
  Ast, FileData, Item, S, YagError, items_of, log_error, print_any_errors,
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
  let mut ast = Ast::default();
  // load + parse multi-threaded with rayon.
  let load_parse_data: Vec<_> =
    build_args.files.par_iter().map(load_parse(&build_args)).collect();
  for items in load_parse_data {
    for item in items {
      // Note(Lokathor): ItemError entries don't have names.
      if let Some(name) = item.0.get_name()
        && let Some(_old_def) = ast.items.insert(name, item)
      {
        todo!("multiple definition: {name}");
      }
    }
  }
  // now we have a basic AST.

  ast.populate_static_sizes();
  ast.populate_ir_bitstructs();
  ast.populate_const_exprs();
  // todo: populate static defs
  ast.do_per_item_data_cleanup();

  for i in ast.items.values() {
    match &i.0 {
      Item::Func(f) => {
        println!();
        println!("== fn {}>", f.name);
        let expr_blocks = separate_ast_statements_into_blocks(&f.body);
        let tac_block = tac_blocks_from_expr_blocks(&expr_blocks);
        for (i, block) in tac_block.iter().enumerate() {
          if i > 0 {
            println!();
          }
          println!("tac_block {} {{", block.id);
          for step in block.steps.iter() {
            println!("  {step}");
          }
          println!("}} then {};", block.next);
        }
      }
      Item::Const(c) => {
        //dbg!(&c);
      }
      _ => (),
    }
  }

  if print_any_errors() { ExitCode::FAILURE } else { ExitCode::SUCCESS }
}

/// This makes a closure that can load and parse the items from a given file.
///
/// Each file will run this closure once, possibly on separate threads.
fn load_parse(build_args: &BuildArgs) -> impl Fn(&PathBuf) -> Vec<S<Item>> {
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

    let trees = trees_of(&tokens, file_data.id());
    if build_args.show_trees {
      println!("{p} TREES: {trees:?}", p = path_buf.display());
    }

    let items = items_of(&trees, file_data);
    if build_args.show_items {
      println!("{p} ITEMS: {items:?}", p = path_buf.display());
    }

    items
  }
}
