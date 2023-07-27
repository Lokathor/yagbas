#![allow(unused_imports)]
#![allow(clippy::while_let_on_iterator)]
#![allow(clippy::ptr_arg)]

use chumsky::{
  container::Seq, prelude::Rich, primitive::*, recovery::via_parser, span::SimpleSpan,
  IterParser, Parser as _,
};
use std::{
  borrow::Cow,
  collections::{hash_map::Entry, HashMap},
};
use yagbas::{
  item::{parse_module_items, Item},
  token::{tokenize_module, Token},
  token_tree::grow_token_trees,
};

use clap::{Args, Parser, Subcommand};

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
  /// Build source files into a rom.
  Build(BuildArgs),

  /// Un-build a rom back into source code.
  Unbuild(UnbuildArgs),
}

#[derive(Args, Debug, Clone)]
pub struct BuildArgs {
  /// One or more source files.
  pub files: Vec<String>,

  /// Output file name. Defaults to the name of the first src file with `.gb`
  /// extension
  #[arg(short, long)]
  pub out: Option<String>,
}

#[derive(Args, Debug, Clone)]
pub struct UnbuildArgs {
  /// The rom file to un-build back into source code.
  pub file: String,

  /// Output file name (prints to stdout by default)
  #[arg(short, long)]
  pub out: Option<String>,
}

pub fn main() {
  let cli = Cli::parse();
  match cli.command {
    Commands::Build(args) => build(args),
    Commands::Unbuild(args) => unbuild(args),
  }
}

pub fn build(args: BuildArgs) {
  use rayon::prelude::*;
  //
  println!("{args:?}");

  if args.files.is_empty() {
    eprintln!("Error: Must provide at least one source file.");
    return;
  }

  let _todo: Vec<_> = args.files.iter().map(build_process_file).collect();
}

fn build_process_file(filename: &String) {
  println!("== {filename}:");
  let module_src: String = match std::fs::read_to_string(filename) {
    Ok(s) => s,
    Err(e) => {
      println!("File Read Error: {e:?}");
      return;
    }
  };

  let tokens: Vec<(Token, SimpleSpan)> = tokenize_module(&module_src);
  //println!("== Tokens: {tokens:?}");

  let (token_trees, tree_parse_errors) = grow_token_trees(&tokens);
  if !tree_parse_errors.is_empty() {
    println!("== Token Tree Parse Errors ==");
    for error in &tree_parse_errors {
      println!("ERR: {error:?}");
    }
  }

  let (items, item_parse_errors) = parse_module_items(&token_trees);
  if !item_parse_errors.is_empty() {
    println!("== Item Parse Errors ==");
    for error in &item_parse_errors {
      println!("ERR: {error:?}");
    }
  }

  let mut consts = HashMap::new();
  let mut sections = HashMap::new();
  let mut statics = HashMap::new();
  let mut item_errors = 0_usize;

  for (item, _span) in items {
    match item {
      Item::ConstDecl(x) => {
        if let Entry::Vacant(e) = consts.entry(x.name.0) {
          e.insert(x);
        } else {
          item_errors += 1;
        }
      }
      Item::SectionDecl(x) => {
        if let Entry::Vacant(e) = sections.entry(x.name.0) {
          e.insert(x);
        } else {
          item_errors += 1;
        }
      }
      Item::StaticDecl(x) => {
        if let Entry::Vacant(e) = statics.entry(x.name.0) {
          e.insert(x);
        } else {
          item_errors += 1;
        }
      }
      Item::ItemError => item_errors += 1,
    }
  }
  if item_errors > 0 {
    println!("{item_errors} item errors encountered.");
  }
  println!("== consts: {consts:?}");
  println!("== sections: {sections:?}");
  println!("== statics: {statics:?}");
}

pub fn unbuild(args: UnbuildArgs) {
  println!("{args:?}");
  //
  let file = &args.file;
  println!("Reading `{file}`...");
  let _rom = match std::fs::read(file) {
    Ok(bytes) => bytes,
    Err(e) => {
      println!("File Read Error: {e:?}");
      return;
    }
  };
  //print_basic_disassembly(&rom)
}
