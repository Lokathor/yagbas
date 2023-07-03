#![allow(clippy::while_let_on_iterator)]

use chumsky::{IterParser, Parser as _};
use yagbas::{
  disassemble::print_basic_disassembly,
  parser::{
    comment_filter::no_comment_tokens, item::Item, token_tree::make_token_trees, *,
  },
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
  println!("{args:?}");

  if args.files.is_empty() {
    eprintln!("Error: Must provide at least one source file.");
    return;
  }

  for file in args.files {
    println!("Reading `{file}`...");
    let prog = match std::fs::read_to_string(file) {
      Ok(bytes) => bytes,
      Err(e) => {
        println!("File Read Error: {e:?}");
        continue;
      }
    };
    let token_list = match no_comment_tokens(&prog) {
      Ok(tokens) => tokens,
      Err(span) => {
        println!("Could not process comment markers: {span:?}");
        return;
      }
    };
    let token_trees = make_token_trees(&token_list);
    let items = {
      let tt = token_trees.output().unwrap();
      let parser = Item::parser().map_with_span(id2).repeated().collect::<Vec<_>>();
      run_parser(parser, tt)
    };
    println!("Items: {items:?}");
  }
}

pub fn unbuild(args: UnbuildArgs) {
  println!("{args:?}");
  //
  let file = &args.file;
  println!("Reading `{file}`...");
  let rom = match std::fs::read(file) {
    Ok(bytes) => bytes,
    Err(e) => {
      println!("File Read Error: {e:?}");
      return;
    }
  };
  print_basic_disassembly(&rom)
}
