#![allow(clippy::while_let_on_iterator)]
#![allow(unused_imports)]

use chumsky::{span::SimpleSpan, IterParser, Parser as _};
use yagbas::{
  comment_filter::no_comment_tokens,
  disassemble::print_basic_disassembly,
  id2,
  item_decl::ItemDecl,
  run_parser,
  token::Token,
  token_tree::{make_token_trees, TokenTree},
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

  let file_results: Vec<_> = args.files.par_iter().map(build_process_file).collect();
  for (result, filename) in file_results.iter().zip(args.files.iter()) {
    println!("== Results for `{filename}` ==");
    match result {
      Ok(items) => {
        for (item, _span) in items {
          println!("Ok: {item:?}");
        }
      }
      Err(err) => println!("Err: {err}"),
    }
  }
}

#[allow(clippy::ptr_arg)]
fn build_process_file(filename: &String) -> Result<Vec<(ItemDecl, SimpleSpan)>, String> {
  let file_string: String =
    std::fs::read_to_string(filename).map_err(|e| e.to_string())?;

  let tokens: Vec<(Token, SimpleSpan)> =
    no_comment_tokens(&file_string).map_err(|e| format!("{e:?}"))?;

  let token_trees: Vec<(TokenTree, SimpleSpan)> =
    make_token_trees(&tokens).into_result().map_err(|v| format!("{v:?}"))?;

  let items: Vec<(ItemDecl, SimpleSpan)> = {
    let item_parser =
      ItemDecl::parser().map_with_span(id2).repeated().collect::<Vec<_>>();
    run_parser(item_parser, &token_trees).into_result().map_err(|v| format!("{v:?}"))?
  };

  Ok(items)
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
