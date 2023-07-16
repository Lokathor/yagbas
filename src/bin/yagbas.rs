#![allow(clippy::while_let_on_iterator)]
#![allow(unused_imports)]

use chumsky::{span::SimpleSpan, IterParser, Parser as _};
use std::borrow::Cow;
use yagbas::{
  ast::Ast,
  disassemble::print_basic_disassembly,
  id2,
  item_decl::ItemDecl,
  item_formed::ItemFormed,
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
  for (ast, filename) in file_results.iter().zip(args.files.iter()) {
    println!("== AST Info For `{filename}` ==");
    for (item, span) in &ast.items {
      let line = ast.line_of(span.start);
      println!("I(L {line}): {item:?}");
    }
    for (error, span) in &ast.errors {
      let line = ast.line_of(span.start);
      println!("ERR(L {line}): {error:?}");
    }
  }
}

#[allow(clippy::ptr_arg)]
fn build_process_file(filename: &String) -> Ast<ItemFormed> {
  let file_string: String = match std::fs::read_to_string(filename) {
    Ok(s) => s,
    Err(e) => {
      return Ast {
        module_text: String::new(),
        lines: Vec::new(),
        items: Vec::new(),
        errors: vec![(Cow::Owned(format!("{e:?}")), SimpleSpan::from(0..0))],
      }
    }
  };

  let tokens: Ast<Token> = Ast::tokenize(file_string);

  let token_trees: Ast<TokenTree> = tokens.into_token_trees();

  let declarations: Ast<ItemDecl> = token_trees.into_declarations();

  let forms: Ast<ItemFormed> = declarations.into_forms();

  forms
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
