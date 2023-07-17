#![allow(clippy::while_let_on_iterator)]
#![allow(unused_imports)]
#![allow(clippy::ptr_arg)]

use chumsky::{span::SimpleSpan, IterParser, Parser as _};
use std::borrow::Cow;
use yagbas::{token::Token, token_tree::TokenTree};

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

  let tokens: Vec<(Token, SimpleSpan)> = Token::lexer(&module_src)
    .spanned()
    .map(|(r, span)| (r.unwrap_or(Token::TokenError), SimpleSpan::from(span)))
    .collect();
  println!("== Tokens: {tokens:?}");

  let span: SimpleSpan = SimpleSpan::from(0..module_src.len());
  let parser = TokenTree::parser().repeated();
  let input = tokens.spanned(span);
  use chumsky::input::Input;
  let parse_result = parser.parse(input);
  println!("{parse_result:?}");
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
