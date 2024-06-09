#![allow(unused)]

use clap::{Args, Parser, Subcommand};
use yagbas::{
  src_files::{FileSpan, SrcFileInfo, SrcID},
  token::Token,
  token_tree::{grow_token_trees, TokenTree},
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
  /// Build source files into a rom.
  Build(BuildArgs),
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

pub fn main() {
  let cli = Cli::parse();
  match cli.command {
    Commands::Build(args) => build(args),
  }
}

pub fn build(args: BuildArgs) {
  println!("{args:?}");

  if args.files.is_empty() {
    eprintln!("Build Error: Must provide at least one source file.");
    return;
  }

  // TODO: rayon this maybe.
  let _todo: Vec<_> = args.files.iter().map(build_process_file).collect();
}

fn build_process_file(filename: &String) {
  println!("== {filename}:");
  let file_info_id = match SrcFileInfo::read_path(&filename) {
    Ok(info) => SrcID::from(info),
    Err(io_error) => {
      eprintln!("File IO Error: {io_error}");
      return;
    }
  };
  let tokens: Vec<(Token, FileSpan)> = file_info_id.iter_tokens().collect();
  for (token, filespan) in &tokens {
    //println!("{filespan}> {token:?}");
  }
  let (trees, tree_errors) = grow_token_trees(&tokens);
  for (token_tree, file_span) in &trees {
    println!("{file_span:?}> {token_tree:?}");
  }
  for tree_error in &tree_errors {
    let span = tree_error.span();
    let found = tree_error.found();
    let reason = tree_error.reason();
    println!("span: {span}");
    println!("found: {found:?}");
    println!("reason: {reason:?}");
  }
}
