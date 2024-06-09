#![allow(unused)]
#![allow(clippy::type_complexity)]

use std::collections::HashMap;

use clap::{Args, Parser, Subcommand};
use yagbas::{
  item::{parse_token_trees_to_items, FnDecl, Item, Statement},
  src_files::{FileSpan, SrcFileInfo, SrcID},
  str_id::StrID,
  token::Token,
  token_tree::{parse_tokens_to_token_trees, TokenTree},
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
  let (token_trees, tree_errors) = parse_tokens_to_token_trees(&tokens);
  for tree_error in &tree_errors {
    println!("== Tree Error: {tree_error:?}");
  }
  let (items, item_errors) = parse_token_trees_to_items(&token_trees);
  for item_error in &item_errors {
    println!("== Item Error: {item_errors:?}");
  }
  let mut code_chunks: HashMap<StrID, Vec<u8>> = HashMap::new();
  let mut label_fixes: Vec<LabelFix> = Vec::new();
  for (item, file_span) in &items {
    match item {
      Item::Fn(FnDecl { name, args, statements }) => {
        assert!(args.is_empty());
        let (chunk, fixes) = do_codegen(*name, statements);
        code_chunks.insert(*name, chunk);
        label_fixes.extend(fixes);
      }
      Item::ItemError => println!("ItemError@{file_span}"),
    };
  }
  println!("{code_chunks:X?}");
  println!("{label_fixes:?}");
}

#[derive(Debug, Clone)]
struct LabelFix {
  source_id: StrID,
  offset_within_source: usize,
  target_id: StrID,
}

/// Gives the output bytes for a fn and all the (label, offset) pairs within the
/// fn we need to update after fns are placed into the rom.
fn do_codegen(
  source_id: StrID, statements: &[(Statement, FileSpan)],
) -> (Vec<u8>, Vec<LabelFix>) {
  let mut out = Vec::new();
  let mut link_notes = Vec::new();
  for (statement, _file_span) in statements {
    match statement {
      Statement::Call { target, .. } => {
        link_notes.push(LabelFix {
          source_id,
          offset_within_source: out.len() + 1,
          target_id: *target,
        });
        out.extend(&[0xCD, 0xFF, 0xFF])
      }
      Statement::Return => out.extend(&[0xC9]),
    }
  }
  (out, link_notes)
}
