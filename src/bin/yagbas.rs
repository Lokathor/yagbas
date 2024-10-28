#![allow(unused)]
#![allow(clippy::type_complexity)]

use ariadne::{
  sources, CharSet, Color, Config, Label, Report, ReportKind, Source,
};
use clap::{Args, Parser, Subcommand};
use std::{
  collections::HashMap,
  path::{Path, PathBuf},
};
use yagbas::{
  parsing::token_tree_p,
  src_files::{
    FileSpan, FileSpanned, ItemParseResult, LexOutput, SrcFile, SrcID,
    TokenTreeParseResult,
  },
  str_id::StrID,
  token::Token,
  token_tree::TokenTree,
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
  /// Prints all tokens within the source files given.
  Tokenize(TokenizeArgs),
  /// Prints all token trees within the source files given.
  Trees(TreesArgs),
  /// Prints all items within the source files given.
  Items(ItemsArgs),
}

#[derive(Args, Debug, Clone)]
pub struct TokenizeArgs {
  /// One or more source files to tokenize.
  pub files: Vec<String>,
}

#[derive(Args, Debug, Clone)]
pub struct TreesArgs {
  /// One or more source files to make into token trees.
  pub files: Vec<String>,
}

#[derive(Args, Debug, Clone)]
pub struct ItemsArgs {
  /// One or more source files to make into items.
  pub files: Vec<String>,
}

pub fn main() {
  let cli = Cli::parse();
  match cli.command {
    Commands::Tokenize(args) => do_tokenize(args),
    Commands::Trees(args) => do_trees(args),
    Commands::Items(args) => do_items(args),
  }
}

pub fn do_tokenize(args: TokenizeArgs) {
  for filename in &args.files {
    let src_file = match SrcFile::read_from_path(&filename) {
      Ok(src_file) => src_file,
      Err(io_error) => {
        eprintln!("{filename}: File IO Error: {io_error}");
        continue;
      }
    };
    let LexOutput { tokens, lex_errors } = src_file.lex_tokens();
    println!("{filename}: {tokens:?}");
    if !lex_errors.is_empty() {
      println!("{filename}: ERRORS: {lex_errors:?}");
    }
  }
}

pub fn do_trees(args: TreesArgs) {
  for filename in &args.files {
    let src_file = match SrcFile::read_from_path(&filename) {
      Ok(src_file) => src_file,
      Err(io_error) => {
        eprintln!("{filename}: File IO Error: {io_error}");
        continue;
      }
    };
    let TokenTreeParseResult { trees, tree_errors } =
      src_file.parse_token_trees();
    println!("{filename}: {trees:?}");
    let id = src_file.get_id();
    let mut err_cache = sources(vec![(id, src_file.text())]);
    for tree_error in tree_errors {
      let file_span: &FileSpan = tree_error.span();
      let span = file_span.start..file_span.end;
      let reason = tree_error.reason();
      let message: String = match reason {
        chumsky::error::RichReason::ExpectedFound { expected, found: None } => {
          format!("Expected: {expected:?}, Found: the end of the file")
        }
        chumsky::error::RichReason::ExpectedFound {
          expected,
          found: Some(t),
        } => {
          format!("Expected: {expected:?}, Found: {t:?}")
        }
        chumsky::error::RichReason::Custom(s) => s.to_string(),
        chumsky::error::RichReason::Many(vec) => format!("{vec:?}"),
      };
      Report::build(ReportKind::Error, (id, span.clone()))
        .with_message("Token Tree Building")
        .with_config(
          Config::default()
            .with_compact(true)
            .with_color(false)
            .with_char_set(CharSet::Ascii),
        )
        .with_label(Label::new((id, span.clone())).with_message(message))
        .with_labels(tree_error.contexts().map(|(name, span)| {
          Label::new((id, file_span.start..file_span.end))
            .with_message(format!("while parsing this {name}"))
        }))
        .finish()
        .eprint(&mut err_cache)
        .unwrap();
    }
  }
}

pub fn do_items(args: ItemsArgs) {
  for filename in &args.files {
    let src_file = match SrcFile::read_from_path(&filename) {
      Ok(src_file) => src_file,
      Err(io_error) => {
        eprintln!("{filename}: File IO Error: {io_error}");
        continue;
      }
    };
    let ItemParseResult { items, item_errors } = src_file.parse_items();
    println!("=ITEMS {filename}: {items:?}");
    if !item_errors.is_empty() {
      println!("=ITEMS {filename}: ERRORS: {item_errors:?}");
    }
  }
}
