#![allow(unused)]
#![allow(clippy::type_complexity)]

use ariadne::{
  sources, CharSet, Color, Config, Label, Report, ReportKind, Source,
};
use chumsky::{error::Rich, span::Span};
use clap::{Args, Parser, Subcommand, ValueEnum};
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

#[derive(Debug, Clone, Copy, Default, ValueEnum)]
pub enum MessageSize {
  Bulky,
  #[default]
  Compact,
  OneLine,
}
impl core::fmt::Display for MessageSize {
  #[inline]
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    core::fmt::Debug::fmt(self, f)
  }
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
  /// Output size for messages (default: compact)
  #[arg(long)]
  pub message_size: Option<MessageSize>,

  /// One or more source files to tokenize.
  pub files: Vec<String>,
}

#[derive(Args, Debug, Clone)]
pub struct TreesArgs {
  /// Output size for messages (default: compact)
  #[arg(long)]
  pub message_size: Option<MessageSize>,

  /// One or more source files to tokenize.
  pub files: Vec<String>,
}

#[derive(Args, Debug, Clone)]
pub struct ItemsArgs {
  /// Output size for messages (default: compact)
  #[arg(long)]
  pub message_size: Option<MessageSize>,

  /// One or more source files to tokenize.
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

fn report_these_errors<T>(
  id: SrcID, errors: &[Rich<'static, T, FileSpan, &str>],
  message_size: MessageSize,
) where
  T: core::fmt::Debug,
{
  let compact_messages: bool = match message_size {
    MessageSize::OneLine => {
      for error in errors {
        println!("{id}: {error:?}");
      }
      return;
    }
    MessageSize::Bulky => false,
    MessageSize::Compact => true,
  };
  let mut the_cache = sources(vec![(id, id.get_src_file().text())]);
  for error in errors {
    let file_span: FileSpan = *error.span();
    Report::build(ReportKind::Error, file_span)
      .with_message(format!("{error:?}"))
      .with_config(
        Config::default()
          .with_color(false)
          .with_char_set(CharSet::Ascii)
          .with_compact(compact_messages),
      )
      .with_labels(error.contexts().map(|(context, file_span)| {
        Label::new(*file_span).with_message(format!("while parsing {context}"))
      }))
      .finish()
      .eprint(&mut the_cache)
      .unwrap();
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
    println!("=TREES {filename}: {trees:?}");
    if !tree_errors.is_empty() {
      report_these_errors(
        src_file.get_id(),
        &tree_errors,
        args.message_size.unwrap_or_default(),
      )
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
      report_these_errors(
        src_file.get_id(),
        &item_errors,
        args.message_size.unwrap_or_default(),
      )
    }
  }
}
