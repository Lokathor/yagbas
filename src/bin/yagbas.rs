#![allow(unused)]
#![allow(clippy::type_complexity)]

use ariadne::{
  sources, CharSet, Color, Config, Label, LabelAttach, Report, ReportKind,
  Source,
};
use chumsky::{error::Rich, span::Span};
use clap::{Args, Parser, Subcommand, ValueEnum};
use std::{
  collections::HashMap,
  path::{Path, PathBuf},
};
use yagbas::{
  ast::parsing::{lex_module_text, parse_items, parse_token_trees},
  errors::YagError,
  file_span::FileSpan,
  file_spanned::FileSpanned,
  src_file::{SrcFile, SrcID},
  str_id::StrID,
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
  Tokens(TokensArgs),
  /// Prints all token trees within the source files given.
  Trees(TreesArgs),
  /// Prints all items within the source files given.
  Items(ItemsArgs),
}

#[derive(Args, Debug, Clone)]
pub struct TokensArgs {
  /// Output size for messages (default: compact)
  #[arg(long)]
  pub message_size: Option<MessageSize>,

  /// One or more source files to print tokens for.
  pub files: Vec<String>,
}

#[derive(Args, Debug, Clone)]
pub struct TreesArgs {
  /// Output size for messages (default: compact)
  #[arg(long)]
  pub message_size: Option<MessageSize>,

  /// One or more source files to print tokens for.
  pub files: Vec<String>,
}

#[derive(Args, Debug, Clone)]
pub struct ItemsArgs {
  /// Output size for messages (default: compact)
  #[arg(long)]
  pub message_size: Option<MessageSize>,

  /// One or more source files to print tokens for.
  pub files: Vec<String>,
}

pub fn main() {
  let cli = Cli::parse();
  match cli.command {
    Commands::Tokens(args) => do_tokens(args),
    Commands::Trees(args) => do_trees(args),
    Commands::Items(args) => do_items(args),
  }
}

fn report_all_the_errors(
  src_files: Vec<SrcFile>, err_bucket: Vec<YagError>,
  message_size: Option<MessageSize>,
) {
  let mut ariadne_cache = sources(
    src_files.iter().map(|src_file| (src_file.get_id(), src_file.text())),
  );
  let base_config = Config::new()
    .with_char_set(CharSet::Ascii)
    .with_color(false)
    .with_label_attach(LabelAttach::Start);
  match message_size.unwrap_or_default() {
    MessageSize::OneLine => {
      for err in err_bucket {
        eprintln!("{}", err.one_line());
      }
    }
    MessageSize::Bulky => {
      for err in err_bucket {
        let config = base_config.with_compact(false);
        err.build_report(config).eprint(&mut ariadne_cache);
      }
    }
    MessageSize::Compact => {
      for err in err_bucket {
        let config = base_config.with_compact(true);
        err.build_report(config).eprint(&mut ariadne_cache);
      }
    }
  }
}

pub fn do_tokens(args: TokensArgs) {
  let mut src_files = Vec::new();
  let mut err_bucket = Vec::new();
  for read_result in args.files.iter().map(SrcFile::read_from_path) {
    match read_result {
      Err(e) => err_bucket.push(e),
      Ok(src_file) => {
        let tokens =
          lex_module_text(src_file.text(), src_file.get_id(), &mut err_bucket);
        //
        let filename = src_file.path().display();
        println!("=TOKENS {filename}: {tokens:?}");
        src_files.push(src_file);
      }
    }
  }
  report_all_the_errors(src_files, err_bucket, args.message_size);
}

pub fn do_trees(args: TreesArgs) {
  let mut src_files = Vec::new();
  let mut err_bucket = Vec::new();
  for read_result in args.files.iter().map(SrcFile::read_from_path) {
    match read_result {
      Err(e) => err_bucket.push(e),
      Ok(src_file) => {
        let tokens =
          lex_module_text(src_file.text(), src_file.get_id(), &mut err_bucket);
        let trees = parse_token_trees(&tokens, &mut err_bucket);
        //
        let filename = src_file.path().display();
        println!("=TREES {filename}: {trees:?}");
        src_files.push(src_file);
      }
    }
  }
  report_all_the_errors(src_files, err_bucket, args.message_size);
}

pub fn do_items(args: ItemsArgs) {
  let mut src_files = Vec::new();
  let mut err_bucket = Vec::new();
  for read_result in args.files.iter().map(SrcFile::read_from_path) {
    match read_result {
      Err(e) => err_bucket.push(e),
      Ok(src_file) => {
        let tokens =
          lex_module_text(src_file.text(), src_file.get_id(), &mut err_bucket);
        let trees = parse_token_trees(&tokens, &mut err_bucket);
        let items = parse_items(&trees, &mut err_bucket);
        //
        let filename = src_file.path().display();
        println!("=ITEM {filename}: {items:?}");
        src_files.push(src_file);
      }
    }
  }
  report_all_the_errors(src_files, err_bucket, args.message_size);
}
