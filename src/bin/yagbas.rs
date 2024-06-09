#![allow(unused)]
#![allow(clippy::type_complexity)]

use std::{
  collections::HashMap,
  path::{Path, PathBuf},
};

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

  let entry_name = "@magic_entry_point";
  let mut rom = vec![0_u8; 0x0150];
  // hack in the entry point code: `di; jp main`
  rom[0x0100] = 0xF3;
  rom[0x0101] = 0xC3;
  rom[0x0102] = 0xFF;
  rom[0x0103] = 0xFF;
  label_fixes.push(LabelFix {
    source_id: StrID::from(entry_name),
    offset_within_source: 2,
    target_id: StrID::from("main"),
  });

  let mut chunk_locations: HashMap<StrID, usize> = HashMap::new();
  chunk_locations.insert(StrID::from(entry_name), 0x0100);
  for (name, data) in &code_chunks {
    // TODO: we have to handle rom banking
    chunk_locations.insert(*name, rom.len());
    rom.extend(data);
  }
  println!("Final Rom Size: {}", rom.len());
  for fix in &label_fixes {
    let source_base: usize = *chunk_locations.get(&fix.source_id).unwrap();
    let source_actual = source_base + fix.offset_within_source;
    let target_usize: usize = *chunk_locations.get(&fix.target_id).unwrap();
    let target_u16: u16 = target_usize.try_into().unwrap();
    let target_bytes: [u8; 2] = target_u16.to_le_bytes();
    rom[source_actual..(source_actual + 2)].copy_from_slice(&target_bytes);
  }
  // TODO: header fixing steps

  let file_path = PathBuf::from(filename);
  let mut path_buf = PathBuf::new();
  path_buf.push("target");
  path_buf.push(file_path.file_name().unwrap());
  path_buf.set_extension("gb");
  println!("out_file: {path_buf:?}");
  std::fs::write(&path_buf, &rom).unwrap();
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
  let mut label_fixes = Vec::new();
  for (statement, _file_span) in statements {
    match statement {
      Statement::Call { target, .. } => {
        label_fixes.push(LabelFix {
          source_id,
          offset_within_source: out.len() + 1,
          target_id: *target,
        });
        out.extend(&[0xCD, 0xFF, 0xFF])
      }
      Statement::Return => out.extend(&[0xC9]),
    }
  }
  (out, label_fixes)
}
