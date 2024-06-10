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
  assert!(item_errors.is_empty());
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

  let mut rom = build_header("temporary99", 0);
  let entry_name = "@magic_entry_point";
  label_fixes.push(LabelFix {
    source_id: StrID::from(entry_name),
    offset_within_source: 2,
    target_id: StrID::from("main"),
    target_offset: 0,
  });

  let mut chunk_locations: HashMap<StrID, usize> = HashMap::new();
  chunk_locations.insert(StrID::from(entry_name), 0x0100);
  for (name, data) in &code_chunks {
    // TODO: we have to handle rom banking
    chunk_locations.insert(*name, rom.len());
    rom.extend(data);
  }
  println!("Final Rom Size: {}", rom.len());
  assert!(rom.len() <= (32 * 1024), "Exceeded no-banking capacity");
  println!("{chunk_locations:?}");
  for fix in &label_fixes {
    let source_base: usize = *chunk_locations.get(&fix.source_id).unwrap();
    let source_actual = source_base + fix.offset_within_source;
    println!("{:?}", fix.target_id);
    let target_usize: usize =
      *chunk_locations.get(&fix.target_id).unwrap() + fix.target_offset;
    let target_u16: u16 = target_usize.try_into().unwrap();
    let target_bytes: [u8; 2] = target_u16.to_le_bytes();
    rom[source_actual..(source_actual + 2)].copy_from_slice(&target_bytes);
  }

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
  target_offset: usize,
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
          target_offset: 0,
        });
        out.extend(&[0xCD, 0xFF, 0xFF])
      }
      Statement::Return => out.extend(&[0xC9]),
      Statement::Loop(content) => {
        let (loop_bytes, mut loop_fixes) = do_codegen(source_id, statements);
        for fix in &mut loop_fixes {
          fix.offset_within_source += out.len();
        }
        label_fixes.push(LabelFix {
          source_id,
          offset_within_source: out.len(),
          target_id: source_id,
          target_offset: out.len() + loop_bytes.len(),
        });
        out.extend(loop_bytes);
        label_fixes.extend(loop_fixes);
        out.extend(&[0xC3, 0xFF, 0xFF])
      }
    }
  }
  (out, label_fixes)
}

fn build_header(title: &str, rom_bank_size: u8) -> Vec<u8> {
  let mut out = vec![0x00_u8; 0x0100_usize];
  // entry point: `di`
  out.extend(&[0xF3]);
  // entry point: `jp main`
  out.extend(&[0xC3, 0xFF, 0xFF]);
  // logo header, required for boot
  out.extend(&[
    0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83,
    0x00, 0x0C, 0x00, 0x0D, 0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E,
    0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99, 0xBB, 0xBB, 0x67, 0x63,
    0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E,
  ]);
  // Add the title
  out.extend(title.bytes().chain(core::iter::repeat(0)).take(11));
  // manufacturer's code
  out.extend(b"RUST");
  // CGB flag
  out.extend(&[0x80]);
  // new licensee code
  out.extend(&[0x00, 0x00]);
  // SGB flag
  out.extend(&[0x03]);
  // MBC selection, for now we always pick MBC5+RAM+Battery
  out.extend(&[0x1B]);
  // ROM size
  out.extend(&[rom_bank_size]);
  // RAM size, here we always pick 16 banks of 8kb each
  out.extend(&[0x04]);
  // game "destinarion" is international
  out.extend(&[0x01]);
  // old licensee code
  out.extend(&[0x00]);
  // rom version number
  out.extend(&[0x00]);
  // header checksum
  out.extend(&[{
    let mut checksum = 0_u8;
    for byte in &out[0x0134..=0x014C] {
      checksum = checksum.wrapping_sub(*byte).wrapping_sub(1);
    }
    checksum
  }]);
  // global checksum, generally ignored
  out.extend(&[0xFF, 0xFF]);

  out
}
