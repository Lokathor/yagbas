use std::{ffi::OsString, path::Path};

use str_id::PathId;
use yagbas::{
  ast::{AstItemKind, AstModule},
  blocks::tac_blocks_of_function,
  cst::Cst,
};

fn main() {
  let arguments: Vec<_> = std::env::args_os().skip(1).collect();
  if arguments.is_empty() {
    eprintln!("usage: yagbas [sub_command]");
    return;
  }
  match arguments[0].to_str() {
    Some("help") | Some("--help") | Some("/?") => do_help(),
    Some("cst") => do_cst(arguments),
    Some("ast") => do_ast(arguments),
    Some("tac") => do_tac(arguments),
    _ => {
      eprintln!("Unknown sub-command.");
      do_help();
    }
  }
}

fn do_tac(mut arguments: Vec<OsString>) {
  debug_assert_eq!(arguments[0].to_str().unwrap(), "tac");
  arguments.remove(0);
  let mut target_files = Vec::new();
  for argument in arguments {
    match argument.to_str() {
      Some("--help") => {
        println!("Usage: yagbas tac [args]");
        println!("show the three-address-code steps for one or more files");
        return;
      }
      _ => target_files.push(argument),
    }
  }
  if target_files.is_empty() {
    println!("(No filenames provided.)")
  }
  for target_file in target_files {
    let path_id = PathId::from(Path::new(&target_file));
    println!("## `{path_id}`");
    match std::fs::read_to_string(&target_file) {
      Ok(src) => {
        let ast = AstModule::from_source(&src);
        for item in ast.items.iter() {
          match &item.kind {
            AstItemKind::Function(f) => {
              let blocks = tac_blocks_of_function(f);
              println!("```");
              for (x, block) in blocks.iter().enumerate() {
                let id = block.id;
                if x > 0 {
                  println!();
                }
                println!("== TacBlock [{id:?}] {{");
                for step in block.steps.iter() {
                  let kind = &step.kind;
                  println!("  {kind:?}");
                }
                let terminator = block.terminator;
                println!("}}then> {terminator:?}");
              }
            }
            _ => continue,
          }
        }
        println!("```");
      }
      Err(e) => {
        println!("File Reading Error: {e:?}");
      }
    }
  }
}

fn do_ast(mut arguments: Vec<OsString>) {
  debug_assert_eq!(arguments[0].to_str().unwrap(), "ast");
  arguments.remove(0);
  let mut target_files = Vec::new();
  for argument in arguments {
    match argument.to_str() {
      Some("--help") => {
        println!("Usage: yagbas ast [args]");
        println!("show the abstract syntax tree for one or more files");
        return;
      }
      _ => target_files.push(argument),
    }
  }
  if target_files.is_empty() {
    println!("(No filenames provided.)")
  }
  for target_file in target_files {
    let path_id = PathId::from(Path::new(&target_file));
    println!("## `{path_id}`");
    match std::fs::read_to_string(&target_file) {
      Ok(src) => {
        let ast = AstModule::from_source(&src);
        println!("```");
        for item in ast.items {
          println!("== {item:#?}");
        }
        println!("```");
      }
      Err(e) => {
        println!("File Reading Error: {e:?}");
      }
    }
  }
}

fn do_cst(mut arguments: Vec<OsString>) {
  debug_assert_eq!(arguments[0].to_str().unwrap(), "cst");
  arguments.remove(0);
  let mut target_files = Vec::new();
  let mut show_trivia = false;
  for argument in arguments {
    match argument.to_str() {
      Some("--help") => {
        println!("Usage: yagbas cst [args]");
        println!("show the concrete syntax tree for one or more files");
        println!("non-file argument options are as follows:");
        println!(
          "--show-trivia      print whitespace and comment tokens in the cst"
        );
        return;
      }
      Some("--show-trivia") => {
        show_trivia = true;
      }
      _ => target_files.push(argument),
    }
  }
  if target_files.is_empty() {
    println!("(No filenames provided.)")
  }
  for target_file in target_files {
    println!("## `{}`", target_file.display());
    match std::fs::read_to_string(&target_file) {
      Ok(src) => {
        println!("```");
        let cst = Cst::from_module_src(&src);
        if show_trivia {
          println!("{cst:#}");
        } else {
          println!("{cst}");
        }
        println!("```");
        cst.assert_no_errors();
      }
      Err(e) => {
        println!("File Reading Error: {e:?}");
      }
    }
  }
}

fn do_help() {
  println!("yagbas is an incomplete compiler.");
  println!("Usage: yagbas [sub_command]");
  println!("Current sub-commands are:");
  println!(" help     this help message.");
  println!(" cst      view the concrete syntax tree for one or more files.");
}
