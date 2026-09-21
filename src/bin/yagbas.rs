use std::{ffi::OsString, path::Path};
use yagbas::{
  ast::{Ast, actions::parse_ast_module, parser::AstParser},
  cst::{
    actions::gather_module,
    parser::{BuildTreeArgs, CstParser},
  },
  path_id::PathId,
};

fn main() {
  let arguments: Vec<_> = std::env::args_os().skip(1).collect();
  if arguments.is_empty() {
    eprintln!("usage: yagbas [sub_command]");
    eprintln!("you can also pass --help for options");
    return;
  }
  match arguments[0].to_str() {
    Some("help") | Some("--help") | Some("/?") => do_help(),
    Some("cst") => do_cst(arguments),
    Some("ast") => do_ast(arguments),
    //Some("nameres") => do_nameres(arguments),
    _ => {
      eprintln!("Unknown sub-command.");
      do_help();
    }
  }
}

#[cfg(false)]
fn do_nameres(mut arguments: Vec<OsString>) {
  debug_assert_eq!(arguments[0].to_str().unwrap(), "nameres");
  arguments.remove(0);
  let mut target_files = Vec::new();
  for argument in arguments {
    match argument.to_str() {
      Some("--help") => {
        println!("Usage: yagbas nameres [args]");
        println!("show the name resolution info of the files given");
        return;
      }
      _ => target_files.push(argument),
    }
  }
  if target_files.is_empty() {
    println!("(No filenames provided.)");
    return;
  }
  let mut ast = Ast::default();
  for target_file in target_files {
    let path = Path::new(&target_file);
    let file_origin = PathId::from(path);
    match std::fs::read_to_string(path) {
      Ok(src) => {
        let mut p = CstParser::new(&src);
        gather_module(&mut p);
        let cst = p.build_tree(BuildTreeArgs { skip_trivial: true });
        let mut ast_parser = AstParser { file_origin, errors: Vec::new() };
        let module = parse_ast_module(&mut ast_parser, file_origin, &cst);
        ast.modules.push(module);
        ast.errors.extend(ast_parser.errors);
      }
      Err(e) => {
        println!("File Reading Error: {e:?}");
      }
    }
  }
  let ir = IrNameResTypeCheck::build_from_ast(ast);
  println!("```");
  for module in &ir.ast.modules {
    println!("> Module: {:?}", module.file_origin);
    for item in &module.items {
      println!(">> {item:#?}");
    }
  }
  for (name, info) in &ir.names {
    println!("> Name: {name:?} = {info:?}");
  }
  for (ty, info) in &ir.types {
    println!("> Type: {ty:?} = {info:?}");
  }
  for error in &ir.ast.errors {
    println!(">> Ast Error: {error:?}");
  }
  println!("```");
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
    println!("(No filenames provided.)");
    return;
  }
  let mut ast = Ast::default();
  for target_file in target_files {
    let path = Path::new(&target_file);
    let file_origin = PathId::from(path);
    match std::fs::read_to_string(path) {
      Ok(src) => {
        let mut p = CstParser::new(&src);
        gather_module(&mut p);
        let cst = p.build_tree(BuildTreeArgs { skip_trivial: true });
        let mut ast_parser = AstParser { file_origin, errors: Vec::new() };
        let module = parse_ast_module(&mut ast_parser, file_origin, &cst);
        ast.modules.push(module);
        ast.errors.extend(ast_parser.errors);
      }
      Err(e) => {
        println!("File Reading Error: {e:?}");
      }
    }
  }
  println!("```");
  for module in ast.modules {
    println!("= Module: {:?}", module.file_origin);
    for item in &module.items {
      println!("== {item:#?}");
    }
  }
  for error in ast.errors {
    println!("== Ast Error: {error:?}");
  }
  println!("```");
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
        let mut p = CstParser::new(&src);
        gather_module(&mut p);
        let cst = p.build_tree(BuildTreeArgs { skip_trivial: !show_trivia });
        if show_trivia {
          println!("{cst:#}");
        } else {
          println!("{cst}");
        }
        println!("```");
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
  println!(" help     This help message.");
  println!(" cst      View the concrete syntax tree for the files.");
  println!(" ast      View the abstract syntax tree for the files.");
  println!(" nameres  Run name resolution on the files.");
}
