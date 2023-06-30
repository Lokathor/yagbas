#![allow(unused)]
#![allow(clippy::while_let_on_iterator)]

use chumsky::{input::Input, prelude::*, span::SimpleSpan, IterParser, Parser};
use serde_json::{json, Value};
use yagbas::{
  disassemble::{bytes_to_op_tokens, OpToken},
  lexer::Token,
  parser::{
    comment_filter::no_comment_tokens, const_decl::ConstDecl,
    token_tree::make_token_trees, *,
  },
};

fn main() {
  //let prog = include_str!("../tests/hello.yag");
  let prog = r#"
    const FOO = 123;
    const NAME = "test-name";
    //section main [rom0] {
    //  // just loop
    //  1: jp 1b;
    //}
  "#;

  let rom = std::fs::read("tests/blargg/01-special.gb").unwrap();
  for (i, bank) in rom.chunks(8 * 1024).enumerate() {
    let mut zeros: u32 = 0;
    println!("/* Bank {i} */");
    for op in bytes_to_op_tokens(bank) {
      match op {
        OpToken::One(0) => zeros += 1,
        _ => {
          match zeros {
            0 => (),
            1 => {
              println!("    {};", OpToken::One(0x00));
              zeros = 0;
            }
            2.. => {
              println!("    zero_bytes!({zeros});");
              zeros = 0;
            }
          }
          println!("    {op};")
        }
      };
    }
    if zeros > 0 {
      println!("    zero_bytes!({zeros});");
    }
  }

  let token_list = match no_comment_tokens(prog) {
    Ok(tokens) => tokens,
    Err(span) => {
      println!("Could not process comment markers: {span:?}");
      return;
    }
  };
  //println!("Token List: {token_list:?}");

  let token_trees = make_token_trees(&token_list);
  //println!("Token Trees: {token_trees:?}");

  let decls = {
    let tt = token_trees.output().unwrap();
    let span: SimpleSpan = if tt.is_empty() {
      (0..0).into()
    } else {
      let start = tt.first().unwrap().1.start;
      let end = tt.last().unwrap().1.end;
      (start..end).into()
    };
    ConstDecl::parser()
      .map_with_span(|tt, span| (tt, span))
      .repeated()
      .collect::<Vec<_>>()
      .parse(tt.spanned(span))
  };
  //println!("Const Decls: {decls:?}");
}
