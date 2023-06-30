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
  println!("Const Decls: {decls:?}");
}
