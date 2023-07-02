#![allow(unused)]
#![allow(clippy::while_let_on_iterator)]

use chumsky::{input::Input, prelude::*, span::SimpleSpan, IterParser, Parser};
use serde_json::{json, Value};
use yagbas::{
  disassemble::{bytes_to_op_tokens, OpToken},
  lexer::Token,
  parser::{
    comment_filter::no_comment_tokens, const_decl::ConstDecl, section::Section,
    token_tree::make_token_trees, *,
  },
};

fn main() {
  //let prog = include_str!("../tests/hello.yag");
  let prog = r#"
    //const FOO = 123;
    //const NAME = "test-name";
    section main [rom0] {
      // just loop
      1: jp 1b;
    }
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

  let sections = {
    let tt = token_trees.output().unwrap();
    let parser = Section::parser()
      .map_with_span(|tt, span| (tt, span))
      .repeated()
      .collect::<Vec<_>>();
    run_parser(parser, tt)
  };
  println!("Sections: {sections:?}");
}
