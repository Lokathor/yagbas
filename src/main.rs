#![allow(unused)]
#![allow(clippy::while_let_on_iterator)]

use chumsky::{input::Input, prelude::*, span::SimpleSpan, IterParser, Parser};
use yagbas::{
  lexer::Token,
  parser::{comment_filter::no_comment_tokens, token_tree::make_token_trees, *},
};

fn main() {
  //let prog = include_str!("../tests/hello.yag");
  let prog = "section foo {} section bar { content in bar; }";

  let token_list = match no_comment_tokens(prog) {
    Ok(tokens) => tokens,
    Err(span) => {
      println!("Could not process comment markers: {span:?}");
      return;
    }
  };
  println!("Token List: {token_list:?}");

  let token_trees = make_token_trees(&token_list);
  println!("Token Trees: {token_trees:?}");
}
