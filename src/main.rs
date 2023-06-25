#![allow(unused)]
#![allow(clippy::while_let_on_iterator)]

use chumsky::{input::Input, prelude::*, span::SimpleSpan, IterParser, Parser};
use yagbas::{lexer::Token, parser::*};

fn main() {
  let prog = include_str!("../tests/hello.yag");

  let tokens: Vec<(Token, SimpleSpan)> = match strip_comments(Token::lexer(prog).spanned()) {
    Ok(tokens) => tokens,
    Err(e) => {
      println!("CommentStripError::{e:?}, <{}>", &prog[e.get_span()]);
      return;
    }
  };

  let x = item_parser()
    .repeated()
    .collect::<Vec<_>>()
    .then_ignore(end())
    .parse(tokens.spanned(SimpleSpan::from(0..prog.len())));
  for error in x.errors() {
    println!("ERROR: {error:?}");
  }
  for item in x.output().unwrap() {
    match item {
      Item::Const(c) => {
        let name = c.0.name.0;
        let val = interpret_num(c.0.val.0);
        println!("Const> Name: {name:?}, Val: {val:?}");
      }
      Item::Section(s) => {
        let name = s.0.name.0;
        let locations = &s.0.locations;
        println!("Section> Name: {name}");
      }
    }
  }
}
