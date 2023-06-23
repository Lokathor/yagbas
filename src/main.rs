#![allow(unused)]
#![allow(clippy::while_let_on_iterator)]

use chumsky::{input::Input, prelude::*, span::SimpleSpan, IterParser, Parser};
use yagbas::{lexer::Token, parser::*};

fn main() {
  let prog = r#"
    const FOO = 1_2;
    const BAR = -2;
    const ZAP = %0101_012;
    const HEK = $FF00;
    section main [rom0] {
      // comment1
      label1:
        ld a FOO;
        ld b $4_6;
        ld c %101_010;
        d = $FF;
      1: /* foo bar baz */
        inc c;
        zero_bytes!(3);
        jr 1b;
    }
  "#;

  let tokens: Vec<(Token, SimpleSpan)> = match strip_comments(Token::lexer(prog).spanned()) {
    Ok(tokens) => tokens,
    Err(e) => {
      println!("{e:?}");
      return;
    }
  };
  //println!("Tokens: {tokens:?}");

  let x = item_parser()
    .repeated()
    .collect::<Vec<_>>()
    .parse(tokens.spanned(SimpleSpan::from(0..prog.len())));
  for item in x.output().unwrap() {
    match item {
      Item::Const(c) => {
        let name = c.0.name.0;
        let val = interpret_num(c.0.val.0);
        println!("Const> Name: {name:?}, Val: {val:?}");
      }
      Item::Section(_) => continue,
    }
  }
}
