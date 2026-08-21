use yagbas::tokenizer::{Token, tokenize};

const SOURCE: &str = include_str!("../../tests/all_tests/minimum_program.yag");

fn main() {
  let tokens: Vec<Token> = tokenize(SOURCE).collect();
  println!("{tokens:?}");
}
