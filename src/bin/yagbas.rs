use yagbas::tokenizer::tokenize;

const SOURCE: &str = include_str!("../../tests/all_tests/minimum_program.yag");

fn main() {
  let tokens = tokenize(SOURCE).collect::<Vec<_>>();
  println!("{tokens:?}");
}
