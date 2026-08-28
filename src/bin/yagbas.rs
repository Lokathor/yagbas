use yagbas::cst::actions::do_module;
use yagbas::cst::parser::CstParser;

const SOURCE: &str = include_str!("../../tests/all_tests/hello_tiles.yag");

fn main() {
  let mut p = CstParser::new(SOURCE);
  do_module(&mut p);
  let cst = p.build_tree();
  println!("== CST: {cst:}");
}
