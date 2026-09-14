use yagbas::cst::{Cst, actions::do_module, parser::CstParser};

#[track_caller]
fn cst_no_errors(src: &str) -> Cst {
  let mut p = CstParser::new(src);
  do_module(&mut p);
  let (cst, errors) = p.build_tree();
  assert!(errors.is_empty(), "Cst Parse Errors: {errors:?}");
  cst
}

#[test]
fn test_empty_module() {
  cst_no_errors("");
  cst_no_errors(" ");
  cst_no_errors("/**/");
}
