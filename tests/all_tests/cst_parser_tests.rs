use yagbas::cst::Cst;

#[test]
fn test_empty_module() {
  let cst = Cst::from_module_src("");
  cst.assert_no_errors();
}
#[test]
fn test_trivia_module() {
  let cst = Cst::from_module_src(
    "
    // just a comment
    ",
  );
  cst.assert_no_errors();
}
#[test]
fn test_empty_function() {
  let cst = Cst::from_module_src("fn foo(){}");
  cst.assert_no_errors();
}
