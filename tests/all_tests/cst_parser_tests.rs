use yagbas::cst::Cst;

#[track_caller]
fn cst_no_errors(src: &str) {
  let cst = Cst::from_module_src(src);
  cst.assert_no_errors();
}

#[test]
fn test_empty_module() {
  cst_no_errors("");
}

#[test]
fn test_trivia_module() {
  cst_no_errors(
    "
    // just a comment
    ",
  );
}

#[test]
fn test_empty_function() {
  cst_no_errors("fn foo(){}");
}

#[test]
fn test_empty_function2() {
  cst_no_errors(
    "fn foo(){
    }",
  );
}

#[test]
fn test_empty_function3() {
  cst_no_errors(
    "fn foo(){
      // comment
    }",
  );
}
