use yagbas::cst::Cst;

#[track_caller]
fn cst_no_errors(src: &str) -> Cst {
  let cst = Cst::from_module_src(src);
  cst.assert_no_errors();
  cst
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
  cst_no_errors("fn foo() {}");
}

#[test]
fn test_empty_function2() {
  cst_no_errors(
    "fn foo() {
    }",
  );
}

#[test]
fn test_empty_function3() {
  cst_no_errors(
    "fn foo() {
      // comment
    }",
  );
}

#[test]
fn test_empty_let_lit_num() {
  cst_no_errors(
    "fn foo() {
      let _ = 1;
    }",
  );
}

#[test]
fn test_empty_let_lit_true() {
  cst_no_errors(
    "fn foo() {
      let _ = true;
    }",
  );
}

#[test]
fn test_empty_let_lit_false() {
  cst_no_errors(
    "fn foo() {
      let _ = false;
    }",
  );
}

#[test]
fn test_empty_let_lit_str() {
  cst_no_errors(
    "fn foo() {
      let _ = \"abc\";
    }",
  );
}
