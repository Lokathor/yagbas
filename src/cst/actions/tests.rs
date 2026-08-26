#![allow(unused_variables)]

use super::*;

#[test]
fn test_infix_expressions() {
  let mut p = CstParser::new("2");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("true");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("false");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("x");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("\"abc\"");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("(2)");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("(2 )");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("2+3");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("2+3+4+5");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("2+3*4+5");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());
}

#[test]
fn test_prefix_expressions() {
  let mut p = CstParser::new("&x");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("& x");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("-x");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("- x");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("-(x)");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("return");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("return x");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("break");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("break 'gather");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("break x");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("break 'gather x");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("..x");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("..=x");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());
}

#[test]
fn test_postfix_expressions() {
  let mut p = CstParser::new("x?");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("x ?");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("x()");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("x ()");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("x ( )");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("x (1)");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("x ( 1)");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("x ( 1 )");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("x ( 1 , 2 )");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("x ( 1 , 2, 3 )");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("x as i32");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("x [ y ]");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("x ..");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("x ..=");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("sqrt(2)");
  assert!(try_value_expr(&mut p).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());
}

#[test]
fn test_try_stmt() {
  let mut p = CstParser::new("let x = 0;");
  assert!(try_stmt(&mut p).is_some(), "{p:?}");
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new("sqrt(2);");
  assert!(try_stmt(&mut p).is_some(), "{p:?}");
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());
}

#[test]
fn test_try_func() {
  let mut p = CstParser::new("fn foo() {}");
  let m = p.open();
  do_func(&mut p, m);
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(
    "fn bar() {
      //
    }",
  );
  let m = p.open();
  do_func(&mut p, m);
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(
    "fn baz() {
      let x = 0;
    }",
  );
  let m = p.open();
  do_func(&mut p, m);
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());
}

#[test]
fn test_do_module() {
  let mut p = CstParser::new("fn foo() {}");
  do_module(&mut p);
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(
    "// empty fn definition
    fn foo() {}",
  );
  do_module(&mut p);
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());
}
