#![allow(unused_variables)]

use super::*;

#[test]
fn test_infix_expressions() {
  let mut p = CstParser::new(tokenize("2").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(tokenize("true").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(tokenize("false").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(tokenize("x").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(tokenize("\"abc\"").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(tokenize("(2)").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(tokenize("(2 )").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(tokenize("2+3").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(tokenize("2+3+4+5").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(tokenize("2+3*4+5").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());
}

#[test]
fn test_prefix_expressions() {
  let mut p = CstParser::new(tokenize("&x").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(tokenize("& x").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(tokenize("-x").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(tokenize("- x").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(tokenize("-(x)").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(tokenize("return").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(tokenize("return x").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(tokenize("break").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(tokenize("break 'gather").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(tokenize("break x").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(tokenize("break 'gather x").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(tokenize("..x").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(tokenize("..=x").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());
}

#[test]
fn test_postfix_expressions() {
  let mut p = CstParser::new(tokenize("x?").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(tokenize("x ?").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(tokenize("x()").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(tokenize("x ()").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(tokenize("x ( )").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(tokenize("x (1)").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(tokenize("x ( 1)").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(tokenize("x ( 1 )").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(tokenize("x ( 1 , 2 )").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(tokenize("x ( 1 , 2, 3 )").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(tokenize("x as i32").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(tokenize("x [ y ]").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(tokenize("x ..").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());

  let mut p = CstParser::new(tokenize("x ..=").collect());
  assert!(try_val_expr(&mut p, 0).is_some());
  let (cst, errs) = p.build_tree();
  assert!(errs.is_empty());
}
