
use super::*;

#[track_caller]
fn trees_no_errors(s: &str) {
  let (_trees, errors) = trees_of(s);
  assert!(errors.is_empty());
}

#[test]
fn basics() {
  trees_no_errors("");
  trees_no_errors("abc");
  trees_no_errors("() {} []");
  trees_no_errors("( {} ) [ (){{{}}} ]");
}
