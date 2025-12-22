use yagbas::*;

fn fake_file_id(u: usize) -> FileID {
  assert!(u != 0);
  unsafe { core::mem::transmute(u) }
}

#[track_caller]
fn assert_no_parse_errors(s: &'static str) -> Vec<AstItem> {
  let tokens = tokens_of(s);
  let (trees, tree_errors) = trees_of(&tokens);
  for tree_error in tree_errors.iter() {
    eprintln!("{tree_error:?}");
  }
  assert!(tree_errors.is_empty());
  let yag_state = YagParserState { file_id: fake_file_id(1), source: s };
  let (items, item_errors) = items_of(&trees, yag_state);
  for item_error in item_errors.iter() {
    eprintln!("{item_error:?}");
  }
  assert!(item_errors.is_empty());
  items
}

#[test]
fn test_empty_source() {
  assert_no_parse_errors("");
}
#[test]
fn test_empty_fn() {
  assert_no_parse_errors("fn foo() {}");
}
#[test]
fn test_let_statements() {
  assert_no_parse_errors(
    "fn foo() {
      let x;
    }",
  );
  assert_no_parse_errors(
    "fn foo() {
      let x: u8;
    }",
  );
  assert_no_parse_errors(
    "fn foo() {
      let x = 1;
    }",
  );
  assert_no_parse_errors(
    "fn foo() {
      let x: u8 = 1;
    }",
  );
}
#[test]
fn test_assign_statements() {
  assert_no_parse_errors(
    "fn foo() {
      x = 3;
    }",
  );
}
#[test]
fn test_bin_op_assign_statements() {
  assert_no_parse_errors(
    "fn foo() {
      x += 3;
      x *= 4;
      x /= 5;
      x %= 6;
      x <<= 7;
      x >>= 8;
      x ^= 9;
      x |= 10;
      x &= 11;
    }",
  );
}

#[test]
fn test_if_statements() {
  assert_no_parse_errors(
    "fn foo() {
      if a {}
      if a {} else {}
      if a {};
      if a {} else {};
      if a {
        let x = 1 + 2;
        4
      }
    }",
  );
}

#[test]
fn test_loop_statements() {
  assert_no_parse_errors(
    "fn foo() {
      loop {}
      loop {};
      'named: loop {}
      'named: loop {};
      loop 3 times {}
      loop 3 times {};
      loop count times {}
      loop count times {};
      loop if FOO { 4 } else { 3 } times {}
    }",
  );
}
