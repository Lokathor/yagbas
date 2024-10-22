use yagbas::{
  item::{parse_token_trees_to_items, FnDecl, Item, Statement},
  src_files::{FileSpanned, SrcFileInfo, SrcID},
  str_id::StrID,
  token_tree::parse_tokens_to_token_trees,
};

#[track_caller]
fn parse_items_no_errors(s: &str) -> Vec<FileSpanned<Item>> {
  let file_info_id = SrcID::from(SrcFileInfo::in_memory(s));
  let tokens: Vec<_> = file_info_id.iter_tokens().collect();
  let (token_trees, tree_errors) = parse_tokens_to_token_trees(&tokens);
  assert!(tree_errors.is_empty());
  let (items, item_errors) = parse_token_trees_to_items(&token_trees);
  assert!(item_errors.is_empty(), "{item_errors:?}");
  items
}

#[test]
fn can_parse_empty_main_fn() {
  let src = r#" fn main() {} "#;
  let items = parse_items_no_errors(src);
  match items.as_slice() {
    [item] => match &item._payload {
      Item::Fn(FnDecl { name, args, statements }) => {
        assert_eq!(name.as_str(), "main");
        assert!(args.is_empty());
        assert!(statements.is_empty());
      }
      other => panic!("wrong item kind found: {other:?}"),
    },
    [] => panic!("no items found!"),
    other => panic!("too many items: {other:?}"),
  }

  let src = r#" fn main() { } "#;
  let items = parse_items_no_errors(src);
  match items.as_slice() {
    [item] => match &item._payload {
      Item::Fn(FnDecl { name, args, statements }) => {
        assert_eq!(name.as_str(), "main");
        assert!(args.is_empty());
        assert!(statements.is_empty());
      }
      other => panic!("wrong item kind found: {other:?}"),
    },
    [] => panic!("no items found!"),
    other => panic!("too many items: {other:?}"),
  }

  // adds a newline to the body
  let src = r#" fn main() {
  } "#;
  let items = parse_items_no_errors(src);
  match items.as_slice() {
    [item] => match &item._payload {
      Item::Fn(FnDecl { name, args, statements }) => {
        assert_eq!(name.as_str(), "main");
        assert!(args.is_empty());
        assert!(statements.is_empty());
      }
      other => panic!("wrong item kind found: {other:?}"),
    },
    [] => panic!("no items found!"),
    other => panic!("too many items: {other:?}"),
  }
}

#[test]
fn can_parse_return_statement() {
  let src = r#" fn main() { return } "#;
  let items = parse_items_no_errors(src);
  match items.as_slice() {
    [item] => match &item._payload {
      Item::Fn(FnDecl { name, args, statements }) => {
        assert_eq!(name.as_str(), "main");
        assert!(args.is_empty());
        match statements.as_slice() {
          [(st, _file_span)] => assert_eq!(*st, Statement::Return),
          [] => panic!("no statements!"),
          other => panic!("too many statements: {other:?}"),
        }
      }
      other => panic!("wrong item kind found: {other:?}"),
    },
    [] => panic!("no items found!"),
    other => panic!("too many items: {other:?}"),
  }

  let src = r#" fn main() {
    return
  } "#;
  let items = parse_items_no_errors(src);
  match items.as_slice() {
    [item] => match &item._payload {
      Item::Fn(FnDecl { name, args, statements }) => {
        assert_eq!(name.as_str(), "main");
        assert!(args.is_empty());
        match statements.as_slice() {
          [(st, _file_span)] => assert_eq!(*st, Statement::Return),
          [] => panic!("no statements!"),
          other => panic!("too many statements: {other:?}"),
        }
      }
      other => panic!("wrong item kind found: {other:?}"),
    },
    [] => panic!("no items found!"),
    other => panic!("too many items: {other:?}"),
  }
}

#[test]
fn can_parse_call_statement() {
  let src = r#" fn main() { foo() } "#;
  let items = parse_items_no_errors(src);
  match items.as_slice() {
    [(item, _file_span)] => match item {
      Item::Fn(FnDecl { name, args, statements }) => {
        assert_eq!(name.as_str(), "main");
        assert!(args.is_empty());
        match statements.as_slice() {
          [(st, _file_span)] => assert_eq!(
            *st,
            Statement::Call { target: StrID::from("foo"), args: Vec::new() }
          ),
          [] => panic!("no statements!"),
          other => panic!("too many statements: {other:?}"),
        }
      }
      other => panic!("wrong item kind found: {other:?}"),
    },
    [] => panic!("no items found!"),
    other => panic!("too many items: {other:?}"),
  }

  let src = r#" fn main() {
    foo()
  } "#;
  let items = parse_items_no_errors(src);
  match items.as_slice() {
    [(item, _file_span)] => match item {
      Item::Fn(FnDecl { name, args, statements }) => {
        assert_eq!(name.as_str(), "main");
        assert!(args.is_empty());
        match statements.as_slice() {
          [(st, _file_span)] => assert_eq!(
            *st,
            Statement::Call { target: StrID::from("foo"), args: Vec::new() }
          ),
          [] => panic!("no statements!"),
          other => panic!("too many statements: {other:?}"),
        }
      }
      other => panic!("wrong item kind found: {other:?}"),
    },
    [] => panic!("no items found!"),
    other => panic!("too many items: {other:?}"),
  }
}

#[test]
fn can_parse_loops() {
  let src = r#" fn main() { loop {} } "#;
  let items = parse_items_no_errors(src);
  match items.as_slice() {
    [(item, _file_span)] => match item {
      Item::Fn(FnDecl { name, args, statements }) => {
        assert_eq!(name.as_str(), "main");
        assert!(args.is_empty());
        match statements.as_slice() {
          [(st, _file_span)] => assert!(matches!(*st, Statement::Loop(_))),
          [] => panic!("no statements!"),
          other => panic!("too many statements: {other:?}"),
        }
      }
      other => panic!("wrong item kind found: {other:?}"),
    },
    [] => panic!("no items found!"),
    other => panic!("too many items: {other:?}"),
  }

  let src = r#"
    fn main() {
      loop{loop{} loop{loop{} loop{}}}
      loop { foo() }
    }
  "#;
  let items = parse_items_no_errors(src);
  match items.as_slice() {
    [(item, _)] => match item {
      Item::Fn(FnDecl { name, args, statements }) => {
        assert_eq!(name.as_str(), "main");
        assert!(args.is_empty());
        match statements.as_slice() {
          [(Statement::Loop(l0), _), (Statement::Loop(l1), _)] => {
            match l0.as_slice() {
              [(Statement::Loop(l0), _), (Statement::Loop(l1), _)] => {
                assert!(l0.is_empty());
                match l1.as_slice() {
                  [(Statement::Loop(l0), _), (Statement::Loop(l1), _)] => {
                    assert!(l0.is_empty());
                    assert!(l1.is_empty());
                  }
                  other => panic!("unknown: {other:?}"),
                };
              }
              other => panic!("unknown: {other:?}"),
            };
            match l1.as_slice() {
              [(Statement::Call { target, args }, _)] => {
                assert_eq!(*target, StrID::from("foo"));
                assert!(args.is_empty());
              }
              other => panic!("unknown: {other:?}"),
            };
          }
          other => panic!("unknown: {other:?}"),
        }
      }
      other => panic!("unknown: {other:?}"),
    },
    other => panic!("unknown: {other:?}"),
  }
}
