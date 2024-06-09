use yagbas::{
  item::{parse_token_trees_to_items, Item},
  src_files::{FileSpan, SrcFileInfo, SrcID},
  token_tree::{parse_tokens_to_token_trees, TokenTree},
};

fn make_trees_no_errors(s: &str) -> Vec<(TokenTree, FileSpan)> {
  let file_info_id = SrcID::from(SrcFileInfo::in_memory(s));
  let tokens: Vec<_> = file_info_id.iter_tokens().collect();
  let (token_trees, tree_errors) = parse_tokens_to_token_trees(&tokens);
  assert!(tree_errors.is_empty());
  token_trees
}

fn parse_items_no_errors(s: &str) -> Vec<(Item, FileSpan)> {
  let trees = make_trees_no_errors(s);
  let (items, item_errors) = parse_token_trees_to_items(&trees);
  assert!(item_errors.is_empty(), "{item_errors:?}");
  items
}

#[test]
fn can_parse_empty_main_fn() {
  let src = r#" fn main() {} "#;
  let items = parse_items_no_errors(src);
  match items.as_slice() {
    [(item, _file_span)] => match item {
      Item::Fn(f) => {
        assert_eq!(f.name.as_str(), "main");
        assert!(f.args.is_empty());
        assert!(f.statements.is_empty());
      }
      other => panic!("wrong item kind found: {other:?}"),
    },
    [] => panic!("no items found!"),
    other => panic!("too many items: {other:?}"),
  }

  let src = r#" fn main() { } "#;
  let items = parse_items_no_errors(src);
  match items.as_slice() {
    [(item, _file_span)] => match item {
      Item::Fn(f) => {
        assert_eq!(f.name.as_str(), "main");
        assert!(f.args.is_empty());
        assert!(f.statements.is_empty());
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
    [(item, _file_span)] => match item {
      Item::Fn(f) => {
        assert_eq!(f.name.as_str(), "main");
        assert!(f.args.is_empty());
        assert!(f.statements.is_empty());
      }
      other => panic!("wrong item kind found: {other:?}"),
    },
    [] => panic!("no items found!"),
    other => panic!("too many items: {other:?}"),
  }
}
