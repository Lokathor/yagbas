use yagbas::tokenizer::Token;
use yagbas::tokenizer::TokenKind;
use yagbas::tokenizer::TokenKind::*;
use yagbas::tokenizer::tokenize;

#[test]
fn test_comment_block_plain() {
  let v = tokenize("/**/").collect::<Vec<_>>();
  assert_eq!(v.len(), 1);
  let t = v[0];
  assert_eq!(t.kind, Comment, "Bad Kind: `{t:?}`");
}
#[test]
fn test_comment_block_nested() {
  let v = tokenize("/*/**/*/").collect::<Vec<_>>();
  assert_eq!(v.len(), 1);
  let t = v[0];
  assert_eq!(t.kind, Comment, "Bad Kind: `{t:?}`");
}
#[test]
fn test_comment_block_open_only() {
  let v = tokenize("/*").collect::<Vec<_>>();
  assert_eq!(v.len(), 1);
  let t = v[0];
  assert_eq!(t.kind, ErrBlockCommentUnclosed, "Bad Kind: `{t:?}`");
}
#[test]
fn test_comment_block_close_only() {
  let v = tokenize("*/").collect::<Vec<_>>();
  assert_eq!(v.len(), 1);
  let t = v[0];
  assert_eq!(t.kind, ErrBlockCommentExtraClose, "Bad Kind: `{t:?}`");
}

#[test]
fn test_comment_line() {
  let mut v: Vec<Token>;

  v = tokenize("//").collect();
  assert_eq!(v.len(), 1);
  let t = v[0];
  assert_eq!(t.kind, Comment, "Bad Kind: `{t:?}`");

  v = tokenize("// big comment line").collect();
  assert_eq!(v.len(), 1);
  let t = v[0];
  assert_eq!(t.kind, Comment, "Bad Kind: `{t:?}`");

  v = tokenize("// */").collect();
  assert_eq!(v.len(), 1);
  let t = v[0];
  assert_eq!(t.kind, Comment, "Bad Kind: `{t:?}`");

  v = tokenize(
    "// big comment line
  !",
  )
  .collect();
  assert_eq!(v.len(), 3); // comment whitespace bang
  let t = v[0];
  assert_eq!(t.kind, Comment, "Bad Kind: `{t:?}`");
}

#[test]
fn test_tokenize_lit_str() {
  let mut v: Vec<Token>;

  v = tokenize(r##""""##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, LitStr, "Bad Kind: `{t:?}`");

  v = tokenize(r##""abc""##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, LitStr, "Bad Kind: `{t:?}`");

  v = tokenize(r##""a\"bc""##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, LitStr, "Bad Kind: `{t:?}`");

  v = tokenize(r##""a\\bc""##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, LitStr, "Bad Kind: `{t:?}`");
}

#[test]
fn test_tokenize_lit_raw_str() {
  let mut v: Vec<Token>;

  v = tokenize(r##"r"""##).collect();
  assert_eq!(v.len(), 2, "Bad Output Len: {v:?}");

  v = tokenize(r##"r#"##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, ErrBadRawValue, "Bad Kind: `{t:?}`");

  v = tokenize(r##"r#""#"##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, LitStr, "Bad Kind: `{t:?}`");

  v = tokenize(r#######"r###""#"#######).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, ErrLitRawStrUnclosed, "Bad Kind: `{t:?}`");

  v = tokenize(r#######"r###""###"#######).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, LitStr, "Bad Kind: `{t:?}`");

  v = tokenize(r#######"r###"abc""###"#######).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, LitStr, "Bad Kind: `{t:?}`");
}

#[test]
fn test_tokenize_lit_str_no_close() {
  let mut v: Vec<Token>;

  v = tokenize(r##"""##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, ErrLitStrUnclosed, "Bad Kind: `{t:?}`");

  v = tokenize(r##"" \""##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, ErrLitStrUnclosed, "Bad Kind: `{t:?}`");
}

#[test]
fn test_tokenize_lit_num() {
  let mut v: Vec<Token>;

  v = tokenize(r##"1"##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, LitNum, "Bad Kind: `{t:?}`");

  v = tokenize(r##"$"##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, Dollar, "Bad Kind: `{t:?}`");

  v = tokenize(r##"%"##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, Percent, "Bad Kind: `{t:?}`");

  v = tokenize(r##"$F"##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, LitNum, "Bad Kind: `{t:?}`");

  v = tokenize(r##"%1"##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, LitNum, "Bad Kind: `{t:?}`");

  v = tokenize(r##"1_u8"##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, LitNum, "Bad Kind: `{t:?}`");
}

#[test]
fn test_tokenize_keyword_and_ident() {
  let mut v: Vec<Token>;

  v = tokenize(r##"fn"##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, KwFn, "Bad Kind: `{t:?}`");

  v = tokenize(r##"static"##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, KwStatic, "Bad Kind: `{t:?}`");

  v = tokenize(r##"foo"##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, Ident, "Bad Kind: `{t:?}`");

  v = tokenize(r##"foo_"##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, Ident, "Bad Kind: `{t:?}`");

  v = tokenize(r##"regal"##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, Ident, "Bad Kind: `{t:?}`");
}

#[test]
fn test_merged_punctuation() {
  let mut v: Vec<Token>;

  v = tokenize("::").collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, ColonColon, "Bad Kind: `{t:?}`");

  v = tokenize("..=").collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, DotDotEqual, "Bad Kind: `{t:?}`");
}
