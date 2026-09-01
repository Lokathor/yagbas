use yagbas::tokenizer::Token;
use yagbas::tokenizer::TokenKind;
use yagbas::tokenizer::TokenKind::*;
use yagbas::tokenizer::tokenize;

/// This makes the tests easy to read
#[track_caller]
fn expect_token(s: &str, k: TokenKind) {
  let v: Vec<Token> = tokenize(s).collect();
  assert_eq!(v.len(), 1);
  let t: Token = v[0];
  assert_eq!(t.kind, k, "Bad Kind: {t:?}");
  let span = t.span_within(s);
  assert_eq!(span, 0..(s.len()), "Bad Span: {span:?}");
}

#[test]
fn test_comment_block_plain() {
  expect_token("/**/", Comment);
}

#[test]
fn test_comment_block_nested() {
  expect_token("/*/**/*/", Comment);
}

#[test]
fn test_comment_block_open_only() {
  expect_token("/*", ErrBlockCommentUnclosed);
}

#[test]
fn test_comment_block_close_only() {
  expect_token("*/", ErrBlockCommentExtraClose);
}

#[test]
fn test_comment_line() {
  expect_token("//", Comment);
}

#[test]
fn test_comment_line_overrides_end_block() {
  expect_token("// */", Comment);
}

#[test]
fn test_lit_str_empty() {
  expect_token("\"\"", LitStr);
}

#[test]
fn test_lit_str_basic() {
  expect_token("\"a\\b\\\"c\"", LitStr);
}

#[test]
fn test_raw_mark_alone() {
  expect_token("r#", ErrBadRawValue);
}

#[test]
fn test_lone_r_is_ident_not_raw_mark() {
  expect_token("r", Ident);
}

#[test]
fn test_raw_str_empty() {
  expect_token(r#####"r#""#"#####, LitStr);
  expect_token(r#####"r##""##"#####, LitStr);
  expect_token(r#####"r###""###"#####, LitStr);
}

#[test]
fn test_tokenize_lit_str_no_close() {
  expect_token(r##"""##, ErrLitStrUnclosed);
  expect_token(r##""\""##, ErrLitStrUnclosed);
}

#[test]
fn test_dollar() {
  expect_token("$", Dollar);
}

#[test]
fn test_percent() {
  expect_token("%", Percent);
}

#[test]
fn test_tokenize_lit_num() {
  expect_token("1", LitNum);
  expect_token("1_u8", LitNum);
  expect_token("$1", LitNum);
  expect_token("%1", LitNum);
}

#[test]
fn test_keyword_fn() {
  expect_token("fn", KwFn);
}

#[test]
fn test_keyword_static() {
  expect_token("static", KwStatic);
}

#[test]
fn test_keyword_mmio() {
  expect_token("mmio", KwMmio);
}

#[test]
fn test_ident() {
  expect_token("_", Ident);
  expect_token("foo", Ident);
}

#[test]
fn test_colon_colon() {
  expect_token("::", ColonColon);
}

#[test]
fn test_dot_dot_eq() {
  expect_token("..=", DotDotEqual);
}

#[test]
fn test_eq_eq() {
  expect_token("==", EqualEqual);
}
