use yagbas::tokenizer::Token;
use yagbas::tokenizer::TokenKind;
use yagbas::tokenizer::TokenKind::*;
use yagbas::tokenizer::tokenize;

/// This makes the tests easy to read
#[track_caller]
fn expect_token(s: &str, k: TokenKind) {
  let v: Vec<Token> = tokenize(s).collect();
  assert_eq!(v.len(), 1, "Bad Token Total: {v:?}");
  let t: Token = v[0];
  assert_eq!(t.kind, k, "Bad Kind: {t:?}");
  assert_eq!(t.span.as_range(), 0..(s.len()), "Bad Span: {t:?}");
}

#[test]
fn test_size_and_align_of_types() {
  assert_eq!(size_of::<TokenKind>(), 1);
}

#[test]
fn test_keywords() {
  expect_token("as", KwAs);
  expect_token("bitbag", KwBitbag);
  expect_token("break", KwBreak);
  expect_token("const", KwConst);
  expect_token("continue", KwContinue);
  expect_token("else", KwElse);
  expect_token("enum", KwEnum);
  expect_token("false", KwFalse);
  expect_token("fn", KwFn);
  expect_token("for", KwFor);
  expect_token("if", KwIf);
  expect_token("impl", KwImpl);
  expect_token("in", KwIn);
  expect_token("let", KwLet);
  expect_token("loop", KwLoop);
  expect_token("match", KwMatch);
  expect_token("mmio", KwMmio);
  expect_token("mod", KwMod);
  expect_token("mut", KwMut);
  expect_token("ram", KwRam);
  expect_token("return", KwReturn);
  expect_token("rom", KwRom);
  expect_token("struct", KwStruct);
  expect_token("static", KwStatic);
  expect_token("super", KwSuper);
  expect_token("true", KwTrue);
  expect_token("use", KwUse);
  expect_token("while", KwWhile);
  expect_token("vol", KwVol);
}

#[test]
fn test_combined_punctuation() {
  expect_token("::", ColonColon);
  expect_token("==", EqualEqual);
  expect_token("!=", BangEqual);
  expect_token("..", DotDot);
  expect_token("..=", DotDotEqual);
  expect_token("+=", PlusEqual);
  expect_token("-=", MinusEqual);
  expect_token("*=", StarEqual);
  expect_token("/=", SlashEqual);
  expect_token("%=", PercentEqual);
  expect_token("&=", AmpersandEqual);
  expect_token("|=", PipeEqual);
  expect_token("^=", CaretEqual);
  expect_token("->", MinusGreater);
}

#[test]
fn test_lone_punctuation() {
  expect_token("!", Bang);
  expect_token("#", Hash);
  expect_token("$", Dollar);
  expect_token("%", Percent);
  expect_token("&", Ampersand);
  expect_token("'", Quote);
  expect_token("(", OpParen);
  expect_token(")", ClParen);
  expect_token("*", Star);
  expect_token("+", Plus);
  expect_token(",", Comma);
  expect_token("-", Minus);
  expect_token(".", Dot);
  expect_token("/", Slash);
  expect_token(":", Colon);
  expect_token(";", Semicolon);
  expect_token("<", LessThan);
  expect_token("=", Equal);
  expect_token(">", GreaterThan);
  expect_token("?", Question);
  expect_token("@", At);
  expect_token("[", OpBracket);
  expect_token("\\", Backslash);
  expect_token("]", ClBracket);
  expect_token("^", Caret);
  expect_token("`", Backtick);
  expect_token("{", OpBrace);
  expect_token("|", Pipe);
  expect_token("}", ClBrace);
  expect_token("~", Tilde);
}

#[test]
fn test_comments() {
  expect_token("/**/", Comment);
  // they can be nested
  expect_token("/*/**/*/", Comment);
  expect_token("/*", ErrBlockCommentUnclosed);
  expect_token("*/", ErrBlockCommentExtraClose);
  expect_token("//", Comment);
  // line comment going to the end of the line "covers up" the block comment
  // opener or closer.
  expect_token("// /*", Comment);
  expect_token("// */", Comment);
}

#[test]
fn test_lit_str() {
  expect_token("\"\"", LitStr);
  expect_token("\"a\\b\\\"c\"", LitStr);
}

#[test]
fn test_raw_values() {
  // with no # after, we get an ident
  expect_token("r", Ident);
  // with a hash after, we have to have a raw value
  expect_token("r#", ErrBadRawValue);
  expect_token(r#####"r#""#"#####, LitStr);
  expect_token(r#####"r##""##"#####, LitStr);
  expect_token(r#####"r###""###"#####, LitStr);
  expect_token(r##"""##, ErrLitStrUnclosed);
  expect_token(r##""\""##, ErrLitStrUnclosed);
}

#[test]
fn test_ident() {
  expect_token("_", Ident);
  expect_token("foo", Ident);
}

#[test]
fn test_tokenize_lit_num() {
  expect_token("1", LitNum);
  expect_token("1_u8", LitNum);
  expect_token("$1", LitNum);
  expect_token("%1", LitNum);
}
