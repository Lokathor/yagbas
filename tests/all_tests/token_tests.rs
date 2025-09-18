
use super::*;

#[track_caller]
pub fn lex_no_errors(s: &str) {
  for (t, _span) in tokens_of(s) {
    assert!(!matches!(t, Token::TokenError));
  }
}

#[test]
fn basic() {
  lex_no_errors("");
  lex_no_errors("fn main() {}");
  lex_no_errors("@#$_&-+()/*':;!?,.~`|={}\\%[]");
  lex_no_errors("123 $FF %1111_11111");
}