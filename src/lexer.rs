use logos::Logos;

use crate::str_id::StrID;

#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[logos(skip r#"[[:space:]]"#)] // ignore this between tokens
pub enum Token {
  /// A standard identifier in C-style langs: `[_a-zA-Z][_a-zA-Z0-9]*`
  #[regex(r"[_a-zA-Z][_a-zA-Z0-9]*", |lex| StrID::from(lex.slice()), priority=2)]
  Ident(StrID),

  /// A punctuation character.
  #[regex(r"[[:punct:]]", |lex| lex.slice().chars().next().unwrap(), priority=1)]
  Punct(char),

  /// Something that's supposed to be a number.
  ///
  /// * `$` or `%` followed by 1 or more word characters
  /// * A digit followed by 0 or more word characters
  ///
  /// Interpreting the token is left for the parser.
  #[regex(r"((\$|%)[[:word:]]+|[[:digit:]][[:word:]]*)", |lex| StrID::from(lex.slice()))]
  NumLit(StrID),
}
impl Token {
  #[inline]
  pub fn ident(s: &str) -> Self {
    Self::Ident(StrID::from(s))
  }
  #[inline]
  pub fn punct(p: char) -> Self {
    Self::Punct(p)
  }
  #[inline]
  pub fn num_lit(s: &str) -> Self {
    Self::NumLit(StrID::from(s))
  }
}
