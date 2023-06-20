use crate::{static_str, StaticStr};
use logos::Logos;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Logos)]
#[logos(skip r#"[ \t\r\n\f]+"#, error = StaticStr)] // ignore this between tokens
pub enum Token {
  /// `//` starts a single-line comment.
  #[regex(r"//[^\r\n]*")]
  CommentSingle,

  /// `/*`, the start of a multi-line comment
  #[token(r"/*", priority = 2)]
  CommentMultiStart,

  /// `*/`, the end of a multi-line comment
  #[token(r"*/", priority = 2)]
  CommentMultiEnd,

  /// A standard identifier in C-style langs: `[_a-zA-Z][_a-zA-Z0-9]*`
  ///
  /// The lone character `_` ends up matching as an Ident rather than a
  /// Punctuation.
  #[regex(r"[_a-zA-Z][_a-zA-Z0-9]*", |lex| static_str(lex.slice()), priority=2)]
  Ident(StaticStr),

  /// A punctuation character (using the `[:punct:]` regex class) that does
  /// *not* match any other case.
  #[regex(r"[[:punct:]]", |lex| lex.slice().chars().next().unwrap())]
  Punct(char),

  /// Something that's supposed to be a number.
  ///
  /// Interpreting the number is left for the parsing stage.
  #[regex(r"((0x|\$)[a-fA-F0-9]|(0b|%)[01]|[0-9])[a-fA-F0-9_]*", |lex| static_str(lex.slice()))]
  Num(StaticStr),

  /// Holds all the stuff *between* two `"`.
  ///
  /// This allows `\"` and `\\` within the string literal that's collected, but
  /// doesn't actually handle escape sequence processing.
  ///
  /// Thanks to `Quirl`, who made this regex: "works by specifying all of the
  /// escape sequences you allow (here just `\"`, and `\\` for a `\` itself) up
  /// front and then requiring the rest of the literal to not be a quote or
  /// escape (that would start with `\`)"
  #[regex(r#""((\\"|\\\\)|[^\\"])*""#, |lex| {let s = lex.slice(); static_str(&s[1..s.len()-1]) })]
  Str(StaticStr),

  /// Keyword: `section`
  #[token("section")]
  KwSection,
  /// Keyword: `const`
  #[token("const")]
  KwConst,
  /// Keyword: `static`
  #[token("static")]
  KwStatic,
}
impl Token {
  #[inline]
  #[must_use]
  pub fn lexer(s: &str) -> logos::Lexer<'_, Token> {
    <Self as Logos>::lexer(s)
  }
  #[inline]
  pub fn is_ident(&self) -> bool {
    matches!(self, Self::Ident(_))
  }
  #[inline]
  pub fn unwrap_ident(self) -> StaticStr {
    match self {
      Self::Ident(i) => i,
      _ => panic!("unwrap_ident on {self:?}"),
    }
  }
  #[inline]
  pub fn is_num(&self) -> bool {
    matches!(self, Self::Num(_))
  }
  #[inline]
  pub fn unwrap_num(self) -> StaticStr {
    match self {
      Self::Num(n) => n,
      _ => panic!("unwrap_num on {self:?}"),
    }
  }
}
impl core::fmt::Debug for Token {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match *self {
      Token::CommentSingle => write!(f, "//"),
      Token::CommentMultiStart => write!(f, "/*"),
      Token::CommentMultiEnd => write!(f, "*/"),
      Token::Ident(i) => write!(f, "{i}"),
      Token::Punct(p) => write!(f, "{p}"),
      Token::Num(n) => write!(f, "{n}"),
      Token::Str(s) => write!(f, "{s:?}"),
      Token::KwSection => write!(f, "section"),
      Token::KwConst => write!(f, "const"),
      Token::KwStatic => write!(f, "static"),
    }
  }
}
