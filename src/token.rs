use crate::Span32;
use chumsky::span::SimpleSpan;
use logos::{Lexer, Logos};

#[derive(
  Debug,
  Clone,
  Copy,
  Default,
  PartialEq,
  Eq,
  PartialOrd,
  Ord,
  Hash,
  logos::Logos,
)]
#[logos(skip r#"[[ \t\r\n]]"#)] // ignore whitespace between tokens
pub enum Token {
  /* TOKEN TREE MARKERS */
  #[regex(r"\[")]
  OpBracket,
  #[regex(r"\]")]
  ClBracket,
  #[regex(r"\{")]
  OpBrace,
  #[regex(r"\}")]
  ClBrace,
  #[regex(r"\(")]
  OpParen,
  #[regex(r"\)")]
  ClParen,
  #[regex(r"/\*")]
  OpBlockComment,
  #[regex(r"\*/")]
  ClBlockComment,

  /* TOKEN TREE LONE ITEMS */
  #[regex(r"///[^\r\n]*")]
  DocComment,
  #[regex(r"//![^\r\n]*")]
  InteriorComment,
  #[regex(r"//[^\r\n]*")]
  LineComment,

  #[regex(r"bitbag")]
  KwBitbag,
  #[regex(r"break")]
  KwBreak,
  #[regex(r"const")]
  KwConst,
  #[regex(r"continue")]
  KwContinue,
  #[regex(r"else")]
  KwElse,
  #[regex(r"fn")]
  KwFn,
  #[regex(r"if")]
  KwIf,
  #[regex(r"let")]
  KwLet,
  #[regex(r"loop")]
  KwLoop,
  #[regex(r"mmio")]
  KwMmio,
  #[regex(r"ram")]
  KwRam,
  #[regex(r"return")]
  KwReturn,
  #[regex(r"rom")]
  KwRom,
  #[regex(r"static")]
  KwStatic,
  #[regex(r"struct")]
  KwStruct,

  #[regex(r"[_a-zA-Z][_a-zA-Z0-9]*")]
  Ident,
  #[regex(r"((\$|%)[[:word:]]+|[[:digit:]][[:word:]]*)")]
  NumLit,
  #[regex(r"false")]
  KwFalse,
  #[regex(r"true")]
  KwTrue,

  #[regex(r"~")]
  Tilde,
  #[regex(r"`")]
  Backtick,
  #[regex(r"!")]
  Exclamation,
  #[regex(r"@")]
  AtSign,
  #[regex(r"#")]
  Hash,
  #[regex(r"\$")]
  Dollar,
  #[regex(r"%")]
  Percent,
  #[regex(r"\^")]
  Caret,
  #[regex(r"&")]
  Ampersand,
  #[regex(r"\*")]
  Asterisk,
  #[regex(r"-")]
  Minus,
  #[regex(r"\+")]
  Plus,
  #[regex(r"=")]
  Equal,
  #[regex(r"\|")]
  Pipe,
  #[regex(r"\\")]
  Backslash,
  #[regex(r":")]
  Colon,
  #[regex(r";")]
  Semicolon,
  #[regex(r"'")]
  Quote,
  #[regex(r"<")]
  LessThan,
  #[regex(r",")]
  Comma,
  #[regex(r">")]
  GreaterThan,
  #[regex(r"\.")]
  Period,
  #[regex(r"\?")]
  Question,
  #[regex(r"/")]
  Slash,

  /* OTHER */
  #[default]
  TokenError,
}
impl Token {
  /// Allows the [lexer][Logos::lexer] method to be called without needing to
  /// import the trait.
  #[inline(always)]
  pub fn lexer<'s>(source: &'s str) -> Lexer<'s, Self> {
    <Self as Logos>::lexer(source)
  }
}

pub fn tokens_of(source: &str) -> Vec<(Token, Span32)> {
  Token::lexer(source)
    .spanned()
    .map(|(token_result, range)| {
      (
        token_result.unwrap_or(Token::TokenError),
        SimpleSpan {
          start: range.start.try_into().unwrap(),
          end: range.end.try_into().unwrap(),
          context: (),
        },
      )
    })
    .collect()
}

/// it's easier to unit test without spans, so we write unit tests with this.
#[allow(unused)]
fn lex(source: &str) -> Vec<Token> {
  Token::lexer(source)
    .map(|token_result| token_result.unwrap_or(Token::TokenError))
    .collect()
}

#[test]
fn test_token_size() {
  // note(lokathor): any change in size might be justified (and so we would update this test), but we should still take note of it happening.
  assert_eq!(core::mem::size_of::<Token>(), 1);
}

#[test]
fn test_lexing() {
  use Token::*;
  assert_eq!(lex(""), &[]);
  assert_eq!(lex("/*"), &[OpBlockComment]);
  assert_eq!(lex("/ *"), &[Slash, Asterisk]);
  assert_eq!(lex("*/"), &[ClBlockComment]);
  assert_eq!(lex("* /"), &[Asterisk, Slash]);
  assert_eq!(lex("///"), &[DocComment]);
  assert_eq!(lex("//"), &[LineComment]);
  assert_eq!(lex("///*"), &[DocComment]);
  assert_eq!(lex("// /*"), &[LineComment]);
  assert_eq!(lex("// */"), &[LineComment]);
  assert_eq!(lex("if"), &[KwIf]);
  assert_eq!(lex("IF"), &[Ident]);
  assert_eq!(lex("_"), &[Ident]);
  assert_eq!(lex("_0"), &[Ident]);
  assert_eq!(lex("0_"), &[NumLit]);
  assert_eq!(lex("$"), &[Dollar]);
  assert_eq!(lex("$F"), &[NumLit]);
  assert_eq!(lex("$ F"), &[Dollar, Ident]);
  assert_eq!(lex("%"), &[Percent]);
  assert_eq!(lex("%F"), &[NumLit]);
  assert_eq!(lex("% F"), &[Percent, Ident]);
  assert_eq!(lex("."), &[Period]);
}

#[derive(Debug)]
pub struct SourcedTokens<'src>(pub &'src str, pub Span32, pub Token);

impl<'src> core::fmt::Display for SourcedTokens<'src> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self.2 {
      Token::Ident => {
        let range = (self.1.start as usize)..(self.1.end as usize);
        write!(f, "Identifier \"{}\"", &self.0[range])
      }
      Token::NumLit => {
        let range = (self.1.start as usize)..(self.1.end as usize);
        write!(f, "Number \"{}\"", &self.0[range])
      }
      Token::OpBracket => write!(f, "["),
      Token::ClBracket => write!(f, "]"),
      Token::OpBrace => write!(f, "{{"),
      Token::ClBrace => write!(f, "}}"),
      Token::OpParen => write!(f, "("),
      Token::ClParen => write!(f, ")"),
      Token::OpBlockComment => write!(f, "/*"),
      Token::ClBlockComment => write!(f, "*/"),
      Token::DocComment => write!(f, "///"),
      Token::InteriorComment => write!(f, "//!"),
      Token::LineComment => write!(f, "//"),
      Token::KwBitbag => write!(f, "`bitbag`"),
      Token::KwBreak => write!(f, "`break`"),
      Token::KwConst => write!(f, "`const`"),
      Token::KwContinue => write!(f, "`continue`"),
      Token::KwElse => write!(f, "`else`"),
      Token::KwFn => write!(f, "`fn`"),
      Token::KwIf => write!(f, "`if`"),
      Token::KwLet => write!(f, "`let`"),
      Token::KwLoop => write!(f, "`loop`"),
      Token::KwMmio => write!(f, "`mmio`"),
      Token::KwRam => write!(f, "`ram`"),
      Token::KwReturn => write!(f, "`return`"),
      Token::KwRom => write!(f, "`rom`"),
      Token::KwStatic => write!(f, "`static`"),
      Token::KwStruct => write!(f, "`struct`"),
      Token::KwFalse => write!(f, "`false`"),
      Token::KwTrue => write!(f, "`true`"),
      Token::Tilde => write!(f, "`~`"),
      Token::Backtick => write!(f, "backtick"),
      Token::Exclamation => write!(f, "!"),
      Token::AtSign => write!(f, "@"),
      Token::Hash => write!(f, "#"),
      Token::Dollar => write!(f, "$"),
      Token::Percent => write!(f, "%"),
      Token::Caret => write!(f, "^"),
      Token::Ampersand => write!(f, "&"),
      Token::Asterisk => write!(f, "*"),
      Token::Minus => write!(f, "-"),
      Token::Plus => write!(f, "+"),
      Token::Equal => write!(f, "="),
      Token::Pipe => write!(f, "|"),
      Token::Backslash => write!(f, "\\"),
      Token::Colon => write!(f, ":"),
      Token::Semicolon => write!(f, ";"),
      Token::Quote => write!(f, "'"),
      Token::LessThan => write!(f, "<"),
      Token::Comma => write!(f, ","),
      Token::GreaterThan => write!(f, ">"),
      Token::Period => write!(f, "."),
      Token::Question => write!(f, "?"),
      Token::Slash => write!(f, "/"),
      Token::TokenError => write!(f, "TokenError"),
    }
  }
}
