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

  #[regex(r"bitstruct")]
  KwBitstruct,
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
  #[regex(r"mut")]
  KwMut,
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
