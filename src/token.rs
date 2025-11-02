use chumsky::span::SimpleSpan;
use logos::{Lexer, Logos};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, logos::Logos)]
#[logos(skip r#"[[ \t\r\n]]"#)] // ignore space and tab between tokens
pub enum Token {
  /* TOKEN TREE MARKERS */
  #[regex(r"\[", priority = 5)]
  OpBracket,
  #[regex(r"\]", priority = 5)]
  ClBracket,
  #[regex(r"\{", priority = 5)]
  OpBrace,
  #[regex(r"\}", priority = 5)]
  ClBrace,
  #[regex(r"\(", priority = 5)]
  OpParen,
  #[regex(r"\)", priority = 5)]
  ClParen,
  #[token(r"/*", priority = 5)]
  OpBlockComment,
  #[token(r"*/", priority = 5)]
  ClBlockComment,

  #[token(r"///[^\r\n]*", priority = 5)]
  DocComment,
  #[regex(r"//[^\r\n]*", priority = 4)]
  LineComment,

  /* KEYWORDS */
  #[regex(r"bitstruct", priority = 4)]
  KwBitStruct,
  #[regex(r"break", priority = 4)]
  KwBreak,
  #[regex(r"const", priority = 4)]
  KwConst,
  #[regex(r"continue", priority = 4)]
  KwContinue,
  #[regex(r"else", priority = 4)]
  KwElse,
  #[regex(r"false", priority = 4)]
  KwFalse,
  #[regex(r"fn", priority = 4)]
  KwFn,
  #[regex(r"if", priority = 4)]
  KwIf,
  #[regex(r"let", priority = 4)]
  KwLet,
  #[regex(r"loop", priority = 4)]
  KwLoop,
  #[regex(r"mmio", priority = 4)]
  KwMmio,
  #[regex(r"mut", priority = 4)]
  KwMut,
  #[regex(r"return", priority = 4)]
  KwReturn,
  #[regex(r"static", priority = 4)]
  KwStatic,
  #[regex(r"struct", priority = 4)]
  KwStruct,
  #[regex(r"true", priority = 4)]
  KwTrue,

  /* IDENTS, NUMBERS, AND LINE COMMENTS */
  #[regex(r"[_a-zA-Z][_a-zA-Z0-9]*", priority = 3)]
  Ident,
  #[regex(r"((\$|%)[[:word:]]+|[[:digit:]][[:word:]]*)", priority = 3)]
  NumLit,

  /* PUNCTUATION (ordered approximately by US QWERTY layout) */
  #[regex(r"~", priority = 2)]
  Tilde,
  #[regex(r"`", priority = 2)]
  Backtick,
  #[regex(r"!", priority = 2)]
  Exclamation,
  #[regex(r"@", priority = 2)]
  AtSign,
  #[regex(r"#", priority = 2)]
  Hash,
  #[regex(r"\$", priority = 2)]
  Dollar,
  #[regex(r"%", priority = 2)]
  Percent,
  #[regex(r"\^", priority = 2)]
  Caret,
  #[regex(r"&", priority = 2)]
  Ampersand,
  #[regex(r"\*", priority = 2)]
  Asterisk,
  #[regex(r"-", priority = 2)]
  Minus,
  #[regex(r"\+", priority = 2)]
  Plus,
  #[regex(r"=", priority = 2)]
  Equal,
  #[regex(r"\|", priority = 2)]
  Pipe,
  #[regex(r"\\", priority = 2)]
  Backslash,
  #[regex(r":", priority = 2)]
  Colon,
  #[regex(r";", priority = 2)]
  Semicolon,
  #[regex(r"'", priority = 2)]
  Quote,
  #[regex(r"<", priority = 2)]
  LessThan,
  #[regex(r",", priority = 2)]
  Comma,
  #[regex(r">", priority = 2)]
  GreaterThan,
  #[regex(r"\.", priority = 2)]
  Period,
  #[regex(r"\?", priority = 2)]
  Question,
  #[regex(r"/", priority = 2)]
  Slash,

  /* OTHER */
  TokenError,
}
impl Token {
  /// Allows the [lexer][Logos::lexer] method to be called without needing to
  /// import the trait.
  #[inline(always)]
  pub fn lexer<'source>(
    source: &'source <Self as Logos>::Source,
  ) -> Lexer<'source, Self>
  where
    <Self as Logos<'source>>::Extras: Default,
  {
    <Self as Logos>::lexer(source)
  }
  /// Allows the [lexer_with_extras][Logos::lexer_with_extras] method to be
  /// called without needing to import the trait.
  #[inline(always)]
  pub fn lexer_with_extras<'source>(
    source: &'source <Self as Logos>::Source, extras: <Self as Logos>::Extras,
  ) -> Lexer<'source, Self> {
    <Self as Logos>::lexer_with_extras(source, extras)
  }
}

#[test]
fn test_token_size() {
  assert_eq!(core::mem::size_of::<Token>(), 1);
}

pub fn tokens_of(source: &str) -> Vec<(Token, SimpleSpan)> {
  Token::lexer(source)
    .spanned()
    .map(|(token, range)| {
      (
        token.unwrap_or(Token::TokenError),
        SimpleSpan { start: range.start, end: range.end, context: () },
      )
    })
    .collect()
}
