use chumsky::span::SimpleSpan;
use logos::{Lexer, Logos};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, logos::Logos)]
#[logos(skip r#"[[ \t]]"#)] // ignore space and tab between tokens
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
  /// `/*`, the start of a multi-line comment
  #[token(r"/*", priority = 5)]
  OpCommentBlock,
  /// `*/`, the end of a multi-line comment
  #[token(r"*/", priority = 5)]
  ClCommentBlock,

  /* KEYWORDS */
  #[regex(r"break", priority = 4)]
  KwBreak,
  #[regex(r"const", priority = 4)]
  KwConst,
  #[regex(r"continue", priority = 4)]
  KwContinue,
  #[regex(r"false", priority = 4)]
  KwFalse,
  #[regex(r"fn", priority = 4)]
  KwFn,
  #[regex(r"if", priority = 4)]
  KwIf,
  #[regex(r"else", priority = 4)]
  KwElse,
  #[regex(r"loop", priority = 4)]
  KwLoop,
  #[regex(r"return", priority = 4)]
  KwReturn,
  #[regex(r"static", priority = 4)]
  KwStatic,
  #[regex(r"true", priority = 4)]
  KwTrue,
  #[regex(r"mut", priority = 4)]
  KwMut,
  #[token("a", priority = 4)]
  #[token("A", priority = 4)]
  KwA,
  #[token("f", priority = 4)]
  #[token("F", priority = 4)]
  KwF,
  #[token("b", priority = 4)]
  #[token("B", priority = 4)]
  KwB,
  #[token("c", priority = 4)]
  #[token("C", priority = 4)]
  KwC,
  #[token("d", priority = 4)]
  #[token("D", priority = 4)]
  KwD,
  #[token("e", priority = 4)]
  #[token("E", priority = 4)]
  KwE,
  #[token("h", priority = 4)]
  #[token("H", priority = 4)]
  KwH,
  #[token("l", priority = 4)]
  #[token("L", priority = 4)]
  KwL,
  #[token("af", priority = 4)]
  #[token("AF", priority = 4)]
  KwAF,
  #[token("bc", priority = 4)]
  #[token("BC", priority = 4)]
  KwBC,
  #[token("de", priority = 4)]
  #[token("DE", priority = 4)]
  KwDE,
  #[token("hl", priority = 4)]
  #[token("HL", priority = 4)]
  KwHL,
  #[token("z", priority = 4)]
  #[token("Z", priority = 4)]
  KwZ,
  #[token("nz", priority = 4)]
  #[token("NZ", priority = 4)]
  KwNZ,
  #[token("nc", priority = 4)]
  #[token("NC", priority = 4)]
  KwNC,

  /* IDENTS AND NUMBERS */
  #[regex(r"[_a-zA-Z][_a-zA-Z0-9]*", priority = 3)]
  Ident,
  #[regex(r"((\$|%)[[:word:]]+|[[:digit:]][[:word:]]*)", priority = 3)]
  NumLit,

  /* PUNCTUATION */
  #[regex(r"//[^\r\n]*", priority = 2)]
  SingleComment,
  #[regex(r"\r\n|\r|\n", priority = 2)]
  Newline,
  #[regex(r"\^", priority = 2)]
  Caret,
  #[regex(r"\.", priority = 2)]
  Period,
  #[regex(r":", priority = 2)]
  Colon,
  #[regex(r",", priority = 2)]
  Comma,
  #[regex(r"=", priority = 2)]
  Equal,
  #[regex(r"!", priority = 2)]
  Exclamation,
  #[regex(r"-", priority = 2)]
  Minus,
  #[regex(r"\|", priority = 2)]
  Pipe,
  #[regex(r"\+", priority = 2)]
  Plus,
  #[regex(r"'", priority = 2)]
  Quote,
  #[regex(r";", priority = 2)]
  Semicolon,
  #[regex(r"\*", priority = 2)]
  Asterisk,
  #[regex(r"/", priority = 2)]
  Slash,
  #[regex(r"%", priority = 2)]
  Percent,
  #[regex(r">", priority = 2)]
  GreaterThan,
  #[regex(r"<", priority = 2)]
  LessThan,
  #[regex(r"&", priority = 2)]
  Ampersand,

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
