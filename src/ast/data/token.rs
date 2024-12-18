use logos::{Lexer, Logos};

use crate::str_id::StrID;

/// The fundamental tokens of Yagbas source code.
///
/// Call [`Token::lexer`] to turn a source [`&str`] into a [`Lexer`], which can
/// be used as an iterator that will correctly produce all the `Token` values.
#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[logos(skip r#"[[ \t]]"#)] // ignore space and tab between tokens
pub enum Token {
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

  #[regex(r"\[", priority = 3)]
  OpBracket,
  #[regex(r"\]", priority = 3)]
  ClBracket,
  #[regex(r"\{", priority = 3)]
  OpBrace,
  #[regex(r"\}", priority = 3)]
  ClBrace,
  #[regex(r"\(", priority = 3)]
  OpParen,
  #[regex(r"\)", priority = 3)]
  ClParen,
  #[token("a", priority = 3)]
  #[token("A", priority = 3)]
  KwA,
  #[token("f", priority = 3)]
  #[token("F", priority = 3)]
  KwF,
  #[token("b", priority = 3)]
  #[token("B", priority = 3)]
  KwB,
  #[token("c", priority = 3)]
  #[token("C", priority = 3)]
  KwC,
  #[token("d", priority = 3)]
  #[token("D", priority = 3)]
  KwD,
  #[token("e", priority = 3)]
  #[token("E", priority = 3)]
  KwE,
  #[token("h", priority = 3)]
  #[token("H", priority = 3)]
  KwH,
  #[token("l", priority = 3)]
  #[token("L", priority = 3)]
  KwL,
  #[token("af", priority = 3)]
  #[token("AF", priority = 3)]
  KwAF,
  #[token("bc", priority = 3)]
  #[token("BC", priority = 3)]
  KwBC,
  #[token("de", priority = 3)]
  #[token("DE", priority = 3)]
  KwDE,
  #[token("hl", priority = 3)]
  #[token("HL", priority = 3)]
  KwHL,
  // Note(Lokathor): We reuse `KwC` as the "carry" token, the context of
  // checking either a register or a condition will always be clear.
  #[token("z", priority = 3)]
  #[token("Z", priority = 3)]
  KwZ,
  #[token("nz", priority = 3)]
  #[token("NZ", priority = 3)]
  KwNZ,
  #[token("nc", priority = 3)]
  #[token("NC", priority = 3)]
  KwNC,
  /// Something that's supposed to be a number.
  ///
  /// * `$` or `%` followed by 1 or more word characters (including digits)
  /// * A digit followed by 0 or more word characters
  ///
  /// Interpreting the token is left for the parser.
  #[regex(r"((\$|%)[[:word:]]+|[[:digit:]][[:word:]]*)", |lex| StrID::from(lex.slice()), priority=3)]
  NumLit(StrID),

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
  #[regex(r"\r\n|\r|\n", priority = 2)]
  Newline,
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
  /// `//` starts a single-line comment, which goes to the end of the line.
  #[regex(r"//[^\r\n]*", priority = 2)]
  CommentSingle,
  /// `/*`, the start of a multi-line comment
  #[token(r"/*", priority = 2)]
  CommentBlockStart,
  /// `*/`, the end of a multi-line comment
  #[token(r"*/", priority = 2)]
  CommentBlockEnd,
  /// A C-like alphanumeric string *other than* a keyword.
  ///
  /// ```text
  /// [_a-zA-Z][_a-zA-Z0-9]*
  /// ```
  ///
  /// Basically, an Ident is something you could use as the name of a function.
  #[regex(r"[_a-zA-Z][_a-zA-Z0-9]*", |lex| StrID::from(lex.slice()), priority=2)]
  Ident(StrID),

  /// An error of some sort during tokenization.
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

  #[inline]
  #[must_use]
  pub fn is_reg8(self) -> bool {
    use Token::*;
    matches!(self, KwA | KwB | KwC | KwD | KwE | KwH | KwL)
  }
}

impl core::fmt::Display for Token {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Token::KwBreak => write!(f, "break"),
      Token::KwConst => write!(f, "const"),
      Token::KwContinue => write!(f, "continue"),
      Token::KwFn => write!(f, "fn"),
      Token::KwIf => write!(f, "if"),
      Token::KwElse => write!(f, "else"),
      Token::KwLoop => write!(f, "loop"),
      Token::KwReturn => write!(f, "return"),
      Token::KwStatic => write!(f, "static"),
      Token::KwMut => write!(f, "mut"),
      Token::OpBracket => write!(f, "{{"),
      Token::ClBracket => write!(f, "}}"),
      Token::OpBrace => write!(f, "["),
      Token::ClBrace => write!(f, "]"),
      Token::OpParen => write!(f, "("),
      Token::ClParen => write!(f, ")"),
      Token::KwA => write!(f, "a"),
      Token::KwF => write!(f, "f"),
      Token::KwB => write!(f, "b"),
      Token::KwC => write!(f, "c"),
      Token::KwD => write!(f, "d"),
      Token::KwE => write!(f, "e"),
      Token::KwH => write!(f, "h"),
      Token::KwL => write!(f, "l"),
      Token::KwAF => write!(f, "af"),
      Token::KwBC => write!(f, "bc"),
      Token::KwDE => write!(f, "de"),
      Token::KwHL => write!(f, "hl"),
      Token::KwZ => write!(f, "z"),
      Token::KwNZ => write!(f, "nz"),
      Token::KwNC => write!(f, "nc"),
      Token::NumLit(str_id) => write!(f, "{str_id}"),
      Token::Colon => write!(f, ":"),
      Token::Comma => write!(f, ","),
      Token::Equal => write!(f, "="),
      Token::Exclamation => write!(f, "!"),
      Token::Minus => write!(f, "-"),
      #[allow(clippy::write_with_newline)]
      Token::Newline => write!(f, "\n"),
      Token::Pipe => write!(f, "|"),
      Token::Plus => write!(f, "+"),
      Token::Quote => write!(f, "'"),
      Token::Semicolon => write!(f, ";"),
      Token::CommentSingle => write!(f, "//"),
      Token::CommentBlockStart => write!(f, "/*"),
      Token::CommentBlockEnd => write!(f, "*/"),
      Token::Ident(str_id) => write!(f, "{str_id}"),
      Token::TokenError => write!(f, "TokenError"),
      Token::GreaterThan => write!(f, ">"),
      Token::LessThan => write!(f, "<"),
      Token::Ampersand => write!(f, "&"),
      Token::KwFalse => write!(f, "false"),
      Token::KwTrue => write!(f, "true"),
      Token::Caret => write!(f, "^"),
      Token::Asterisk => write!(f, "*"),
      Token::Slash => write!(f, "/"),
      Token::Percent => write!(f, "%"),
      Token::Period => write!(f, "."),
    }
  }
}
