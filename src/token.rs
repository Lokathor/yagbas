use logos::{Lexer, Logos};

use crate::str_id::StrID;

/// The fundamental tokens of Yagbas source code.
///
/// Call [`Token::lexer`] to turn a source [`&str`] into a [`Lexer`], which can
/// be used as an iterator that will correctly produce all the `Token` values.
#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[logos(skip r#"[[ \t]]"#)] // ignore this between tokens
pub enum Token {
  #[regex(r"break")]
  KwBreak,
  #[regex(r"const")]
  KwConst,
  #[regex(r"fn")]
  KwFn,
  #[regex(r"if")]
  KwIf,
  #[regex(r"loop")]
  KwLoop,
  #[regex(r"return")]
  KwReturn,
  #[regex(r"static")]
  KwStatic,
  #[regex(r"u16")]
  KwU16,
  #[regex(r"u8")]
  KwU8,

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

  #[token("z", priority = 3)]
  #[token("Z", priority = 3)]
  KwZ,
  #[token("nz", priority = 3)]
  #[token("NZ", priority = 3)]
  KwNZ,
  // Note(Lokathor): `carry` condition and `c` register both tokenize as KwC. The
  // context makes it clear what the user meant.
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
  #[regex(r";", priority = 2)]
  Semicolon,

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
  pub fn lexer<'source>(source: &'source <Self as Logos>::Source) -> Lexer<'source, Self>
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
