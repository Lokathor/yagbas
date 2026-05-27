use logos::Lexer;
use logos::Logos;

#[derive(Debug, Clone, Copy)]
pub struct Token {
  pub kind: TokenKind,
  pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Logos)]
#[logos(skip r#"[[:space:]]"#)]
#[logos(error = TokenizerError)]
pub enum TokenKind {
  /* Token Tree Markers */
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
  OpCommentBlock,
  #[regex(r"\*/")]
  ClCommentBlock,

  /* Stuff Where You Slice The Source To Check What It Was */
  #[regex(r"[_a-zA-Z][_a-zA-Z0-9]*")]
  Ident,
  #[regex(r"((\$|%)[[:word:]]+|[[:digit:]][[:word:]]*)")]
  LitNum,
  #[regex(r#"""#, end_lit_string)]
  LitStr,
  #[regex(r#"r#*\""#, end_lit_raw_string)]
  LitRawStr,
  #[regex(r"///[^\r\n]*", allow_greedy = true)]
  CommentDoc,
  #[regex(r"//![^\r\n]*", allow_greedy = true)]
  CommentInnerDoc,
  #[regex(r"//[^\r\n]*", allow_greedy = true)]
  CommentLine,

  /* Stuff That's Always One Thing */
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
  #[regex(r"false")]
  KwFalse,
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

  LexerUnknown,
  LitStrNoCloseQuote,

  EndOfFile,
}

/// This type should never be deliberately used outside of the `tokenizer`
/// module, but must be public because it appears in an interface.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum TokenizerError {
  #[default]
  LexerUnknown,
  LitStrNoCloseQuote,
}

fn end_lit_string(lex: &mut Lexer<TokenKind>) -> Result<(), TokenizerError> {
  let prefix = lex.slice();
  // check assumptions from the regex.
  debug_assert!(prefix.len() == 1);
  debug_assert!(prefix == "\"");
  loop {
    // find a `"` in the remainder.
    let remainder = lex.remainder();
    match remainder.find('"') {
      Some(position) => {
        // we found a `"`, advance the lexer.
        lex.bump(position + 1);
        // count how many `\` are on the end of the slice'd portion. when the
        // number of backslashes is odd then the last one escapes the current
        // double quote we've found, and we must continue the loop.
        let end_slash_count = lex
          .slice()
          .as_bytes()
          .iter()
          .rev()
          .skip(1)
          .take_while(|b| **b == b'\\')
          .count();
        if end_slash_count % 2 != 0 {
          continue;
        } else {
          return Ok(());
        }
      }
      None => {
        // when no `"` remain the litstr is unclosed, which is an error.
        lex.bump(remainder.len());
        return Err(TokenizerError::LitStrNoCloseQuote);
      }
    }
  }
}

fn end_lit_raw_string(_lex: &mut Lexer<TokenKind>) -> Option<()> {
  todo!()
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
  pub start: usize,
  pub end: usize,
}
impl Span {
  pub fn as_range(self) -> core::ops::Range<usize> {
    self.start..self.end
  }
}

pub fn tokenize(source: &str) -> impl Iterator<Item = Token> + Clone + '_ {
  TokenKind::lexer(source).spanned().map(|(res, range)| Token {
    kind: match res {
      Ok(kind) => kind,
      Err(TokenizerError::LexerUnknown) => TokenKind::LexerUnknown,
      Err(TokenizerError::LitStrNoCloseQuote) => TokenKind::LitStrNoCloseQuote,
    },
    span: Span { start: range.start, end: range.end },
  })
}

#[test]
fn test_tokenize() {
  let mut v: Vec<Token> = tokenize("").collect();
  assert!(v.is_empty());

  let x = "\"abc\"";
  v = tokenize(x).collect();
  assert_eq!(v[0].kind, TokenKind::LitStr);
  assert_eq!(v[0].span.as_range(), 0..x.len());

  let x = "\"a\\\"bc\"";
  v = tokenize(x).collect();
  assert_eq!(v[0].kind, TokenKind::LitStr);
  assert_eq!(v[0].span.as_range(), 0..x.len());

  let x = "\"a\\bc\"";
  v = tokenize(x).collect();
  assert_eq!(v[0].kind, TokenKind::LitStr);
  assert_eq!(v[0].span.as_range(), 0..x.len());

  let x = "\"";
  v = tokenize(x).collect();
  assert_eq!(v[0].kind, TokenKind::LitStrNoCloseQuote);
  assert_eq!(v[0].span.as_range(), 0..x.len());

  let x = "\"\\\"";
  v = tokenize(x).collect();
  assert_eq!(v[0].kind, TokenKind::LitStrNoCloseQuote);
  assert_eq!(v[0].span.as_range(), 0..x.len());
}
