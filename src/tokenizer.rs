use core::range::Range;
use logos::Lexer;
use logos::Logos;

/// One token of source code.
#[derive(Debug, Clone, Copy)]
pub struct Token {
  pub kind: TokenKind,
  pub span: Range<usize>,
}

/// The different kinds of source token.
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

  /* Keywords */
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

  /* Punctuation */
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

  LexerUnknown,
  LitStrNoCloseQuote,
  LitRawStrNoCloseQuote,

  EndOfFile,
}

/// This type should never be deliberately used outside of the `tokenizer`
/// module, but must be public because it appears in an interface.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum TokenizerError {
  #[default]
  LexerUnknown,
  LitStrNoCloseQuote,
  LitRawStrNoCloseQuote,
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

// similar to `end_lit_string`, but looking for matching hashes after the
// closing double quote.
fn end_lit_raw_string(
  lex: &mut Lexer<TokenKind>,
) -> Result<(), TokenizerError> {
  let prefix = lex.slice();
  debug_assert!(prefix.len() >= 2);
  let hashes = &prefix[..prefix.len() - 1][1..];
  debug_assert!(hashes.chars().all(|h| h == '#'));
  let hash_len = hashes.len();
  loop {
    let remainder = lex.remainder();
    match remainder.find('"') {
      Some(position) => {
        lex.bump(position + 1);
        let trailing_hashes =
          lex.remainder().as_bytes().iter().take_while(|h| **h == b'#').count();
        if trailing_hashes != hash_len {
          continue;
        } else {
          lex.bump(hash_len);
          return Ok(());
        }
      }
      None => {
        lex.bump(remainder.len());
        return Err(TokenizerError::LitRawStrNoCloseQuote);
      }
    }
  }
}

/// Convert source code string slice into an iterator of tokens.
pub fn tokenize(source: &str) -> impl Iterator<Item = Token> + Clone + '_ {
  TokenKind::lexer(source).spanned().map(|(res_kind, op_range)| Token {
    kind: match res_kind {
      Ok(kind) => kind,
      Err(TokenizerError::LexerUnknown) => TokenKind::LexerUnknown,
      Err(TokenizerError::LitStrNoCloseQuote) => TokenKind::LitStrNoCloseQuote,
      Err(TokenizerError::LitRawStrNoCloseQuote) => {
        TokenKind::LitRawStrNoCloseQuote
      }
    },
    span: op_range.into(),
  })
}

#[test]
fn test_tokenize_empty_input() {
  let mut v: Vec<Token> = tokenize("").collect();
  assert!(v.is_empty());
}

#[test]
fn test_tokenize_lit_str() {
  let mut v: Vec<Token>;

  let x = "\"\"";
  v = tokenize(x).collect();
  assert_eq!(v[0].kind, TokenKind::LitStr);
  assert_eq!(v[0].span.iter().count(), x.len());

  let x = "\"abc\"";
  v = tokenize(x).collect();
  assert_eq!(v[0].kind, TokenKind::LitStr);
  assert_eq!(v[0].span.iter().count(), x.len());

  let x = "\"a\\\"bc\"";
  v = tokenize(x).collect();
  assert_eq!(v[0].kind, TokenKind::LitStr);
  assert_eq!(v[0].span.iter().count(), x.len());

  let x = "\"a\\bc\"";
  v = tokenize(x).collect();
  assert_eq!(v[0].kind, TokenKind::LitStr);
  assert_eq!(v[0].span.iter().count(), x.len());

  let x = "\"";
  v = tokenize(x).collect();
  assert_eq!(v[0].kind, TokenKind::LitStrNoCloseQuote);
  assert_eq!(v[0].span.iter().count(), x.len());

  let x = "\"\\\"";
  v = tokenize(x).collect();
  assert_eq!(v[0].kind, TokenKind::LitStrNoCloseQuote);
  assert_eq!(v[0].span.iter().count(), x.len());
}

#[test]
fn test_tokenize_lit_raw_str() {
  let mut v: Vec<Token>;

  let x = "r\"\"";
  v = tokenize(x).collect();
  assert_eq!(v[0].kind, TokenKind::LitRawStr);
  assert_eq!(v[0].span.iter().count(), x.len());

  let x = "r#\"\"#";
  v = tokenize(x).collect();
  assert_eq!(v[0].kind, TokenKind::LitRawStr);
  assert_eq!(v[0].span.iter().count(), x.len());

  let x = "r##\"abc\"##";
  v = tokenize(x).collect();
  assert_eq!(v[0].kind, TokenKind::LitRawStr);
  assert_eq!(v[0].span.iter().count(), x.len());

  let x = "r###\"ab\"##c\"###";
  v = tokenize(x).collect();
  assert_eq!(v[0].kind, TokenKind::LitRawStr);
  assert_eq!(v[0].span.iter().count(), x.len());

  let x = "r###\"ab";
  v = tokenize(x).collect();
  assert_eq!(v[0].kind, TokenKind::LitRawStrNoCloseQuote);
  assert_eq!(v[0].span.iter().count(), x.len());

  let x = "r##\"abc\"#";
  v = tokenize(x).collect();
  assert_eq!(v[0].kind, TokenKind::LitRawStrNoCloseQuote);
  assert_eq!(v[0].span.iter().count(), x.len());
}
