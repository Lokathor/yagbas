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
#[logos(skip r#"[[:space:]]"#)] // ignore whitespace between tokens
pub enum Token {
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
  #[regex(r#""((\\")|[^"\\])*""#)]
  LitStr,
  #[regex(r#"r#*\""#, raw_string)]
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

  #[regex(r"==")]
  CmpEq,
  #[regex(r"!=")]
  CmpNe,
  #[regex(r">=")]
  CmpGe,
  #[regex(r"<=")]
  CmpLt,

  #[regex(r"&&")]
  BoolAnd,
  #[regex(r"\|\|")]
  BoolOr,

  #[regex(r"->")]
  RightArrow,
  #[regex(r"::")]
  Path,

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

  #[default]
  UnknownToken,
}

fn raw_string(lex: &mut logos::Lexer<Token>) -> Option<()> {
  let pre = lex.slice().as_bytes();
  // Enforced by the regex.
  debug_assert!(pre.len() >= 2);
  debug_assert!(pre[0] == b'r' && pre[pre.len() - 1] == b'"');
  debug_assert!(pre[1..pre.len() - 1].iter().all(|&b| b == b'#'));
  let hashes = pre.len().checked_sub(2).unwrap();
  let rest_str = lex.remainder();
  // Handle the 0-hash case here since it can be done more efficiently (and
  // doing so simplifies the loop below).
  if hashes == 0 {
    if let Some(idx) = rest_str.find('"') {
      lex.bump(idx + 1);
      return Some(());
    } else {
      return None;
    }
  }
  let rest = rest_str.as_bytes();
  // Look for `"###` for the right number of hashes;
  // Turns into Some((close_quote_idx, 0)) when we see a `"`, and then counts up for each hash.
  let mut seen_hashes = None::<(usize, usize)>;
  for (i, &byte) in rest.iter().enumerate() {
    if byte == b'"' {
      seen_hashes = Some((i, 0));
    } else if let Some(&mut (_end, ref mut n)) = seen_hashes.as_mut() {
      if byte == b'#' {
        *n += 1;
        if *n == hashes {
          lex.bump(i + 1);
          return Some(());
        }
      } else {
        seen_hashes = None;
      }
    }
  }
  None
}

pub fn tokenize(
  source: &str,
) -> impl Iterator<Item = (Token, core::ops::Range<usize>)> + Clone + '_ {
  <Token as logos::Logos>::lexer(source).spanned().map(
    |(res, span)| match res {
      Ok(token) => (token, span),
      Err(()) => (Token::UnknownToken, span),
    },
  )
}
