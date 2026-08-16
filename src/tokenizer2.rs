use core::range::Range;

#[inline]
pub fn tokenize(s: &str) -> impl Iterator<Item = Token> + '_ {
  use TokenKind::*;
  const fn r(start: usize, end: usize) -> Range<usize> {
    Range { start, end }
  }
  let mut bytes = s.as_bytes().iter().copied().enumerate().peekable();
  let s_len = s.len();
  core::iter::from_fn(move || {
    loop {
      let (start, byte) = bytes.next()?;
      let (kind, span) = match byte {
        // whitespace is skipped over
        b' ' | b'\t' | b'\r' | b'\n' => continue,

        // handle comment markers with highest priority
        b'/' => match bytes.peek() {
          Some((_, b'*')) => {
            let _ = bytes.next();
            (CommentOpBlock, r(start, start + 2))
          }
          Some((_, b'/')) => {
            let end = loop {
              match bytes.peek() {
                Some((x, b'\r')) | Some((x, b'\n')) => break *x,
                None => break s_len,
                _ => {
                  let _ = bytes.next();
                }
              }
            };
            (CommentLine, r(start, end))
          }
          _ => (Slash, r(start, start + 1)),
        },
        b'*' => match bytes.peek() {
          Some((_, b'/')) => {
            let _ = bytes.next();
            (CommentClBlock, r(start, start + 2))
          }
          _ => (Star, r(start, start + 1)),
        },

        // literal strings are complicated by escape characters
        b'"' => {
          let mut backslash_count = 0;
          let end = loop {
            match bytes.next() {
              None => {
                return Some(Token {
                  kind: ErrLitStrUnclosed,
                  span: r(start, s_len),
                });
              }
              Some((_, b'\\')) => {
                backslash_count += 1;
              }
              Some((end, b'"')) => {
                if backslash_count % 2 != 0 {
                  backslash_count = 0;
                  continue;
                } else {
                  break end + 1;
                }
              }
              _ => backslash_count = 0,
            }
          };
          (LitStr, r(start, end))
        }

        b'0'..=b'9' => todo!("rust-style literal number"),
        b'%' => todo!("possible binary literal number"),
        b'$' => todo!("possible hex literal number"),
        b'r' => todo!("possible raw string literal"),
        b'A'..=b'Z' | b'a'..=b'z' => todo!("ident or something"),

        // punctuation mark general case
        b'!'..=b'/' | b':'..=b'@' | b'['..=b'`' | b'{'..=b'~' => {
          let t = core::mem::transmute::<u8, TokenKind>;
          // Safety: all bytes in the pattern are variants within the TokenKind enum.
          (unsafe { t(byte) }, r(start, start + 1))
        }

        // all other bytes are out of range
        ..=0x1F | 0x7F.. => (ErrUnknown, r(start, start + 1)),
      };
      return Some(Token { kind, span });
    }
  })
}

#[derive(Debug, Clone, Copy)]
pub struct Token {
  pub kind: TokenKind,
  pub span: Range<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum TokenKind {
  ErrUnknown,
  ErrLitStrUnclosed,
  //
  Bang = b'!',
  DoubleQuote = b'"',
  Hash = b'#',
  Dollar = b'$',
  Percent = b'%',
  Ampersand = b'&',
  Quote = b'\'',
  OpParen = b'(',
  ClParen = b')',
  Star = b'*',
  Plus = b'+',
  Comma = b',',
  Minus = b'-',
  Dot = b'.',
  Slash = b'/',
  Colon = b':',
  Semicolon = b';',
  LessThan = b'<',
  Equal = b'=',
  GreaterThan = b'>',
  Question = b'?',
  At = b'@',
  OpBracket = b'[',
  Backslash = b'\\',
  ClBracket = b']',
  Caret = b'^',
  Underscore = b'_',
  Backtick = b'`',
  OpBrace = b'{',
  Pipe = b'|',
  ClBrace = b'}',
  Tilde = b'~',
  //
  KwFn,
  KwStruct,
  KwBitbag,
  KwConst,
  KwStatic,
  KwIf,
  KwElse,
  KwLoop,
  KwBreak,
  KwContinue,
  KwReturn,
  KwLet,
  KwMut,
  //
  CommentOpBlock,
  CommentClBlock,
  CommentLine,
  //
  Ident,
  LitNum,
  LitStr,
  LitRawStr,
  LitBoolTrue,
  LitBoolFalse,
}

#[test]
fn test_tokenize_single_chars() {
  use TokenKind::*;
  let mut v: Vec<Token>;
  let singles = [
    ("(", OpParen),
    (")", ClParen),
    ("{", OpBrace),
    ("}", ClBrace),
    (":", Colon),
  ];
  for (s, k) in singles.iter().copied() {
    v = tokenize(s).collect();
    assert_eq!(v.len(), 1);
    let t = v[0];
    assert_eq!(t.kind, k, "Bad Kind: `{s}`");
    assert_eq!(t.span.iter().count(), 1, "Bad Span: `{s}`");
  }
}

#[test]
fn test_comment_block_op_and_cl() {
  use TokenKind::*;
  let mut v: Vec<Token>;

  v = tokenize("/*").collect();
  assert_eq!(v.len(), 1);
  let t = v[0];
  assert_eq!(t.kind, CommentOpBlock, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 2, "Bad Span: `{t:?}`");

  v = tokenize("*/").collect();
  assert_eq!(v.len(), 1);
  let t = v[0];
  assert_eq!(t.kind, CommentClBlock, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 2, "Bad Span: `{t:?}`");
}

#[test]
fn test_comment_line() {
  use TokenKind::*;
  let mut v: Vec<Token>;

  v = tokenize("//").collect();
  assert_eq!(v.len(), 1);
  let t = v[0];
  assert_eq!(t.kind, CommentLine, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 2, "Bad Span: `{t:?}`");

  v = tokenize("// big comment line").collect();
  assert_eq!(v.len(), 1);
  let t = v[0];
  assert_eq!(t.kind, CommentLine, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 19, "Bad Span: `{t:?}`");

  v = tokenize(
    "// big comment line
  !",
  )
  .collect();
  assert_eq!(v.len(), 2);
  let t = v[0];
  assert_eq!(t.kind, CommentLine, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 19, "Bad Span: `{t:?}`");
}

#[test]
fn test_tokenize_lit_str() {
  use TokenKind::*;
  let mut v: Vec<Token>;

  v = tokenize(r##" "" "##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, LitStr, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 2, "Bad Span: `{t:?}`");

  v = tokenize(r##" "abc" "##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, LitStr, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 5, "Bad Span: `{t:?}`");

  v = tokenize(r##" "a\"bc" "##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, LitStr, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 7, "Bad Span: `{t:?}`");

  v = tokenize(r##" "a\\bc" "##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, LitStr, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 7, "Bad Span: `{t:?}`");
}

#[test]
fn test_tokenize_lit_str_no_close() {
  use TokenKind::*;
  let mut v: Vec<Token>;

  v = tokenize(r##" " "##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, ErrLitStrUnclosed, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 2, "Bad Span: `{t:?}`");

  v = tokenize(r##" " \" "##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, ErrLitStrUnclosed, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 5, "Bad Span: `{t:?}`");
}
