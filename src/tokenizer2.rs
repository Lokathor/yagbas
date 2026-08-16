use core::iter::*;
use core::range::Range;
use core::slice::Iter;

#[inline]
pub fn tokenize(s: &str) -> impl Iterator<Item = Token> + '_ {
  use TokenKind::*;
  #[inline(always)]
  const fn r(start: usize, end: usize) -> Range<usize> {
    Range { start, end }
  }
  fn handle_literal_num(
    start: usize, bytes: &mut Peekable<Enumerate<Copied<Iter<'_, u8>>>>,
  ) -> (TokenKind, Range<usize>) {
    let mut end = start;
    while let Some((_, b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z' | b'_')) =
      bytes.peek()
    {
      end = bytes.next().unwrap().0;
    }
    (LitNum, r(start, end + 1))
  }
  fn handle_keyword_or_ident(
    _start: usize, _bytes: &mut Peekable<Enumerate<Copied<Iter<'_, u8>>>>,
  ) -> (TokenKind, Range<usize>) {
    todo!()
  }
  let mut bytes = s.as_bytes().iter().copied().enumerate().peekable();
  let s_len = s.len();
  core::iter::from_fn(move || {
    loop {
      let (start, byte) = bytes.next()?;
      let (kind, span) = match byte {
        b' ' | b'\t' | b'\r' | b'\n' => continue,

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
        b'r' => todo!("raw string literal, or goto ident"),

        b'$' => match bytes.peek() {
          Some((_, b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z')) => {
            handle_literal_num(start, &mut bytes)
          }
          _ => (Dollar, r(start, start + 1)),
        },
        b'%' => match bytes.peek() {
          Some((_, b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z')) => {
            handle_literal_num(start, &mut bytes)
          }
          _ => (Percent, r(start, start + 1)),
        },
        b'0'..=b'9' => handle_literal_num(start, &mut bytes),

        b'A'..=b'Z' | b'a'..=b'z' | b'_' => {
          handle_keyword_or_ident(start, &mut bytes)
        }

        b'!'..=b'/' | b':'..=b'@' | b'['..=b'`' | b'{'..=b'~' => {
          let t = core::mem::transmute::<u8, TokenKind>;
          // Safety: all bytes in the pattern are variants within the TokenKind enum.
          (unsafe { t(byte) }, r(start, start + 1))
        }

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
  ErrLitRawStrUnclosed,
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
  KwBitbag,
  KwBreak,
  KwConst,
  KwContinue,
  KwElse,
  KwFalse,
  KwFn,
  KwIf,
  KwLet,
  KwLoop,
  KwMut,
  KwReturn,
  KwStruct,
  KwStatic,
  KwTrue,
  //
  CommentOpBlock,
  CommentClBlock,
  CommentLine,
  //
  Ident,
  LitNum,
  LitStr,
  LitRawStr,
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

#[test]
fn test_tokenize_lit_num() {
  use TokenKind::*;
  let mut v: Vec<Token>;

  v = tokenize(r##"1"##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, LitNum, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 1, "Bad Span: `{t:?}`");

  v = tokenize(r##"1 "##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, LitNum, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 1, "Bad Span: `{t:?}`");

  v = tokenize(r##"$"##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, Dollar, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 1, "Bad Span: `{t:?}`");

  v = tokenize(r##"%"##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, Percent, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 1, "Bad Span: `{t:?}`");

  v = tokenize(r##"$F"##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, LitNum, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 2, "Bad Span: `{t:?}`");

  v = tokenize(r##"%1"##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, LitNum, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 2, "Bad Span: `{t:?}`");

  v = tokenize(r##"1_u8"##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, LitNum, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 4, "Bad Span: `{t:?}`");
}
