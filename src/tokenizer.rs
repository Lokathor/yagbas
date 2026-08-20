use crate::r;
use TokenKind::*;
use core::iter::*;
use core::range::Range;
use core::slice::Iter;

type TokenIterInternal<'a> = Peekable<Enumerate<Copied<Iter<'a, u8>>>>;

#[derive(Debug, Clone)]
pub struct TokenIter<'a> {
  src: &'a str,
  bytes: TokenIterInternal<'a>,
}
impl<'a> TokenIter<'a> {
  pub fn new(src: &'a str) -> Self {
    Self { src, bytes: src.as_bytes().iter().copied().enumerate().peekable() }
  }

  fn handle_block_comment(
    &mut self, start: usize,
  ) -> (TokenKind, Range<usize>) {
    let mut depth = 1;
    let (mut end, b) = self.bytes.next().unwrap();
    debug_assert_eq!(b, b'*');
    loop {
      debug_assert!(depth > 0);
      match self.bytes.next() {
        None => return (ErrBlockCommentUnclosed, r(start, end + 1)),
        Some((x, b'/')) => {
          end = x;
          // possible nested block
          if let Some((_, b'*')) = self.bytes.peek() {
            end = self.bytes.next().unwrap().0;
            depth += 1;
          }
        }
        Some((x, b'*')) => {
          end = x;
          // possible end block
          if let Some((_, b'/')) = self.bytes.peek() {
            end = self.bytes.next().unwrap().0;
            depth -= 1;
            if depth == 0 {
              break;
            }
          }
        }
        Some((x, _)) => {
          end = x;
        }
      }
    }
    (Comment, r(start, end + 1))
  }

  fn handle_literal_str(&mut self, start: usize) -> (TokenKind, Range<usize>) {
    let mut backslash_count = 0;
    let end = loop {
      match self.bytes.next() {
        None => {
          return (ErrLitStrUnclosed, r(start, self.src.len()));
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

  fn handle_literal_raw_value(
    &mut self, start: usize,
  ) -> (TokenKind, Range<usize>) {
    debug_assert_eq!(self.bytes.peek().unwrap().1, b'#');
    let mut end = start;
    let mut hash_count = 0;
    while let Some((_, b'#')) = self.bytes.peek() {
      hash_count += 1;
      end = self.bytes.next().unwrap().0;
    }
    match self.bytes.next() {
      Some((_, b'"')) => (),
      _ => return (ErrBadRawValue, r(start, end + 1)),
    }
    debug_assert!(hash_count > 0);
    'find_double_quote: loop {
      match self.bytes.next() {
        None => return (ErrLitRawStrUnclosed, r(start, self.src.len())),
        Some((x, b'"')) => {
          end = x;
          let mut remaining = hash_count;
          'count_hashes: while remaining > 0 {
            match self.bytes.peek() {
              None => return (ErrLitRawStrUnclosed, r(start, self.src.len())),
              Some((_x, b'#')) => {
                end = self.bytes.next().unwrap().0;
              }
              Some((_, _y)) => {
                continue 'find_double_quote;
              }
            }
            remaining -= 1;
          }
          break 'find_double_quote;
        }
        Some((_i, _b)) => {}
      }
    }
    (LitStr, r(start, end + 1))
  }

  fn handle_literal_num(&mut self, start: usize) -> (TokenKind, Range<usize>) {
    let mut end = start;
    while let Some((_, b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z' | b'_')) =
      self.bytes.peek()
    {
      end = self.bytes.next().unwrap().0;
    }
    (LitNum, r(start, end + 1))
  }

  fn handle_keyword_or_ident(
    &mut self, start: usize,
  ) -> (TokenKind, Range<usize>) {
    let mut end = start;
    while let Some((_, b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z' | b'_')) =
      self.bytes.peek()
    {
      end = self.bytes.next().unwrap().0;
    }
    end += 1;
    let captured = &self.src[start..end];
    let kind = match captured {
      "bitbag" => KwBitbag,
      "break" => KwBreak,
      "const" => KwConst,
      "continue" => KwContinue,
      "else" => KwElse,
      "false" => KwFalse,
      "fn" => KwFn,
      "for" => KwFor,
      "if" => KwIf,
      "impl" => KwImpl,
      "let" => KwLet,
      "loop" => KwLoop,
      "mut" => KwMut,
      "return" => KwReturn,
      "struct" => KwStruct,
      "static" => KwStatic,
      "true" => KwTrue,
      "use" => KwUse,
      _ => Ident,
    };
    (kind, r(start, end))
  }
}
impl<'a> Iterator for TokenIter<'a> {
  type Item = Token;

  fn next(&mut self) -> Option<Self::Item> {
    let (start, byte) = self.bytes.next()?;
    let (kind, span) = match byte {
      // whitespace
      b' ' | b'\t' | b'\r' | b'\n' => {
        let mut end = start;
        'label: while let Some((_, b' ' | b'\t' | b'\r' | b'\n')) =
          self.bytes.peek()
        {
          end = self.bytes.next().unwrap().0;
        }
        (Whitespace, r(start, end + 1))
      }
      // comments
      b'/' => match self.bytes.peek() {
        Some((_, b'*')) => self.handle_block_comment(start),
        Some((_, b'/')) => {
          let end = loop {
            match self.bytes.peek() {
              Some((x, b'\r')) | Some((x, b'\n')) => break *x,
              None => break self.src.len(),
              _ => {
                let _ = self.bytes.next();
              }
            }
          };
          (Comment, r(start, end))
        }
        _ => (Slash, r(start, start + 1)),
      },
      b'*' => match self.bytes.peek() {
        Some((_, b'/')) => {
          let end = self.bytes.next().unwrap().0;
          (ErrBlockCommentExtraClose, r(start, end + 1))
        }
        _ => (Star, r(start, start + 1)),
      },
      // string literals
      b'"' => self.handle_literal_str(start),
      b'r' if self.bytes.peek().map(|b| b.1 == b'#').unwrap_or(false) => {
        self.handle_literal_raw_value(start)
      }
      // number literals
      b'$' => match self.bytes.peek() {
        Some((_, b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z')) => {
          self.handle_literal_num(start)
        }
        _ => (Dollar, r(start, start + 1)),
      },
      b'%' => match self.bytes.peek() {
        Some((_, b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z')) => {
          self.handle_literal_num(start)
        }
        _ => (Percent, r(start, start + 1)),
      },
      b'0'..=b'9' => self.handle_literal_num(start),
      // keywords, idents, and punctuation
      b'A'..=b'Z' | b'a'..=b'z' | b'_' => self.handle_keyword_or_ident(start),
      b'!'..=b'/' | b':'..=b'@' | b'['..=b'`' | b'{'..=b'~' => {
        let t = core::mem::transmute::<u8, TokenKind>;
        // Safety: all bytes in the pattern are variants within the TokenKind enum.
        (unsafe { t(byte) }, r(start, start + 1))
      }
      // otherwise it's out of range
      ..=0x1F | 0x7F.. => (ErrUnknown, r(start, start + 1)),
    };
    Some(Token { kind, span })
  }
}

#[inline]
pub fn tokenize(src: &str) -> TokenIter<'_> {
  TokenIter::new(src)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
  pub kind: TokenKind,
  pub span: Range<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum TokenKind {
  ErrUnknown,
  ErrBlockCommentUnclosed,
  ErrBlockCommentExtraClose,
  ErrLitStrUnclosed,
  ErrLitRawStrUnclosed,
  ErrBadRawValue,
  ErrEndOfFile,
  //
  KwBitbag,
  KwBreak,
  KwConst,
  KwContinue,
  KwElse,
  KwFalse,
  KwFn,
  KwFor,
  KwIf,
  KwImpl,
  KwLet,
  KwLoop,
  KwMut,
  KwReturn,
  KwStruct,
  KwStatic,
  KwTrue,
  KwUse,
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
  Whitespace,
  Comment,
  //
  Ident,
  LitNum,
  LitStr,
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
fn test_comment_block() {
  use TokenKind::*;
  let mut v: Vec<Token>;

  v = tokenize("/**/").collect();
  assert_eq!(v.len(), 1);
  let t = v[0];
  assert_eq!(t.kind, Comment, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 4, "Bad Span: `{t:?}`");

  v = tokenize("/*/**/*/").collect();
  assert_eq!(v.len(), 1);
  let t = v[0];
  assert_eq!(t.kind, Comment, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 8, "Bad Span: `{t:?}`");

  v = tokenize("/*").collect();
  assert_eq!(v.len(), 1);
  let t = v[0];
  assert_eq!(t.kind, ErrBlockCommentUnclosed, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 2, "Bad Span: `{t:?}`");

  v = tokenize("*/").collect();
  assert_eq!(v.len(), 1);
  let t = v[0];
  assert_eq!(t.kind, ErrBlockCommentExtraClose, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 2, "Bad Span: `{t:?}`");
}

#[test]
fn test_comment_line() {
  use TokenKind::*;
  let mut v: Vec<Token>;

  v = tokenize("//").collect();
  assert_eq!(v.len(), 1);
  let t = v[0];
  assert_eq!(t.kind, Comment, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 2, "Bad Span: `{t:?}`");

  v = tokenize("// big comment line").collect();
  assert_eq!(v.len(), 1);
  let t = v[0];
  assert_eq!(t.kind, Comment, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 19, "Bad Span: `{t:?}`");

  v = tokenize("// */").collect();
  assert_eq!(v.len(), 1);
  let t = v[0];
  assert_eq!(t.kind, Comment, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 5, "Bad Span: `{t:?}`");

  v = tokenize(
    "// big comment line
  !",
  )
  .collect();
  assert_eq!(v.len(), 3); // comment whitespace bang
  let t = v[0];
  assert_eq!(t.kind, Comment, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 19, "Bad Span: `{t:?}`");
}

#[test]
fn test_tokenize_lit_str() {
  use TokenKind::*;
  let mut v: Vec<Token>;

  v = tokenize(r##""""##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, LitStr, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 2, "Bad Span: `{t:?}`");

  v = tokenize(r##""abc""##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, LitStr, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 5, "Bad Span: `{t:?}`");

  v = tokenize(r##""a\"bc""##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, LitStr, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 7, "Bad Span: `{t:?}`");

  v = tokenize(r##""a\\bc""##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, LitStr, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 7, "Bad Span: `{t:?}`");
}

#[test]
fn test_tokenize_lit_raw_str() {
  use TokenKind::*;
  let mut v: Vec<Token>;

  v = tokenize(r##"r"""##).collect();
  assert_eq!(v.len(), 2, "Bad Output Len: {v:?}");

  v = tokenize(r##"r#"##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, ErrBadRawValue, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 2, "Bad Span: `{t:?}`");

  v = tokenize(r##"r#""#"##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, LitStr, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 5, "Bad Span: `{t:?}`");

  v = tokenize(r#######"r###""#"#######).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, ErrLitRawStrUnclosed, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 7, "Bad Span: `{t:?}`");

  v = tokenize(r#######"r###""###"#######).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, LitStr, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 9, "Bad Span: `{t:?}`");

  v = tokenize(r#######"r###"abc""###"#######).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, LitStr, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 13, "Bad Span: `{t:?}`");
}

#[test]
fn test_tokenize_lit_str_no_close() {
  use TokenKind::*;
  let mut v: Vec<Token>;

  v = tokenize(r##"""##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, ErrLitStrUnclosed, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 1, "Bad Span: `{t:?}`");

  v = tokenize(r##"" \""##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, ErrLitStrUnclosed, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 4, "Bad Span: `{t:?}`");
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

#[test]
fn test_tokenize_keyword_and_ident() {
  use TokenKind::*;
  let mut v: Vec<Token>;

  v = tokenize(r##"fn"##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, KwFn, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 2, "Bad Span: `{t:?}`");

  v = tokenize(r##"static"##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, KwStatic, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 6, "Bad Span: `{t:?}`");

  v = tokenize(r##"foo"##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, Ident, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 3, "Bad Span: `{t:?}`");

  v = tokenize(r##"foo_"##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, Ident, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 4, "Bad Span: `{t:?}`");

  v = tokenize(r##"regal"##).collect();
  assert_eq!(v.len(), 1, "Bad Output Len: {v:?}");
  let t = v[0];
  assert_eq!(t.kind, Ident, "Bad Kind: `{t:?}`");
  assert_eq!(t.span.iter().count(), 5, "Bad Span: `{t:?}`");
}
