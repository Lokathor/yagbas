use core::range::Range;

#[inline]
pub fn tokenize(s: &str) -> impl Iterator<Item = Token> + '_ {
  use TokenKind::*;
  const fn r(start: usize, end: usize) -> Range<usize> {
    Range { start, end }
  }
  let mut bytes = s.as_bytes().iter().copied().enumerate().peekable();
  core::iter::from_fn(move || {
    loop {
      let (start, byte) = bytes.next()?;
      let (kind, span) = match byte {
        // whitespace is skipped over
        b' ' | b'\t' | b'\r' | b'\n' => continue,
        // TODO: string literal handling

        // TODO: comments

        // TODO: number literals

        // TODO: keywords and idents
        b'0'..=b'9' => todo!("num lit"),
        b'A'..=b'Z' | b'a'..=b'z' => todo!("ident or something"),

        // catch all for other punctuation
        b'!'..=b'/' | b':'..=b'@' | b'['..=b'`' | b'{'..=b'~' => {
          // Safety: all bytes in the pattern are variants within the TokenKind enum.
          (unsafe { core::mem::transmute(byte) }, r(start, start + 1))
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
    assert_eq!(t.kind, k, "Single Kind: `{s}`");
    assert_eq!(t.span.iter().count(), 1, "Single Len: `{s}`");
  }
}
