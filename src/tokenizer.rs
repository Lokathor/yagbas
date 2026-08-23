#![warn(missing_docs)]

//! Types and utilities for tokenizing Yagbas source code.

use crate::r;
use TokenKind::*;
use core::iter::*;
use core::range::Range;
use core::slice::Iter;

type TokenIterInternal<'a> = Peekable<Enumerate<Copied<Iter<'a, u8>>>>;

/// An iterator over a module's source code which produces [Token] values.
#[derive(Debug, Clone)]
pub struct TokenIter<'a> {
  src: &'a str,
  bytes: TokenIterInternal<'a>,
}
impl<'a> TokenIter<'a> {
  /// Constructs a new iterator pointed to the start of the source.
  #[inline]
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
      "as" => KwAs,
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
      "match" => KwMatch,
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
        Some((_, b'=')) => {
          let end = self.bytes.next().unwrap().0;
          (SlashEqual, r(start, end + 1))
        }
        _ => (Slash, r(start, start + 1)),
      },
      b'*' => match self.bytes.peek() {
        Some((_, b'/')) => {
          let end = self.bytes.next().unwrap().0;
          (ErrBlockCommentExtraClose, r(start, end + 1))
        }
        Some((_, b'=')) => {
          let end = self.bytes.next().unwrap().0;
          (StarEqual, r(start, end + 1))
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
        Some((_, b'=')) => {
          let end = self.bytes.next().unwrap().0;
          (PercentEqual, r(start, end + 1))
        }
        _ => (Percent, r(start, start + 1)),
      },
      b'0'..=b'9' => self.handle_literal_num(start),
      // keywords, idents
      b'A'..=b'Z' | b'a'..=b'z' | b'_' => self.handle_keyword_or_ident(start),
      // double punctuation
      b':' if self.bytes.peek().map_or(0, |(_, b)| *b) == b':' => {
        let _ = self.bytes.next();
        (ColonColon, r(start, start + 2))
      }
      b'=' if self.bytes.peek().map_or(0, |(_, b)| *b) == b'=' => {
        let _ = self.bytes.next();
        (EqualEqual, r(start, start + 2))
      }
      b'!' if self.bytes.peek().map_or(0, |(_, b)| *b) == b'=' => {
        let _ = self.bytes.next();
        (BangEqual, r(start, start + 2))
      }
      b'+' if self.bytes.peek().map_or(0, |(_, b)| *b) == b'=' => {
        let _ = self.bytes.next();
        (PlusEqual, r(start, start + 2))
      }
      b'-' if self.bytes.peek().map_or(0, |(_, b)| *b) == b'=' => {
        let _ = self.bytes.next();
        (MinusEqual, r(start, start + 2))
      }
      b'&' if self.bytes.peek().map_or(0, |(_, b)| *b) == b'=' => {
        let _ = self.bytes.next();
        (AmpersandEqual, r(start, start + 2))
      }
      b'|' if self.bytes.peek().map_or(0, |(_, b)| *b) == b'=' => {
        let _ = self.bytes.next();
        (PipeEqual, r(start, start + 2))
      }
      b'^' if self.bytes.peek().map_or(0, |(_, b)| *b) == b'=' => {
        let _ = self.bytes.next();
        (CaretEqual, r(start, start + 2))
      }
      b'.' if self.bytes.peek().map_or(0, |(_, b)| *b) == b'.' => {
        let _ = self.bytes.next(); // consume second '.'
        // possible '=' after the second '.'
        match self.bytes.peek().map_or(0, |(_, b)| *b) {
          b'=' => {
            let _ = self.bytes.next();
            (DotDotEqual, r(start, start + 3))
          }
          _ => (DotDot, r(start, start + 2)),
        }
      }
      // fallback for all other punctuation cases
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

/// Alternative way to make a [TokenIter]
#[inline]
pub fn tokenize(src: &str) -> TokenIter<'_> {
  TokenIter::new(src)
}

/// An individual element of Yagbas source.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
  #[allow(missing_docs)]
  pub kind: TokenKind,
  /// The span within the source where the token was found.
  pub span: Range<usize>,
}

/// The possible kinds of token that can exist in Yagbas source.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum TokenKind {
  // error cases
  /// The lexer doesn't know what this was.
  ErrUnknown,
  /// A block comment was opened but not closed.
  ErrBlockCommentUnclosed,
  /// A block comment was closed without any preceeding open markers.
  ErrBlockCommentExtraClose,
  /// A literal string was unclosed.
  ErrLitStrUnclosed,
  /// A literal raw string was unclosed.
  ErrLitRawStrUnclosed,
  /// A raw specifier prefix was given but then it didn't turn into an allowed raw value.
  /// * Currently, only raw strings are allowed.
  ErrBadRawValue,
  /// Dummy value for code to use when indexing tokens out of bounds.
  ErrEndOfFile,

  // keywords
  /// `as`
  KwAs,
  /// `bitbag`
  KwBitbag,
  /// `break`
  KwBreak,
  /// `const`
  KwConst,
  /// `continue`
  KwContinue,
  /// `else`
  KwElse,
  /// `false`
  KwFalse,
  /// `fn`
  KwFn,
  /// `for`
  KwFor,
  /// `if`
  KwIf,
  /// `impl`
  KwImpl,
  /// `let`
  KwLet,
  /// `loop`
  KwLoop,
  /// `match`
  KwMatch,
  /// `mut`
  KwMut,
  /// `return`
  KwReturn,
  /// `struct`
  KwStruct,
  /// `static`
  KwStatic,
  /// `true`
  KwTrue,
  /// `use`
  KwUse,

  // individual punctuation
  /// `!`, aka exclamation mark
  Bang = b'!',
  /// `"`
  DoubleQuote = b'"',
  /// `#`
  Hash = b'#',
  /// `$`
  Dollar = b'$',
  /// `%`
  Percent = b'%',
  /// `&`
  Ampersand = b'&',
  /// `'`
  Quote = b'\'',
  /// `(`
  OpParen = b'(',
  /// `)`
  ClParen = b')',
  /// `*`
  Star = b'*',
  /// `+`
  Plus = b'+',
  /// `,`
  Comma = b',',
  /// `-`
  Minus = b'-',
  /// `.`
  Dot = b'.',
  /// `/`
  Slash = b'/',
  /// `:`
  Colon = b':',
  /// `;`
  Semicolon = b';',
  /// `<`
  LessThan = b'<',
  /// `=`
  Equal = b'=',
  /// `>`
  GreaterThan = b'>',
  /// `?`
  Question = b'?',
  /// `@`
  At = b'@',
  /// `[`
  OpBracket = b'[',
  /// `\`
  Backslash = b'\\',
  /// `]`
  ClBracket = b']',
  /// `^`
  Caret = b'^',
  /// `_`
  Underscore = b'_',
  /// ``` ` ```
  Backtick = b'`',
  /// `{`
  OpBrace = b'{',
  /// `|`
  Pipe = b'|',
  /// `}`
  ClBrace = b'}',
  /// `~`
  Tilde = b'~',

  // merged punctuation (makes parsing much easier)
  /// `::`
  ColonColon,
  /// `==`
  EqualEqual,
  /// `!=`
  BangEqual,
  /// `..`
  DotDot,
  /// `..=`
  DotDotEqual,
  /// `+=`
  PlusEqual,
  /// `-=`
  MinusEqual,
  /// `*=`
  StarEqual,
  /// `/=`
  SlashEqual,
  /// `%=`
  PercentEqual,
  /// `&=`
  AmpersandEqual,
  /// `|=`
  PipeEqual,
  /// `^=`
  CaretEqual,

  // varying non-code elements
  /// Any number of spaces, tabs, newlines, and/or carrage returns.
  Whitespace,
  /// line comment or block comment.
  Comment,

  // varying code elements
  /// Assembly style identifier: letter or underscore followed by a letter, underscore, or digit.
  Ident,
  /// Rust style number literal with two special cases:
  /// * `%` is an allowed prefix, putting the literal into binary mode.
  /// * `$` is allowed as a prefix, putting the literal into hex mode.
  LitNum,
  /// Rust style string or raw string.
  LitStr,
}
