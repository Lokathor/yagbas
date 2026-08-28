//! Types and utilities for tokenizing Yagbas source code.

use TokenKind::*;
use core::iter::{Copied, Enumerate, Peekable};
use core::slice::Iter;

/// An individual element of Yagbas source.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
  /// The kind of token we found.
  pub kind: TokenKind,
  /// Where the token was found.
  ///
  /// Yagbas source files can't exceed 4GB.
  pub position: u32,
}
impl Token {
  /// If the token's kind is an error kind.
  pub const fn is_error(self) -> bool {
    use TokenKind::*;
    matches!(
      self.kind,
      ErrUnknown
        | ErrBadRawValue
        | ErrBlockCommentExtraClose
        | ErrBlockCommentUnclosed
        | ErrEndOfFile
        | ErrLitRawStrUnclosed
        | ErrLitStrUnclosed,
    )
  }
  /// Gets the span of a token within the source string it came from.
  ///
  /// * If the source string is incorrect for this token, this could give incorrect answers or even trigger a panic.
  #[track_caller]
  pub fn span_within(self, src: &str) -> core::ops::Range<usize> {
    let pos_usize = self.position as usize;
    let sub_str = &src[pos_usize..];
    let mut it = tokenize(sub_str);
    let _self_token = it.next();
    let len = it.next().map(|tk| tk.position as usize).unwrap_or(sub_str.len());
    pos_usize..(pos_usize + len)
  }
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
  /// `enum`
  KwEnum,
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
  /// `in`
  KwIn,
  /// `let`
  KwLet,
  /// `loop`
  KwLoop,
  /// `match`
  KwMatch,
  /// `mmio`
  KwMmio,
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

/// An iterator over a module's source code which produces [Token] values.
#[derive(Debug, Clone)]
pub struct TokenIter<'a> {
  src: &'a str,
  bytes: Peekable<Enumerate<Copied<Iter<'a, u8>>>>,
}
impl<'a> TokenIter<'a> {
  /// Constructs a new iterator pointed to the start of the source.
  #[inline]
  pub fn new(src: &'a str) -> Self {
    Self { src, bytes: src.as_bytes().iter().copied().enumerate().peekable() }
  }

  fn handle_block_comment(&mut self, position: u32) -> Option<Token> {
    let mut depth = 1;
    let b = self.bytes.next().unwrap().1;
    debug_assert_eq!(b, b'*');
    loop {
      debug_assert!(depth > 0);
      match self.bytes.next() {
        None => return Some(Token { kind: ErrBlockCommentUnclosed, position }),
        Some((_, b'/')) => {
          // possible nested block
          if let Some((_, b'*')) = self.bytes.peek() {
            self.bytes.next();
            depth += 1;
          }
        }
        Some((_, b'*')) => {
          // possible end block
          if let Some((_, b'/')) = self.bytes.peek() {
            self.bytes.next();
            depth -= 1;
            if depth == 0 {
              break;
            }
          }
        }
        Some(_) => {}
      }
    }
    Some(Token { kind: Comment, position })
  }

  fn handle_literal_str(&mut self, position: u32) -> Option<Token> {
    let mut backslash_count = 0;
    loop {
      match self.bytes.next() {
        None => {
          return Some(Token { kind: ErrLitStrUnclosed, position });
        }
        Some((_, b'\\')) => {
          backslash_count += 1;
        }
        Some((_, b'"')) => {
          if backslash_count % 2 != 0 {
            backslash_count = 0;
            continue;
          } else {
            break;
          }
        }
        _ => backslash_count = 0,
      }
    }
    Some(Token { kind: LitStr, position })
  }

  fn handle_literal_raw_value(&mut self, position: u32) -> Option<Token> {
    debug_assert_eq!(self.bytes.peek().unwrap().1, b'#');
    let mut hash_count = 0;
    while let Some((_, b'#')) = self.bytes.peek() {
      hash_count += 1;
      self.bytes.next();
    }
    match self.bytes.next() {
      Some((_, b'"')) => (),
      _ => return Some(Token { kind: ErrBadRawValue, position }),
    }
    debug_assert!(hash_count > 0);
    'find_double_quote: loop {
      match self.bytes.next() {
        None => return Some(Token { kind: ErrLitRawStrUnclosed, position }),
        Some((_, b'"')) => {
          let mut remaining = hash_count;
          'count_hashes: while remaining > 0 {
            match self.bytes.peek() {
              None => {
                return Some(Token { kind: ErrLitRawStrUnclosed, position });
              }
              Some((_x, b'#')) => {
                self.bytes.next();
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
    Some(Token { kind: LitStr, position })
  }

  fn handle_literal_num(&mut self, position: u32) -> Option<Token> {
    while let Some((_, b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z' | b'_')) =
      self.bytes.peek()
    {
      self.bytes.next();
    }
    Some(Token { kind: LitNum, position })
  }

  fn handle_keyword_or_ident(&mut self, position: u32) -> Option<Token> {
    let start = position as usize;
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
      "enum" => KwEnum,
      "false" => KwFalse,
      "fn" => KwFn,
      "for" => KwFor,
      "if" => KwIf,
      "impl" => KwImpl,
      "in" => KwIn,
      "let" => KwLet,
      "loop" => KwLoop,
      "match" => KwMatch,
      "mmio" => KwMmio,
      "mut" => KwMut,
      "return" => KwReturn,
      "struct" => KwStruct,
      "static" => KwStatic,
      "true" => KwTrue,
      "use" => KwUse,
      _ => Ident,
    };
    Some(Token { kind, position })
  }
}
impl<'a> Iterator for TokenIter<'a> {
  type Item = Token;

  fn next(&mut self) -> Option<Self::Item> {
    let (position_usize, byte) = self.bytes.next()?;
    debug_assert!(u32::try_from(position_usize).is_ok());
    let position = position_usize as u32;
    match byte {
      // whitespace
      b' ' | b'\t' | b'\r' | b'\n' => {
        while let Some((_, b' ' | b'\t' | b'\r' | b'\n')) = self.bytes.peek() {
          self.bytes.next();
        }
        Some(Token { kind: Whitespace, position })
      }
      // comments
      b'/' => match self.bytes.peek() {
        Some((_, b'*')) => self.handle_block_comment(position),
        Some((_, b'/')) => {
          loop {
            match self.bytes.peek() {
              Some((_, b'\r')) | Some((_, b'\n')) => break,
              None => break,
              _ => {
                let _ = self.bytes.next();
              }
            }
          }
          Some(Token { kind: Comment, position })
        }
        Some((_, b'=')) => {
          self.bytes.next();
          Some(Token { kind: SlashEqual, position })
        }
        _ => Some(Token { kind: Slash, position }),
      },
      b'*' => match self.bytes.peek() {
        Some((_, b'/')) => {
          self.bytes.next();
          Some(Token { kind: ErrBlockCommentExtraClose, position })
        }
        Some((_, b'=')) => {
          self.bytes.next();
          Some(Token { kind: StarEqual, position })
        }
        _ => Some(Token { kind: Star, position }),
      },
      // string literals
      b'"' => self.handle_literal_str(position),
      b'r' if self.bytes.peek().map(|b| b.1 == b'#').unwrap_or(false) => {
        self.handle_literal_raw_value(position)
      }
      // number literals
      b'$' => match self.bytes.peek() {
        Some((_, b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z')) => {
          self.handle_literal_num(position)
        }
        _ => Some(Token { kind: Dollar, position }),
      },
      b'%' => match self.bytes.peek() {
        Some((_, b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z')) => {
          self.handle_literal_num(position)
        }
        Some((_, b'=')) => {
          self.bytes.next().unwrap();
          Some(Token { kind: PercentEqual, position })
        }
        _ => Some(Token { kind: Percent, position }),
      },
      b'0'..=b'9' => self.handle_literal_num(position),
      // keywords, idents
      b'A'..=b'Z' | b'a'..=b'z' | b'_' => {
        self.handle_keyword_or_ident(position)
      }
      // double punctuation
      b':' if self.bytes.peek().map_or(0, |(_, b)| *b) == b':' => {
        let _ = self.bytes.next();
        Some(Token { kind: ColonColon, position })
      }
      b'=' if self.bytes.peek().map_or(0, |(_, b)| *b) == b'=' => {
        let _ = self.bytes.next();
        Some(Token { kind: EqualEqual, position })
      }
      b'!' if self.bytes.peek().map_or(0, |(_, b)| *b) == b'=' => {
        let _ = self.bytes.next();
        Some(Token { kind: BangEqual, position })
      }
      b'+' if self.bytes.peek().map_or(0, |(_, b)| *b) == b'=' => {
        let _ = self.bytes.next();
        Some(Token { kind: PlusEqual, position })
      }
      b'-' if self.bytes.peek().map_or(0, |(_, b)| *b) == b'=' => {
        let _ = self.bytes.next();
        Some(Token { kind: MinusEqual, position })
      }
      b'&' if self.bytes.peek().map_or(0, |(_, b)| *b) == b'=' => {
        let _ = self.bytes.next();
        Some(Token { kind: AmpersandEqual, position })
      }
      b'|' if self.bytes.peek().map_or(0, |(_, b)| *b) == b'=' => {
        let _ = self.bytes.next();
        Some(Token { kind: PipeEqual, position })
      }
      b'^' if self.bytes.peek().map_or(0, |(_, b)| *b) == b'=' => {
        let _ = self.bytes.next();
        Some(Token { kind: CaretEqual, position })
      }
      b'.' if self.bytes.peek().map_or(0, |(_, b)| *b) == b'.' => {
        let _ = self.bytes.next(); // consume second '.'
        // possible '=' after the second '.'
        match self.bytes.peek().map_or(0, |(_, b)| *b) {
          b'=' => {
            let _ = self.bytes.next();
            Some(Token { kind: DotDotEqual, position })
          }
          _ => Some(Token { kind: DotDot, position }),
        }
      }
      // fallback for all other punctuation cases
      b'!'..=b'/' | b':'..=b'@' | b'['..=b'`' | b'{'..=b'~' => {
        let t = core::mem::transmute::<u8, TokenKind>;
        // Safety: all bytes in the pattern are variants within the TokenKind enum.
        Some(Token { kind: unsafe { t(byte) }, position })
      }
      // otherwise it's out of range
      ..=0x1F | 0x7F.. => Some(Token { kind: ErrUnknown, position }),
    }
  }
}

/// Alternative way to make a [TokenIter]
#[inline]
pub fn tokenize(src: &str) -> TokenIter<'_> {
  TokenIter::new(src)
}
