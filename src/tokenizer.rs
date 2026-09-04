//! Types and utilities for tokenizing Yagbas source code.

use TokenKind::*;

use crate::Span;

/// An individual element of Yagbas source.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
  /// The kind of token we found.
  pub kind: TokenKind,
  /// Where the token was found.
  ///
  /// Yagbas source files can't exceed 4GB.
  pub span: Span,
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
}

/// The possible kinds of token that can exist in Yagbas source.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
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
  /// variant used for the Default impl
  #[default]
  ErrDefault,

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
  bytes: &'a [u8],
  position: usize,
  span: Span,
}
impl<'a> TokenIter<'a> {
  /// Constructs a new iterator pointed to the start of the source.
  #[inline]
  pub fn new(src: &'a str) -> Self {
    debug_assert!(u32::try_from(src.len()).is_ok());
    let bytes = src.as_bytes();
    let position = 0;
    let span = Span::new(0, 0);
    Self { bytes, position, span }
  }

  /// When in bounds, get a byte then advance the span and possition.
  ///
  /// If at bounds or out of bounds then returns `None`.
  fn next_byte(&mut self) -> Option<u8> {
    if self.position < self.bytes.len() {
      let out = self.bytes[self.position];
      self.span.end += 1;
      self.position += 1;
      Some(out)
    } else {
      None
    }
  }

  /// Peek at the next source byte
  fn peek_byte(&self) -> Option<u8> {
    self.bytes.get(self.position).copied()
  }

  /// find the close to a block comment
  ///
  /// * assumes that the position is currently pointed at the '*' byte after a
  ///   '/' byte.
  fn handle_block_comment(&mut self) -> Option<Token> {
    let byte = self.next_byte().unwrap();
    debug_assert_eq!(byte, b'*');
    let mut depth = 1;
    loop {
      debug_assert!(depth > 0);
      match self.next_byte() {
        None => {
          return Some(Token {
            kind: ErrBlockCommentUnclosed,
            span: self.span,
          });
        }
        Some(b'/') => {
          // possible nested block
          if let Some(b'*') = self.peek_byte() {
            self.next_byte();
            depth += 1;
          }
        }
        Some(b'*') => {
          // possible end block
          if let Some((b'/')) = self.peek_byte() {
            self.next_byte();
            depth -= 1;
            if depth == 0 {
              break;
            }
          }
        }
        Some(_) => {}
      }
    }
    Some(Token { kind: Comment, span: self.span })
  }

  fn handle_literal_str(&mut self) -> Option<Token> {
    let mut backslash_count = 0;
    loop {
      match self.next_byte() {
        None => {
          return Some(Token { kind: ErrLitStrUnclosed, span: self.span });
        }
        Some(b'\\') => {
          backslash_count += 1;
        }
        Some(b'"') => {
          if backslash_count % 2 != 0 {
            backslash_count = 0;
            continue;
          } else {
            break;
          }
        }
        Some(_) => {
          backslash_count = 0;
        }
      }
    }
    Some(Token { kind: LitStr, span: self.span })
  }

  fn handle_literal_raw_value(&mut self) -> Option<Token> {
    debug_assert_eq!(self.peek_byte().unwrap(), b'#');
    let mut hash_count = 0;
    while let Some(b'#') = self.peek_byte() {
      hash_count += 1;
      self.next_byte();
    }
    match self.next_byte() {
      Some(b'"') => {}
      _ => return Some(Token { kind: ErrBadRawValue, span: self.span }),
    }
    debug_assert!(hash_count > 0);
    'find_double_quote: loop {
      match self.next_byte() {
        None => {
          return Some(Token { kind: ErrLitRawStrUnclosed, span: self.span });
        }
        Some(b'"') => {
          let mut remaining = hash_count;
          'count_hashes: while remaining > 0 {
            match self.peek_byte() {
              None => {
                return Some(Token {
                  kind: ErrLitRawStrUnclosed,
                  span: self.span,
                });
              }
              Some(b'#') => {
                self.next_byte();
              }
              Some(_) => {
                continue 'find_double_quote;
              }
            }
            remaining -= 1;
          }
          break 'find_double_quote;
        }
        Some(_) => {}
      }
    }
    Some(Token { kind: LitStr, span: self.span })
  }

  fn handle_literal_num(&mut self) -> Option<Token> {
    while let Some(b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z' | b'_') =
      self.peek_byte()
    {
      self.next_byte();
    }
    Some(Token { kind: LitNum, span: self.span })
  }

  fn handle_keyword_or_ident(&mut self) -> Option<Token> {
    while let Some(b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z' | b'_') =
      self.peek_byte()
    {
      self.next_byte();
    }
    let captured = &self.bytes[self.span.as_range()];
    let kind = match captured {
      b"as" => KwAs,
      b"bitbag" => KwBitbag,
      b"break" => KwBreak,
      b"const" => KwConst,
      b"continue" => KwContinue,
      b"else" => KwElse,
      b"enum" => KwEnum,
      b"false" => KwFalse,
      b"fn" => KwFn,
      b"for" => KwFor,
      b"if" => KwIf,
      b"impl" => KwImpl,
      b"in" => KwIn,
      b"let" => KwLet,
      b"loop" => KwLoop,
      b"match" => KwMatch,
      b"mmio" => KwMmio,
      b"mut" => KwMut,
      b"return" => KwReturn,
      b"struct" => KwStruct,
      b"static" => KwStatic,
      b"true" => KwTrue,
      b"use" => KwUse,
      _ => Ident,
    };
    Some(Token { kind, span: self.span })
  }
}
impl<'a> Iterator for TokenIter<'a> {
  type Item = Token;

  fn next(&mut self) -> Option<Self::Item> {
    // reset the span
    self.span.start = self.position as u32;
    self.span.end = self.position as u32;
    //
    match self.next_byte()? {
      // whitespace
      b' ' | b'\t' | b'\r' | b'\n' => {
        while let Some(b' ' | b'\t' | b'\r' | b'\n') = self.peek_byte() {
          self.next_byte();
        }
        Some(Token { kind: Whitespace, span: self.span })
      }
      // comments
      b'/' => match self.peek_byte() {
        Some(b'*') => self.handle_block_comment(),
        Some(b'/') => {
          loop {
            match self.peek_byte() {
              Some(b'\r') | Some(b'\n') => break,
              None => break,
              _ => {
                self.next_byte();
              }
            }
          }
          Some(Token { kind: Comment, span: self.span })
        }
        Some(b'=') => {
          self.next_byte();
          Some(Token { kind: SlashEqual, span: self.span })
        }
        _ => Some(Token { kind: Slash, span: self.span }),
      },
      b'*' => match self.peek_byte() {
        Some(b'/') => {
          self.next_byte();
          Some(Token { kind: ErrBlockCommentExtraClose, span: self.span })
        }
        Some(b'=') => {
          self.next_byte();
          Some(Token { kind: StarEqual, span: self.span })
        }
        _ => Some(Token { kind: Star, span: self.span }),
      },
      // string literals
      b'"' => self.handle_literal_str(),
      b'r' if self.peek_byte().map(|b| b == b'#').unwrap_or(false) => {
        self.handle_literal_raw_value()
      }
      // number literals
      b'$' => match self.peek_byte() {
        Some(b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z') => {
          self.handle_literal_num()
        }
        _ => Some(Token { kind: Dollar, span: self.span }),
      },
      b'%' => match self.peek_byte() {
        Some(b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z') => {
          self.handle_literal_num()
        }
        Some(b'=') => {
          self.next_byte().unwrap();
          Some(Token { kind: PercentEqual, span: self.span })
        }
        _ => Some(Token { kind: Percent, span: self.span }),
      },
      b'0'..=b'9' => self.handle_literal_num(),
      // keywords, idents
      b'A'..=b'Z' | b'a'..=b'z' | b'_' => self.handle_keyword_or_ident(),
      // double punctuation
      b':' if self.peek_byte() == Some(b':') => {
        self.next_byte();
        Some(Token { kind: ColonColon, span: self.span })
      }
      b'=' if self.peek_byte() == Some(b'=') => {
        self.next_byte();
        Some(Token { kind: EqualEqual, span: self.span })
      }
      b'!' if self.peek_byte() == Some(b'=') => {
        self.next_byte();
        Some(Token { kind: BangEqual, span: self.span })
      }
      b'+' if self.peek_byte() == Some(b'=') => {
        self.next_byte();
        Some(Token { kind: PlusEqual, span: self.span })
      }
      b'-' if self.peek_byte() == Some(b'=') => {
        self.next_byte();
        Some(Token { kind: MinusEqual, span: self.span })
      }
      b'&' if self.peek_byte() == Some(b'=') => {
        self.next_byte();
        Some(Token { kind: AmpersandEqual, span: self.span })
      }
      b'|' if self.peek_byte() == Some(b'=') => {
        self.next_byte();
        Some(Token { kind: PipeEqual, span: self.span })
      }
      b'^' if self.peek_byte() == Some(b'=') => {
        self.next_byte();
        Some(Token { kind: CaretEqual, span: self.span })
      }
      b'.' if self.peek_byte() == Some(b'.') => {
        self.next_byte(); // consume second '.'
        // possible '=' after the second '.'
        match self.peek_byte() {
          Some(b'=') => {
            self.next_byte(); // consume '='
            Some(Token { kind: DotDotEqual, span: self.span })
          }
          _ => Some(Token { kind: DotDot, span: self.span }),
        }
      }
      // fallback for all other punctuation cases
      x @ b'!'..=b'/' | x @ b':'..=b'@' | x @ b'['..=b'`' | x @ b'{'..=b'~' => {
        let t = core::mem::transmute::<u8, TokenKind>;
        // Safety: all bytes in the pattern are variants within the TokenKind enum.
        Some(Token { kind: unsafe { t(x) }, span: self.span })
      }
      // otherwise it's out of range
      ..=0x1F | 0x7F.. => Some(Token { kind: ErrUnknown, span: self.span }),
    }
  }
}

/// Alternative way to make a [TokenIter]
#[inline]
pub fn tokenize(src: &str) -> TokenIter<'_> {
  TokenIter::new(src)
}
