//! Types and utilities for tokenizing Yagbas source code.

use TokenKind::*;

use crate::Span;

/// An individual element of Yagbas source.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
  /// The kind of token we found.
  pub kind: TokenKind,
  /// Where the token was found in a source string.
  ///
  /// Yagbas source files can't exceed 4GB.
  pub span: Span,
}

/// The possible kinds of token that can exist in Yagbas source.
///
/// The ordering of these variants isn't important except that the single
/// punctuation variants must have a tag value equal to their ascii byte value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(u8)]
pub enum TokenKind {
  // error cases
  /// The lexer doesn't know what this was.
  ErrUnknownByte,
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

  // individual punctuation
  /// `!`, aka exclamation mark
  Bang = b'!',
  /// `"` (never produced, kept for transmute safety)
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
  /// `_` (never produced, kept for transmute safety)
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

  // merged punctuation (makes parts of parsing much easier)
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
  /// `->`
  MinusGreater,

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
  /// `mod`
  KwMod,
  /// `mut`
  KwMut,
  /// `ram`
  KwRam,
  /// `return`
  KwReturn,
  /// `rom`
  KwRom,
  /// `struct`
  KwStruct,
  /// `static`
  KwStatic,
  /// `super`
  KwSuper,
  /// `true`
  KwTrue,
  /// `use`
  KwUse,
  /// `while`
  KwWhile,
  /// `vol`
  KwVol,

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
impl TokenKind {
  pub const fn is_error(self) -> bool {
    matches!(
      self,
      ErrUnknownByte
        | ErrBadRawValue
        | ErrBlockCommentExtraClose
        | ErrBlockCommentUnclosed
        | ErrEndOfFile
        | ErrLitRawStrUnclosed
        | ErrLitStrUnclosed,
    )
  }

  pub const fn is_item_keyword(self) -> bool {
    matches!(
      self,
      KwFn | KwStatic | KwConst | KwStruct | KwBitbag | KwEnum | KwUse
    )
  }

  pub const fn is_keyword(self) -> bool {
    matches!(
      self,
      KwAs
        | KwBitbag
        | KwBreak
        | KwConst
        | KwContinue
        | KwElse
        | KwEnum
        | KwFalse
        | KwFn
        | KwFor
        | KwIf
        | KwImpl
        | KwIn
        | KwLet
        | KwLoop
        | KwMatch
        | KwMmio
        | KwMod
        | KwMut
        | KwRam
        | KwReturn
        | KwRom
        | KwStruct
        | KwStatic
        | KwSuper
        | KwTrue
        | KwUse
        | KwWhile
        | KwVol
    )
  }

  pub const fn is_punctuation(self) -> bool {
    matches!(
      self,
      Bang
        | DoubleQuote
        | Hash
        | Dollar
        | Percent
        | Ampersand
        | Quote
        | OpParen
        | ClParen
        | Star
        | Plus
        | Comma
        | Minus
        | Dot
        | Slash
        | Colon
        | Semicolon
        | LessThan
        | Equal
        | GreaterThan
        | Question
        | At
        | OpBracket
        | Backslash
        | ClBracket
        | Caret
        | Underscore
        | Backtick
        | OpBrace
        | Pipe
        | ClBrace
        | Tilde
        | ColonColon
        | EqualEqual
        | BangEqual
        | DotDot
        | DotDotEqual
        | PlusEqual
        | MinusEqual
        | StarEqual
        | SlashEqual
        | PercentEqual
        | AmpersandEqual
        | PipeEqual
        | CaretEqual
        | MinusGreater
    )
  }

  pub const fn fixed_str(self) -> Option<&'static str> {
    Some(match self {
      Bang => "!",
      DoubleQuote => "\"",
      Hash => "#",
      Dollar => "$",
      Percent => "%",
      Ampersand => "&",
      Quote => "'",
      OpParen => "(",
      ClParen => ")",
      Star => "*",
      Plus => "+",
      Comma => ",",
      Minus => "-",
      Dot => ".",
      Slash => "/",
      Colon => ":",
      Semicolon => ";",
      LessThan => "<",
      Equal => "=",
      GreaterThan => ">",
      Question => "?",
      At => "@",
      OpBracket => "[",
      Backslash => "\\",
      ClBracket => "]",
      Caret => "^",
      Underscore => "_",
      Backtick => "`",
      OpBrace => "{",
      Pipe => "|",
      ClBrace => "}",
      Tilde => "~",
      ColonColon => "::",
      EqualEqual => "==",
      BangEqual => "!=",
      DotDot => "..",
      DotDotEqual => "..=",
      PlusEqual => "+=",
      MinusEqual => "-=",
      StarEqual => "*=",
      SlashEqual => "/=",
      PercentEqual => "%=",
      AmpersandEqual => "&=",
      PipeEqual => "|=",
      CaretEqual => "^=",
      MinusGreater => "->",
      KwAs => "as",
      KwBitbag => "bitbag",
      KwBreak => "break",
      KwConst => "const",
      KwContinue => "continue",
      KwElse => "else",
      KwEnum => "enum",
      KwFalse => "false",
      KwFn => "fn",
      KwFor => "for",
      KwIf => "if",
      KwImpl => "impl",
      KwIn => "in",
      KwLet => "let",
      KwLoop => "loop",
      KwMatch => "match",
      KwMmio => "mmio",
      KwMod => "mod",
      KwMut => "mut",
      KwRam => "ram",
      KwReturn => "return",
      KwRom => "rom",
      KwStruct => "struct",
      KwStatic => "static",
      KwSuper => "super",
      KwTrue => "true",
      KwUse => "use",
      KwWhile => "while",
      KwVol => "vol",
      Whitespace
      | Comment
      | Ident
      | LitNum
      | LitStr
      | ErrUnknownByte
      | ErrBlockCommentUnclosed
      | ErrBlockCommentExtraClose
      | ErrLitStrUnclosed
      | ErrLitRawStrUnclosed
      | ErrBadRawValue
      | ErrEndOfFile
      | ErrDefault => return None,
    })
  }
}

/// An iterator over a module's source code which produces [Token] values.
#[derive(Debug, Clone)]
pub struct TokenIter<'a> {
  bytes: &'a [u8],
  position: usize,
  span_start: u32,
  span_end: u32,
}
impl<'a> TokenIter<'a> {
  /// Constructs a new iterator pointed to the start of the source.
  #[inline]
  pub fn new(src: &'a str) -> Self {
    debug_assert!(u32::try_from(src.len()).is_ok());
    let bytes = src.as_bytes();
    Self { bytes, position: 0, span_start: 0, span_end: 0 }
  }

  /// When in bounds, get a byte then advance the span and possition.
  ///
  /// If at bounds or out of bounds then returns `None`.
  fn next_byte(&mut self) -> Option<u8> {
    if self.position < self.bytes.len() {
      let out = self.bytes[self.position];
      self.span_end += 1;
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

  /// Wraps and returns the current span data.
  fn get_span(&self) -> Span {
    Span::new(self.span_start, self.span_end)
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
            span: self.get_span(),
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
    Some(Token { kind: Comment, span: self.get_span() })
  }

  /// With the lexer pointed at the byte just after the opening `"`, find the
  /// closing `"` that matches it.
  fn handle_literal_str(&mut self) -> Option<Token> {
    debug_assert_eq!(self.bytes[self.position - 1], b'"');
    let mut backslash_count = 0;
    loop {
      match self.next_byte() {
        None => {
          return Some(Token {
            kind: ErrLitStrUnclosed,
            span: self.get_span(),
          });
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
    Some(Token { kind: LitStr, span: self.get_span() })
  }

  /// With the lexer pointed at a `#` immediately after a `r`, finish this raw
  /// value token.
  ///
  /// * Currently only handles raw string literals.
  fn handle_literal_raw_value(&mut self) -> Option<Token> {
    debug_assert_eq!(self.peek_byte(), Some(b'#'));
    let mut hash_count = 0;
    while let Some(b'#') = self.peek_byte() {
      hash_count += 1;
      self.next_byte();
    }
    match self.next_byte() {
      Some(b'"') => {}
      _ => return Some(Token { kind: ErrBadRawValue, span: self.get_span() }),
    }
    debug_assert!(hash_count > 0);
    'find_double_quote: loop {
      match self.next_byte() {
        None => {
          return Some(Token {
            kind: ErrLitRawStrUnclosed,
            span: self.get_span(),
          });
        }
        Some(b'"') => {
          let mut remaining = hash_count;
          'count_hashes: while remaining > 0 {
            match self.peek_byte() {
              None => {
                return Some(Token {
                  kind: ErrLitRawStrUnclosed,
                  span: self.get_span(),
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
    Some(Token { kind: LitStr, span: self.get_span() })
  }

  /// Having just consumed the first byte of a number literal, this finishes
  /// that number literal.
  fn handle_literal_num(&mut self) -> Option<Token> {
    while let Some(b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z' | b'_') =
      self.peek_byte()
    {
      self.next_byte();
    }
    Some(Token { kind: LitNum, span: self.get_span() })
  }

  /// Having just consumed the first byte of a keyword or ident, finish the
  /// token.
  fn handle_keyword_or_ident(&mut self) -> Option<Token> {
    while let Some(b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z' | b'_') =
      self.peek_byte()
    {
      self.next_byte();
    }
    let span = self.get_span();
    let captured = &self.bytes[span.as_range()];
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
      b"mod" => KwMod,
      b"mut" => KwMut,
      b"ram" => KwRam,
      b"return" => KwReturn,
      b"rom" => KwRom,
      b"struct" => KwStruct,
      b"static" => KwStatic,
      b"super" => KwSuper,
      b"true" => KwTrue,
      b"use" => KwUse,
      b"while" => KwWhile,
      b"vol" => KwVol,
      _ => Ident,
    };
    Some(Token { kind, span })
  }
}
impl<'a> Iterator for TokenIter<'a> {
  type Item = Token;

  fn next(&mut self) -> Option<Self::Item> {
    // reset the span
    self.span_start = self.position as u32;
    self.span_end = self.position as u32;
    //
    match self.next_byte()? {
      // whitespace
      b' ' | b'\t' | b'\r' | b'\n' => {
        while let Some(b' ' | b'\t' | b'\r' | b'\n') = self.peek_byte() {
          self.next_byte();
        }
        Some(Token { kind: Whitespace, span: self.get_span() })
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
          Some(Token { kind: Comment, span: self.get_span() })
        }
        Some(b'=') => {
          self.next_byte();
          Some(Token { kind: SlashEqual, span: self.get_span() })
        }
        _ => Some(Token { kind: Slash, span: self.get_span() }),
      },
      b'*' => match self.peek_byte() {
        Some(b'/') => {
          self.next_byte();
          Some(Token { kind: ErrBlockCommentExtraClose, span: self.get_span() })
        }
        Some(b'=') => {
          self.next_byte();
          Some(Token { kind: StarEqual, span: self.get_span() })
        }
        _ => Some(Token { kind: Star, span: self.get_span() }),
      },
      // string literals
      b'"' => self.handle_literal_str(),
      b'r' if self.peek_byte() == Some(b'#') => self.handle_literal_raw_value(),
      // number literals
      b'$' => match self.peek_byte() {
        Some(b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z') => {
          self.handle_literal_num()
        }
        _ => Some(Token { kind: Dollar, span: self.get_span() }),
      },
      b'%' => match self.peek_byte() {
        Some(b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z') => {
          self.handle_literal_num()
        }
        Some(b'=') => {
          self.next_byte().unwrap();
          Some(Token { kind: PercentEqual, span: self.get_span() })
        }
        _ => Some(Token { kind: Percent, span: self.get_span() }),
      },
      b'0'..=b'9' => self.handle_literal_num(),
      // keywords, idents
      b'A'..=b'Z' | b'a'..=b'z' | b'_' => self.handle_keyword_or_ident(),
      // double punctuation
      b'-' if self.peek_byte() == Some(b'>') => {
        self.next_byte();
        Some(Token { kind: MinusGreater, span: self.get_span() })
      }
      b':' if self.peek_byte() == Some(b':') => {
        self.next_byte();
        Some(Token { kind: ColonColon, span: self.get_span() })
      }
      b'=' if self.peek_byte() == Some(b'=') => {
        self.next_byte();
        Some(Token { kind: EqualEqual, span: self.get_span() })
      }
      b'!' if self.peek_byte() == Some(b'=') => {
        self.next_byte();
        Some(Token { kind: BangEqual, span: self.get_span() })
      }
      b'+' if self.peek_byte() == Some(b'=') => {
        self.next_byte();
        Some(Token { kind: PlusEqual, span: self.get_span() })
      }
      b'-' if self.peek_byte() == Some(b'=') => {
        self.next_byte();
        Some(Token { kind: MinusEqual, span: self.get_span() })
      }
      b'&' if self.peek_byte() == Some(b'=') => {
        self.next_byte();
        Some(Token { kind: AmpersandEqual, span: self.get_span() })
      }
      b'|' if self.peek_byte() == Some(b'=') => {
        self.next_byte();
        Some(Token { kind: PipeEqual, span: self.get_span() })
      }
      b'^' if self.peek_byte() == Some(b'=') => {
        self.next_byte();
        Some(Token { kind: CaretEqual, span: self.get_span() })
      }
      b'.' if self.peek_byte() == Some(b'.') => {
        self.next_byte(); // consume second '.'
        // possible '=' after the second '.'
        match self.peek_byte() {
          Some(b'=') => {
            self.next_byte(); // consume '='
            Some(Token { kind: DotDotEqual, span: self.get_span() })
          }
          _ => Some(Token { kind: DotDot, span: self.get_span() }),
        }
      }
      // fallback for all other punctuation cases
      x @ b'!'..=b'/' | x @ b':'..=b'@' | x @ b'['..=b'`' | x @ b'{'..=b'~' => {
        let t = core::mem::transmute::<u8, TokenKind>;
        // Safety: all bytes in the pattern are variants within the TokenKind enum.
        Some(Token { kind: unsafe { t(x) }, span: self.get_span() })
      }
      // otherwise it's out of range
      ..=0x1F | 0x7F.. => {
        Some(Token { kind: ErrUnknownByte, span: self.get_span() })
      }
    }
  }
}

/// Alternative way to make a [TokenIter]
#[inline]
pub fn tokenize(src: &str) -> TokenIter<'_> {
  TokenIter::new(src)
}
