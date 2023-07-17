#![allow(unused_imports)]

use core::ops::Range;
use std::{
  borrow::Cow,
  collections::HashSet,
  sync::{OnceLock, PoisonError, RwLock},
};

use chumsky::{
  extra::ParserExtra,
  input::{BorrowInput, SpannedInput, ValueInput},
  prelude::*,
  primitive::Just,
  span::{SimpleSpan, Span},
  IterParser, ParseResult, Parser,
};
use rayon::prelude::*;

pub mod token;
use token::{Token, Token::*};

pub mod token_tree;
use token_tree::{TokenTree, TokenTree::*};

pub type StaticStr = &'static str;
pub type CowStr = Cow<'static, str>;
pub type ErrRichToken<'a> = extra::Err<Rich<'a, Token>>;
pub type ErrRichTokenTree<'a> = extra::Err<Rich<'a, TokenTree>>;
pub type InputSlice<'a, T> = SpannedInput<T, SimpleSpan, &'a [(T, SimpleSpan)]>;
pub type TokenTreeInput<'a> =
  SpannedInput<TokenTree, SimpleSpan, &'a [(TokenTree, SimpleSpan)]>;

/// Convert any str into a static str, using a global cache.
#[inline]
pub fn static_str(s: &str) -> StaticStr {
  static STR_CACHE: OnceLock<RwLock<HashSet<StaticStr>>> = OnceLock::new();
  let rw_lock = STR_CACHE.get_or_init(|| RwLock::new(HashSet::new()));
  let read = rw_lock.read().unwrap_or_else(PoisonError::into_inner);
  if let Some(out) = read.get(s) {
    out
  } else {
    drop(read);
    let mut write = rw_lock.write().unwrap_or_else(PoisonError::into_inner);
    // It's *possible* that the string was inserted after we dropped the reader
    // before we acquired the writer, so we check a second time.
    if let Some(out) = write.get(s) {
      out
    } else {
      let leaked: StaticStr = Box::leak(s.to_string().into_boxed_str());
      write.insert(leaked);
      leaked
    }
  }
}

/// "Identity, 2-arg"
///
/// This just wraps the two values as a tuple. This is only really useful as a
/// higher order function to pass to map and similar when we want to join
/// multi-arg inputs into a single value output.
#[inline]
#[must_use]
pub const fn id2<A, B>(a: A, b: B) -> (A, B) {
  (a, b)
}

/*

pub mod ast;
pub mod block_entry;
pub mod cond;
pub mod const_decl;
pub mod const_expr;
pub mod const_formed;
pub mod inst;
pub mod inst_use;
pub mod item_decl;
pub mod item_formed;
pub mod jump_target;
pub mod label;
pub mod macro_use;
pub mod place;
pub mod place16;
pub mod place8;
pub mod place_const;
pub mod place_indirect;
pub mod place_use;
pub mod section_decl;
pub mod section_formed;

pub mod disassemble;

pub fn spanless<T>(spanned: &[(T, SimpleSpan)]) -> impl Iterator<Item = &T> {
  spanned.iter().map(|(t, _s)| t)
}

/// Runs a parser from `T` to `O` on a slice of `T`, giving a [ParseResult]
#[inline]
pub fn run_parser<'a, P, T, O, E>(
  parser: P, data: &'a [(T, SimpleSpan)],
) -> ParseResult<O, E::Error>
where
  P: Parser<'a, InputSlice<'a, T>, O, E>,
  E: ParserExtra<'a, InputSlice<'a, T>>,
  <E as ParserExtra<'a, InputSlice<'a, T>>>::State: Default,
  <E as ParserExtra<'a, InputSlice<'a, T>>>::Context: Default,
{
  // calculate the likely span value based on the first and last token, assumes
  // that the tokens are still properly in order.
  let span: SimpleSpan = if data.is_empty() {
    (0..0).into()
  } else {
    let start = data.first().unwrap().1.start;
    let end = data.last().unwrap().1.end;
    (start..end).into()
  };
  let input = data.spanned(span);
  parser.parse(input)
}

pub fn ident<'a, I>() -> impl Parser<'a, I, StaticStr, ErrRichTokenTree<'a>> + Clone
where
  I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
{
  select! {
    Lone(Ident(i)) => i,
  }
}

pub fn num_lit<'a, I>() -> impl Parser<'a, I, StaticStr, ErrRichTokenTree<'a>> + Clone
where
  I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
{
  select! {
    Lone(NumLit(i)) => i,
  }
}

pub fn bracket_group<'a, I>(
) -> impl Parser<'a, I, Vec<(TokenTree, SimpleSpan)>, ErrRichTokenTree<'a>>
where
  I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
{
  select! {
    Brackets(tts) => tts,
  }
}

pub fn brace_group<'a, I>(
) -> impl Parser<'a, I, Vec<(TokenTree, SimpleSpan)>, ErrRichTokenTree<'a>>
where
  I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
{
  select! {
    Braces(tts) => tts,
  }
}

pub fn paren_group<'a, I>(
) -> impl Parser<'a, I, Vec<(TokenTree, SimpleSpan)>, ErrRichTokenTree<'a>> + Clone
where
  I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
{
  select! {
    Parens(tts) => tts,
  }
}

pub fn colon<'a, I>() -> impl Parser<'a, I, (), ErrRichTokenTree<'a>>
where
  I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
{
  just(Lone(Punct(':'))).ignored()
}

pub fn semicolon<'a, I>() -> impl Parser<'a, I, (), ErrRichTokenTree<'a>>
where
  I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
{
  just(Lone(Punct(';'))).ignored()
}

pub fn bang<'a, I>() -> impl Parser<'a, I, (), ErrRichTokenTree<'a>> + Clone
where
  I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
{
  just(Lone(Punct('!'))).ignored()
}

pub fn equal<'a, I>() -> impl Parser<'a, I, (), ErrRichTokenTree<'a>>
where
  I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
{
  just(Lone(Punct('='))).ignored()
}

pub fn comma<'a, I>() -> impl Parser<'a, I, (), ErrRichTokenTree<'a>>
where
  I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
{
  just(Lone(Punct(','))).ignored()
}

pub fn lit_to_value(lit: &str) -> Result<i32, CowStr> {
  fn hex_to_value(hex: &str) -> Result<i32, CowStr> {
    let mut total = 0_i32;
    for ch in hex.chars().filter(|ch| *ch != '_') {
      total = total.checked_mul(16).ok_or(Cow::Borrowed("Overflow"))?;
      let value = match ch as u8 {
        b'a'..=b'f' => i32::from(ch as u8) - i32::from(b'a') + 10,
        b'A'..=b'F' => i32::from(ch as u8) - i32::from(b'A') + 10,
        b'0'..=b'9' => i32::from(ch as u8) - i32::from(b'0'),
        _ => return Err(Cow::Owned(format!("Illegal Hexadecimal Digit: `{ch}`"))),
      };
      total = total.checked_add(value).ok_or(Cow::Borrowed("Overflow"))?;
    }
    Ok(total)
  }
  fn bin_to_value(bin: &str) -> Result<i32, CowStr> {
    let mut total = 0_i32;
    for ch in bin.chars().filter(|ch| *ch != '_') {
      total = total.checked_mul(2).ok_or(Cow::Borrowed("Overflow"))?;
      let value = match ch as u8 {
        b'0'..=b'1' => i32::from(ch as u8) - i32::from(b'0'),
        _ => return Err(Cow::Owned(format!("Illegal Binary Digit: `{ch}`"))),
      };
      total = total.checked_add(value).ok_or(Cow::Borrowed("Overflow"))?;
    }
    Ok(total)
  }
  fn dec_to_value(dec: &str) -> Result<i32, CowStr> {
    let mut total = 0_i32;
    for ch in dec.chars().filter(|ch| *ch != '_') {
      total = total.checked_mul(10).ok_or(Cow::Borrowed("Overflow"))?;
      let value = match ch as u8 {
        b'0'..=b'9' => i32::from(ch as u8) - i32::from(b'0'),
        _ => return Err(Cow::Owned(format!("Illegal Decimal Digit: `{ch}`"))),
      };
      total = total.checked_add(value).ok_or(Cow::Borrowed("Overflow"))?;
    }
    Ok(total)
  }
  //
  if let Some(neg) = lit.strip_prefix('-') {
    lit_to_value(neg).map(|i| -i)
  } else if let Some(pos) = lit.strip_prefix('+') {
    lit_to_value(pos)
  } else if let Some(hex) = lit.strip_prefix('$') {
    hex_to_value(hex)
  } else if let Some(hex) = lit.strip_prefix("0x") {
    hex_to_value(hex)
  } else if let Some(bin) = lit.strip_prefix('%') {
    bin_to_value(bin)
  } else if let Some(bin) = lit.strip_prefix("0b") {
    bin_to_value(bin)
  } else {
    dec_to_value(lit)
  }
}

*/
