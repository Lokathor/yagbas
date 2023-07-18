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
pub type TokenSlice<'a> = SpannedInput<Token, SimpleSpan, &'a [(Token, SimpleSpan)]>;
pub type InputSlice<'a, T> = SpannedInput<T, SimpleSpan, &'a [(T, SimpleSpan)]>;
pub type TokenTreeSlice<'a> =
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstExpr {
  Value(i32),
  Ident(StaticStr),
  Add(Box<(Self, SimpleSpan)>, Box<(Self, SimpleSpan)>),
  Sub(Box<(Self, SimpleSpan)>, Box<(Self, SimpleSpan)>),
  UnknownError(CowStr),
}
// https://gist.github.com/zesterer/e0a896ef16fdc95a4749851ebb0d8461 ???
impl ConstExpr {
  pub fn parser<'a>(
  ) -> impl Parser<'a, TokenTreeSlice<'a>, Self, ErrRichTokenTree<'a>> + Clone {
    recursive(|expr| {
      let ident = ident().map(Self::Ident);

      let lit = num_lit().map(|lit| match lit_to_value(lit) {
        Ok(i) => Self::Value(i),
        Err(e) => Self::UnknownError(e),
      });

      let parens = expr.clone().nested_in(select_ref! {
        Parens(tokens) = span => {
          let span: SimpleSpan = span;
          tokens.spanned(span)
        },
      });

      let atom = choice((ident, lit, parens));

      let neg = minus().ignore_then(atom.clone().map_with_span(id2)).map(
        |(rhs, span)| match &rhs {
          ConstExpr::Value(r) => ConstExpr::Value(r.wrapping_neg()),
          _ => ConstExpr::Sub(
            Box::new((ConstExpr::Value(0), SimpleSpan::from(0..0))),
            Box::new((rhs, span)),
          ),
        },
      );
      let pos = plus().ignore_then(atom.clone());

      let atom_plus_atom = atom
        .clone()
        .map_with_span(id2)
        .then_ignore(plus())
        .then(atom.clone().map_with_span(id2))
        .map(|((lhs, lh_span), (rhs, rh_span))| match (&lhs, &rhs) {
          (ConstExpr::Value(l), ConstExpr::Value(r)) if l.checked_add(*r).is_some() => {
            ConstExpr::Value(*l + *r)
          }
          _ => ConstExpr::Add(Box::new((lhs, lh_span)), Box::new((rhs, rh_span))),
        });
      let atom_minus_atom = atom
        .clone()
        .map_with_span(id2)
        .then_ignore(minus())
        .then(atom.clone().map_with_span(id2))
        .map(|((lhs, lh_span), (rhs, rh_span))| match (&lhs, &rhs) {
          (ConstExpr::Value(l), ConstExpr::Value(r)) if l.checked_sub(*r).is_some() => {
            ConstExpr::Value(*l - *r)
          }
          _ => ConstExpr::Sub(Box::new((lhs, lh_span)), Box::new((rhs, rh_span))),
        });

      choice((atom_plus_atom, atom_minus_atom, neg, pos, atom))
    })
  }
}

pub fn ident<'a>(
) -> impl Parser<'a, TokenTreeSlice<'a>, StaticStr, ErrRichTokenTree<'a>> + Clone {
  select! {
    Lone(Ident(i)) => i,
  }
}
pub fn num_lit<'a>(
) -> impl Parser<'a, TokenTreeSlice<'a>, StaticStr, ErrRichTokenTree<'a>> + Clone {
  select! {
    Lone(NumLit(n)) => n,
  }
}

pub fn plus<'a>() -> impl Parser<'a, TokenTreeSlice<'a>, (), ErrRichTokenTree<'a>> + Clone
{
  just(Lone(Punct('+'))).ignored()
}
pub fn minus<'a>() -> impl Parser<'a, TokenTreeSlice<'a>, (), ErrRichTokenTree<'a>> + Clone
{
  just(Lone(Punct('-'))).ignored()
}
pub fn bang<'a>() -> impl Parser<'a, TokenTreeSlice<'a>, (), ErrRichTokenTree<'a>> + Clone
{
  just(Lone(Punct('!'))).ignored()
}
pub fn semicolon<'a>(
) -> impl Parser<'a, TokenTreeSlice<'a>, (), ErrRichTokenTree<'a>> + Clone {
  just(Lone(Punct(';'))).ignored()
}
pub fn comma<'a>() -> impl Parser<'a, TokenTreeSlice<'a>, (), ErrRichTokenTree<'a>> + Clone
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
