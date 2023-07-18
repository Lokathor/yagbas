#![allow(unused_imports)]

use chumsky::{
  extra::ParserExtra,
  input::{BorrowInput, SpannedInput, ValueInput},
  prelude::*,
  primitive::Just,
  span::{SimpleSpan, Span},
  IterParser, ParseResult, Parser,
};
use core::ops::Range;
use std::{
  borrow::Cow,
  collections::HashSet,
  sync::{OnceLock, PoisonError, RwLock},
};

pub mod token;
use token::{Token, Token::*};

pub mod token_tree;
use token_tree::{TokenTree, TokenTree::*};

pub mod const_expr;

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
pub fn pipe<'a>() -> impl Parser<'a, TokenTreeSlice<'a>, (), ErrRichTokenTree<'a>> + Clone
{
  just(Lone(Punct('|'))).ignored()
}
pub fn caret<'a>() -> impl Parser<'a, TokenTreeSlice<'a>, (), ErrRichTokenTree<'a>> + Clone
{
  just(Lone(Punct('^'))).ignored()
}
pub fn colon<'a>() -> impl Parser<'a, TokenTreeSlice<'a>, (), ErrRichTokenTree<'a>> + Clone
{
  just(Lone(Punct(':'))).ignored()
}
pub fn ampersand<'a>(
) -> impl Parser<'a, TokenTreeSlice<'a>, (), ErrRichTokenTree<'a>> + Clone {
  just(Lone(Punct('&'))).ignored()
}
