#![allow(unused_imports)]

use core::ops::Range;
use std::{
  collections::HashSet,
  sync::{OnceLock, PoisonError, RwLock},
};

use chumsky::{
  extra::ParserExtra,
  input::SpannedInput,
  prelude::Input,
  span::{SimpleSpan, Span},
  ParseResult, Parser,
};

pub mod comment_filter;
pub mod token;
pub mod token_tree;

pub mod disassemble;
pub mod parser;

pub type StaticStr = &'static str;

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

pub fn spanless<T>(spanned: &[(T, SimpleSpan)]) -> impl Iterator<Item = &T> {
  spanned.iter().map(|(t, _s)| t)
}

pub type InputSlice<'a, T> = SpannedInput<T, SimpleSpan, &'a [(T, SimpleSpan)]>;

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
