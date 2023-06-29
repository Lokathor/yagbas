#![allow(unused_imports)]

use core::ops::Range;
use std::{
  collections::HashSet,
  sync::{OnceLock, PoisonError, RwLock},
};

use chumsky::span::Span;

pub mod disassemble;
pub mod lexer;
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
    let leaked: StaticStr = Box::leak(s.to_string().into_boxed_str());
    write.insert(leaked);
    leaked
  }
}
