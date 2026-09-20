#![allow(unused_mut)]
#![allow(unused_braces)]
#![allow(unused_parens)]
#![allow(unused_labels)]
#![allow(clippy::needless_return)]
#![allow(clippy::field_reassign_with_default)]
//#![warn(missing_docs)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![deny(unreachable_patterns)]
#![allow(clippy::useless_format)]

//! Yagbas is a compiler for a language of the same name.
//!
//! The SemVer version of this crate does **not** refer to the crate library. It
//! only refers to the language/compiler portion of the project. Contents of the
//! library and its operation may change at any time.

use crate::non_max_u32::NonMaxU32;

pub mod kvec;
pub mod non_max_u32;
pub mod os_str_id;

#[forbid(unsafe_code)]
pub mod operators;

pub mod tokenizer;

#[forbid(unsafe_code)]
pub mod cst;

#[forbid(unsafe_code)]
pub mod ast;

#[forbid(unsafe_code)]
pub mod ir_nameres_typecheck;

/// A span within a source file.
///
/// Because we use `u32` positions, Yagbas source files are limited in size to
/// 4GB, which is entirely reasonable.
#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
  /// the start of the span
  pub start: NonMaxU32,
  /// the **exclusive** end of the span.
  pub end: u32,
}
impl Span {
  /// Makes the new span.
  pub const fn new(start: u32, end: u32) -> Self {
    Self { start: NonMaxU32::try_new(start).unwrap(), end }
  }
  /// Convert the span to a [Range], so you can index with it.
  pub const fn as_range(self) -> core::ops::Range<usize> {
    (self.start.get() as usize)..(self.end as usize)
  }
}
impl core::fmt::Debug for Span {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    core::fmt::Debug::fmt(&self.as_range(), f)
  }
}
// todo: make this a non-max u64 for quick hashing?
