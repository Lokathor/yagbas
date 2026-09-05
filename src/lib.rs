#![allow(unused_mut)]
#![allow(unused_braces)]
#![allow(unused_parens)]
#![allow(unused_labels)]
#![allow(clippy::needless_return)]
#![allow(clippy::field_reassign_with_default)]
#![warn(missing_docs)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![deny(unreachable_patterns)]

//! Yagbas is a compiler for a language of the same name.
//!
//! The SemVer version of this crate does **not** refer to the crate library. It
//! only refers to the language/compiler portion of the project. Contents of the
//! library and its operation may change at any time.

use core::ops::Range;

/// A span within a source file.
///
/// Yagbas source files are limited in size to 4GB, which is pretty reasonable
/// because we're compiling code for the game boy here.
#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
  /// start of the span
  pub start: u32,
  /// exclusive end of the span.
  pub end: u32,
}
impl Span {
  /// Makes the new span.
  pub const fn new(start: u32, end: u32) -> Self {
    Self { start, end }
  }
  /// Convert the span to a [Range], so you can index with it.
  pub const fn as_range(self) -> Range<usize> {
    (self.start as usize)..(self.end as usize)
  }
}
impl core::fmt::Debug for Span {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    core::fmt::Debug::fmt(&self.as_range(), f)
  }
}

pub mod tokenizer;

#[forbid(unsafe_code)]
pub mod cst;

#[forbid(unsafe_code)]
#[allow(missing_docs)]
pub mod ast;

#[forbid(unsafe_code)]
#[allow(missing_docs)]
pub mod ir_nameres;
