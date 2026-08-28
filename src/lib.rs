#![allow(unused_mut)]
#![allow(unused_braces)]
#![allow(unused_parens)]
#![allow(unused_labels)]
#![warn(missing_docs)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![deny(unreachable_patterns)]

//! Yagbas is a compiler for a language of the same name.

use core::range::Range;

#[forbid(unsafe_code)]
pub mod cst;

pub mod tokenizer;

/// Shorthand to make a [Range]
#[inline(always)]
const fn r(start: usize, end: usize) -> Range<usize> {
  Range { start, end }
}
