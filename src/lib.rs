//#![forbid(unsafe_code)]
#![allow(unused_mut)]
#![allow(unused_braces)]
#![allow(unused_parens)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![deny(unreachable_patterns)]
#![allow(unused_labels)]

//! Yagbas is a compiler for a language of the same name.

use core::range::Range;

pub mod cst;
pub mod tokenizer;

/// Shorthand to make a [Range]
#[inline(always)]
const fn r(start: usize, end: usize) -> Range<usize> {
  Range { start, end }
}
