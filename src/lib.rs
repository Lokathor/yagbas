#![allow(unused_mut)]
#![allow(unused_braces)]
#![allow(unused_parens)]
#![allow(unused_labels)]
#![warn(missing_docs)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![deny(unreachable_patterns)]

//! Yagbas is a compiler for a language of the same name.
//!
//! The SemVer version of this crate does not refer to the crate library. It
//! only refers to the language/compiler portion of the project. Contents of the
//! library and its operation may change at any time.

pub mod tokenizer;

#[forbid(unsafe_code)]
pub mod cst;

#[forbid(unsafe_code)]
#[allow(missing_docs)]
pub mod ast;

#[forbid(unsafe_code)]
#[allow(missing_docs)]
pub mod basic_blocks;
