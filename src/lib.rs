//#![forbid(unsafe_code)]
#![allow(unused_mut)]
#![allow(unused_braces)]
#![allow(unused_parens)]
#![allow(unused_imports)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![deny(unreachable_patterns)]
#![allow(unused_labels)]

//! Yagbas is a compiler for a language of the same name.

pub mod parser;
pub mod tokenizer;
