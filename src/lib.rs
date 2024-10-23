#![allow(unused)]
#![allow(clippy::let_and_return)]
#![allow(clippy::type_complexity)]

//! Yagbas is a compiler for a language of the same name.
//!
//! * This package is on crates.io so that people can `cargo install yagbas` and
//!   get the compiler that way.
//! * Only the compiler's command line interface and the language that it
//!   compiles are covered by the semantic version number.
//! * The library content of the `yagbas` package (the documentation you're
//!   reading right now) is **NOT** covered by the semantic version of the
//!   package.

use chumsky::{extra::Err, input::SpannedInput, prelude::*};
use item::Item;
use src_files::FileSpan;
use token::Token;
use token_tree::TokenTree;

pub mod asm;
pub mod item;
pub mod src_files;
pub mod str_id;
pub mod token;
pub mod token_tree;

pub type ErrRichToken<'a> = Err<Rich<'a, Token, FileSpan>>;
pub type ErrRichTokenTree<'a> = Err<Rich<'a, TokenTree, FileSpan>>;
pub type SliceInput<'a, T> = SpannedInput<T, FileSpan, &'a [(T, FileSpan)]>;
pub type TokenSliceInput<'a> = SliceInput<'a, Token>;
pub type TokenTreeSliceInput<'a> = SliceInput<'a, TokenTree>;
