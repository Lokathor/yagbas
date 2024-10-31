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

use src_files::FileSpanned;
use str_id::StrID;

pub mod asm;
pub mod checks;
pub mod item;
pub mod parsing;
pub mod src_files;
pub mod statement;
pub mod str_id;
pub mod token;
pub mod token_tree;

#[derive(Debug, Clone)]
pub enum YagError {
  MultipleDefinitions { name: StrID, sites: Vec<FileSpanned<StrID>> },
}
