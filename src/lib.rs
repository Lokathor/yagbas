#![allow(clippy::let_and_return)]
#![allow(unused_imports)]
#![allow(clippy::type_complexity)]

use chumsky::{extra::Err, input::SpannedInput, prelude::*};
use item::Item;
use src_files::FileSpan;
use token::Token;
use token_tree::TokenTree;

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
