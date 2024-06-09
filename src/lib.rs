#![allow(clippy::let_and_return)]
#![allow(unused_imports)]

use chumsky::{extra::Err, input::SpannedInput, prelude::*};
use src_files::FileSpan;
use token::Token;
use token_tree::TokenTree;

pub mod src_files;
pub mod str_id;
pub mod token;
pub mod token_tree;

pub type ErrRichToken<'a> = Err<Rich<'a, token::Token, src_files::FileSpan>>;
pub type ErrRichTokenTree<'a> = Err<Rich<'a, token_tree::TokenTree, src_files::FileSpan>>;
pub type TokenSliceInput<'a> = SpannedInput<Token, FileSpan, &'a [(Token, FileSpan)]>;
pub type TokenTreeSliceInput<'a> =
  SpannedInput<TokenTree, FileSpan, &'a [(TokenTree, FileSpan)]>;
