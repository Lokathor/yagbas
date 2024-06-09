#![allow(clippy::let_and_return)]
#![allow(unused_imports)]

use chumsky::prelude::*;

pub mod src_files;
pub mod str_id;
pub mod token;
pub mod token_tree;

pub type ErrRichToken<'a> = extra::Err<Rich<'a, token::Token, src_files::FileSpan>>;
pub type ErrRichTokenTree<'a> =
  extra::Err<Rich<'a, token_tree::TokenTree, src_files::FileSpan>>;
