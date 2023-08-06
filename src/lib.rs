#![allow(unused_imports)]
#![allow(clippy::let_and_return)]

use chumsky::{input::SpannedInput, prelude::*, span::SimpleSpan, *};

pub mod str_id;
use str_id::StrID;

pub mod src_files;

pub mod token;
use token::{Token, Token::*};

pub mod token_tree;
use token_tree::{TokenTree, TokenTree::*};

pub type ErrRichToken<'a> = extra::Err<Rich<'a, Token>>;
pub type ErrRichTokenTree<'a> = extra::Err<Rich<'a, TokenTree>>;

pub type TokenSlice<'a> = SpannedInput<Token, SimpleSpan, &'a [(Token, SimpleSpan)]>;

//pub mod util_junk;
//use util_junk::*;
//
//pub mod const_expr;
//pub mod item;
//pub mod static_expr;
