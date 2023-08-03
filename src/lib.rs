#![allow(unused_imports)]
#![allow(clippy::let_and_return)]

use chumsky::{prelude::*, span::SimpleSpan, *};

pub mod str_id;
use str_id::StrID;

pub mod static_str;
use static_str::{CowStr, StaticStr};

pub mod token;
use token::{Token, Token::*};

pub mod token_tree;
use token_tree::{TokenTree, TokenTree::*};

pub mod util_junk;
use util_junk::*;

pub mod const_expr;
pub mod item;
pub mod static_expr;
