use core::ops::{Deref, DerefMut, Range};

use crate::{lexer::Token, StaticStr};
use chumsky::{
  input::{BoxedStream, Stream, ValueInput},
  prelude::*,
  primitive::*,
  select, Parser,
};
use logos::Span;

pub mod comment_filter;
pub mod token_tree;

use comment_filter::*;
use token_tree::*;

pub type MyParseErr<'a> = extra::Err<Rich<'a, Token>>;
