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
pub mod const_decl;
pub mod token_tree;

use comment_filter::*;
use token_tree::*;
use Token::*;
use TokenTree::*;

pub type ErrRichToken<'a> = extra::Err<Rich<'a, Token>>;
pub type ErrRichTokenTree<'a> = extra::Err<Rich<'a, TokenTree>>;

pub(crate) fn spanned<T>(t: T, span: SimpleSpan) -> (T, SimpleSpan) {
  (t, span)
}
