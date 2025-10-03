#![forbid(unsafe_code)]
#![allow(unused_imports)]
#![allow(unused_braces)]
#![allow(unused_mut)]
#![allow(clippy::needless_lifetimes)]
#![warn(missing_debug_implementations)]
#![warn(missing_copy_implementations)]

use std::ops::Range;

use crate::{FileData, FileID, Token, TokenTree, trees_of};
use chumsky::{
  extra::{Err, Full, ParserExtra, SimpleState},
  input::{BorrowInput, MapExtra, ValueInput},
  prelude::*,
};
use str_id::StrID;
use derive_more::Display;

/// This is a macro instead of a function because I can't figure out what type
/// signature to put on this expression so that Rust lets me actually use the
/// darn thing in more than one place.
macro_rules! statement_recovery_strategy {
  () => {
    via_parser(
      any()
        .and_is(statement_sep_p().not())
        .repeated()
        .at_least(1)
        .to(Statement::StatementError),
    )
  };
}

mod junk_drawer;
use junk_drawer::*;

/// Generic typed value paired with a `SimpleSpan`
#[derive(Clone, Copy, Display)]
#[display("{_0}")]
pub struct S<T>(pub T, pub SimpleSpan);
impl<T> S<T> {
  pub fn from_extras<'src, 'b, I, E>(
    t: T, ex: &mut MapExtra<'src, 'b, I, E>,
  ) -> Self
  where
    I: Input<'src, Span = SimpleSpan>,
    E: ParserExtra<'src, I>,
  {
    Self(t, ex.span())
  }
}
impl<T> core::fmt::Debug for S<T>
where
  T: core::fmt::Debug,
{
  #[inline]
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    core::fmt::Debug::fmt(&self.0, f)
  }
}

/// The Abstract Syntax Tree for the whole program.
///
/// While each individual item is from a particular source file, the AST
/// combines all the items across all the files.
#[derive(Debug, Clone)]
pub struct Ast {
  pub items: Vec<S<Item>>,
}

/// An item is basically "a top level definition within a source file".
#[derive(Debug, Clone)]
pub enum Item {
  BitStruct(AstBitStruct),
  Const(AstConst),
  Func(AstFunc),
  Static(AstStatic),
  Struct(AstStruct),
  ItemError,
}

mod bitstruct;
pub use bitstruct::*;

mod const_;
pub use const_::*;

mod func;
pub use func::*;

mod static_;
pub use static_::*;

mod struct_;
pub use struct_::*;

mod statement;
pub use statement::*;

mod expr;
pub use expr::*;

/// The registers that can be named anywhere in any expression.
///
/// This type isn't strictly necessary, we could use [StrID] in all places where
/// there's a Register value, but it's a little faster to sort out registers
/// from identifiers ahead of time.
#[derive(Debug, Clone, Copy, Display)]
pub enum Register {
  A,
  B,
  C,
  D,
  E,
  H,
  L,
  AF,
  BC,
  DE,
  HL,
  SP,
}

#[derive(Debug, Clone)]
pub enum AstParseError {
  Token(Rich<'static, Token>),
  TokenTree(Rich<'static, TokenTree>),
}

pub fn items_of(
  mut file_data: &'static FileData,
) -> (Vec<S<Item>>, Vec<AstParseError>) {
  let (trees, tree_parse_errors) = trees_of(file_data.content());
  let mut ast_parse_errors =
    tree_parse_errors.into_iter().map(AstParseError::Token).collect();
  let eoi: SimpleSpan = match trees.last() {
    Some(s) => s.1,
    None => return (Vec::new(), ast_parse_errors),
  };
  let recovery = via_parser(
    item_start_p()
      .then(any().and_is(item_start_p().not()).repeated())
      .to(Item::ItemError),
  );

  let module_parser = item_p(make_tt_input)
    .padded_by(newline_p().repeated())
    .recover_with(recovery)
    .map_with(S::from_extras)
    .repeated()
    .collect::<Vec<_>>();

  let (opt_out, item_errors) = module_parser
    .parse_with_state(
      make_tt_input(&trees[..], eoi),
      &mut SimpleState(file_data),
    )
    .into_output_errors();
  let out = opt_out.unwrap_or_default();

  ast_parse_errors.extend(
    item_errors
      .into_iter()
      .map(|error| AstParseError::TokenTree(error.into_owned())),
  );

  (out, ast_parse_errors)
}

type AstExtras<'src> =
  Full<Rich<'src, TokenTree, SimpleSpan>, SimpleState<&'static FileData>, ()>;

/// Parses any keyword that begins an item.
fn item_start_p<'src, I>()
-> impl Parser<'src, I, Token, AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::KwBitStruct) => Token::KwBitStruct,
    TokenTree::Lone(Token::KwConst) => Token::KwConst,
    TokenTree::Lone(Token::KwFn) => Token::KwFn,
    TokenTree::Lone(Token::KwStatic) => Token::KwStatic,
    TokenTree::Lone(Token::KwStruct) => Token::KwStruct,
  }
}

/// Parse one [Item]
fn item_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, Item, AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
  M: Fn(&'src [(TokenTree, SimpleSpan)], SimpleSpan) -> I + Copy + 'src,
{
  let bs = bitstruct_p(make_input).map(Item::BitStruct);
  let c = const_p(make_input).map(Item::Const);
  let f = func_p(make_input).map(Item::Func);
  let sta = static_p(make_input).map(Item::Static);
  let struct_ = struct_p(make_input).map(Item::Struct);

  choice((bs, c, f, sta, struct_))
}

fn register_p<'src, I>()
-> impl Parser<'src, I, Register, AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::KwA) => { Register::A },
    TokenTree::Lone(Token::KwB) => { Register::B },
    TokenTree::Lone(Token::KwC) => { Register::C },
    TokenTree::Lone(Token::KwD) => { Register::D },
    TokenTree::Lone(Token::KwE) => { Register::E },
    TokenTree::Lone(Token::KwH) => { Register::H },
    TokenTree::Lone(Token::KwL) => { Register::L },
    TokenTree::Lone(Token::KwAF) => { Register::AF },
    TokenTree::Lone(Token::KwBC) => { Register::BC },
    TokenTree::Lone(Token::KwDE) => { Register::DE },
    TokenTree::Lone(Token::KwHL) => { Register::HL },
    TokenTree::Lone(Token::KwSP) => { Register::SP },
  }
}

/// Makes a token tree slice into an [Input](chumsky::input::Input).
///
/// This is used by all our parsers that need to look at the content of a token
/// tree group.
///
/// * When making a parser that looks at the content of a token tree group, we
///   need to run a parser on that inner content.
/// * Running a parser on the content of a tree group uses `Input::map` to turn
///   our custom [S<T>] type (which chumsky doesn't know about) into `(T,
///   FileSpan)` (which is what chumsky is expecting).
/// * If any part of such a parser is recursive (eg, statements, expressions,
///   etc) then we'd get a type error because Rust doesn't see two calls to
///   `Input::map` with different closures as being the same type just because
///   the two closures have the exact same expression.
/// * So we force all our calls to any parser that uses grouped content to go
///   through this one specific function, which gives them all the exact same
///   mapping closure, which lets Rust see that they're all the same type.
pub fn make_tt_input<'src>(
  trees: &'src [(TokenTree, SimpleSpan)], eoi: SimpleSpan,
) -> impl BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>
{
  Input::map(trees, eoi, |(tree, span)| (tree, span))
}

pub fn parse_num_lit(str_id: StrID) -> Option<i32> {
  let s = str_id.as_str();
  let mut t = 0_i32;
  if let Some(hex_str) = s.strip_prefix('$') {
    // hexadecimal
    for c in hex_str.chars() {
      match c.to_ascii_lowercase() {
        '_' => continue,
        'a'..='f' => {
          t *= 16;
          t += (c as u8 - 'a' as u8) as i32;
        }
        '0'..='9' => {
          t *= 16;
          t += (c as u8 - '0' as u8) as i32;
        }
        _ => return None,
      }
    }
  } else if let Some(bin_str) = s.strip_prefix('%') {
    // binary
    for c in bin_str.chars() {
      match c {
        '_' => continue,
        '0'..='1' => {
          t *= 2;
          t += (c as u8 - '0' as u8) as i32;
        }
        _ => return None,
      }
    }
  } else {
    // decimal
    for c in s.chars() {
      match c {
        '_' => continue,
        '0'..='9' => {
          t *= 10;
          t += (c as u8 - '0' as u8) as i32;
        }
        _ => return None,
      }
    }
  }
  Some(t)
}
