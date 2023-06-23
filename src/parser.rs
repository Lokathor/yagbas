use core::ops::{Deref, DerefMut, Range};

use crate::{lexer::Token, StaticStr};
use chumsky::{
  input::{BoxedStream, Stream, ValueInput},
  prelude::*,
  primitive::*,
  select,
  span::SimpleSpan,
  Parser,
};
use logos::Span;

#[derive(Debug, Clone)]
pub enum CommentStripError {
  Lex(Span),
  UnmatchedMultiOpen(Span),
  UnmatchedMultiClose(Span),
}

pub fn strip_comments(
  i: impl Iterator<Item = (Result<Token, ()>, Span)>,
) -> Result<Vec<(Token, SimpleSpan)>, CommentStripError> {
  let mut out: Vec<(Token, SimpleSpan)> = vec![];
  let mut comment_levels = 0;
  let mut last_zero_opener = 0..0;
  for (res, span) in i {
    let token = match res {
      Err(()) => return Err(CommentStripError::Lex(span)),
      Ok(Token::CommentSingle) => continue,
      Ok(Token::CommentMultiStart) => {
        if comment_levels == 0 {
          last_zero_opener = span;
        }
        comment_levels += 1;
        continue;
      }
      Ok(Token::CommentMultiEnd) => {
        if comment_levels == 0 {
          return Err(CommentStripError::UnmatchedMultiClose(span));
        }
        comment_levels -= 1;
        continue;
      }
      Ok(_) if comment_levels > 0 => continue,
      Ok(t) => t,
    };
    out.push((token, span.into()));
  }
  if comment_levels > 0 {
    Err(CommentStripError::UnmatchedMultiOpen(last_zero_opener))
  } else {
    Ok(out)
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InterpretNumErr {
  IllegalHexDigit(char),
  IllegalBinDigit(char),
  IllegalDecDigit(char),
  Overflow,
}

pub fn interpret_num(mut num: &str) -> Result<i32, InterpretNumErr> {
  let mut is_neg = false;
  if let Some(s) = num.strip_prefix('-') {
    is_neg = true;
    num = s;
  };
  fn interpret_hex(num: &str) -> Result<i32, InterpretNumErr> {
    let mut out: i32 = 0;
    for ch in num.chars() {
      let x: i32 = match ch {
        '_' => continue,
        '0'..='9' => (ch as i32) - ('0' as i32),
        'a'..='f' => (ch as i32) - ('a' as i32),
        'A'..='F' => (ch as i32) - ('A' as i32),
        other => return Err(InterpretNumErr::IllegalHexDigit(other)),
      };
      out = match out.checked_mul(16).and_then(|out| out.checked_add(x)) {
        Some(new_out) => new_out,
        None => return Err(InterpretNumErr::Overflow),
      };
    }
    Ok(out)
  }
  fn interpret_bin(num: &str) -> Result<i32, InterpretNumErr> {
    let mut out: i32 = 0;
    for ch in num.chars() {
      let x: i32 = match ch {
        '_' => continue,
        '0'..='1' => (ch as i32) - ('0' as i32),
        other => return Err(InterpretNumErr::IllegalBinDigit(other)),
      };
      out = match out.checked_mul(2).and_then(|out| out.checked_add(x)) {
        Some(new_out) => new_out,
        None => return Err(InterpretNumErr::Overflow),
      };
    }
    Ok(out)
  }
  fn interpret_dec(num: &str) -> Result<i32, InterpretNumErr> {
    let mut out: i32 = 0;
    for ch in num.chars() {
      let x: i32 = match ch {
        '_' => continue,
        '0'..='9' => (ch as i32) - ('0' as i32),
        other => return Err(InterpretNumErr::IllegalDecDigit(other)),
      };
      out = match out.checked_mul(10).and_then(|out| out.checked_add(x)) {
        Some(new_out) => new_out,
        None => return Err(InterpretNumErr::Overflow),
      };
    }
    Ok(out)
  }
  let val: i32 = if let Some(s) = num.strip_prefix('$') {
    interpret_hex(s)?
  } else if let Some(s) = num.strip_prefix("0x") {
    interpret_hex(s)?
  } else if let Some(s) = num.strip_prefix('%') {
    interpret_bin(s)?
  } else if let Some(s) = num.strip_prefix("0b") {
    interpret_bin(s)?
  } else {
    interpret_dec(num)?
  };
  Ok(if is_neg { -val } else { val })
}

pub type MyParseErr<'a> = extra::Err<Rich<'a, Token>>;

#[derive(Clone, Copy)]
pub struct Spanned<T>(pub T, pub SimpleSpan);
impl<T> core::fmt::Debug for Spanned<T>
where
  T: core::fmt::Debug,
{
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let data = &self.0;
    let span = &self.1;
    write!(f, "{data:?} (@{span})")
  }
}

#[derive(Debug, Clone, Copy)]
pub struct ConstDecl {
  pub name: Spanned<StaticStr>,
  pub val: Spanned<StaticStr>,
}
pub fn const_decl_parser<'a, I>() -> impl Parser<'a, I, ConstDecl, MyParseErr<'a>>
where
  I: ValueInput<'a, Token = crate::lexer::Token, Span = SimpleSpan>,
{
  let ident = select! {
    Token::Ident(i) = span => Spanned(i, span),
  };
  let num = select! {
    Token::Num(n) = span => Spanned(n, span),
  };

  just(Token::KwConst)
    .ignore_then(ident)
    .then_ignore(just(Token::Punct('=')))
    .then(num)
    .then_ignore(just(Token::Punct(';')))
    .map(|(name, val)| ConstDecl { name, val })
}

#[derive(Debug, Clone, Copy)]
pub enum LabelName {
  Ident(StaticStr),
  Num(StaticStr),
}

#[derive(Debug, Clone, Copy)]
pub struct Label {
  pub name: Spanned<LabelName>,
}
pub fn label_parser<'a, I>() -> impl Parser<'a, I, Label, MyParseErr<'a>>
where
  I: ValueInput<'a, Token = crate::lexer::Token, Span = SimpleSpan>,
{
  let ident = select! {
    t @ Token::Ident(_) = span => (t, span),
  };
  let num = select! {
    t @ Token::Num(_) = span => (t, span),
  };

  ident.or(num).then_ignore(just(Token::Punct(':'))).map(|(name, name_span)| Label {
    name: Spanned(
      match name {
        Token::Ident(i) => LabelName::Ident(i),
        Token::Num(i) => LabelName::Num(i),
        _ => unimplemented!(),
      },
      name_span,
    ),
  })
}

#[derive(Debug, Clone)]
pub enum TokenTree {
  Single(Token),
  Parens(Vec<Spanned<Self>>),
  Brackets(Vec<Spanned<Self>>),
}
pub fn tt_parser<'a, I>() -> impl Parser<'a, I, TokenTree, MyParseErr<'a>>
where
  I: ValueInput<'a, Token = crate::lexer::Token, Span = SimpleSpan>,
{
  recursive(|tt| {
    let token_list = tt.map_with_span(Spanned).repeated().collect::<Vec<_>>();

    // Looks like `(...)`
    let parens = token_list
      .clone()
      .map(TokenTree::Parens)
      .delimited_by(just(Token::Punct('(')), just(Token::Punct(')')));

    // Looks like `[...]`
    let brackets = token_list
      .map(TokenTree::Brackets)
      .delimited_by(just(Token::Punct('[')), just(Token::Punct(']')));

    // Looks like `5` or `"hello"`
    let single = none_of([Token::Punct(')'), Token::Punct(';')]).map(TokenTree::Single);

    parens.or(brackets).or(single)
  })
}

#[derive(Debug, Clone)]
pub struct Directive {
  pub name: Spanned<StaticStr>,
  pub body: Spanned<TokenTree>,
}
pub fn directive_parser<'a, I>() -> impl Parser<'a, I, Directive, MyParseErr<'a>>
where
  I: ValueInput<'a, Token = crate::lexer::Token, Span = SimpleSpan>,
{
  let ident = select! {
    Token::Ident(i) = span => Spanned(i, span),
  };

  /*
  // this version makes parens optional around a single arg directive
  ident
    .then_ignore(just(Token::Punct('!')))
    .then(tt_parser().map_with_span(Spanned))
    .map(|(name, body)| Directive { name, body })
  */

  // this version *requires* parens around the directive args
  ident
    .then_ignore(just(Token::Punct('!')))
    .then(
      tt_parser()
        .map_with_span(Spanned)
        .repeated()
        .collect()
        .map(TokenTree::Parens)
        .map_with_span(Spanned)
        .delimited_by(just(Token::Punct('(')), just(Token::Punct(')'))),
    )
    .then_ignore(just(Token::Punct(';')))
    .map(|(name, body)| Directive { name, body })
}

#[derive(Debug, Clone)]
pub struct Statement(Vec<Spanned<Token>>);
pub fn statement_parser<'a, I>() -> impl Parser<'a, I, Statement, MyParseErr<'a>>
where
  I: ValueInput<'a, Token = crate::lexer::Token, Span = SimpleSpan>,
{
  none_of(Token::Punct(';'))
    .map_with_span(Spanned)
    .repeated()
    .collect::<Vec<_>>()
    .map(Statement)
    .then_ignore(just(Token::Punct(';')))
}

#[derive(Debug, Clone)]
pub enum BlockElement {
  Label(Label),
  Statement(Statement),
}
pub fn block_element_parser<'a, I>() -> impl Parser<'a, I, BlockElement, MyParseErr<'a>>
where
  I: ValueInput<'a, Token = crate::lexer::Token, Span = SimpleSpan>,
{
  let label = label_parser().map(BlockElement::Label);
  let statement = statement_parser().map(BlockElement::Statement);
  label.or(statement)
}

#[derive(Debug, Clone)]
pub struct Block(Vec<Spanned<BlockElement>>);
pub fn block_parser<'a, I>() -> impl Parser<'a, I, Block, MyParseErr<'a>>
where
  I: ValueInput<'a, Token = crate::lexer::Token, Span = SimpleSpan>,
{
  block_element_parser()
    .map_with_span(Spanned)
    .repeated()
    .collect::<Vec<_>>()
    .map(Block)
    .delimited_by(just(Token::Punct('{')), just(Token::Punct('}')))
}

#[derive(Debug, Clone)]
pub struct Section {
  pub name: Spanned<StaticStr>,
  pub locations: Vec<Spanned<StaticStr>>,
  pub block: Spanned<Block>,
}
pub fn section_parser<'a, I>() -> impl Parser<'a, I, Section, MyParseErr<'a>>
where
  I: ValueInput<'a, Token = crate::lexer::Token, Span = SimpleSpan>,
{
  let ident = select! {
    Token::Ident(i) = span => Spanned(i, span),
  };
  let location_list = ident
    .separated_by(just(Token::Punct(',')))
    .collect::<Vec<_>>()
    .delimited_by(just(Token::Punct('[')), just(Token::Punct(']')));

  just(Token::KwSection)
    .ignore_then(ident)
    .then(location_list)
    .then(block_parser().map_with_span(Spanned))
    .map(|((name, locations), block)| Section { name, locations, block })
}

#[derive(Debug, Clone)]
pub enum Item {
  Const(Spanned<ConstDecl>),
  Section(Spanned<Section>),
}
pub fn item_parser<'a, I>() -> impl Parser<'a, I, Item, MyParseErr<'a>>
where
  I: ValueInput<'a, Token = crate::lexer::Token, Span = SimpleSpan>,
{
  let const_decl = const_decl_parser().map_with_span(Spanned).map(Item::Const);
  let section = section_parser().map_with_span(Spanned).map(Item::Section);

  const_decl.or(section)
}
