#![allow(unused_imports)]
#![allow(unused_braces)]
#![allow(unused_mut)]

use std::ops::Range;

use chumsky::{
  extra::{Err, Full, ParserExtra, SimpleState},
  input::{BorrowInput, MapExtra, ValueInput},
  prelude::*,
};
use str_id::StrID;
use yagbas_srcfiletypes::{FileData, FileID, Token, TokenTree, trees_of};

/// Generic typed value paired with a `SimpleSpan`
#[derive(Debug, Clone)]
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
  BitStruct(BitStruct),
  Const(Const),
  Func(Func),
  Static(Static),
  Struct(Struct),
  ItemError,
}

/// Bitpacked structured data.
///
/// An 8-bit value, where each bit position can have a field name.
#[derive(Debug, Clone)]
pub struct BitStruct {
  pub file_id: FileID,
  pub name: S<StrID>,
  /// `field_name: bit` list
  pub fields: Vec<(TokenTree, SimpleSpan)>,
}

/// Constant definition
///
/// This associates a name to a particular constant expression. A const is only
/// used during compile time, it doesn't add data in the compiled binary.
#[derive(Debug, Clone)]
pub struct Const {
  pub file_id: FileID,
  pub name: S<StrID>,
  pub expr: Vec<(TokenTree, SimpleSpan)>,
}

/// Function definition
///
/// Defines a body of code that can be executed.
#[derive(Debug, Clone)]
pub struct Func {
  pub file_id: FileID,
  pub name: S<StrID>,
  pub args: Vec<(TokenTree, SimpleSpan)>,
  pub body: Vec<(TokenTree, SimpleSpan)>,
}

/// Static definition
///
/// This is a block of data that's either in ROM or RAM. Generally this doesn't
/// represent code, but instead some other form of data that the program uses,
/// such as tile data.
#[derive(Debug, Clone)]
pub struct Static {
  pub file_id: FileID,
  pub name: S<StrID>,
  pub expr: Vec<(TokenTree, SimpleSpan)>,
}

/// Structured data.
#[derive(Debug, Clone)]
pub struct Struct {
  pub file_id: FileID,
  pub name: S<StrID>,
  /// `field_name: field_type` list
  pub fields: Vec<(TokenTree, SimpleSpan)>,
}

/// Branching construct.
#[derive(Debug, Clone)]
pub struct IfElse {
  pub condition: S<Expr>,
  pub if_body: Vec<S<Statement>>,
  pub else_body: Vec<S<Statement>>,
}

/// Repeating code construct.
#[derive(Debug, Clone)]
pub struct Loop {
  pub name: Option<S<StrID>>,
  pub body: Vec<S<Statement>>,
}

/// Any of the things that can go in a body of code.
#[derive(Debug, Clone)]
pub enum Statement {
  Expr(Expr),
  IfElse(Box<IfElse>),
  Loop(Box<Loop>),
  Break(Option<StrID>),
  Continue(Option<StrID>),
  Call(StrID),
  Return,
  StatementError,
}

/// An expression is a thing that can be made of sub-expressions.
///
/// Within the AST, expressions are untyped. Applying types (and generating type
/// errors) happens after the basic AST is parsed.
#[derive(Debug, Clone)]
pub enum Expr {
  /// `123`, `$FF`, `%10_01_00_00`, etc
  NumberLiteral(StrID),

  /// `main`, `memcpy`, etc
  Ident(StrID),

  /// `a`, `hl`, etc
  Register(Register),

  /// `LcdCtrl { display_on, bg_on }`
  BitStructLiteral(Box<Vec<S<StrID>>>),

  /// `Position { x: 15, y: 20 }`
  StructLiteral(Box<Vec<(S<StrID>, S<Self>)>>),

  /// `{ expr0, expr1, ..., exprN }`
  List(Box<Vec<S<Expr>>>),

  /// `[hl]`
  Deref(Box<Vec<S<Expr>>>),

  /// `macro_name!( expr0, expr1, ... exprN )`
  MacroUse(Box<Vec<S<Expr>>>),

  /// `array.0`, `b.LcdCtrl.bg_on`, etc
  Dot(Box<[S<Self>; 2]>),

  /// `-12`
  Neg(Box<S<Self>>),

  /// `a = 12`
  Assign(Box<[S<Self>; 2]>),

  /// `12 + 4`
  Add(Box<[S<Self>; 2]>),

  /// `12 - 4`
  Sub(Box<[S<Self>; 2]>),

  /// `12 * 4`
  Mul(Box<[S<Self>; 2]>),

  /// `12 / 4`
  Div(Box<[S<Self>; 2]>),

  /// `12 & 4`
  BitAnd(Box<[S<Self>; 2]>),

  /// `12 | 4`
  BitOr(Box<[S<Self>; 2]>),

  /// `12 ^ 4`
  BitXor(Box<[S<Self>; 2]>),

  /// Any error during expression processing
  ExprError,
}

/// The registers that can be named anywhere in any expression.
///
/// This type isn't strictly necessary, we could use [StrID] in all places where
/// there's a Register value, but it's a little faster to sort out registers
/// from identifiers ahead of time.
#[derive(Debug, Clone, Copy)]
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

  let module_parser = item_p()
    .padded_by(newline_p().repeated())
    .recover_with(recovery)
    .map_with(S::from_extras)
    .repeated()
    .collect::<Vec<_>>();

  let (opt_out, item_errors) = module_parser
    .parse_with_state(
      Input::map(&trees[..], eoi, |(tree, span)| (tree, span)),
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
fn item_p<'src, I>() -> impl Parser<'src, I, Item, AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  let bs = bitstruct_p().map(Item::BitStruct);
  let c = const_p().map(Item::Const);
  let f = func_p().map(Item::Func);
  let sta = static_p().map(Item::Static);
  let stru = struct_p().map(Item::Struct);

  choice((bs, c, f, sta, stru))
}

/// Parse one [BitStruct]
fn bitstruct_p<'src, I>()
-> impl Parser<'src, I, BitStruct, AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  let keyword = select! { TokenTree::Lone(Token::KwBitStruct) => () };
  let name = ident_p();
  let fields = braces_p();

  keyword.ignore_then(name).then(fields).map_with(|(name, fields), ex| {
    let state: &mut SimpleState<&'static FileData> = ex.state();
    let file_id: FileID = state.id();
    BitStruct { file_id, name, fields }
  })
}

/// Parse one [Const]
fn const_p<'src, I>() -> impl Parser<'src, I, Const, AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  let keyword = select! { TokenTree::Lone(Token::KwConst) => () };
  let name = ident_p();
  let expr = braces_p();

  keyword.ignore_then(name).then(expr).map_with(|(name, expr), ex| {
    let state: &mut SimpleState<&'static FileData> = ex.state();
    let file_id: FileID = state.id();
    Const { file_id, name, expr }
  })
}

/// Parse one [Func]
fn func_p<'src, I>() -> impl Parser<'src, I, Func, AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  let keyword = select! { TokenTree::Lone(Token::KwFn) => () };
  let name = ident_p();
  let args = parens_p();
  let body = braces_p();

  keyword.ignore_then(name).then(args).then(body).map_with(
    |((name, args), body), ex| {
      let state: &mut SimpleState<&'static FileData> = ex.state();
      let file_id: FileID = state.id();
      Func { file_id, name, args, body }
    },
  )
}

/// Parse one [Static]
fn static_p<'src, I>() -> impl Parser<'src, I, Static, AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  let keyword = select! { TokenTree::Lone(Token::KwStatic) => () };
  let name = ident_p();
  let expr = braces_p();

  keyword.ignore_then(name).then(expr).map_with(|(name, expr), ex| {
    let state: &mut SimpleState<&'static FileData> = ex.state();
    let file_id: FileID = state.id();
    Static { file_id, name, expr }
  })
}

/// Parse one [Struct]
fn struct_p<'src, I>() -> impl Parser<'src, I, Struct, AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  let keyword = select! { TokenTree::Lone(Token::KwStruct) => () };
  let name = ident_p();
  let fields = braces_p();

  keyword.ignore_then(name).then(fields).map_with(|(name, fields), ex| {
    let state: &mut SimpleState<&'static FileData> = ex.state();
    let file_id: FileID = state.id();
    Struct { file_id, name, fields }
  })
}

/// Parse a lone [Token::Ident] and get the spanned [StrID] it's for.
fn ident_p<'src, I>() -> impl Parser<'src, I, S<StrID>, AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  // Note(Lokathor): All these intermediate steps with explicit types are
  // basically necessary otherwise rustc will fall over and cry.
  select! {
    TokenTree::Lone(Token::Ident) = ex => {
      let state: &mut SimpleState<&'static FileData> = ex.state();
      let file_content: &'static str = state.content();
      let span: SimpleSpan = ex.span();
      let range: Range<usize> = span.into();
      let str_id = StrID::from(&file_content[range]);
      S::from_extras(str_id, ex)
    }
  }
}

fn parens_p<'src, I>()
-> impl Parser<'src, I, Vec<(TokenTree, SimpleSpan)>, AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Parens(body) => { body }
  }
}

fn braces_p<'src, I>()
-> impl Parser<'src, I, Vec<(TokenTree, SimpleSpan)>, AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Braces(body) => { body }
  }
}

fn newline_p<'src, I>() -> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::Newline) => { () }
  }
}
