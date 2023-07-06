use core::ops::{Deref, DerefMut, Range};

use crate::{lexer::Token, StaticStr};
use chumsky::{
  extra::ParserExtra,
  input::{BoxedStream, SpannedInput, Stream, ValueInput},
  prelude::*,
  primitive::*,
  select, Parser,
};
use logos::Span;

pub mod comment_filter;
pub mod item;
pub mod section_decl;
pub mod statement_decl;

use comment_filter::*;
use item::*;
use section_decl::*;
use statement_decl::*;
use Token::*;
use TokenTree::*;

/// "Identity, 2-arg"
///
/// This just wraps the two values as a tuple. This is only really useful as a
/// higher order function to pass to map and similar when we want to join
/// multi-arg inputs into a single value output.
#[inline]
#[must_use]
pub const fn id2<A, B>(a: A, b: B) -> (A, B) {
  (a, b)
}

pub struct DebugListWithoutSpans<'a, T>(pub &'a [(T, SimpleSpan)]);
impl<'a, T> core::fmt::Debug for DebugListWithoutSpans<'a, T>
where
  T: core::fmt::Debug,
{
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut x = f.debug_list();
    for (t, _) in self.0 {
      x.entry(t);
    }
    x.finish()
  }
}

pub type ErrRichToken<'a> = extra::Err<Rich<'a, Token>>;
pub type ErrRichTokenTree<'a> = extra::Err<Rich<'a, TokenTree>>;
pub type InputSlice<'a, T> = SpannedInput<T, SimpleSpan, &'a [(T, SimpleSpan)]>;

/// Runs a parser from `T` to `O` on a slice of `T`, giving a [ParseResult]
#[inline]
pub fn run_parser<'a, P, T, O, E>(
  parser: P, data: &'a [(T, SimpleSpan)],
) -> ParseResult<O, E::Error>
where
  P: Parser<'a, InputSlice<'a, T>, O, E>,
  E: ParserExtra<'a, InputSlice<'a, T>>,
  <E as ParserExtra<'a, InputSlice<'a, T>>>::State: Default,
  <E as ParserExtra<'a, InputSlice<'a, T>>>::Context: Default,
{
  // calculate the likely span value based on the first and last token, assumes
  // that the tokens are still properly in order.
  let span: SimpleSpan = if data.is_empty() {
    (0..0).into()
  } else {
    let start = data.first().unwrap().1.start;
    let end = data.last().unwrap().1.end;
    (start..end).into()
  };
  let input = data.spanned(span);
  parser.parse(input)
}

/// A lone token or a list of tokens in one of three grouping styles.
///
/// Collecting a raw token list into token trees ensures that all the
/// opening/closing markers of all the groupings are balanced before trying to
/// do any more advanced parsing.
///
/// * See: [make_token_trees]
#[derive(Clone, PartialEq, Eq)]
pub enum TokenTree {
  Lone(Token),
  Parens(Vec<(Self, SimpleSpan)>),
  Brackets(Vec<(Self, SimpleSpan)>),
  Braces(Vec<(Self, SimpleSpan)>),
}
impl core::fmt::Debug for TokenTree {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Lone(t) => core::fmt::Debug::fmt(&t, f),
      Parens(ts) => {
        if ts.len() > 10 {
          write!(f, "Parens({} len)", ts.len())
        } else {
          write!(f, "Parens({:?})", DebugListWithoutSpans(ts))
        }
      }
      Brackets(ts) => write!(f, "Brackets({:?})", DebugListWithoutSpans(ts)),
      Braces(ts) => write!(f, "Braces({:?})", DebugListWithoutSpans(ts)),
    }
  }
}
impl TokenTree {
  /// Parses for just one token tree.
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichToken<'a>>
  where
    I: ValueInput<'a, Token = crate::lexer::Token, Span = SimpleSpan>,
  {
    recursive(|tt| {
      let token_list = tt.map_with_span(|i, s| (i, s)).repeated().collect::<Vec<_>>();

      // Looks like `(...)`
      let parens = token_list
        .clone()
        .delimited_by(just(Punct('(')), just(Punct(')')))
        .map(TokenTree::Parens);

      // Looks like `[...]`
      let brackets = token_list
        .clone()
        .delimited_by(just(Punct('[')), just(Punct(']')))
        .map(TokenTree::Brackets);

      // Looks like `{...}`
      let braces = token_list
        .clone()
        .delimited_by(just(Punct('{')), just(Punct('}')))
        .map(TokenTree::Braces);

      // Looks like something that does *NOT* close one of the other types.
      let single = none_of([Punct(')'), Punct(']'), Punct('}')]).map(TokenTree::Lone);

      parens.or(brackets).or(braces).or(single)
    })
  }
}

#[inline]
pub fn make_token_trees(
  tokens: &[(Token, SimpleSpan)],
) -> ParseResult<Vec<(TokenTree, SimpleSpan)>, Rich<'_, Token>> {
  let parser = TokenTree::parser().map_with_span(id2).repeated().collect::<Vec<_>>();
  //
  run_parser(parser, tokens)
}

pub fn ident_parser<'a, I>() -> impl Parser<'a, I, StaticStr, ErrRichTokenTree<'a>>
where
  I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
{
  select! {
    Lone(Ident(i)) => i,
  }
}

pub fn num_lit_parser<'a, I>() -> impl Parser<'a, I, StaticStr, ErrRichTokenTree<'a>>
where
  I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
{
  select! {
    Lone(NumLit(n)) => n,
  }
}

#[derive(Clone, Copy)]
pub enum Place8 {
  A,
  B,
  C,
  D,
  E,
  H,
  L,
  AddrHL,
}
impl core::fmt::Debug for Place8 {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Place8::A => write!(f, "a"),
      Place8::B => write!(f, "b"),
      Place8::C => write!(f, "c"),
      Place8::D => write!(f, "d"),
      Place8::E => write!(f, "e"),
      Place8::H => write!(f, "h"),
      Place8::L => write!(f, "l"),
      Place8::AddrHL => write!(f, "[hl]"),
    }
  }
}

#[derive(Clone)]
pub enum ConstExpr {
  Lit(StaticStr),
  Macro(MacroInvoke),
}
impl core::fmt::Debug for ConstExpr {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      ConstExpr::Lit(s) => write!(f, "{s}"),
      ConstExpr::Macro(m) => write!(f, "{m:?}"),
    }
  }
}
impl ConstExpr {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    num_lit_parser().map(Self::Lit)
  }
}

#[derive(Clone)]
pub enum Data8 {
  Place(Place8),
  Immediate(ConstExpr),
}
impl core::fmt::Debug for Data8 {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Data8::Place(p) => write!(f, "{p:?}"),
      Data8::Immediate(c) => write!(f, "{c:?}"),
    }
  }
}

#[derive(Clone)]
pub struct ConstDecl {
  pub name: (StaticStr, SimpleSpan),
  pub expr: (ConstExpr, SimpleSpan),
}
impl core::fmt::Debug for ConstDecl {
  /// this cuts out some of the SimpleSpan junk from a debug print compared to
  /// using the derive.
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("ConstDecl")
      .field("name", &self.name.0)
      .field("expr", &self.expr.0)
      .finish()
  }
}
impl ConstDecl {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    let kw_const = just(Lone(KwConst));

    let equal = just(Lone(Punct('=')));

    let semicolon = just(Lone(Punct(';')));

    kw_const
      .ignore_then(ident_parser().map_with_span(id2))
      .then_ignore(equal)
      .then(ConstExpr::parser().map_with_span(id2))
      .then_ignore(semicolon)
      .map(|(name, expr)| Self { name, expr })
  }
}

#[derive(Clone, Copy)]
pub enum Label {
  Ident(StaticStr),
  Num(StaticStr),
}
impl core::fmt::Debug for Label {
  /// this cuts out some of the SimpleSpan junk from a debug print compared to
  /// using the derive.
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Label::Ident(i) => write!(f, "{i}"),
      Label::Num(n) => write!(f, "{n}"),
    }
  }
}
impl Label {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    let i = ident_parser().map(Self::Ident);
    let n = num_lit_parser().map(Self::Num);

    i.or(n)
  }
}

#[derive(Clone)]
pub struct MacroInvoke {
  pub name: (StaticStr, SimpleSpan),
  pub args: (Vec<(TokenTree, SimpleSpan)>, SimpleSpan),
}
impl core::fmt::Debug for MacroInvoke {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}!(", self.name.0)?;
    if self.args.0.len() > 10 {
      write!(f, "{{{} token trees}}", self.args.0.len())?;
    } else {
      write!(f, "{:?}", DebugListWithoutSpans(&self.args.0))?;
    }
    write!(f, ")")
  }
}
impl MacroInvoke {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    let name = ident_parser().map_with_span(id2);

    let bang = just(Lone(Punct('!')));

    let args = select! {
      Parens(tt) => tt,
    }
    .map_with_span(id2);

    name.then_ignore(bang).then(args).map(|(name, args)| Self { name, args })
  }
}

#[derive(Clone)]
pub enum BlockElement {
  Label(Label),
  Macro(MacroInvoke),
  Statement(StatementDecl),
}
impl core::fmt::Debug for BlockElement {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      BlockElement::Label(l) => write!(f, "{l:?}:"),
      BlockElement::Statement(s) => write!(f, "{s:?};"),
      BlockElement::Macro(m) => write!(f, "{m:?};"),
    }
  }
}
impl BlockElement {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    let colon = just(Lone(Punct(':')));
    let semicolon = just(Lone(Punct(';')));

    let label = Label::parser().map(BlockElement::Label).then_ignore(colon);
    let macro_invoke =
      MacroInvoke::parser().map(BlockElement::Macro).then_ignore(semicolon.clone());
    let statement =
      StatementDecl::parser().map(BlockElement::Statement).then_ignore(semicolon.clone());

    label.or(macro_invoke).or(statement)
  }
}
