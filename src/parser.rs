use core::ops::{Deref, DerefMut, Range};

use crate::{token::Token, StaticStr};
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

use comment_filter::*;
use item::*;
use section_decl::*;
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
    I: ValueInput<'a, Token = crate::token::Token, Span = SimpleSpan>,
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
impl Place8 {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    select! {
      Lone(RegA) => Self::A,
      Lone(RegB) => Self::B,
      Lone(RegC) => Self::C,
      Lone(RegD) => Self::D,
      Lone(RegE) => Self::E,
      Lone(RegH) => Self::H,
      Lone(RegL) => Self::L,
      Brackets(inner) if matches!(&*inner, [(Lone(RegHL), _)]) => Self::AddrHL,
    }
  }
}

#[derive(Clone, Copy)]
pub enum Place16 {
  BC,
  DE,
  HL,
  SP,
}
impl core::fmt::Debug for Place16 {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Place16::BC => write!(f, "bc"),
      Place16::DE => write!(f, "de"),
      Place16::HL => write!(f, "hl"),
      Place16::SP => write!(f, "sp"),
    }
  }
}
impl Place16 {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    select! {
      Lone(RegBC) =>Place16::BC,
      Lone(RegDE) =>Place16::DE,
      Lone(RegHL) =>Place16::HL,
      Lone(RegSP) =>Place16::SP,
    }
  }
}

#[derive(Clone)]
pub enum LoadSpecialAddr {
  BC,
  DE,
  HLinc,
  HLdec,
  Immediate(ConstExpr),
}
impl core::fmt::Debug for LoadSpecialAddr {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      LoadSpecialAddr::Immediate(i) => write!(f, "{i:?}"),
      LoadSpecialAddr::BC => write!(f, "bc"),
      LoadSpecialAddr::DE => write!(f, "de"),
      LoadSpecialAddr::HLinc => write!(f, "hl++"),
      LoadSpecialAddr::HLdec => write!(f, "hl--"),
    }
  }
}
impl LoadSpecialAddr {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    let imm = ConstExpr::parser().map(Self::Immediate);
    let bc = just(Lone(RegBC)).map(|_| Self::BC);
    let de = just(Lone(RegDE)).map(|_| Self::DE);
    let hl_inc = {
      let sym = just(Lone(Punct('+'))).ignored();
      let sym_sym = sym.clone().then(sym.clone()).ignored();
      let symbol_choices = sym_sym.or(sym).ignored();
      just(Lone(RegHL)).ignored().then(symbol_choices).map(|_| Self::HLinc)
    };
    let hl_dec = {
      let sym = just(Lone(Punct('-'))).ignored();
      let sym_sym = sym.clone().then(sym.clone()).ignored();
      let symbol_choices = sym_sym.or(sym).ignored();
      just(Lone(RegHL)).ignored().then(symbol_choices).map(|_| Self::HLdec)
    };
    bc.or(de).or(hl_inc).or(hl_dec).or(imm)
  }
}

#[derive(Clone)]
pub enum ConstExpr {
  Lit(StaticStr),
  Ident(StaticStr),
  Macro(MacroInvoke),
}
impl core::fmt::Debug for ConstExpr {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      ConstExpr::Lit(s) => write!(f, "{s}"),
      ConstExpr::Macro(m) => write!(f, "{m:?}"),
      ConstExpr::Ident(i) => write!(f, "{i}"),
    }
  }
}
impl ConstExpr {
  pub const ZERO: Self = Self::Lit("0");

  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    let lit = num_lit_parser().map(Self::Lit);
    let ident = ident_parser().map(Self::Ident);
    let mac = MacroInvoke::parser().map(Self::Macro);

    lit.or(mac).or(ident)
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
impl Data8 {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    let p = Place8::parser().map(Self::Place);
    let i = ConstExpr::parser().map(Self::Immediate);
    p.or(i)
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
  Statement(Statement),
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
      Statement::parser().map(BlockElement::Statement).then_ignore(semicolon.clone());

    label.or(macro_invoke).or(statement)
  }
}

#[derive(Clone, Copy)]
pub enum AluOp {
  Add,
  Adc,
  Sub,
  Sbc,
  And,
  Xor,
  Or,
  Cp,
}
impl core::fmt::Debug for AluOp {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      AluOp::Add => write!(f, "add"),
      AluOp::Adc => write!(f, "adc"),
      AluOp::Sub => write!(f, "sub"),
      AluOp::Sbc => write!(f, "sbc"),
      AluOp::And => write!(f, "and"),
      AluOp::Xor => write!(f, "xor"),
      AluOp::Or => write!(f, "or"),
      AluOp::Cp => write!(f, "cp"),
    }
  }
}
impl AluOp {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    select! {
      Lone(InstADD) => Self::Add,
      Lone(InstADC) => Self::Adc,
      Lone(InstSUB) => Self::Sub,
      Lone(InstSBC) => Self::Sbc,
      Lone(InstAND) => Self::And,
      Lone(InstXOR) => Self::Xor,
      Lone(InstOR) => Self::Or,
      Lone(InstCP) => Self::Cp,
    }
  }
}

#[derive(Clone, Copy)]
pub enum Cond {
  Cy,
  Nc,
  Ze,
  Nz,
  Al,
}
impl core::fmt::Debug for Cond {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Cond::Cy => write!(f, "cy"),
      Cond::Nc => write!(f, "nc"),
      Cond::Ze => write!(f, "ze"),
      Cond::Nz => write!(f, "nz"),
      Cond::Al => write!(f, "al"),
    }
  }
}
impl Cond {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    select! {
      Lone(CondCY) => Self::Cy,
      Lone(CondNC) => Self::Nc,
      Lone(CondZE) => Self::Ze,
      Lone(CondNZ) => Self::Nz,
      Lone(CondAL) => Self::Al,
    }
  }
}

#[derive(Clone)]
pub enum JumpStatement {
  HL,
  Immediate(Cond, ConstExpr),
}
impl core::fmt::Debug for JumpStatement {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      JumpStatement::HL => write!(f, "jp hl"),
      JumpStatement::Immediate(Cond::Al, expr) => write!(f, "jp {expr:?}"),
      JumpStatement::Immediate(cond, expr) => write!(f, "jp {cond:?}, {expr:?}"),
    }
  }
}
impl JumpStatement {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    let jp = just(Lone(InstJP)).ignored();
    let hl = just(Lone(RegHL)).ignored();
    let comma = just(Lone(Punct(',')));
    let jp_hl = jp.clone().then(hl).map(|_| Self::HL);
    let jp_cond = jp
      .clone()
      .then(Cond::parser())
      .then_ignore(comma)
      .then(ConstExpr::parser())
      .map(|(((), cond), expr)| JumpStatement::Immediate(cond, expr));
    let jp_always = jp
      .clone()
      .then(ConstExpr::parser())
      .map(|((), expr)| JumpStatement::Immediate(Cond::Al, expr));
    jp_hl.or(jp_cond).or(jp_always)
  }
}

#[derive(Clone, Copy)]
pub enum DecStatement {
  Place8(Place8),
  Place16(Place16),
}
impl core::fmt::Debug for DecStatement {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      DecStatement::Place8(p8) => write!(f, "dec {p8:?}"),
      DecStatement::Place16(p16) => write!(f, "dec {p16:?}"),
    }
  }
}
impl DecStatement {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    let dec = just(Lone(InstDEC)).ignored();
    dec
      .then(Place8::parser().map(Self::Place8).or(Place16::parser().map(Self::Place16)))
      .map(|(_, x)| x)
  }
}

#[derive(Clone)]
pub struct AluStatement {
  op: AluOp,
  data: Data8,
}
impl core::fmt::Debug for AluStatement {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    let op = self.op;
    let data = &self.data;
    write!(f, "{op:?} a, {data:?}")
  }
}
impl AluStatement {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    let a_comma = just(Lone(RegA)).ignored().then_ignore(just(Lone(Punct(','))));
    let with_a = AluOp::parser()
      .then_ignore(a_comma)
      .then(Data8::parser())
      .map(|(op, data)| Self { op, data });
    let no_a = AluOp::parser().then(Data8::parser()).map(|(op, data)| Self { op, data });
    with_a.or(no_a)
  }
}

#[derive(Clone)]
pub enum LoadStatement {
  Place8(Place8, Data8),
  Place16(Place16, ConstExpr),
  SpecialAddrFromA(Vec<(TokenTree, SimpleSpan)>),
  AFromSpecialAddr(Vec<(TokenTree, SimpleSpan)>),
  ImmFromSP(Vec<(TokenTree, SimpleSpan)>),
  ImmFromA(Vec<(TokenTree, SimpleSpan)>),
  HLFromSPDelta(ConstExpr),
  SPFromHL,
}
impl core::fmt::Debug for LoadStatement {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      LoadStatement::Place8(p8, d8) => write!(f, "ld {p8:?}, {d8:?}"),
      LoadStatement::Place16(p16, expr) => write!(f, "ld {p16:?}, {expr:?}"),
      LoadStatement::SpecialAddrFromA(s) => write!(f, "ld [{s:?}], a"),
      LoadStatement::AFromSpecialAddr(s) => write!(f, "ld a, [{s:?}]"),
      LoadStatement::ImmFromSP(i) => write!(f, "ld [{i:?}], sp"),
      LoadStatement::ImmFromA(i) => write!(f, "ld [{i:?}], a"),
      LoadStatement::HLFromSPDelta(d) => write!(f, "ld hl, sp + {d:?}"),
      LoadStatement::SPFromHL => write!(f, "ld sp, hl"),
    }
  }
}
impl LoadStatement {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    let ld = just(Lone(InstLD)).ignored();
    let comma = just(Lone(Punct(','))).ignored();
    let plus = just(Lone(Punct('+'))).ignored();
    let a = just(Lone(RegA)).ignored();
    let sp = just(Lone(RegSP)).ignored();
    let hl = just(Lone(RegHL)).ignored();
    //
    let place8 = ld
      .clone()
      .ignore_then(Place8::parser())
      .then_ignore(comma.clone())
      .then(Data8::parser())
      .map(|(p8, d8)| Self::Place8(p8, d8));
    let place16 = ld
      .clone()
      .ignore_then(Place16::parser())
      .then_ignore(comma.clone())
      .then(ConstExpr::parser())
      .map(|(p16, expr)| Self::Place16(p16, expr));
    let spec_from_a = ld
      .clone()
      .ignore_then(select! {
        Brackets(tts) => tts,
      })
      .then_ignore(comma.clone())
      .then_ignore(a.clone())
      .map(Self::SpecialAddrFromA);
    let a_from_spec = ld
      .clone()
      .then_ignore(a.clone())
      .then_ignore(comma.clone())
      .ignore_then(select! {
        Brackets(tts) => tts,
      })
      .map(Self::AFromSpecialAddr);
    let imm_from_sp = ld
      .clone()
      .ignore_then(select! {
        Brackets(tts) => tts,
      })
      .then_ignore(comma.clone())
      .then_ignore(sp.clone())
      .map(Self::ImmFromSP);
    let imm_from_a = ld
      .clone()
      .ignore_then(select! {
        Brackets(tts) => tts,
      })
      .then_ignore(comma.clone())
      .then_ignore(a.clone())
      .map(Self::ImmFromA);
    let hl_from_sp_delta = {
      let has_expr = ld
        .clone()
        .then_ignore(hl.clone())
        .then_ignore(comma.clone())
        .then_ignore(sp.clone())
        .then_ignore(plus.clone())
        .ignore_then(ConstExpr::parser())
        .map(Self::HLFromSPDelta);
      let no_expr = ld
        .clone()
        .then_ignore(hl.clone())
        .then_ignore(comma.clone())
        .then_ignore(sp.clone())
        .map(|_| Self::HLFromSPDelta(ConstExpr::ZERO));
      has_expr.or(no_expr)
    };
    let sp_from_hl = ld
      .clone()
      .then_ignore(sp.clone())
      .then_ignore(comma.clone())
      .then_ignore(hl.clone())
      .map(|_| Self::SPFromHL);
    //
    place8
      .or(place16)
      .or(spec_from_a)
      .or(a_from_spec)
      .or(imm_from_sp)
      .or(imm_from_a)
      .or(hl_from_sp_delta)
      .or(sp_from_hl)
  }
}

#[derive(Clone)]
pub enum Statement {
  Load(LoadStatement),
  Alu(AluStatement),
  Jump(JumpStatement),
  Dec(DecStatement),
}
impl core::fmt::Debug for Statement {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Statement::Load(x) => write!(f, "{x:?}"),
      Statement::Alu(x) => write!(f, "{x:?}"),
      Statement::Jump(x) => write!(f, "{x:?}"),
      Statement::Dec(x) => write!(f, "{x:?}"),
    }
  }
}
impl Statement {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    let load = LoadStatement::parser().map(Self::Load);
    let alu = AluStatement::parser().map(Self::Alu);
    let jump = JumpStatement::parser().map(Self::Jump);
    let dec = DecStatement::parser().map(Self::Dec);
    load.or(alu).or(jump).or(dec)
  }
}
