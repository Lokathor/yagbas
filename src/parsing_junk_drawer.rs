use super::*;
use crate::Token::*;
use crate::TokenTree::*;

pub fn kw_bitstruct_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwBitstruct) => ()
  }
}
pub fn kw_break_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwBreak) => ()
  }
}
pub fn kw_const_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwConst) => ()
  }
}
pub fn kw_continue_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwContinue) => ()
  }
}
pub fn kw_else_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwElse) => ()
  }
}
pub fn kw_fn_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwFn) => ()
  }
}
pub fn kw_if_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwIf) => ()
  }
}
pub fn kw_let_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwLet) => ()
  }
}
pub fn kw_loop_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwLoop) => ()
  }
}
pub fn kw_mmio_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwMmio) => ()
  }
}
pub fn kw_mut_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwMut) => ()
  }
}
pub fn kw_return_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwReturn) => ()
  }
}
pub fn kw_rom_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwRom) => ()
  }
}
pub fn kw_static_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwStatic) => ()
  }
}
pub fn kw_struct_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwStruct) => ()
  }
}

pub fn ident_p<'src>() -> impl YagParser<'src, StrID> {
  select! {
    Lone(Ident) = ex => {
      let state: &SimpleState<YagParserState> = ex.state();
      let source: &str = state.source;
      let span: Span32 = ex.span();
      let range: Range<usize> = span.start.try_into().unwrap()..span.end.try_into().unwrap();
      let str_id = StrID::from(&source[range]);
      str_id
    }
  }
}
pub fn num_lit_p<'src>() -> impl YagParser<'src, StrID> {
  select! {
    Lone(NumLit) = ex => {
      let state: &SimpleState<YagParserState> = ex.state();
      let source: &str = state.source;
      let span: Span32 = ex.span();
      let range: Range<usize> = span.start.try_into().unwrap()..span.end.try_into().unwrap();
      let str_id = StrID::from(&source[range]);
      str_id
    }
  }
}
pub fn bool_p<'src>() -> impl YagParser<'src, bool> {
  select! {
    Lone(KwTrue) => true,
    Lone(KwFalse) => false,
  }
}
