use super::*;

#[derive(Debug, Display, Clone, Copy)]
pub enum TacVar {
  #[display("a")]
  A,
  #[display("b")]
  B,
  #[display("c")]
  C,
  #[display("d")]
  D,
  #[display("e")]
  E,
  #[display("f")]
  F,
  #[display("h")]
  H,
  #[display("l")]
  L,
  #[display("af")]
  AF,
  #[display("bc")]
  BC,
  #[display("de")]
  DE,
  #[display("hl")]
  HL,
  #[display("sp")]
  SP,
  #[display("[bc]")]
  BCmem,
  #[display("[de]")]
  DEmem,
  #[display("[hl]")]
  HLmem,
  #[display("[c]")]
  Cmem,
  #[display("tmp_{_0}")]
  Temp(u32),
  #[display("{_0}")]
  Ident(StrID),
  #[display("{_0}")]
  Value(i32),
  #[display("[${_0:04X}]")]
  MemAtAddr(u16),
  #[display("[{_0}]")]
  MemAtSymbol(StrID),
  #[display("size_of_static!({_0})")]
  SizeOfStatic(StrID),
  #[display("&{_0}")]
  RefToSymbol(StrID),
}

#[derive(Debug, Display, Clone, Copy, Default)]
pub enum TacStep {
  #[default]
  TacStepError,

  /// ident()
  #[display("{_0}()")]
  Call(StrID),

  /// `dst = src`
  #[display("{_0} = {_1}")]
  Set(TacVar, TacVar),

  /// `var++`
  #[display("{_0}++")]
  Inc(TacVar),

  /// `var--`
  #[display("{_0}--")]
  Dec(TacVar),
}

#[derive(Debug, Clone, Default)]
pub enum TacFlow {
  #[default]
  TacFlowError,
  Return,
  Always(BlockID),

  AEqImm(i8, BlockID, BlockID),
  ANeImm(i8, BlockID, BlockID),
  ALtImm(i8, BlockID, BlockID),
  AGeImm(i8, BlockID, BlockID),

  AEqVar(TacVar, BlockID, BlockID),
  ANeVar(TacVar, BlockID, BlockID),
  ALtVar(TacVar, BlockID, BlockID),
  AGeVar(TacVar, BlockID, BlockID),

  /// `var++ == 0`
  VarPpEq0(TacVar, BlockID, BlockID),
  /// `var-- == 0`
  VarMmEq0(TacVar, BlockID, BlockID),
  /// `var++ != 0`
  VarPpNe0(TacVar, BlockID, BlockID),
  /// `var-- != 0`
  VarMmNe0(TacVar, BlockID, BlockID),
}

#[derive(Debug, Clone)]
pub struct TacBlock {
  pub id: BlockID,
  pub steps: Vec<TacStep>,
  pub flow: TacFlow,
}

pub fn tac_blocks_from_expr_blocks(expr_blocks: &[ExprBlock]) -> Vec<TacBlock> {
  todo!()
}