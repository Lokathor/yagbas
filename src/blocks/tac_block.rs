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
impl From<Register> for TacVar {
  fn from(reg: Register) -> Self {
    match reg {
      Register::A => TacVar::A,
      Register::B => TacVar::B,
      Register::C => TacVar::C,
      Register::D => TacVar::D,
      Register::E => TacVar::E,
      Register::H => TacVar::H,
      Register::L => TacVar::L,
      Register::AF => TacVar::AF,
      Register::BC => TacVar::BC,
      Register::DE => TacVar::DE,
      Register::HL => TacVar::HL,
      Register::SP => TacVar::SP,
    }
  }
}

#[derive(Debug, Display, Clone, Copy, Default)]
pub enum TacStep {
  #[default]
  TacStepError,

  #[display("R8({_0}) = I8({_1})")]
  AssignReg8Const8(TacVar, u8),
  #[display("R16({_0}) = I16({_1})")]
  AssignReg16Const16(TacVar, u16),

  #[display("Mem(${_0:04X}) = a")]
  WriteAToMem(u16),
  #[display("a = Mem(${_0:04X})")]
  ReadMemToA(u16),
}

#[derive(Debug, Display, Clone, Copy, Default)]
pub enum TacFlow {
  #[default]
  TacFlowError,
  Return,
  #[display("Always({_0})")]
  Always(BlockID),

  #[display("if a == {_0} then {_1} else {_2}")]
  AEqImm(i8, BlockID, BlockID),
  #[display("if a != {_0} then {_1} else {_2}")]
  ANeImm(i8, BlockID, BlockID),
  #[display("if a < {_0} then {_1} else {_2}")]
  ALtImm(i8, BlockID, BlockID),
  #[display("if a >= {_0} then {_1} else {_2}")]
  AGeImm(i8, BlockID, BlockID),

  #[display("if a == {_0} then {_1} else {_2}")]
  AEqVar(TacVar, BlockID, BlockID),
  #[display("if a != {_0} then {_1} else {_2}")]
  ANeVar(TacVar, BlockID, BlockID),
  #[display("if a < {_0} then {_1} else {_2}")]
  ALtVar(TacVar, BlockID, BlockID),
  #[display("if a >= {_0} then {_1} else {_2}")]
  AGeVar(TacVar, BlockID, BlockID),

  #[display("if {_0}++ == 0 then {_1} else {_2}")]
  VarPpEq0(TacVar, BlockID, BlockID),
  #[display("if {_0}-- == 0 then {_1} else {_2}")]
  VarMmEq0(TacVar, BlockID, BlockID),
  #[display("if {_0}++ != 0 then {_1} else {_2}")]
  VarPpNe0(TacVar, BlockID, BlockID),
  #[display("if {_0}-- != 0 then {_1} else {_2}")]
  VarMmNe0(TacVar, BlockID, BlockID),
}

#[derive(Debug, Clone)]
pub struct TacBlock {
  pub id: BlockID,
  pub steps: Vec<TacStep>,
  pub next: TacFlow,
}
impl TacBlock {
  pub fn new(id: BlockID) -> Self {
    Self { id, steps: Vec::new(), next: TacFlow::TacFlowError }
  }
}

pub fn tac_blocks_from_expr_blocks(expr_blocks: &[ExprBlock]) -> Vec<TacBlock> {
  let mut out_blocks = Vec::new();

  for expr_block in expr_blocks {
    let mut out_block = TacBlock::new(expr_block.id);
    for expr_step in &expr_block.steps {
      tac_steps_from_expr_step(&expr_step.0, &mut out_block.steps);
    }
    out_block.next =
      tac_flow_from_expr_flow(&expr_block.next, &mut out_block.steps);
    out_blocks.push(out_block);
  }

  out_blocks
}

fn tac_steps_from_expr_step(
  expr_block_step: &ExprBlockStep, tacs: &mut Vec<TacStep>,
) {
  match expr_block_step {
    ExprBlockStep::ExprBlockStepError => {
      tacs.push(TacStep::TacStepError);
    }
    other => {
      eprintln!("BAD_TAC_STEP: {other}");
      tacs.push(TacStep::TacStepError);
    }
  }
}

fn tac_flow_from_expr_flow(
  expr_flow: &ExprBlockFlow, _tacs: &mut Vec<TacStep>,
) -> TacFlow {
  match expr_flow {
    ExprBlockFlow::Always(id) => TacFlow::Always(*id),
    ExprBlockFlow::Return => TacFlow::Return,
    ExprBlockFlow::Branch(_expr, _t, _f) => {
      // todo
      TacFlow::TacFlowError
    }
  }
}
