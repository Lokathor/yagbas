use super::*;

#[derive(Debug, Clone, Copy)]
pub enum MathOp {
  AddCarry,
  Add,
  BitAnd,
  Compare,
  BitOr,
  SubCarry,
  Sub,
  BitXor,
}

#[derive(Debug, Clone, Copy)]
pub enum BitsOp {
  Test(u8),
  Set(u8),
  Clear(u8),
  Swap,
  RotateLeft,
  RotateLeftCarryless,
  RotateRight,
  RotateRightCarryless,
  ShiftLeftArithmetic,
  ShiftRightArithmetic,
  ShiftRightLogical,
}

#[derive(Debug, Clone, Copy)]
pub enum Reg8 {
  A,
  B,
  C,
  D,
  E,
  H,
  L,
}
#[derive(Debug, Clone, Copy)]
pub enum Reg16 {
  BC,
  DE,
  HL,
}
#[derive(Debug, Clone, Copy)]
pub enum Condition {
  Carry,
  NoCarry,
  Zero,
  NonZero,
  Always,
}

#[derive(Debug, Clone)]
pub enum Mir {
  MathAReg8(MathOp, Reg8),
  MathAHlt(MathOp),
  MathAImm8(MathOp, u8),
  IncReg8(Reg8),
  IncHlt,
  DecReg8(Reg8),
  DecHlt,
  AddHlImm16(u16),
  IncReg16(Reg16),
  DecReg16(Reg16),
  BitOpReg8(BitsOp, Reg8),
  BitOpHlt(BitsOp),
  RotateLeftA,
  RotateLeftCarrylessA,
  RotateRightA,
  RotateRightCarrylessA,
  LoadReg8Reg8(Reg8, Reg8),
  LoadReg8Imm8(Reg8, u8),
  LoadReg16Imm16(Reg16, u16),
  LoadHltReg8(Reg8),
  LoadHltImm8(u8),
  LoadReg8Hlt(Reg8),
  LoadReg16tA(Reg16),
  LoadImm16tA(u16),
  LoadHighImm16tA(u16),
  LoadHighCtA,
  LoadAReg16t(Reg16),
  LoadAImm16t(u16),
  LoadHighAImm16t(u16),
  LoadHighACt,
  LoadHlIncA,
  LoadHlDecA,
  LoadAHlInc,
  LoadAHlDec,
  Call(Condition, StrID),
  JumpHL,
  Jump(Condition, StrID),
  Return(Condition),
  ReturnFromInterrupt,
  ResetVector(u8),
  AddHlSp,
  AddSpImm8(i8),
  DecSp,
  IncSp,
  LoadSpImm16(u16),
  LoadImm16tSp(u16),
  LoadHlSpDelta(i8),
  LoadSpHl,
  PopAF,
  PopReg16(Reg16),
  PushAF,
  PushReg16(Reg16),
  ComplimentCarryFlag,
  ComplimentA,
  DecimalAdjustAccumulator,
  DisableInterrupts,
  EnableInterrupts,
  Halt,
  Nop,
  SetCarryFlag,
}
