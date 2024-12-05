use super::*;

/// Performs math using the A register along with some other value.
#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
  AddCarry,
  Add,
  BitAnd,
  Compare,
  BitOr,
  SubCarry,
  Sub,
  BitXor,
}

/// Manipulates the bits of a single value
#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
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

/// The standard 8-bit registers
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

/// The standard 16-bit register pairs
#[derive(Debug, Clone, Copy)]
pub enum Reg16 {
  BC,
  DE,
  HL,
}

/// The conditions for calls and jumps
#[derive(Debug, Clone, Copy)]
pub enum Condition {
  Carry,
  NoCarry,
  Zero,
  NonZero,
  Always,
}

/// A value that can be expressed in a single line of assembly.
#[derive(Debug, Clone)]
pub enum Asm {
  Label(StrID),
  MathAReg8(BinaryOp, Reg8),
  MathAHlt(BinaryOp),
  MathAImm8(BinaryOp, u8),
  IncReg8(Reg8),
  IncHlt,
  DecReg8(Reg8),
  DecHlt,
  AddHlImm16(u16),
  IncReg16(Reg16),
  DecReg16(Reg16),
  BitOpReg8(UnaryOp, Reg8),
  BitOpHlt(UnaryOp),
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
  CallLabel(Condition, StrID),
  JumpHL,
  JumpLabel(Condition, StrID),
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
