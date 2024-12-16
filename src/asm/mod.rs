use super::*;

/// Performs math using the A register along with some other value.
#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
  /// `adc`
  AddCarry,

  /// `add`
  Add,

  /// `and`
  BitAnd,

  /// `cp`
  Compare,

  /// `or`
  BitOr,

  /// `sbc`
  SubCarry,

  /// `sub`
  Sub,

  /// `xor`
  BitXor,
}

/// Manipulates the bits of a single value
#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
  /// `bit n,`
  Test(u8),

  /// `set n,`
  Set(u8),

  /// `res n,`
  Clear(u8),

  /// `swap`
  Swap,

  /// `rl`
  RotateLeft,

  /// `rlc`
  RotateLeftCarryless,

  /// `rr`
  RotateRight,

  /// `rlc`
  RotateRightCarryless,

  /// `sla`
  ///
  /// shift left, signed or unsigned
  ShiftLeftArithmetic,

  /// `sra`
  ///
  /// shift right signed
  ShiftRightArithmetic,

  /// `srl`
  ///
  /// shift right unsigned
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
///
/// * Parts ending in `t` use that as the target. Eg: "Hlt" is "HL target",
///   meaning `[hl]`
#[derive(Debug, Clone)]
pub enum Asm {
  /// `label:`
  Label(StrID),

  /// `op a, r8`
  MathAReg8(BinaryOp, Reg8),

  /// `op a, [hl]
  MathAHlt(BinaryOp),

  /// `op a, imm8`
  MathAImm8(BinaryOp, u8),

  /// `inc reg8`
  IncReg8(Reg8),

  /// `inc [hl]`
  IncHlt,

  /// `dec reg8`
  DecReg8(Reg8),

  /// `dec [hl]`
  DecHlt,

  /// `add [hl], imm16`
  AddHlImm16(u16),

  /// `inc reg16`
  IncReg16(Reg16),

  /// `dec reg16`
  DecReg16(Reg16),

  /// `op reg8`
  BitOpReg8(UnaryOp, Reg8),

  /// `op [hl]`
  BitOpHlt(UnaryOp),

  /// `rla`
  RotateLeftA,

  /// `rlca`
  RotateLeftCarrylessA,

  /// `rra`
  RotateRightA,

  /// `rrca`
  RotateRightCarrylessA,

  /// `ld reg8, reg8`
  LoadReg8Reg8(Reg8, Reg8),

  /// ld reg8, imm8`
  LoadReg8Imm8(Reg8, u8),

  /// `ld reg16, imm16`
  LoadReg16Imm16(Reg16, u16),

  /// `ld reg16, label`
  LoadReg16Label(Reg16, StrID),

  /// `ld [hl], reg8`
  LoadHltReg8(Reg8),

  /// `ld [hl], imm8`
  LoadHltImm8(u8),

  /// `ld reg8, [hl]`
  LoadReg8Hlt(Reg8),

  /// `ld [reg16], a`
  LoadReg16tA(Reg16),

  /// `ld [imm16], a`
  LoadImm16tA(u16),

  /// `ld [label], a`
  LoadLabelA(StrID),

  /// `ldh [imm8], a`
  LoadHighImm8A(u8),

  /// `ldh [c], a`
  LoadHighCtA,

  /// `ld a, [r16]`
  LoadAReg16t(Reg16),

  /// `ld a, [imm16]`
  LoadAImm16t(u16),

  /// `ld a, [label]`
  LoadALabel(StrID),

  /// `ldh a, [imm8]`
  LoadHighAImm16t(u8),

  /// `ldh a, [c]`
  LoadHighACt,

  /// `ld [hl+], a`
  LoadHlIncA,

  /// `ld [hl-], a`
  LoadHlDecA,

  /// `ld a, [hl+]`
  LoadAHlInc,

  /// `ld a, [hl-]`
  LoadAHlDec,

  /// `call label`
  CallLabel(Condition, StrID),

  /// `jp hl`
  JumpHL,

  /// `jp label`
  JumpLabel(Condition, StrID),

  /// `ret` or `ret cond`
  Return(Condition),

  /// `reti`
  ReturnFromInterrupt,

  /// `rst vec`
  ResetVector(u8),

  /// `add hl, sp`
  AddHlSp,

  /// `add sp, signed_imm8`
  AddSpImm8(i8),

  /// `dec sp`
  DecSp,

  /// `inc sp`
  IncSp,

  /// `ld sp, imm16`
  LoadSpImm16(u16),

  /// `ld sp, label`
  LoadSpLabel(StrID),

  /// `ld [imm16], sp`
  LoadImm16tSp(u16),

  /// `ld [label], sp`
  LoadLabelSp(StrID),

  /// `ld hl, sp + signed_imm8`
  LoadHlSpDelta(i8),

  /// `ld sp, hl`
  LoadSpHl,

  /// `pop af`
  PopAF,

  /// `pop reg16`
  PopReg16(Reg16),

  /// `push af`
  PushAF,

  /// `push reg16`
  PushReg16(Reg16),

  /// `scf`
  SetCarryFlag,

  /// `ccf`
  ComplimentCarryFlag,

  /// `cpl`
  ComplimentA,

  /// `daa`
  DecimalAdjustAccumulator,

  /// `di`
  DisableInterrupts,

  /// `ei`
  EnableInterrupts,

  /// `halt`
  Halt,

  /// `stop`
  ///
  /// * This instruction should *always* be followed by a `nop`, because the
  ///   instruction after `stop` is executed or not depending on some hard to
  ///   predict factors.
  /// * See [Pandocs: Using the STOP Instruction](https://gbdev.io/pandocs/Reducing_Power_Consumption.html#using-the-stop-instruction)
  Stop,

  /// `nop`
  Nop,
}
