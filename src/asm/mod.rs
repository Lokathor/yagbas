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
  /// `c,`
  Carry,

  /// `nc,`
  NoCarry,

  /// `z,`
  Zero,

  /// `nz,`
  NonZero,

  /// nothing is printed for the "always" condition.
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

  /// `op a, [hl]`
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
  ///
  /// * Uses the prefix byte.
  BitOpReg8(UnaryOp, Reg8),

  /// `op [hl]`
  ///
  /// * Uses the prefix byte.
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

  /// `ld reg8, imm8`
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
  /// * This instruction is 1 byte itself but should *always* be followed by a
  ///   `nop`, because the instruction after `stop` is executed or not
  ///   depending on some very hard to predict factors.
  /// * See [Pandocs: Using the STOP Instruction](https://gbdev.io/pandocs/Reducing_Power_Consumption.html#using-the-stop-instruction)
  Stop,

  /// `nop`
  Nop,
}
impl Asm {
  /// The size of this assembly within a rom.
  pub const fn rom_size(self) -> usize {
    match self {
      Asm::Label(_) => 0,
      Asm::MathAReg8(_, _)
      | Asm::MathAHlt(_)
      | Asm::IncReg8(_)
      | Asm::IncHlt
      | Asm::DecReg8(_)
      | Asm::DecHlt
      | Asm::IncReg16(_)
      | Asm::DecReg16(_)
      | Asm::RotateLeftA
      | Asm::RotateLeftCarrylessA
      | Asm::RotateRightA
      | Asm::RotateRightCarrylessA
      | Asm::LoadReg8Reg8(_, _)
      | Asm::LoadHltReg8(_)
      | Asm::LoadReg8Hlt(_)
      | Asm::LoadReg16tA(_)
      | Asm::LoadHighCtA
      | Asm::LoadAReg16t(_)
      | Asm::LoadHighACt
      | Asm::LoadHlIncA
      | Asm::LoadHlDecA
      | Asm::LoadAHlInc
      | Asm::LoadAHlDec
      | Asm::JumpHL
      | Asm::Return(_)
      | Asm::ReturnFromInterrupt
      | Asm::ResetVector(_)
      | Asm::AddHlSp
      | Asm::DecSp
      | Asm::IncSp
      | Asm::LoadSpHl
      | Asm::PopAF
      | Asm::PopReg16(_)
      | Asm::PushAF
      | Asm::PushReg16(_)
      | Asm::SetCarryFlag
      | Asm::ComplimentCarryFlag
      | Asm::ComplimentA
      | Asm::DecimalAdjustAccumulator
      | Asm::DisableInterrupts
      | Asm::EnableInterrupts
      | Asm::Halt
      | Asm::Stop
      | Asm::Nop => 1,
      Asm::MathAImm8(_, _)
      | Asm::BitOpReg8(_, _)
      | Asm::BitOpHlt(_)
      | Asm::LoadReg8Imm8(_, _)
      | Asm::LoadHltImm8(_)
      | Asm::LoadHighImm8A(_)
      | Asm::LoadHighAImm16t(_)
      | Asm::JumpLabel(_, _)
      | Asm::AddSpImm8(_)
      | Asm::LoadHlSpDelta(_) => 2,
      Asm::AddHlImm16(_)
      | Asm::LoadReg16Imm16(_, _)
      | Asm::LoadReg16Label(_, _)
      | Asm::LoadImm16tA(_)
      | Asm::LoadLabelA(_)
      | Asm::LoadAImm16t(_)
      | Asm::LoadALabel(_)
      | Asm::CallLabel(_, _)
      | Asm::LoadSpImm16(_)
      | Asm::LoadSpLabel(_)
      | Asm::LoadImm16tSp(_)
      | Asm::LoadLabelSp(_) => 3,
    }
  }
}
