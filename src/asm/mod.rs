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
impl core::fmt::Display for BinaryOp {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      match self {
        BinaryOp::AddCarry => "adc",
        BinaryOp::Add => "add",
        BinaryOp::BitAnd => "and",
        BinaryOp::Compare => "cp",
        BinaryOp::BitOr => "or",
        BinaryOp::SubCarry => "sbc",
        BinaryOp::Sub => "sub",
        BinaryOp::BitXor => "xor",
      }
    )
  }
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
impl core::fmt::Display for UnaryOp {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      match self {
        UnaryOp::Test(0) => "bit 0,",
        UnaryOp::Test(1) => "bit 1,",
        UnaryOp::Test(2) => "bit 2,",
        UnaryOp::Test(3) => "bit 3,",
        UnaryOp::Test(4) => "bit 4,",
        UnaryOp::Test(5) => "bit 5,",
        UnaryOp::Test(6) => "bit 6,",
        UnaryOp::Test(7) => "bit 7,",
        UnaryOp::Set(0) => "set 0,",
        UnaryOp::Set(1) => "set 1,",
        UnaryOp::Set(2) => "set 2,",
        UnaryOp::Set(3) => "set 3,",
        UnaryOp::Set(4) => "set 4,",
        UnaryOp::Set(5) => "set 5,",
        UnaryOp::Set(6) => "set 6,",
        UnaryOp::Set(7) => "set 7,",
        UnaryOp::Clear(0) => "res 0,",
        UnaryOp::Clear(1) => "res 1,",
        UnaryOp::Clear(2) => "res 2,",
        UnaryOp::Clear(3) => "res 3,",
        UnaryOp::Clear(4) => "res 4,",
        UnaryOp::Clear(5) => "res 5,",
        UnaryOp::Clear(6) => "res 6,",
        UnaryOp::Clear(7) => "res 7,",
        UnaryOp::Swap => "swap",
        UnaryOp::RotateLeft => "rl",
        UnaryOp::RotateLeftCarryless => "rlc",
        UnaryOp::RotateRight => "rr",
        UnaryOp::RotateRightCarryless => "rlc",
        UnaryOp::ShiftLeftArithmetic => "sla",
        UnaryOp::ShiftRightArithmetic => "sra",
        UnaryOp::ShiftRightLogical => "srl",
        _ => unimplemented!(),
      }
    )
  }
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
impl TryFrom<Register> for Reg8 {
  type Error = ();
  #[inline]
  fn try_from(value: Register) -> Result<Self, Self::Error> {
    Ok(match value {
      Register::A => Reg8::A,
      Register::B => Reg8::B,
      Register::C => Reg8::C,
      Register::D => Reg8::D,
      Register::E => Reg8::E,
      Register::H => Reg8::H,
      Register::L => Reg8::L,
      _ => return Err(()),
    })
  }
}
impl core::fmt::Display for Reg8 {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Reg8::A => "a",
        Reg8::B => "b",
        Reg8::C => "c",
        Reg8::D => "d",
        Reg8::E => "e",
        Reg8::H => "h",
        Reg8::L => "l",
      }
    )
  }
}

/// The standard 16-bit register pairs
#[derive(Debug, Clone, Copy)]
pub enum Reg16 {
  BC,
  DE,
  HL,
}
impl TryFrom<Register> for Reg16 {
  type Error = ();
  #[inline]
  fn try_from(value: Register) -> Result<Self, Self::Error> {
    Ok(match value {
      Register::BC => Reg16::BC,
      Register::DE => Reg16::DE,
      Register::HL => Reg16::HL,
      _ => return Err(()),
    })
  }
}
impl core::fmt::Display for Reg16 {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Reg16::BC => "bc",
        Reg16::DE => "de",
        Reg16::HL => "hl",
      }
    )
  }
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
impl core::fmt::Display for Condition {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Condition::Carry => "c",
        Condition::NoCarry => "nc",
        Condition::Zero => "z",
        Condition::NonZero => "nz",
        Condition::Always => unimplemented!(),
      }
    )
  }
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

  /// `ld [hli], a`
  LoadHlIncA,

  /// `ld [hld], a`
  LoadHlDecA,

  /// `ld a, [hli]`
  LoadAHlInc,

  /// `ld a, [hld]`
  LoadAHlDec,

  /// `call label`
  CallLabel(Condition, StrID),

  /// `jp hl`
  JumpToHL,

  /// `jp label`
  JumpToLabel(Condition, StrID),

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

  /// A raw sequence of bytes, generally "data" rather than "code".
  RawBytes(Vec<u8>),
}
impl Asm {
  /// The size of this assembly within a rom.
  pub fn rom_size(&self) -> usize {
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
      | Asm::JumpToHL
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
      | Asm::JumpToLabel(_, _)
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
      Asm::RawBytes(vec) => vec.len(),
    }
  }
}
impl core::fmt::Display for Asm {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Asm::Label(str_id) => write!(f, "{str_id}"),
      Asm::MathAReg8(binary_op, reg8) => write!(f, "{binary_op} a, {reg8}"),
      Asm::MathAHlt(binary_op) => write!(f, "{binary_op} a, [hl]"),
      Asm::MathAImm8(binary_op, i) => write!(f, "{binary_op} a, ${i:X}"),
      Asm::IncReg8(reg8) => write!(f, "inc {reg8}"),
      Asm::IncHlt => write!(f, "inc [hl]"),
      Asm::DecReg8(reg8) => write!(f, "dec {reg8}"),
      Asm::DecHlt => write!(f, "dec [hl]"),
      Asm::AddHlImm16(i) => write!(f, "add hl, ${i:X}"),
      Asm::IncReg16(reg16) => write!(f, "inc {reg16}"),
      Asm::DecReg16(reg16) => write!(f, "dec {reg16}"),
      Asm::BitOpReg8(unary_op, reg8) => write!(f, "{unary_op} {reg8}"),
      Asm::BitOpHlt(unary_op) => write!(f, "{unary_op} [hl]"),
      Asm::RotateLeftA => write!(f, "rla"),
      Asm::RotateLeftCarrylessA => write!(f, "rlca"),
      Asm::RotateRightA => write!(f, "rra"),
      Asm::RotateRightCarrylessA => write!(f, "rrca"),
      Asm::LoadReg8Reg8(left, right) => write!(f, "ld {left}, {right}"),
      Asm::LoadReg8Imm8(reg8, i) => write!(f, "ld {reg8}, ${i:X}"),
      Asm::LoadReg16Imm16(reg16, i) => write!(f, "ld {reg16}, ${i:X}"),
      Asm::LoadReg16Label(reg16, str_id) => write!(f, "ld {reg16}, {str_id}"),
      Asm::LoadHltReg8(reg8) => write!(f, "ld [hl], {reg8}"),
      Asm::LoadHltImm8(i) => write!(f, "ld [hl], ${i:X}"),
      Asm::LoadReg8Hlt(reg8) => write!(f, "ld {reg8}, [hl]"),
      Asm::LoadReg16tA(reg16) => write!(f, "ld [{reg16}], a"),
      Asm::LoadImm16tA(imm16) => write!(f, "ld [${imm16:X}], a"),
      Asm::LoadLabelA(label) => write!(f, "ld [{label}], a"),
      Asm::LoadHighImm8A(i) => write!(f, "ldh, [{i}], a"),
      Asm::LoadHighCtA => write!(f, "ldh [c], a"),
      Asm::LoadAReg16t(reg16) => write!(f, "ld a, [{reg16}]"),
      Asm::LoadAImm16t(imm16) => write!(f, "ld a, [${imm16:X}]"),
      Asm::LoadALabel(label) => write!(f, "ld a, [{label}]"),
      Asm::LoadHighAImm16t(imm8) => write!(f, "ldh a, [${imm8:X}]"),
      Asm::LoadHighACt => write!(f, "ldh a, [c]"),
      Asm::LoadHlIncA => write!(f, "ld [hli], a"),
      Asm::LoadHlDecA => write!(f, "ld [hld], a"),
      Asm::LoadAHlInc => write!(f, "ld a, [hli]"),
      Asm::LoadAHlDec => write!(f, "ld a, [hld]"),
      Asm::CallLabel(Condition::Always, label) => write!(f, "call {label}"),
      Asm::CallLabel(cond, label) => write!(f, "call {cond}, {label}"),
      Asm::JumpToHL => write!(f, "jp hl"),
      Asm::JumpToLabel(Condition::Always, label) => write!(f, "jp {label}"),
      Asm::JumpToLabel(cond, label) => write!(f, "jp {cond}, {label}"),
      Asm::Return(Condition::Always) => write!(f, "ret"),
      Asm::Return(cond) => write!(f, "ret {cond}"),
      Asm::ReturnFromInterrupt => write!(f, "reti"),
      Asm::ResetVector(x) => write!(f, "rst ${x:02X}"),
      Asm::AddHlSp => write!(f, "add hl, sp"),
      Asm::AddSpImm8(i) => write!(f, "add sp, {i}"),
      Asm::DecSp => write!(f, "dec sp"),
      Asm::IncSp => write!(f, "inc sp"),
      Asm::LoadSpImm16(imm16) => write!(f, "ld sp, {imm16}"),
      Asm::LoadSpLabel(label) => write!(f, "ld sp, {label}"),
      Asm::LoadImm16tSp(imm16) => write!(f, "ld [{imm16}], sp"),
      Asm::LoadLabelSp(label) => write!(f, "ld [{label}], sp"),
      Asm::LoadHlSpDelta(i) => write!(f, "ld hl, sp {i:+}"),
      Asm::LoadSpHl => write!(f, "ld sp, hl"),
      Asm::PopAF => write!(f, "pop af"),
      Asm::PopReg16(reg16) => write!(f, "pop {reg16}"),
      Asm::PushAF => write!(f, "push af"),
      Asm::PushReg16(reg16) => write!(f, "push {reg16}"),
      Asm::SetCarryFlag => write!(f, "scf"),
      Asm::ComplimentCarryFlag => write!(f, "ccf"),
      Asm::ComplimentA => write!(f, "cpl"),
      Asm::DecimalAdjustAccumulator => write!(f, "daa"),
      Asm::DisableInterrupts => write!(f, "di"),
      Asm::EnableInterrupts => write!(f, "ei"),
      Asm::Halt => write!(f, "halt"),
      Asm::Stop => write!(f, "stop"),
      Asm::Nop => write!(f, "nop"),
      Asm::RawBytes(vec) => {
        for (i, chunk) in vec.chunks(16).enumerate() {
          if i > 0 {
            write!(f, "\n    ")?;
          }
          write!(f, "db ")?;
          for (i, byte) in chunk.iter().enumerate() {
            if i > 0 {
              write!(f, ",")?;
            }
            write!(f, "${byte:02X}")?;
          }
        }
        Ok(())
      }
    };
    Ok(())
  }
}
