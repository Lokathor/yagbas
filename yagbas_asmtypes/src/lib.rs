#![forbid(unsafe_code)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]

//! The types for assembly manipulation by the [yagbas](https://docs.rs/yagbas)
//! compiler.
//!
//! This crate is a sub-crate of yagbas. Use in any other contexts is not
//! intentionally supported.

use str_id::StrID;

#[test]
fn check_type_size() {
  // Note(Lokathor): It's not an absolute requirement that the size never
  // change, but we should definitely at least be aware of any change in size.
  assert_eq!(size_of::<Asm>(), size_of::<[usize; 2]>());
}

/// A single line of assembly code.
///
/// Instructions that use addresses have both a [u16] "literal" form as well as
/// a [StrID] "symbol" form, which keeps the size of the `Asm` type smaller. The
/// address of a symbol is always a 16-bit value, but the final value generally
/// isn't known until linking is performed.
///
/// Instructions that use "the HL target" are directed at the 8-bit memory
/// location that is currently in the `HL` register.
#[derive(Debug, Clone)]
pub enum Asm {
  /// A label within the code.
  ///
  /// The [Label Declaration][label-dec] rules of RGBDS allow standard C-style
  /// names, and also `#` and `@` are allowed. The `.` is a special character
  /// used for local labels, read their docs for an explanation.
  ///
  /// [label-dec]: https://rgbds.gbdev.io/docs/v0.9.1/rgbasm.5#Label_declaration
  Label(StrID),

  /// `ld reg8, reg8`
  ///
  /// On some emulators, certain operations which load a value back into the
  /// same register are used to help with debugging.
  LdReg8Reg8(Reg8, Reg8),

  /// `ld reg8, imm8`
  LdReg8Imm8(Reg8, u8),

  /// `ld reg16, imm16`
  LdReg16Lit(Reg16, u16),

  /// `ld reg16, sym`
  LdReg16Sym(Reg16, StrID),

  /// `ld [hl], reg8`
  LdHltReg8(Reg8),

  /// `ld [hl], imm8`
  LdHltImm8(u8),

  /// `ld reg8, [hl]`
  LdReg8Hlt(Reg8),

  /// `ld [reg16], a`
  LdReg16tA(Reg16),

  /// `ld [imm16], a`
  LdLitA(u16),

  /// `ld [sym], a`
  LdSymA(StrID),

  /// `ldh [imm16], a`
  LdhLitA(u16),

  /// `ldh [sym], a`
  LdhSymA(StrID),

  /// `ldh [c], a`
  LdhCA,

  /// `ld a, [reg16]`
  LdAReg16t(Reg16),

  /// `ld a, [imm16]`
  LdALit(u16),

  /// `ld a, [sym]`
  LdASym(StrID),

  /// `ldh a, [imm16]`
  LdhALit(u16),

  /// `ldh a, [sym]`
  LdhASym(StrID),

  /// `ldh a, [c]`
  LdhAC,

  /// `ld [hli], a`
  LdHliA,

  /// `ld [hld], a`
  LdHldA,

  /// `ld a, [hli]`
  LdAHli,

  /// `ld a, [hld]`
  LdAHld,

  /// `bin_op a, reg8`
  BinOpReg8(BinOp, Reg8),

  /// `bin_op a, [hl]`
  BinOpHlt,

  /// `bin_op a, imm8`
  BinOpImm8(u8),

  /// `dec reg8`
  DecReg8(Reg8),

  /// `dec [hl]`
  DecHlt,

  /// `inc reg8`
  IncReg8(Reg8),

  /// `inc [hl]`
  IncHlt,

  /// `add hl, reg16`
  AddHlReg16(Reg16),

  /// `dec reg16`
  DecReg16(Reg16),

  /// `inc reg16`
  IncReg16(Reg16),

  /// `cpl a`, "compliment A"
  Cpl,

  /// `bit u3, reg8`
  BitTestReg8(U3, Reg8),

  /// `bit u3, [hl]`
  BitTestHlt(U3),

  /// `res u3, reg8`
  ResetReg8(U3, Reg8),

  /// `res u3, [hl]`
  ResetHlt(U3),

  /// `set u3, reg8`
  SetReg8(U3, Reg8),

  /// `set u3, [hl]`
  SetHlt(U3),

  /// `un_op reg8`
  UnOpReg8(Reg8),

  /// `un_op [hl]`
  UnOpHlt,

  /// `rla`
  Rla,

  /// `rlca`
  Rlca,

  /// `rra`
  Rra,

  /// `rrca`
  Rrca,

  CallLit(u16),
  CallSym(StrID),
  CallCondLit(Cond, u16),
  CallCondSym(Cond, StrID),
  JumpHl,
  JumpLit(u16),
  JumpSym(StrID),
  JumpCondLit(Cond, u16),
  JumpCondSym(Cond, StrID),
  JumpRelLit(u16),
  JumpRelSym(StrID),
  JumpCondRelLit(Cond, u16),
  JumpCondRelSym(Cond, StrID),
  ReturnCond(Cond),
  Return,
  ReturnFromInterrupt,
  Reset(RstVec),

  Ccf,
  Scf,

  AddHlSp,
  AddSpDelta(i8),
  DecSp,
  IncSp,
  LdSpLit(u16),
  LdSpSym(StrID),
  LdLitSp(u16),
  LdSymSp(StrID),
  LdHlSpDelta(i8),
  LdSpHl,
  PopAF,
  PopReg16(Reg16),
  PushAF,
  PushReg16(Reg16),

  /// `di`
  DI,

  /// `ei`
  EI,

  /// `halt`
  Halt,

  /// `daa`, "Decimal Adjust Accumulator".
  DAA,

  /// `nop`, "no operation"
  ///
  /// This instruction does nothing but spend some time.
  Nop,

  /// `stop`
  ///
  /// * This instruction is 1 byte itself but should *always* be followed by a
  ///   `nop`, because the instruction after `stop` is executed or not depending
  ///   on some very hard to predict factors.
  /// * See [Pandocs: Using the STOP Instruction](https://gbdev.io/pandocs/Reducing_Power_Consumption.html#using-the-stop-instruction)
  Stop,

  /// A sequence of data bytes that should appear directly in the assembly.
  ///
  /// This is generally static data of some form, but it could also be code
  /// you've encoded by hand if you need to be sure the assembly optimizer won't
  /// shorten a certain code segment.
  ///
  /// The double-indirection of the payload data lets us keep the size of the
  /// overall `Asm` type as small as possible.
  DataBytes(Box<Vec<u8>>),
}

/// The 8-bit registers usable with most 8-bit instructions.
#[derive(Debug, Clone, Copy)]
#[allow(missing_docs)]
pub enum Reg8 {
  A,
  B,
  C,
  D,
  E,
  H,
  L,
}

/// The three 16-bit registers available for use with most 16-bit instructions.
#[derive(Debug, Clone, Copy)]
#[allow(missing_docs)]
pub enum Reg16 {
  BC,
  DE,
  HL,
}

/// An unsigned 3-bit value, a value in `0 ..= 7`.
#[derive(Debug, Clone, Copy)]
#[allow(missing_docs)]
pub enum U3 {
  _0,
  _1,
  _2,
  _3,
  _4,
  _5,
  _6,
  _7,
}

/// A CPU condition.
#[derive(Debug, Clone, Copy)]
pub enum Cond {
  /// Zero
  Z,
  /// Non-zero
  NZ,
  /// Carry
  C,
  /// No-carry
  NC,
}

/// One of the reset vector addresses.
///
/// The values are written using hexadecimal.
#[derive(Debug, Clone, Copy)]
#[allow(missing_docs)]
pub enum RstVec {
  X00,
  X08,
  X10,
  X18,
  X20,
  X28,
  X30,
  X38,
}

/// An unary operation, that operates on a single register.
#[derive(Debug, Clone, Copy)]
pub enum UnOp {
  /// Rotate left (with carry).
  Rl,
  /// Rotate left (carryless).
  Rlc,
  /// Rotate right (with carry).
  Rr,
  /// Rotate right (carryless).
  Rrc,
  /// Shift left arithmetic.
  Sla,
  /// Shift right arithmetic.
  Sra,
  /// Shift right logical.
  Srl,
  /// Swap the high and low 4-bit chunks.
  Swap,
}

/// A binary operation, using `A` and another register, outputting to `A`.
#[derive(Debug, Clone, Copy)]
pub enum BinOp {
  /// Add with carry
  Adc,
  /// Add
  Add,
  /// BitAnd
  And,
  /// Compare (sets flags based on `a - x` result)
  Cp,
  /// BitOr
  Or,
  /// Subtract with carry
  Sbc,
  /// Subtract
  Sub,
  /// BitXor
  Xor,
}
