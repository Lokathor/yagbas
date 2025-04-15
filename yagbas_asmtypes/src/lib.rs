#![forbid(unsafe_code)]
#![warn(missing_debug_implementations)]
#![warn(missing_copy_implementations)]
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
#[non_exhaustive]
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
  BinOpHlt(BinOp),

  /// `bin_op a, imm8`
  BinOpImm8(BinOp, u8),

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
  UnOpReg8(UnOp, Reg8),

  /// `un_op [hl]`
  UnOpHlt(UnOp),

  /// `rla`
  Rla,

  /// `rlca`
  Rlca,

  /// `rra`
  Rra,

  /// `rrca`
  Rrca,

  /// `call lit`
  CallLit(u16),

  /// `call sym`
  CallSym(StrID),

  /// `call cond, lit`
  CallCondLit(Cond, u16),

  /// `call cond, sym`
  CallCondSym(Cond, StrID),

  /// `jp hl`
  JumpHl,

  /// `jp lit`
  JumpLit(u16),

  /// `jp sym`
  JumpSym(StrID),

  /// `jp cond, lit`
  JumpCondLit(Cond, u16),

  /// `jp cond, sym`
  JumpCondSym(Cond, StrID),

  /// `jr lit`
  JumpRelLit(u16),

  /// `jp sym`
  JumpRelSym(StrID),

  /// `jr cond, lit`
  JumpRelCondLit(Cond, u16),

  /// `jr cond, sym`
  JumpRelCondSym(Cond, StrID),

  /// `ret cond`
  ReturnCond(Cond),

  /// `ret`
  Return,

  /// `reti`
  ReturnFromInterrupt,

  /// `rst vec`
  Reset(RstVec),

  /// `ccf`, "compliment carry flag"
  Ccf,

  /// `scf`, "set carry flag"
  Scf,

  /// `add hl, sp`
  AddHlSp,

  /// `add sp, i8`
  AddSpDelta(i8),

  /// `dec sp`
  DecSp,

  /// `inc sp`
  IncSp,

  /// `ld sp, lit`
  LdSpLit(u16),

  /// `ld sp, sym`
  LdSpSym(StrID),

  /// `ld [lit], sp`
  LdLitSp(u16),

  /// `ld [sym], sp`
  LdSymSp(StrID),

  /// `ld hl, sp+i8`
  LdHlSpDelta(i8),

  /// `ld sp, hl`
  LdSpHl,

  /// `pop af`
  PopAF,

  /// `pop reg16`
  PopReg16(Reg16),

  /// `push af`
  PushAF,

  /// `push reg16`
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

  /// A sequence of pixel indexes for tile data.
  ///
  /// A tile is always 8 pixels tall, so this is *expected* to always contain a
  /// multiple of 8 values, but that is not a hard requirement.
  ///
  /// GB tiles are index-mapped, with 2 bits per pixel. Each row of eight
  /// indexes is 2 bytes, but instead of being directly encoded as two `[u2;4]`
  /// values in a row, they are split into "bitplanes". The first byte stores
  /// each low bit of the eight indexs, and the second byte stores the high bits
  /// of the eight indexes. Because this is complicated to reason about when
  /// trying to enter data by hand, `rgbasm` allows for using the \` character
  /// followed by a series of plain digits to directly output index values, and
  /// it will convert the data for you.
  ///
  /// ```text
  /// dw `01230123 ; This is equivalent to `db $55,$33`
  /// ```
  ///
  /// When using the backtick syntax, indexes are written with the digits left
  /// to right as they would appear in the actual tile, so `10002222` has an
  /// index of `1` in the leftmost pixel and an index of `2` in all four of the
  /// right side pixels. This means that a `u32` is large enough to hold any
  /// pixel row we need to store, but we must always output the value using
  /// `{val:08}` so that any leading zeroes are properly preserved.
  TileIndexes(Box<Vec<u32>>),
}
impl Asm {
  /// Determines the size of the instruction within a rom.
  #[inline]
  #[must_use]
  pub fn rom_size(&self) -> usize {
    match self {
      Asm::Label(_) => 0,
      Asm::LdReg8Reg8(_, _) => 1,
      Asm::LdReg8Imm8(_, _) => 2,
      Asm::LdReg16Lit(_, _) => 3,
      Asm::LdReg16Sym(_, _) => 3,
      Asm::LdHltReg8(_) => 1,
      Asm::LdHltImm8(_) => 2,
      Asm::LdReg8Hlt(_) => 1,
      Asm::LdReg16tA(_) => 3,
      Asm::LdLitA(_) => 3,
      Asm::LdSymA(_) => 3,
      Asm::LdhLitA(_) => 2,
      Asm::LdhSymA(_) => 2,
      Asm::LdhCA => 1,
      Asm::LdAReg16t(_) => 3,
      Asm::LdALit(_) => 3,
      Asm::LdASym(_) => 3,
      Asm::LdhALit(_) => 2,
      Asm::LdhASym(_) => 2,
      Asm::LdhAC => 1,
      Asm::LdHliA => 1,
      Asm::LdHldA => 1,
      Asm::LdAHli => 1,
      Asm::LdAHld => 1,
      Asm::BinOpReg8(_, _) => 1,
      Asm::BinOpHlt(_) => 1,
      Asm::BinOpImm8(_, _) => 2,
      Asm::DecReg8(_) => 1,
      Asm::DecHlt => 1,
      Asm::IncReg8(_) => 1,
      Asm::IncHlt => 1,
      Asm::AddHlReg16(_) => 1,
      Asm::DecReg16(_) => 1,
      Asm::IncReg16(_) => 1,
      Asm::Cpl => 1,
      Asm::BitTestReg8(_, _) => 2,
      Asm::BitTestHlt(_) => 2,
      Asm::ResetReg8(_, _) => 2,
      Asm::ResetHlt(_) => 2,
      Asm::SetReg8(_, _) => 2,
      Asm::SetHlt(_) => 2,
      Asm::UnOpReg8(_, _) => 2,
      Asm::UnOpHlt(_) => 2,
      Asm::Rla => 1,
      Asm::Rlca => 1,
      Asm::Rra => 1,
      Asm::Rrca => 1,
      Asm::CallLit(_) => 3,
      Asm::CallSym(_) => 3,
      Asm::CallCondLit(_, _) => 3,
      Asm::CallCondSym(_, _) => 3,
      Asm::JumpHl => 1,
      Asm::JumpLit(_) => 3,
      Asm::JumpSym(_) => 3,
      Asm::JumpCondLit(_, _) => 3,
      Asm::JumpCondSym(_, _) => 3,
      Asm::JumpRelLit(_) => 2,
      Asm::JumpRelSym(_) => 2,
      Asm::JumpRelCondLit(_, _) => 2,
      Asm::JumpRelCondSym(_, _) => 2,
      Asm::ReturnCond(_) => 1,
      Asm::Return => 1,
      Asm::ReturnFromInterrupt => 1,
      Asm::Reset(_) => 1,
      Asm::Ccf => 1,
      Asm::Scf => 1,
      Asm::AddHlSp => 1,
      Asm::AddSpDelta(_) => 1,
      Asm::DecSp => 1,
      Asm::IncSp => 1,
      Asm::LdSpLit(_) => 3,
      Asm::LdSpSym(_) => 3,
      Asm::LdLitSp(_) => 3,
      Asm::LdSymSp(_) => 3,
      Asm::LdHlSpDelta(_) => 2,
      Asm::LdSpHl => 1,
      Asm::PopAF => 1,
      Asm::PopReg16(_) => 1,
      Asm::PushAF => 1,
      Asm::PushReg16(_) => 1,
      Asm::DI => 1,
      Asm::EI => 1,
      Asm::Halt => 1,
      Asm::DAA => 1,
      Asm::Nop => 1,
      Asm::Stop => 1,
      Asm::DataBytes(items) => items.len(),
      Asm::TileIndexes(items) => items.len() * core::mem::size_of::<u16>(),
    }
  }
}
impl core::fmt::Display for Asm {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Asm::Label(label) => write!(f, "{label}:"),
      Asm::LdReg8Reg8(dst, src) => write!(f, "ld {dst}, {src}"),
      Asm::LdReg8Imm8(reg, imm) => write!(f, "ld {reg}, ${imm:02X}"),
      Asm::LdReg16Lit(reg, lit) => write!(f, "ld {reg}, ${lit:04X}"),
      Asm::LdReg16Sym(reg, sym) => write!(f, "ld {reg}, {sym}"),
      Asm::LdHltReg8(reg) => write!(f, "ld [hl], {reg}"),
      Asm::LdHltImm8(imm) => write!(f, "ld [hl], ${imm:02X}"),
      Asm::LdReg8Hlt(reg) => write!(f, "ld {reg}, [hl]"),
      Asm::LdReg16tA(reg) => write!(f, "ld [{reg}], a"),
      Asm::LdLitA(lit) => write!(f, "ld [${lit:04X}], a"),
      Asm::LdSymA(sym) => write!(f, "ld [{sym}], a"),
      Asm::LdhLitA(lit) => write!(f, "ldh ${lit:04X}, a"),
      Asm::LdhSymA(sym) => write!(f, "ldh {sym}, a"),
      Asm::LdhCA => write!(f, "ld [c], a"),
      Asm::LdAReg16t(reg) => write!(f, "ld a, [{reg}]"),
      Asm::LdALit(lit) => write!(f, "ld a, [${lit:04X}]"),
      Asm::LdASym(sym) => write!(f, "ld a, [{sym}]"),
      Asm::LdhALit(lit) => write!(f, "ldh a, [${lit:04X}]"),
      Asm::LdhASym(sym) => write!(f, "ldh a, [{sym}]"),
      Asm::LdhAC => write!(f, "ldh a, [c]"),
      Asm::LdHliA => write!(f, "ld [hli], a"),
      Asm::LdHldA => write!(f, "ld [hld], a"),
      Asm::LdAHli => write!(f, "ld a, [hli]"),
      Asm::LdAHld => write!(f, "ld a, [hld]"),
      Asm::BinOpReg8(op, reg) => write!(f, "{op} a, {reg}"),
      Asm::BinOpHlt(op) => write!(f, "{op} a, [hl]"),
      Asm::BinOpImm8(op, imm) => write!(f, "{op} a, ${imm:02X}"),
      Asm::DecReg8(reg) => write!(f, "dec {reg}"),
      Asm::DecHlt => write!(f, "dec [hl]"),
      Asm::IncReg8(reg) => write!(f, "inc {reg}"),
      Asm::IncHlt => write!(f, "inc [hl]"),
      Asm::AddHlReg16(reg) => write!(f, "add hl, {reg}"),
      Asm::DecReg16(reg) => write!(f, "dec {reg}"),
      Asm::IncReg16(reg) => write!(f, "inc {reg}"),
      Asm::Cpl => write!(f, "cpl a"),
      Asm::BitTestReg8(u3, reg) => write!(f, "bit {u3}, {reg}"),
      Asm::BitTestHlt(u3) => write!(f, "bit {u3}, [hl]"),
      Asm::ResetReg8(u3, reg) => write!(f, "res {u3}, {reg}"),
      Asm::ResetHlt(u3) => write!(f, "res {u3}, [hl]"),
      Asm::SetReg8(u3, reg) => write!(f, "set {u3}, {reg}"),
      Asm::SetHlt(u3) => write!(f, "set {u3}, [hl]"),
      Asm::UnOpReg8(op, reg) => write!(f, "{op} {reg}"),
      Asm::UnOpHlt(op) => write!(f, "{op} [hl]"),
      Asm::Rla => write!(f, "rla"),
      Asm::Rlca => write!(f, "rlca"),
      Asm::Rra => write!(f, "rra"),
      Asm::Rrca => write!(f, "rrca"),
      Asm::CallLit(lit) => write!(f, "call ${lit:04X}"),
      Asm::CallSym(sym) => write!(f, "call {sym}"),
      Asm::CallCondLit(cond, lit) => write!(f, "call {cond}, ${lit:04X}"),
      Asm::CallCondSym(cond, sym) => write!(f, "call {cond}, {sym}"),
      Asm::JumpHl => write!(f, "jp hl"),
      Asm::JumpLit(lit) => write!(f, "jp ${lit:04X}"),
      Asm::JumpSym(sym) => write!(f, "jp {sym}"),
      Asm::JumpCondLit(cond, lit) => write!(f, "jp {cond}, ${lit:04X}"),
      Asm::JumpCondSym(cond, sym) => write!(f, "jp {cond}, {sym}"),
      Asm::JumpRelLit(lit) => write!(f, "jr ${lit:04X}"),
      Asm::JumpRelSym(sym) => write!(f, "jr {sym}"),
      Asm::JumpRelCondLit(cond, lit) => write!(f, "jp {cond}, ${lit:04X}"),
      Asm::JumpRelCondSym(cond, sym) => write!(f, "jp {cond}, {sym}"),
      Asm::ReturnCond(cond) => write!(f, "ret {cond}"),
      Asm::Return => write!(f, "ret"),
      Asm::ReturnFromInterrupt => write!(f, "reti"),
      Asm::Reset(vec) => write!(f, "rst {vec}"),
      Asm::Ccf => write!(f, "ccf"),
      Asm::Scf => write!(f, "scf"),
      Asm::AddHlSp => write!(f, "add hl, sp"),
      Asm::AddSpDelta(i) => write!(f, "add sp, {i}"),
      Asm::DecSp => write!(f, "dec sp"),
      Asm::IncSp => write!(f, "inc sp"),
      Asm::LdSpLit(lit) => write!(f, "ld sp, ${lit:04X}"),
      Asm::LdSpSym(sym) => write!(f, "ld sp, {sym}"),
      Asm::LdLitSp(lit) => write!(f, "ld ${lit:04X}, sp"),
      Asm::LdSymSp(sym) => write!(f, "ld {sym}, sp"),
      Asm::LdHlSpDelta(i) => write!(f, "ld hl, sp{i:+}"),
      Asm::LdSpHl => write!(f, "ld sp, hl"),
      Asm::PopAF => write!(f, "pop af"),
      Asm::PopReg16(reg) => write!(f, "pop {reg}"),
      Asm::PushAF => write!(f, "push af"),
      Asm::PushReg16(reg) => write!(f, "push {reg}"),
      Asm::DI => write!(f, "di"),
      Asm::EI => write!(f, "ei"),
      Asm::Halt => write!(f, "halt"),
      Asm::DAA => write!(f, "daa"),
      Asm::Nop => write!(f, "nop"),
      Asm::Stop => write!(f, "stop"),
      Asm::DataBytes(items) => {
        for (i, chunk) in items.chunks(16).enumerate() {
          if i > 0 {
            writeln!(f)?;
          }
          write!(f, "db ")?;
          for (i, c) in chunk.iter().enumerate() {
            if i > 0 {
              write!(f, ", ")?;
            }
            write!(f, "${c:02X}")?;
          }
        }
        Ok(())
      }
      Asm::TileIndexes(items) => {
        for (i, item) in items.iter().enumerate() {
          if i > 0 {
            writeln!(f)?;
          }
          if *item > 99999999_u32 {
            eprintln!("Warning: Illegal pixel index value: {item}");
          }
          write!(f, "dw `{item:08}")?;
        }
        Ok(())
      }
    }
  }
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

/// The three 16-bit registers available for use with most 16-bit instructions.
#[derive(Debug, Clone, Copy)]
#[allow(missing_docs)]
pub enum Reg16 {
  BC,
  DE,
  HL,
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
impl core::fmt::Display for U3 {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      match self {
        U3::_0 => "0",
        U3::_1 => "1",
        U3::_2 => "2",
        U3::_3 => "3",
        U3::_4 => "4",
        U3::_5 => "5",
        U3::_6 => "6",
        U3::_7 => "7",
      }
    )
  }
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
impl core::fmt::Display for Cond {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Cond::Z => "z",
        Cond::NZ => "nz",
        Cond::C => "c",
        Cond::NC => "nc",
      }
    )
  }
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
impl core::fmt::Display for RstVec {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      match self {
        RstVec::X00 => "$00",
        RstVec::X08 => "$08",
        RstVec::X10 => "$10",
        RstVec::X18 => "$18",
        RstVec::X20 => "$20",
        RstVec::X28 => "$28",
        RstVec::X30 => "$30",
        RstVec::X38 => "$38",
      }
    )
  }
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
impl core::fmt::Display for UnOp {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      match self {
        UnOp::Rl => "rl",
        UnOp::Rlc => "rlc",
        UnOp::Rr => "rr",
        UnOp::Rrc => "rrc",
        UnOp::Sla => "sla",
        UnOp::Sra => "sra",
        UnOp::Srl => "srl",
        UnOp::Swap => "swap",
      }
    )
  }
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
  ///
  /// * Zero flag is set when `x` is equal to `a`
  /// * Carry flag is set when `x` is greater than `a`
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
impl core::fmt::Display for BinOp {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      match self {
        BinOp::Adc => "adc",
        BinOp::Add => "add",
        BinOp::And => "and",
        BinOp::Cp => "cp",
        BinOp::Or => "or",
        BinOp::Sbc => "sbc",
        BinOp::Sub => "sub",
        BinOp::Xor => "xor",
      }
    )
  }
}
