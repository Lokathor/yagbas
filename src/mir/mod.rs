use super::*;

pub mod binary_op;
pub mod condition;
pub mod reg16;
pub mod reg8;
pub mod unary_op;

#[derive(Debug, Clone)]
pub enum Mir {
  /// standard looping over a body of "inner" steps
  Loop(MirLoop),

  /// when a condition holds, do this
  If(MirIf),

  /// both break and continue are just "jump"
  ///
  /// * if necessary, you can tell if it was break or continue by the tail of
  ///   the label it targets.
  /// * Converting jumps to a label into relative jumps happens at the assembly
  ///   stage, in MIR all static jumps are to a label.
  JumpLabel(StrID),

  /// call to another function
  ///
  /// * No actual abi system currently, so basically all calls are total
  ///   optimization black holes.
  Call(StrID),

  /// go back to the caller
  Return,

  /// `inc reg16`
  ///
  /// * **Flags:** none
  Inc16(Reg16),

  /// `dec reg16`
  ///
  /// * **Flags:** none
  Dec16(Reg16),

  /// `inc reg8`
  ///
  /// * **Flags:** `z`
  Inc8(Reg8),

  /// `dec reg8`
  ///
  /// * **Flags:** `z`
  Dec8(Reg8),

  /// `a = [reg16]`
  AssignAReg16t(Reg16),

  /// `[reg16] = a`
  AssignReg16tA(Reg16),

  /// `a = [imm16]`
  AssignAImm16t(u16),

  /// `[imm16] = a`
  AssignImm16tA(u16),

  /// `reg8 = imm8`
  AssignReg8Imm8(Reg8, u8),

  /// `reg16 = imm16`
  AssignReg16Imm16(Reg16, u16),

  /// `reg16 = label`
  ///
  /// This is necessary to load the address of an item, since we don't know the
  /// address of an item until after linking.
  AssignReg16Label(Reg16, StrID),
}
impl Mir {
  #[inline]
  #[must_use]
  pub fn zero_effect(&self) -> FlagEffect {
    match self {
      Mir::Return
      | Mir::Inc16(_)
      | Mir::Dec16(_)
      | Mir::AssignAReg16t(_)
      | Mir::AssignReg16tA(_)
      | Mir::AssignAImm16t(_)
      | Mir::AssignImm16tA(_)
      | Mir::AssignReg8Imm8(_, _)
      | Mir::AssignReg16Imm16(_, _)
      | Mir::AssignReg16Label(_, _) => FlagEffect::NoEffect,
      Mir::Inc8(_) | Mir::Dec8(_) => FlagEffect::ByOutput,
      Mir::Loop(_) | Mir::If(_) | Mir::JumpLabel(_) | Mir::Call(_) => {
        FlagEffect::Unknown
      }
    }
  }

  #[inline]
  #[must_use]
  pub fn carry_effect(&self) -> FlagEffect {
    match self {
      Mir::Return
      | Mir::Inc16(_)
      | Mir::Dec16(_)
      | Mir::AssignAReg16t(_)
      | Mir::AssignReg16tA(_)
      | Mir::AssignAImm16t(_)
      | Mir::AssignImm16tA(_)
      | Mir::AssignReg8Imm8(_, _)
      | Mir::AssignReg16Imm16(_, _)
      | Mir::AssignReg16Label(_, _)
      | Mir::Inc8(_)
      | Mir::Dec8(_) => FlagEffect::NoEffect,
      Mir::Loop(_) | Mir::If(_) | Mir::JumpLabel(_) | Mir::Call(_) => {
        FlagEffect::Unknown
      }
    }
  }
}

#[derive(Debug, Clone)]
pub struct MirLoop {
  pub steps: Vec<Mir>,
  pub canonical_name: StrID,
}

#[derive(Debug, Clone)]
pub struct MirIf {
  pub condition: Condition,
  pub steps: Vec<Mir>,
  pub canonical_name: StrID,
}

#[derive(Debug, Clone, Copy)]
pub enum FlagEffect {
  /// This action doesn't affect this flag
  NoEffect,
  /// This action always sets the flag to true.
  AlwaysTrue,
  /// This action always sets the flag to false.
  AlwaysFalse,
  /// This flag sets the flag based on the output of the operation.
  ByOutput,
  /// The flag effect can't be stated in a normal way, probably because we're
  /// doing a control flow thing.
  Unknown,
}
