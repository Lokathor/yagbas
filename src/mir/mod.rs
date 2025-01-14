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
  Jump(StrID),

  /// call to another function
  ///
  /// * No actual abi system currently, so basically all calls are total
  ///   optimization black holes.
  Call(StrID),

  /// Return control flow back to the caller
  Return,

  /// Return from an interrupt handler
  ReturnFromInterrupt,

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
  LoadAReg16t(Reg16),

  /// `[reg16] = a`
  StoreReg16tA(Reg16),

  /// `a = [$FF00+c]`
  ///
  /// Load from the high-page with `c` as an offset
  LoadAHpc,

  /// `[$FF00+c] = a`
  ///
  /// Store to the high-page with `c` as an offset
  StoreHpcA,

  /// `a = [imm16]`
  LoadAImm16t(u16),

  /// `[imm16] = a`
  StoreImm16tA(u16),

  /// `a = [label]`
  LoadALabelT(u16),

  /// `[label] = a`
  StoreLabelTA(u16),

  /// `reg8 = imm8`
  AssignReg8Imm8(Reg8, u8),

  /// `reg16 = imm16`
  AssignReg16Imm16(Reg16, u16),

  /// `reg16 = label`
  AssignReg16Label(Reg16, StrID),

  /// Does `a = op(arg0, arg1)` for any binary op.
  BinOpReg(BinaryOp, Reg8, Reg8),

  /// Does `op(arg)` for any binary op.
  ///
  /// * In some cases this modifies `arg` in place
  /// * In other cases this set the flags.
  UnOpReg(UnaryOp, Reg8),
}
impl Mir {
  #[inline]
  #[must_use]
  pub fn zero_effect(&self) -> FlagEffect {
    match self {
      Mir::Return
      | Mir::ReturnFromInterrupt
      | Mir::Inc16(_)
      | Mir::Dec16(_)
      | Mir::LoadAReg16t(_)
      | Mir::StoreReg16tA(_)
      | Mir::LoadAImm16t(_)
      | Mir::StoreImm16tA(_)
      | Mir::AssignReg8Imm8(_, _)
      | Mir::AssignReg16Imm16(_, _)
      | Mir::AssignReg16Label(_, _)
      | Mir::LoadALabelT(_)
      | Mir::StoreLabelTA(_)
      | Mir::LoadAHpc
      | Mir::StoreHpcA => FlagEffect::NoEffect,
      Mir::Inc8(_) | Mir::Dec8(_) => FlagEffect::ByOutput,
      Mir::Loop(_) | Mir::If(_) | Mir::Jump(_) | Mir::Call(_) => {
        FlagEffect::Unknown
      }
      // TODO: With some ops we statically know the answer is actually going to
      // be `FlagEffect::AlwaysTrue` if both input registers are `a`.
      Mir::BinOpReg(binary_op, _, _) => FlagEffect::ByOutput,
      Mir::UnOpReg(unary_op, _) => FlagEffect::ByOutput,
    }
  }

  #[inline]
  #[must_use]
  pub fn carry_effect(&self) -> FlagEffect {
    match self {
      Mir::Return
      | Mir::ReturnFromInterrupt
      | Mir::Inc16(_)
      | Mir::Dec16(_)
      | Mir::LoadAReg16t(_)
      | Mir::StoreReg16tA(_)
      | Mir::LoadAImm16t(_)
      | Mir::StoreImm16tA(_)
      | Mir::AssignReg8Imm8(_, _)
      | Mir::AssignReg16Imm16(_, _)
      | Mir::AssignReg16Label(_, _)
      | Mir::Inc8(_)
      | Mir::Dec8(_)
      | Mir::LoadALabelT(_)
      | Mir::StoreLabelTA(_)
      | Mir::LoadAHpc
      | Mir::StoreHpcA => FlagEffect::NoEffect,
      Mir::Loop(_) | Mir::If(_) | Mir::Jump(_) | Mir::Call(_) => {
        FlagEffect::Unknown
      }
      // TODO: some ops actually always set carry to 0, or even don't touch the
      // flag.
      Mir::BinOpReg(binary_op, _, _) => FlagEffect::ByOutput,
      Mir::UnOpReg(unary_op, _) => FlagEffect::ByOutput,
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
