use super::*;

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
  Jump(MirJump),

  /// call to another function
  ///
  /// * No actual abi system currently, so basically all calls are total
  ///   optimization black holes.
  Call(MirCall),

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
}

#[derive(Debug, Clone)]
pub struct MirLoop {
  pub steps: Vec<Mir>,
  pub canonical_name: StrID,
  pub canonical_start: StrID,
  pub canonical_end: StrID,
}

#[derive(Debug, Clone)]
pub struct MirIf {
  pub condition: Condition,
  pub steps: Vec<Mir>,
  pub canonical_name: StrID,
  pub canonical_end: StrID,
}

#[derive(Debug, Clone, Copy)]
pub struct MirJump {
  pub target: StrID,
}

#[derive(Debug, Clone, Copy)]
pub struct MirCall {
  pub target: StrID,
  pub abi: Abi,
}

#[derive(Debug, Clone, Copy)]
pub struct Abi {
  // todo: an ABI system
}
