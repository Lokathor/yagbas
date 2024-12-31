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
