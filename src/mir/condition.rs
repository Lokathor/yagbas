use super::*;

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

  /// The "always" condition doesn't get written down in assembly output.
  Always,
}
impl core::fmt::Display for Condition {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Condition::Carry => "c, ",
        Condition::NoCarry => "nc, ",
        Condition::Zero => "z, ",
        Condition::NonZero => "nz, ",
        Condition::Always => "",
      }
    )
  }
}
