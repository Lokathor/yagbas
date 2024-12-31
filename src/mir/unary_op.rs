use super::*;

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
