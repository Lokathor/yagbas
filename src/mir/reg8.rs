use super::*;

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
