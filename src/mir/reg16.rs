use super::*;

/// The standard 16-bit register pairs
///
/// * Importantly, this **excludes** `af` and `sp`.
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
