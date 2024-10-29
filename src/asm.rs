use crate::str_id::StrID;

#[derive(Debug, Clone, Copy)]
pub enum Asm {
  Label(StrID),
  Nop,
  Call(Cond, StrID),
  Return(Cond),
  Jump(Cond, StrID),
}
impl core::fmt::Display for Asm {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    use core::fmt::Write;
    match self {
      Asm::Label(name) => write!(f, "{name}:"),
      Asm::Nop => write!(f, "nop"),
      Asm::Call(Cond::Always, target) => write!(f, "call {target}"),
      Asm::Call(cond, target) => write!(f, "call {cond}, {target}"),
      Asm::Return(Cond::Always) => write!(f, "ret"),
      Asm::Return(cond) => write!(f, "ret {cond}"),
      Asm::Jump(Cond::Always, target) => write!(f, "jp {target}"),
      Asm::Jump(cond, target) => write!(f, "jp {cond}, {target}"),
    }
  }
}

#[derive(Debug, Clone, Copy)]
pub enum Cond {
  NonZero,
  NoCarry,
  Zero,
  Carry,
  Always,
}
impl core::fmt::Display for Cond {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    use core::fmt::Write;
    match self {
      Cond::NonZero => write!(f, "nz"),
      Cond::NoCarry => write!(f, "nc"),
      Cond::Zero => write!(f, "z"),
      Cond::Carry => write!(f, "c"),
      // Note(Lokathor): This variant shouldn't actually be printed, but if it
      // does get printed somehow we'll just fudge it.
      Cond::Always => write!(f, "al"),
    }
  }
}
