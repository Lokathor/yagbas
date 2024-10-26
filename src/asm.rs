use crate::str_id::StrID;

#[derive(Debug, Clone, Copy)]
pub enum Cond {
  NonZero,
  NoCarry,
  Zero,
  Carry,
  Always,
}

#[derive(Debug, Clone, Copy)]
pub enum Asm {
  Label(StrID),
  Nop,
  Call(Cond, StrID),
  Return(Cond, StrID),
  Jump(Cond, StrID),
}
impl Asm {
  #[inline]
  #[must_use]
  pub const fn bytes(self) -> &'static [u8] {
    match self {
      Asm::Label(str_id) => &[],
      Asm::Nop => &[0x00],
      Asm::Call(cond, _) => match cond {
        Cond::NonZero => &[0xC4, 0xFC, 0xFD],
        Cond::NoCarry => &[0xD4, 0xFC, 0xFD],
        Cond::Zero => &[0xCC, 0xFC, 0xFD],
        Cond::Carry => &[0xDC, 0xFC, 0xFD],
        Cond::Always => &[0xCD, 0xFC, 0xFD],
      },
      Asm::Return(cond, str_id) => match cond {
        Cond::NonZero => &[0xC0, 0xFC, 0xFD],
        Cond::NoCarry => &[0xD0, 0xFC, 0xFD],
        Cond::Zero => &[0xC8, 0xFC, 0xFD],
        Cond::Carry => &[0xD8, 0xFC, 0xFD],
        Cond::Always => &[0xC9, 0xFC, 0xFD],
      },
      Asm::Jump(cond, str_id) => match cond {
        Cond::NonZero => &[0xC2, 0xFC, 0xFD],
        Cond::NoCarry => &[0xD2, 0xFC, 0xFD],
        Cond::Zero => &[0xCA, 0xFC, 0xFD],
        Cond::Carry => &[0xDA, 0xFC, 0xFD],
        Cond::Always => &[0xC3, 0xFC, 0xFD],
      },
    }
  }
}
