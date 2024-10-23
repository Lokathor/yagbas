
#[derive(Debug, Clone, Copy)]
pub enum Cond {
  Zero,
  NonZero,
  Carry,
  NoCarry,
  Always,
}

#[derive(Debug, Clone, Copy)]
pub enum Asm {
  Label(StrID),
  Call(Cond, StrID),
  Return(Cond, StrID),
}
