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
