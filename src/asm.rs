
use crate::str_id::StrID;

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
  Nop,
  Call(Cond, StrID),
  Return(Cond, StrID),
  Jump(Cond, StrID),
}
