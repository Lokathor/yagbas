use super::*;

mod statement;
pub use statement::*;

mod if_else;
pub use if_else::*;

mod static_;
pub use static_::*;

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

#[derive(Debug, Clone, Copy)]
pub enum Reg16 {
  AF,
  BC,
  DE,
  HL,
  SP,
}
