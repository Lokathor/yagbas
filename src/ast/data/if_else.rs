use super::*;

#[derive(Debug, Clone)]
pub struct IfElse {
  pub test: FileSpanned<Expression>,
  pub if_body: Vec<FileSpanned<Statement>>,
  pub else_body: Vec<FileSpanned<Statement>>,
  pub id: usize,
}
impl IfElse {
  /// Gets the next ID value for a new loop.
  ///
  /// This is automatically used by `new` and `new_with_name`.
  #[inline]
  fn generate_next_id() -> usize {
    use core::sync::atomic::{AtomicUsize, Ordering};
    static NEXT_LOOP_ID: AtomicUsize = AtomicUsize::new(1);
    NEXT_LOOP_ID.fetch_add(1, core::sync::atomic::Ordering::Relaxed)
  }

  #[inline]
  pub fn new(
    test: FileSpanned<Expression>, if_body: Vec<FileSpanned<Statement>>,
    else_body: Vec<FileSpanned<Statement>>,
  ) -> Self {
    Self { test, if_body, else_body, id: Self::generate_next_id() }
  }

  pub fn expressions_mut(
    &mut self,
  ) -> impl '_ + InternalIteratorRec<Item = &'_ mut FileSpanned<Expression>> {
    adhoc_internal_iterator_rec!(
      'r, self, |this: &'r mut IfElse, yield_| -> &'r mut FileSpanned<Expression> {
        yield_(&mut this.test)?;
        for stmt in this.if_body.iter_mut() {
          stmt.expressions_mut().try_for_each_rec(yield_)?;
        }
        for stmt in this.else_body.iter_mut() {
          stmt.expressions_mut().try_for_each_rec(yield_)?;
        }
      }
    )
  }

  pub fn calls_ref(
    &self,
  ) -> impl '_ + InternalIteratorRec<Item = &'_ FileSpanned<Call>> {
    adhoc_internal_iterator_rec!(
      'r, self, |this: &'r IfElse, yield_| -> &'r FileSpanned<Call> {
        for stmt in this.if_body.iter() {
          stmt.calls_ref().try_for_each_rec(yield_)?;
        }
        for stmt in this.else_body.iter() {
          stmt.calls_ref().try_for_each_rec(yield_)?;
        }
      }
    )
  }

  pub fn calls_mut(
    &mut self,
  ) -> impl '_ + InternalIteratorRec<Item = &'_ mut FileSpanned<Call>> {
    adhoc_internal_iterator_rec!(
      'r, self, |this: &'r mut IfElse, yield_| -> &'r mut FileSpanned<Call> {
        for stmt in this.if_body.iter_mut() {
          stmt.calls_mut().try_for_each_rec(yield_)?;
        }
        for stmt in this.else_body.iter_mut() {
          stmt.calls_mut().try_for_each_rec(yield_)?;
        }
      }
    )
  }

  pub fn write_code(
    &self, loop_stack: &mut Vec<(Option<StrID>, StrID)>,
    out: &mut impl Extend<Asm>,
  ) {
    match &self.test._payload {
      Expression::Eq(left, right) => match &left._payload {
        Expression::Register(reg) => match Reg8::try_from(reg._payload) {
          Ok(Reg8::A) => {
            // a == ...
            match &right._payload {
              Expression::I32(i) => {
                let imm8 =
                  i32_to_imm8(i._payload).expect("right side out of range");
                out.extend([Asm::MathAImm8(BinaryOp::Compare, imm8)]);
              }
              Expression::Register(r) => {
                let reg8 =
                  Reg8::try_from(r._payload).expect("illegal register");
                out.extend([Asm::MathAReg8(BinaryOp::Compare, reg8)]);
              }
              Expression::Deref(xpr) => match &xpr._payload {
                Expression::Register(reg) if reg._payload == Register::HL => {
                  out.extend([Asm::MathAHlt(BinaryOp::Compare)]);
                }
                other => todo!("unhandled deref target: {other:?}"),
              },
              other => todo!("unhandled right side in equality: {other:?}"),
            }
            // a==whatever does a compare, so `z` will be *set*
            match self.if_body.as_slice() {
              [] => (),
              [s] => match &s._payload {
                Statement::Break(src_target) => {
                  if let Some(canonical) =
                    loop_stack.iter().rev().find(|(name, _)| {
                      src_target._payload.map(|s| s._payload) == *name
                    })
                  {
                    let c = canonical.1;
                    out.extend([Asm::JumpToLabel(
                      Condition::Zero,
                      StrID::from(format!("{c}#end")),
                    )]);
                  } else {
                    todo!("unhandled missing break target")
                  }
                }
                Statement::Continue(src_target) => {
                  if let Some(canonical) =
                    loop_stack.iter().rev().find(|(name, _)| {
                      src_target._payload.map(|s| s._payload) == *name
                    })
                  {
                    let c = canonical.1;
                    out.extend([Asm::JumpToLabel(
                      Condition::Zero,
                      StrID::from(format!("{c}#start")),
                    )]);
                  } else {
                    todo!("unhandled missing continue target")
                  }
                }
                Statement::Return => {
                  out.extend([Asm::Return(Condition::Zero)]);
                }
                other => todo!(),
              },
              many_statements => {
                let id = self.id;
                let end_label = StrID::from(format!(".if{id}#if_body#end"));
                out.extend([Asm::JumpToLabel(Condition::NonZero, end_label)]);
                for statement in many_statements.iter() {
                  statement.write_code(loop_stack, out);
                }
                out.extend([Asm::Label(end_label)]);
              }
            }
            match self.else_body.as_slice() {
              [] => (),
              other => todo!("unhandled else body"),
            }
          }
          other => todo!("unhandled register as left in equality: {other:?}"),
        },
        other => todo!("unhandled left in equality test: {other:?}"),
      },

      Expression::Ne(left, right) => match &left._payload {
        Expression::Register(reg) => match Reg8::try_from(reg._payload) {
          Ok(Reg8::A) => {
            // a != ...
            match &right._payload {
              Expression::I32(i) => {
                let imm8 =
                  i32_to_imm8(i._payload).expect("right side out of range");
                out.extend([Asm::MathAImm8(BinaryOp::Compare, imm8)]);
              }
              Expression::Register(r) => {
                let reg8 =
                  Reg8::try_from(r._payload).expect("illegal register");
                out.extend([Asm::MathAReg8(BinaryOp::Compare, reg8)]);
              }
              Expression::Deref(xpr) => match &xpr._payload {
                Expression::Register(reg) if reg._payload == Register::HL => {
                  out.extend([Asm::MathAHlt(BinaryOp::Compare)]);
                }
                other => todo!("unhandled deref target: {other:?}"),
              },
              other => todo!("unhandled right side in equality: {other:?}"),
            }
            // a!=whatever does a compare, so `z` will be *clear*
            match self.if_body.as_slice() {
              [] => (),
              [s] => match &s._payload {
                Statement::Break(src_target) => {
                  if let Some(canonical) =
                    loop_stack.iter().rev().find(|(name, _)| {
                      src_target._payload.map(|s| s._payload) == *name
                    })
                  {
                    let c = canonical.1;
                    out.extend([Asm::JumpToLabel(
                      Condition::NonZero,
                      StrID::from(format!("{c}#end")),
                    )]);
                  } else {
                    todo!("unhandled missing break target")
                  }
                }
                Statement::Continue(src_target) => {
                  if let Some(canonical) =
                    loop_stack.iter().rev().find(|(name, _)| {
                      src_target._payload.map(|s| s._payload) == *name
                    })
                  {
                    let c = canonical.1;
                    out.extend([Asm::JumpToLabel(
                      Condition::NonZero,
                      StrID::from(format!("{c}#start")),
                    )]);
                  } else {
                    todo!("unhandled missing continue target")
                  }
                }
                Statement::Return => {
                  out.extend([Asm::Return(Condition::NonZero)]);
                }
                other => todo!(),
              },
              many_statements => {
                let id = self.id;
                let end_label = StrID::from(format!(".if{id}#if#end"));
                out.extend([Asm::JumpToLabel(Condition::NonZero, end_label)]);
                for statement in many_statements.iter() {
                  statement.write_code(loop_stack, out);
                }
                out.extend([Asm::Label(end_label)]);
              }
            }
            match self.else_body.as_slice() {
              [] => (),
              other => todo!("unhandled else body"),
            }
          }
          other => todo!("unhandled register as left in equality: {other:?}"),
        },
        Expression::Dec(xpr) => match &xpr._payload {
          Expression::Register(reg) => match Reg8::try_from(reg._payload) {
            Ok(reg8) => {
              out.extend([Asm::DecReg8(reg8)]);
              // reg-- != 0, so `z` will be *clear* when it's the "true" case
              match self.if_body.as_slice() {
                [] => (),
                [s] => match &s._payload {
                  Statement::Break(src_target) => {
                    if let Some(canonical) =
                      loop_stack.iter().rev().find(|(name, _)| {
                        src_target._payload.map(|s| s._payload) == *name
                      })
                    {
                      let c = canonical.1;
                      out.extend([Asm::JumpToLabel(
                        Condition::NonZero,
                        StrID::from(format!("{c}#end")),
                      )]);
                    } else {
                      todo!("unhandled missing break target")
                    }
                  }
                  Statement::Continue(src_target) => {
                    if let Some(canonical) =
                      loop_stack.iter().rev().find(|(name, _)| {
                        src_target._payload.map(|s| s._payload) == *name
                      })
                    {
                      let c = canonical.1;
                      out.extend([Asm::JumpToLabel(
                        Condition::NonZero,
                        StrID::from(format!("{c}#start")),
                      )]);
                    } else {
                      todo!("unhandled missing continue target")
                    }
                  }
                  Statement::Return => {
                    out.extend([Asm::Return(Condition::NonZero)]);
                  }
                  other => todo!(),
                },
                many_statements => {
                  let id = self.id;
                  let end_label = StrID::from(format!(".if{id}#if_body#end"));
                  out.extend([Asm::JumpToLabel(Condition::NonZero, end_label)]);
                  for statement in many_statements.iter() {
                    statement.write_code(loop_stack, out);
                  }
                  out.extend([Asm::Label(end_label)]);
                }
              }
              match self.else_body.as_slice() {
                [] => (),
                other => todo!("unhandled else body"),
              }
            }
            Err(_) => todo!("unhandled not a reg8 in dec in not-equal"),
          },
          other => todo!("unhandled inc target in not-equal"),
        },
        other => todo!("unhandled left in not-equal test: {other:?}"),
      },

      other => todo!("unhandled test form: {other:?}"),
    }
  }
}
