//! A simple const evaluation engine that can run before type checking.
//!
//! The purpose of this step us to allow for evaluating expressions used as array lengths, so that array lengths are already known before type checking begins.
//! This only needs to support literal parsing, simple math ops, and converting const names into the expression they stand for.
//! If an expression would require type info to evaluate, we simply don't handle it at this step (and so it's not allowed in an array length).
//!
//! As the compiler develops the process can become more sophisticated, but for now we just do this basic thing.

use crate::{
  TypeId, YagError,
  ast::{NumberPrintHint, ValueExpr, ValueExprKind, visitor::TreeVisitMut},
  ir_nameres_typecheck::IrNameResTypeCheck,
  operators::{BinOpKind, UnOpKind},
  path_id::PathId,
};
use std::collections::hash_map::Entry;

#[derive(Debug)]
pub struct ConstEvaluator<'a> {
  /// Note: Don't access `self.ir.ast` during the `TreeVisitMut`
  ///
  /// The Ast in here is a dummy value during the walk.
  pub ir: &'a mut IrNameResTypeCheck,
  /// needed for error messages.
  pub file_origin: Option<PathId>,
  /// TypeId for literal integers.
  pub lit_id: TypeId,
  /// errors go here, and it's merged back into the Ast error list later.
  pub errors: Vec<YagError>,
}

impl<'a> TreeVisitMut for ConstEvaluator<'a> {
  fn visit_module(&mut self, module: &mut crate::ast::Module) {
    self.file_origin = Some(module.file_origin);
  }

  // TODO: replace const identifier use with the const's expression. we will need to watch for cyclical definitions as we go.
  fn visit_value_expr(&mut self, vx: &mut ValueExpr) {
    match &mut *vx.kind {
      ValueExprKind::LiteralNumber(ln) => match parse_number_literal(ln) {
        Ok(value) => {
          let print_hint = get_print_hint_from_literal(ln);
          match self.ir.vx_id_to_ty_id.entry(vx.id) {
            Entry::Occupied(_) => todo!(),
            Entry::Vacant(ve) => {
              ve.insert(self.lit_id);
            }
          }
          *vx.kind = ValueExprKind::Number { value, print_hint };
        }
        Err(message) => {
          self.errors.push(YagError {
            file_origin: self.file_origin.unwrap(),
            span: vx.span,
            message,
          });
          *vx.kind = ValueExprKind::ErrValueExprKind;
        }
      },
      ValueExprKind::UnOp { op: UnOpKind::Negative, operand } => {
        match &mut *operand.kind {
          ValueExprKind::Number { value, print_hint } => {
            match value.checked_neg() {
              Some(new_val) => {
                *vx.kind = ValueExprKind::Number {
                  value: new_val,
                  print_hint: *print_hint,
                };
              }
              None => {
                self.errors.push(YagError {
                  file_origin: self.file_origin.unwrap(),
                  span: vx.span,
                  message: format!("Const Eval Overflow"),
                });
                *vx.kind = ValueExprKind::ErrValueExprKind;
              }
            }
          }
          _ => (),
        }
      }
      ValueExprKind::BinOp { left, op, right } => {
        match (&mut *left.kind, &mut *right.kind) {
          (
            ValueExprKind::Number { value: l, print_hint: l_hint },
            ValueExprKind::Number { value: r, print_hint: r_hint },
          ) => {
            let opt_new_val = match op {
              BinOpKind::Add => l.checked_add(*r),
              BinOpKind::Sub => l.checked_sub(*r),
              BinOpKind::Mul => l.checked_mul(*r),
              BinOpKind::Div => l.checked_div(*r),
              BinOpKind::Rem => l.checked_rem(*r),
              BinOpKind::ShiftLeft => {
                u32::try_from(*r).ok().and_then(|r| l.checked_shl(r))
              }
              BinOpKind::ShiftRight => {
                u32::try_from(*r).ok().and_then(|r| l.checked_shr(r))
              }
              BinOpKind::BitAnd => Some(*l & *r),
              BinOpKind::BitOr => Some(*l | *r),
              BinOpKind::BitXor => Some(*l ^ *r),
              _ => return,
            };
            match opt_new_val {
              Some(new_val) => {
                *vx.kind = ValueExprKind::Number {
                  value: new_val,
                  print_hint: l_hint.or(*r_hint),
                };
              }
              None => {
                self.errors.push(YagError {
                  file_origin: self.file_origin.unwrap(),
                  span: vx.span,
                  message: format!("Const Eval Overflow"),
                });
                *vx.kind = ValueExprKind::ErrValueExprKind;
              }
            }
          }
          _ => (),
        }
      }
      _ => (),
    }
  }
}

fn parse_number_literal(s: &str) -> Result<i64, String> {
  if let Some(hex) = s.strip_prefix('$').or_else(|| s.strip_prefix("0x")) {
    hex.chars().filter(|ch| ch != &'_').try_fold(
      0_i64,
      |b, new_ch| match new_ch {
        '0'..='9' => b
          .checked_mul(16)
          .ok_or_else(|| format!("Const Eval Overflow"))?
          .checked_add((new_ch as i64) - ('0' as i64))
          .ok_or_else(|| format!("Const Eval Overflow")),
        'a'..='f' => b
          .checked_mul(16)
          .ok_or_else(|| format!("Const Eval Overflow"))?
          .checked_add(10 + (new_ch as i64) - ('a' as i64))
          .ok_or_else(|| format!("Const Eval Overflow")),
        'A'..='F' => b
          .checked_mul(16)
          .ok_or_else(|| format!("Const Eval Overflow"))?
          .checked_add(10 + (new_ch as i64) - ('A' as i64))
          .ok_or_else(|| format!("Const Eval Overflow")),
        other => Err(format!("Not a Hex digit: `{other}`")),
      },
    )
  } else if let Some(bin) = s.strip_prefix('%').or_else(|| s.strip_prefix("0b"))
  {
    bin.chars().filter(|ch| ch != &'_').try_fold(
      0_i64,
      |b, new_ch| match new_ch {
        '0'..='1' => b
          .checked_mul(2)
          .ok_or_else(|| format!("Const Eval Overflow"))?
          .checked_add((new_ch as i64) - ('0' as i64))
          .ok_or_else(|| format!("Const Eval Overflow")),
        other => Err(format!("Not a Binary digit: `{other}`")),
      },
    )
  } else {
    s.chars().filter(|ch| ch != &'_').try_fold(
      0_i64,
      |b, new_ch| match new_ch {
        '0'..='9' => b
          .checked_mul(10)
          .ok_or_else(|| format!("Const Eval Overflow"))?
          .checked_add((new_ch as i64) - ('0' as i64))
          .ok_or_else(|| format!("Const Eval Overflow")),
        other => Err(format!("Not a Decimal digit: `{other}`")),
      },
    )
  }
}

fn get_print_hint_from_literal(s: &str) -> Option<NumberPrintHint> {
  if s.starts_with('$') || s.starts_with("0x") {
    Some(NumberPrintHint::Hex)
  } else if s.starts_with('%') || s.starts_with("0b") {
    Some(NumberPrintHint::Binary)
  } else {
    None
  }
}
