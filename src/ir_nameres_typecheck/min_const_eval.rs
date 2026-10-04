use crate::{
  TypeId, YagError,
  ast::{NumberPrintHint, ValueExpr, ValueExprKind, visitor::TreeVisitMut},
  ir_nameres_typecheck::IrNameResTypeCheck,
  path_id::PathId,
};
use std::collections::hash_map::Entry;

#[derive(Debug)]
pub struct ConstEvaluator<'a> {
  pub ir: &'a mut IrNameResTypeCheck,
  pub file_origin: Option<PathId>,
  pub lit_id: TypeId,
  pub errors: Vec<YagError>,
}

impl<'a> TreeVisitMut for ConstEvaluator<'a> {
  fn visit_module(&mut self, module: &mut crate::ast::Module) {
    self.file_origin = Some(module.file_origin);
  }

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
      // TODO: support more math ops!
      _ => (),
    }
  }
}

fn parse_number_literal(s: &str) -> Result<i64, String> {
  // TODO: it will be ugly code but we should probably use all checked
  // operations in the accumulation step.
  if let Some(hex) = s.strip_prefix('$').or_else(|| s.strip_prefix("0x")) {
    hex.chars().filter(|ch| ch != &'_').try_fold(
      0_i64,
      |b, new_ch| match new_ch {
        '0'..='9' => Ok(b * 16 + ((new_ch as i64) - ('0' as i64))),
        'a'..='f' => Ok(b * 16 + (10 + (new_ch as i64) - ('a' as i64))),
        'A'..='F' => Ok(b * 16 + (10 + (new_ch as i64) - ('A' as i64))),
        other => Err(format!("Not a Hex digit: `{other}`")),
      },
    )
  } else if let Some(bin) = s.strip_prefix('%').or_else(|| s.strip_prefix("0b"))
  {
    bin.chars().filter(|ch| ch != &'_').try_fold(
      0_i64,
      |b, new_ch| match new_ch {
        '0'..='1' => Ok(b * 2 + ((new_ch as i64) - ('0' as i64))),
        other => Err(format!("Not a Binary digit: `{other}`")),
      },
    )
  } else {
    s.chars().filter(|ch| ch != &'_').try_fold(
      0_i64,
      |b, new_ch| match new_ch {
        '0'..='9' => Ok(b * 10 + ((new_ch as i64) - ('0' as i64))),
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
