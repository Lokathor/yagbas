use slotmap::{SlotMap, new_key_type};
use str_id::StrId;

use crate::{
  Span,
  ast::{
    Ast, AstBody, AstExprVal, AstExprValKind, AstItem, AstItemKind,
    AstStatementKind,
  },
};

new_key_type! {
  pub struct NameId;
}

#[derive(Debug, Clone, Copy)]
pub struct NameInfo {
  pub origin: StrId,
  pub span: Span,
  pub text: StrId,
  pub kind: NameKind,
}

#[derive(Debug, Clone, Copy)]
pub enum NameKind {
  StaticMmio,
  Constant,
  Function,
  FunctionArgument,
  LetVariable,
}

/// Intermediate Representation with Name Resolution
#[derive(Debug, Clone, Default)]
pub struct IrNameres {
  pub ast: Ast,
  pub names: SlotMap<NameId, NameInfo>,
}
impl IrNameres {
  pub fn from_ast(ast: Ast) -> Self {
    let mut out = Self::default();
    out.ast = ast;
    out.generate_all_names();
    out
  }

  fn generate_all_names(&mut self) {
    for module in self.ast.modules.iter() {
      // first gather all items at this level
      for item in module.items.iter() {
        let (text, span, kind) = match &item.kind {
          AstItemKind::ErrAstItemKind => continue,
          AstItemKind::StaticMmio(ast_static_mmio) => (
            ast_static_mmio.name,
            ast_static_mmio.name_span,
            NameKind::StaticMmio,
          ),
          AstItemKind::Constant(ast_constant) => {
            (ast_constant.name, ast_constant.name_span, NameKind::Constant)
          }
          AstItemKind::Function(ast_function) => {
            (ast_function.name, ast_function.name_span, NameKind::Function)
          }
        };
        let info = NameInfo { origin: module.origin, span, text, kind };
        self.names.insert(info);
      }

      // then delve into each item at this level
      for item in module.items.iter() {
        names_for_item(&mut self.names, item, module.origin);
      }
    }
  }
}

fn names_for_expr(
  names: &mut SlotMap<NameId, NameInfo>, xpr: &AstExprVal, origin: StrId,
) {
  match &xpr.kind {
    AstExprValKind::If(ast_expr_val, ast_body) => {
      names_for_expr(names, ast_expr_val, origin);
      names_for_body(names, ast_body, origin);
    }
    AstExprValKind::Loop(ast_body) => {
      names_for_body(names, ast_body, origin);
    }
    AstExprValKind::For(ast_expr_val, ast_expr_val1, ast_body) => {
      names_for_expr(names, ast_expr_val, origin);
      names_for_expr(names, ast_expr_val1, origin);
      names_for_body(names, ast_body, origin);
    }
    AstExprValKind::Dereference(ast_expr_val)
    | AstExprValKind::Reference(ast_expr_val) => {
      names_for_expr(names, ast_expr_val, origin);
    }
    AstExprValKind::Access(ast_expr_val, ast_expr_val1)
    | AstExprValKind::Add(ast_expr_val, ast_expr_val1)
    | AstExprValKind::AddAssign(ast_expr_val, ast_expr_val1)
    | AstExprValKind::ArrayIndex(ast_expr_val, ast_expr_val1)
    | AstExprValKind::Assign(ast_expr_val, ast_expr_val1)
    | AstExprValKind::BitAnd(ast_expr_val, ast_expr_val1)
    | AstExprValKind::BitAndAssign(ast_expr_val, ast_expr_val1)
    | AstExprValKind::BitOr(ast_expr_val, ast_expr_val1)
    | AstExprValKind::BitOrAssign(ast_expr_val, ast_expr_val1)
    | AstExprValKind::BitXor(ast_expr_val, ast_expr_val1)
    | AstExprValKind::BitXorAssign(ast_expr_val, ast_expr_val1)
    | AstExprValKind::CmpEq(ast_expr_val, ast_expr_val1)
    | AstExprValKind::CmpGe(ast_expr_val, ast_expr_val1)
    | AstExprValKind::CmpGt(ast_expr_val, ast_expr_val1)
    | AstExprValKind::CmpLe(ast_expr_val, ast_expr_val1)
    | AstExprValKind::CmpLt(ast_expr_val, ast_expr_val1)
    | AstExprValKind::CmpNe(ast_expr_val, ast_expr_val1)
    | AstExprValKind::ConditionalAnd(ast_expr_val, ast_expr_val1)
    | AstExprValKind::ConditionalOr(ast_expr_val, ast_expr_val1)
    | AstExprValKind::Div(ast_expr_val, ast_expr_val1)
    | AstExprValKind::DivAssign(ast_expr_val, ast_expr_val1)
    | AstExprValKind::Mul(ast_expr_val, ast_expr_val1)
    | AstExprValKind::MulAssign(ast_expr_val, ast_expr_val1)
    | AstExprValKind::Path(ast_expr_val, ast_expr_val1)
    | AstExprValKind::RangeExclusive(ast_expr_val, ast_expr_val1)
    | AstExprValKind::RangeInclusive(ast_expr_val, ast_expr_val1)
    | AstExprValKind::Rem(ast_expr_val, ast_expr_val1)
    | AstExprValKind::RemAssign(ast_expr_val, ast_expr_val1)
    | AstExprValKind::ShiftLeft(ast_expr_val, ast_expr_val1)
    | AstExprValKind::ShiftLeftAssign(ast_expr_val, ast_expr_val1)
    | AstExprValKind::ShiftRight(ast_expr_val, ast_expr_val1)
    | AstExprValKind::ShiftRightAssign(ast_expr_val, ast_expr_val1)
    | AstExprValKind::Sub(ast_expr_val, ast_expr_val1)
    | AstExprValKind::SubAssign(ast_expr_val, ast_expr_val1) => {
      names_for_expr(names, ast_expr_val, origin);
      names_for_expr(names, ast_expr_val1, origin);
    }
    _ => (),
  }
}

fn names_for_body(
  names: &mut SlotMap<NameId, NameInfo>, body: &AstBody, origin: StrId,
) {
  // first gather all items at this level
  for stmt in body.statements.iter() {
    match &stmt.kind {
      AstStatementKind::Item(ast_item) => {
        let (text, span, kind) = match &ast_item.kind {
          AstItemKind::ErrAstItemKind => continue,
          AstItemKind::StaticMmio(ast_static_mmio) => (
            ast_static_mmio.name,
            ast_static_mmio.name_span,
            NameKind::StaticMmio,
          ),
          AstItemKind::Constant(ast_constant) => {
            (ast_constant.name, ast_constant.name_span, NameKind::Constant)
          }
          AstItemKind::Function(ast_function) => {
            (ast_function.name, ast_function.name_span, NameKind::Function)
          }
        };
        let info = NameInfo { origin, span, text, kind };
        names.insert(info);
      }
      _ => continue,
    }
  }
  // then delve into each item at this level
  for stmt in body.statements.iter() {
    match &stmt.kind {
      AstStatementKind::ErrAstStatementKind => continue,
      AstStatementKind::Let(ast_let) => match &ast_let.pattern.kind {
        AstExprValKind::Identifier(i) => {
          let info = NameInfo {
            origin,
            text: *i,
            span: ast_let.pattern.span,
            kind: NameKind::LetVariable,
          };
          names.insert(info);
        }
        _other => {
          dbg!(_other);
          continue;
        }
      },
      AstStatementKind::Expression(ast_expr_val) => {
        names_for_expr(names, ast_expr_val, origin);
      }
      AstStatementKind::Item(ast_item) => {
        names_for_item(names, ast_item, origin);
      }
    }
  }
}

fn names_for_item(
  names: &mut SlotMap<NameId, NameInfo>, item: &AstItem, origin: StrId,
) {
  match &item.kind {
    AstItemKind::ErrAstItemKind => return,
    AstItemKind::StaticMmio(ast_static_mmio) => {
      names_for_expr(names, &ast_static_mmio.address, origin);
    }
    AstItemKind::Constant(ast_constant) => {
      names_for_expr(names, &ast_constant.xpr, origin);
    }
    AstItemKind::Function(ast_function) => {
      for arg in ast_function.arguments.iter() {
        let info = NameInfo {
          origin,
          span: arg.name_span,
          text: arg.name,
          kind: NameKind::FunctionArgument,
        };
        names.insert(info);
      }
      names_for_body(names, &ast_function.body, origin);
    }
  }
}
