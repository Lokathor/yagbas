#![allow(unused)]

use std::collections::hash_map::Entry;

use crate::ItemId;
use crate::LocalNameId;
use crate::PathId;
use crate::Span;
use crate::TypeId;
use crate::YagError;
use crate::ast::Ast;
use crate::ast::Item;
use crate::ast::ItemKind;
use crate::ast::PointerAccessKind;
use crate::ast::TypeExprKind;
use crate::ast::ValueExpr;
use crate::ast::ValueExprKind;
use crate::ir_nameres_typecheck::IrNameResTypeCheck;
use crate::ir_nameres_typecheck::Type;
