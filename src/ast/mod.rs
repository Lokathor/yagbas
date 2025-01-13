use std::collections::BTreeMap;

use super::*;

pub mod data;
pub mod parsing;

#[derive(Debug, Clone, Default)]
pub struct Ast {
  pub evaluated_consts: BTreeMap<StrID, i32>,
  pub evaluated_statics: BTreeMap<StrID, Vec<u8>>,
  pub consts: BTreeMap<StrID, FileSpanned<Const>>,
  pub functions: BTreeMap<StrID, FileSpanned<Function>>,
  pub statics: BTreeMap<StrID, FileSpanned<Static>>,
  pub err_bucket: Vec<YagError>,
}
impl Ast {
  pub fn from_items(items: Vec<FileSpanned<Item>>) -> Self {
    // Note(Lokathor): Currently all items are in a single namespace, so here we
    // verify that we don't define the same identifier more than once.
    let mut everything_map: BTreeMap<StrID, Vec<FileSpanned<Item>>> =
      BTreeMap::new();
    for item in items {
      if let Some(name) = item.get_name() {
        everything_map.entry(name).or_default().push(item);
      }
    }
    let mut out = Self::default();
    for mut definitions in everything_map.into_values() {
      assert!(!definitions.is_empty());
      if definitions.len() > 1 {
        out.err_bucket.push(YagError::MultipleDefinitions(definitions))
      } else {
        let definition = definitions.pop().unwrap();
        let name = definition.get_name().unwrap();
        match definition._payload {
          Item::Function(f) => assert!(out.functions.insert(name, f).is_none()),
          Item::Const(c) => assert!(out.consts.insert(name, c).is_none()),
          Item::Static(s) => assert!(out.statics.insert(name, s).is_none()),
          Item::ItemError => unreachable!(),
        };
      }
    }
    out
  }

  pub fn run_const_eval(&mut self) {
    for (k, v) in self.consts.iter_mut() {
      let c = &mut v._payload;
      match c.expression._payload {
        Expression::NumLit(n) => {
          if let Some(x) = num_lit_to_i32(n) {
            self.evaluated_consts.insert(*k, x);
          } else {
            todo!("handle a num_lit that doesn't parse cleanly")
          }
        }
        _ => todo!("handle anything that isn't a num_lit"),
      }
    }
    for k in self.evaluated_consts.keys() {
      self.consts.remove(k);
    }
  }

  pub fn run_static_eval(&mut self) {
    for (k, v) in self.statics.iter_mut() {
      let s = &mut v._payload;
      let bytes = s
        .bytes
        .iter()
        .map(|xpr| match xpr._payload {
          Expression::NumLit(n) => {
            if let Some(i32_) = num_lit_to_i32(n) {
              match u8::try_from(i32_) {
                Ok(u8_) => u8_,
                Err(_) => match i8::try_from(i32_) {
                  Ok(i8_) => i8_ as u8,
                  Err(_) => {
                    todo!("handle values that can't be stored in 8 bits")
                  }
                },
              }
            } else {
              todo!("handle a num_lit that doesn't parse cleanly")
            }
          }
          _ => todo!("handle anything that isn't a num_lit"),
        })
        .collect();
      self.evaluated_statics.insert(*k, bytes);
    }
    for k in self.evaluated_statics.keys() {
      self.statics.remove(k);
    }
  }

  pub fn check_all_calls_valid(&mut self) {
    for function in self.functions.values() {
      function.calls_ref().for_each(|c| {
        if !self.functions.contains_key(&c.target) {
          if self.statics.contains_key(&c.target) {
            self.err_bucket.push(YagError::CalledAStatic(c.clone()))
          } else {
            self.err_bucket.push(YagError::CalledAMissingFunction(c.clone()))
          }
        }
      });
    }
  }

  pub fn resolve_size_of_static(&mut self) {
    for func in self.functions.values_mut() {
      func.expressions_mut().for_each(|xpr| {
        xpr.map_macros(&mut |id, tts| {
          if id.as_str() == "size_of_static" {
            match tts.as_slice() {
              [tt] => match tt._payload {
                TokenTree::Lone(Token::Ident(static_name)) => {
                  if let Some(s) = self.evaluated_statics.get(&static_name) {
                    let _payload: i32 = s.len().try_into().unwrap();
                    let _span = id._span.join(tts._span);
                    Expression::I32(FileSpanned { _payload, _span })
                  } else {
                    self.err_bucket.push(todo!());
                    Expression::ExpressionError
                  }
                }
                _ => {
                  self.err_bucket.push(todo!());
                  Expression::ExpressionError
                }
              },
              _ => {
                self.err_bucket.push(todo!());
                Expression::ExpressionError
              }
            }
          } else {
            Expression::Macro(id, tts)
          }
        });
      });
    }
  }

  pub fn resolve_numeric_literals(&mut self) {
    for func in self.functions.values_mut() {
      func.expressions_mut().for_each(|xpr| {
        xpr.map_num_lit(&mut |num| {
          if let Some(x) = num_lit_to_i32(num) {
            Expression::I32(FileSpanned::new(x, num._span))
          } else {
            self.err_bucket.push(todo!());
            Expression::ExpressionError
          }
        });
      });
    }
  }

  pub fn resolve_identifiers(&mut self) {
    for func in self.functions.values_mut() {
      func.expressions_mut().for_each(|xpr| {
        xpr.map_ident(&mut |i| {
          if let Some(x) = self.evaluated_consts.get(&i) {
            Expression::I32(FileSpanned::new(*x, i._span))
          } else if self.evaluated_statics.contains_key(&i) {
            Expression::StaticIdent(i)
          } else {
            self.err_bucket.push(YagError::ConstIdentifierUndefined(i));
            Expression::ExpressionError
          }
        });
      });
    }
  }

  pub fn resolve_ref(&mut self) {
    for func in self.functions.values_mut() {
      func.expressions_mut().for_each(|xpr| {
        xpr.map_ref(&mut |xpr| match xpr._payload {
          Expression::StaticIdent(i) => Expression::RefToStatic(i),
          _ => {
            self.err_bucket.push(todo!());
            Expression::ExpressionError
          }
        });
      });
    }
  }

  pub fn simplify_constant_values(&mut self) {
    for func in self.functions.values_mut() {
      func.expressions_mut().for_each(|xpr| {
        xpr.simplify_value();
      });
    }
  }

  pub fn generate_assembly_items(&self) -> BTreeMap<StrID, Vec<Asm>> {
    let mut out = BTreeMap::new();

    for (name, data) in self.evaluated_statics.iter() {
      let label = Asm::Label(*name);
      out.insert(*name, vec![label, Asm::RawBytes(data.clone())]);
    }
    for (f_, func) in self.functions.iter() {
      out.insert(*f_, func._payload.generate_code());
    }

    out
  }
}

#[inline]
fn num_lit_to_i32(n: FileSpanned<StrID>) -> Option<i32> {
  // remove underscores
  let s: String = n.as_str().chars().filter(|c| *c != '_').collect();
  if let Some(b) = s.strip_prefix("%") {
    // binary
    i32::from_str_radix(b, 2).ok()
  } else if let Some(h) = s.strip_prefix("$") {
    // hexadecimal
    i32::from_str_radix(h, 16).ok()
  } else {
    // decimal
    i32::from_str_radix(&s, 10).ok()
  }
}

#[inline]
fn i32_to_imm8(i: i32) -> Option<u8> {
  u8::try_from(i).or_else(|_| i8::try_from(i).map(|i| i as u8)).ok()
}

#[inline]
fn i32_to_imm16(i: i32) -> Option<u16> {
  u16::try_from(i).or_else(|_| i16::try_from(i).map(|i| i as u16)).ok()
}
