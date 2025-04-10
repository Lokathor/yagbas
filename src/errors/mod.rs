use ariadne::{Cache, CharSet, Config, Label, Report, ReportKind};
use chumsky::error::Rich;

use super::*;
use core::fmt::Write;
use std::{
  borrow::Cow,
  collections::{hash_map::Entry, HashMap},
};

#[derive(Debug, Clone)]
pub enum YagError {
  FileIO { filename: String, message: String },
  Tokenization(FileSpan),
  TokenTree(Rich<'static, Token, FileSpan>),
  Item(Rich<'static, TokenTree, FileSpan>),
  MultipleDefinitions(Vec<FileSpanned<Item>>),
  ConstIdentifierUndefined(FileSpanned<StrID>),
  CalledAStatic(FileSpanned<Call>),
  CalledAMissingFunction(FileSpanned<Call>),
}
impl YagError {
  pub fn one_line(&self) -> String {
    use core::fmt::Write;
    match self {
      YagError::FileIO { filename, message } => {
        format!("Error: {filename}: IO: {message}")
      }
      YagError::Tokenization(file_span) => {
        format!("Error: Tokenization: {file_span:?}")
      }
      YagError::TokenTree(rich) => format!("Error: Token Tree: {rich:?}"),
      YagError::Item(rich) => format!("Error: Item: {rich:?}"),
      YagError::MultipleDefinitions(vec) => {
        let name = vec[0].get_name().unwrap();
        let mut s = format!(
          "Error:: MultipleDefinitions: `{name}` defined more than once: "
        );
        for (i, span) in vec.iter().map(|v| v._span).enumerate() {
          if i > 0 {
            write!(s, ", ").ok();
          }
          write!(s, "{span}").ok();
        }
        s
      }
      YagError::ConstIdentifierUndefined(i) => {
        let payload = i._payload;
        let span = i._span;
        format!("Error: Constant Identifier Undefined: `{payload}` {span}")
      }
      YagError::CalledAStatic(call) => {
        let payload = call._payload.target;
        let span = call._span;
        format!("Error: Called A Static: `{payload}` {span}")
      }
      YagError::CalledAMissingFunction(call) => {
        let payload = call._payload.target;
        let span = call._span;
        format!("Error: Called A Missing Function: `{payload}` {span}")
      }
    }
  }

  pub fn build_report(&self, config: Config) -> Report<FileSpan> {
    match self {
      YagError::FileIO { filename, message } => {
        Report::build(ReportKind::Error, FileSpan::default())
          .with_config(config)
          .with_message(format!("{filename}: IO: {message}"))
          .finish()
      }
      YagError::Tokenization(file_span) => {
        Report::build(ReportKind::Error, *file_span)
          .with_config(config)
          .with_message("Tokenization Error")
          .with_label(Label::new(*file_span))
          .finish()
      }
      YagError::TokenTree(rich) => {
        Report::build(ReportKind::Error, *rich.span())
          .with_config(config)
          .with_message(format!("{rich:?}"))
          .with_labels(rich.contexts().map(|(context, file_span)| {
            Label::new(*file_span)
              .with_message(format!("while parsing {context}"))
          }))
          .finish()
      }
      YagError::Item(rich) => Report::build(ReportKind::Error, *rich.span())
        .with_config(config)
        .with_message(match rich.reason() {
          chumsky::error::RichReason::ExpectedFound { expected, found } => {
            let found = match found {
              None => Cow::Borrowed("the end of input"),
              Some(pat) => {
                let ref_tt: &TokenTree = pat;
                Cow::Owned(format!("`{ref_tt}`"))
              }
            };
            let expected = match expected.as_slice() {
              [] => {
                Cow::Borrowed("some unknown thing (this is probably a bug)")
              }
              [one_thing] => Cow::Owned(format!("{one_thing}")),
              many_things => {
                let mut s = String::new();
                let (last, leading) = many_things.split_last().unwrap();
                for (i, thing) in leading.iter().enumerate() {
                  use core::fmt::Write;
                  if i > 0 {
                    write!(s, ", ").ok();
                  }
                  write!(s, "{thing}").ok();
                }
                if leading.len() > 1 {
                  write!(s, ",").ok();
                }
                write!(s, " or {last:?}").ok();
                Cow::Owned(s)
              }
            };
            format!("Found {found}, but expected {expected}")
          }
          _ => format!("{rich:?}"),
        })
        .finish(),
      YagError::MultipleDefinitions(vec) => {
        Report::build(ReportKind::Error, vec[0]._span)
          .with_config(config)
          .with_message("Multiple Definitions Error")
          .with_labels(vec.iter().map(|v| Label::new(v._span)))
          .finish()
      }
      YagError::ConstIdentifierUndefined(i) => {
        Report::build(ReportKind::Error, i._span)
          .with_config(config)
          .with_message(format!(
            "Constant Identifier Undefined: `{}`",
            i._payload
          ))
          .with_label(Label::new(i._span))
          .finish()
      }
      YagError::CalledAStatic(call) => {
        Report::build(ReportKind::Error, call._span)
          .with_config(config)
          .with_message(format!("Called A Static: `{}`", call._payload.target))
          .with_label(Label::new(call._span))
          .finish()
      }
      YagError::CalledAMissingFunction(call) => {
        Report::build(ReportKind::Error, call._span)
          .with_config(config)
          .with_message(format!(
            "Called A Missing Function: `{}`",
            call._payload.target
          ))
          .with_label(Label::new(call._span))
          .finish()
      }
    }
  }
}
