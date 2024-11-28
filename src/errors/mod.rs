use ariadne::{Cache, CharSet, Config, Label, Report, ReportKind};
use chumsky::error::Rich;

use crate::{
  ast::{Item, ItemKind, Token, TokenTree},
  file_span::FileSpan,
  file_spanned::FileSpanned,
  src_file::SrcID,
  str_id::StrID,
};
use core::fmt::Write;
use std::{
  borrow::Cow,
  collections::{hash_map::Entry, HashMap},
};

#[derive(Debug, Clone)]
pub enum YagError {
  FileIO { filename: String, message: String },
  Tokenization(FileSpan),
  TokenTree(Rich<'static, Token, FileSpan, &'static str>),
  Item(Rich<'static, TokenTree, FileSpan, &'static str>),
}
impl YagError {
  pub fn one_line(&self) -> String {
    match self {
      YagError::FileIO { filename, message } => {
        format!("Error: {filename}: IO: {message}")
      }
      YagError::Tokenization(file_span) => {
        format!("Error: Tokenization: {file_span:?}")
      }
      YagError::TokenTree(rich) => format!("Error: Token Tree: {rich:?}"),
      YagError::Item(rich) => format!("Error: Item: {rich:?}"),
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
        .with_label(Label::new(*rich.span()).with_message("unexpected"))
        //.with_labels(rich.contexts().map(|(context, file_span)| {
        //  Label::new(*file_span)
        //    .with_message(format!("while parsing {context}"))
        //}))
        .finish(),
    }
  }
}
