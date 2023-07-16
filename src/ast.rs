use crate::{item_decl::ItemDecl, item_formed::ItemFormed, token_tree::make_token_trees};

use super::*;

pub struct Ast<T> {
  pub module_text: String,
  pub lines: Vec<usize>,
  pub items: Vec<(T, SimpleSpan)>,
  pub errors: Vec<(CowStr, SimpleSpan)>,
}

impl<T> Ast<T> {
  pub fn generate_lines(&mut self) {
    let mut b = 0;
    self.lines.clear();
    for line in self.module_text.lines() {
      self.lines.push(b);
      b += line.len();
    }
  }
  pub fn line_of(&self, pos_byte: usize) -> usize {
    1 + if self.lines.is_empty() {
      0
    } else {
      match self.lines.binary_search(&pos_byte) {
        Ok(i) => i,
        Err(if_it_was) => if_it_was - 1,
      }
    }
  }
}

impl Ast<Token> {
  /// Tokenize a module's source code, removing comments in the process.
  pub fn tokenize(module_text: String) -> Ast<Token> {
    let mut out =
      Self { lines: Vec::new(), module_text, items: Vec::new(), errors: Vec::new() };
    out.generate_lines();

    let mut comment_level = 0_usize;
    for (lex_result, span) in Token::lexer(&out.module_text).spanned() {
      match lex_result {
        Ok(Token::CommentSingle) => continue,
        Ok(Token::CommentBlockStart) => comment_level += 1,
        Ok(Token::CommentBlockEnd) if comment_level > 0 => comment_level -= 1,
        Ok(Token::CommentBlockEnd) => {
          out.errors.push((Cow::Borrowed("Illegal Comment Block End"), span.into()));
        }
        Ok(_) if comment_level > 0 => continue,
        Ok(token) => out.items.push((token, span.into())),
        Err(_) if comment_level > 0 => continue,
        Err(()) => out.errors.push((Cow::Borrowed("Couldn't Lex"), span.into())),
      }
    }

    out
  }

  pub fn into_token_trees(self) -> Ast<TokenTree> {
    let (trees, rich_errors) = make_token_trees(&self.items).into_output_errors();

    let mut out = Ast {
      items: trees.unwrap_or_default(),
      errors: self.errors,
      module_text: self.module_text,
      lines: self.lines,
    };
    rich_errors.into_iter().for_each(|err| {
      let span = *err.span();
      let msg = Cow::Owned(format!("{err:?}"));
      out.errors.push((msg, span));
    });

    out
  }
}

impl Ast<TokenTree> {
  pub fn into_declarations(self) -> Ast<ItemDecl> {
    let item_parser =
      ItemDecl::parser().map_with_span(id2).repeated().collect::<Vec<_>>();
    let (declarations, rich_errors) =
      run_parser(item_parser, &self.items).into_output_errors();

    let mut out = Ast {
      items: declarations.unwrap_or_default(),
      errors: self.errors,
      module_text: self.module_text,
      lines: self.lines,
    };
    rich_errors.into_iter().for_each(|err| {
      let span = *err.span();
      let msg = Cow::Owned(format!("{err:?}"));
      out.errors.push((msg, span));
    });

    out
  }
}

impl Ast<ItemDecl> {
  pub fn into_forms(self) -> Ast<ItemFormed> {
    let results: Vec<_> = self
      .items
      .into_par_iter()
      .map(|(decl, span)| (ItemFormed::try_from(decl), span))
      .collect();
    let mut out = Ast {
      items: Vec::new(),
      errors: self.errors,
      module_text: self.module_text,
      lines: self.lines,
    };
    results.into_iter().for_each(|(r, span)| match r {
      Ok(i) => out.items.push((i, span)),
      Err(errs) => errs.into_iter().for_each(|e| out.errors.push((e, span))),
    });
    out
  }
}
