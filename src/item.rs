use super::*;
use src_files::FileSpanned;
use str_id::StrID;
use Token::*;
use TokenTree::*;

/// Parse all the token trees of a file into the items of the file
#[allow(clippy::type_complexity)]
pub fn parse_token_trees_to_items(
  token_trees: &[(TokenTree, FileSpan)],
) -> (Vec<FileSpanned<Item>>, Vec<Rich<'static, TokenTree, FileSpan>>) {
  let last_span = if let Some((_tt, file_span)) = token_trees.last() {
    let mut x = *file_span;
    x.start = x.end;
    x
  } else {
    return (Vec::new(), Vec::new());
  };
  let parser = Item::parser()
    .map_with(|item, ex| FileSpanned { _payload: item, _span: ex.span() })
    .padded_by(just(Lone(Newline)).repeated())
    .repeated()
    .collect::<Vec<_>>();
  let input = token_trees.spanned(last_span);
  let (items, errors) = parser.parse(input).into_output_errors();
  (
    items.unwrap_or_default(),
    errors.into_iter().map(|r| r.into_owned()).collect::<Vec<_>>(),
  )
}

/// Parses one separator between statements.
pub fn statement_sep<'a>() ->
  impl Parser<'a, TokenTreeSliceInput<'a>, (), ErrRichTokenTree<'a>> + Clone {
  select! {
    Lone(Newline) => (),
    Lone(Semicolon) => (),
  }
}

#[derive(Debug, Clone)]
pub enum Item {
  Fn(FnDecl),
  ItemError,
}
impl Item {
  pub fn parser<'a>(
  ) -> impl Parser<'a, TokenTreeSliceInput<'a>, Self, ErrRichTokenTree<'a>> + Clone
  {
    let fn_decl = FnDecl::parser().map(Self::Fn);

    choice((fn_decl,))
  }
}

#[derive(Debug, Clone)]
pub struct FnDecl {
  pub name: StrID,
  pub args: Vec<FileSpanned<TokenTree>>,
  pub statements: Vec<FileSpanned<Statement>>,
}
impl FnDecl {
  pub fn parser<'a>(
  ) -> impl Parser<'a, TokenTreeSliceInput<'a>, Self, ErrRichTokenTree<'a>> + Clone
  {
    let kw_fn = just(Lone(KwFn));
    let fn_name = select! {
      Lone(Ident(i)) => i
    };
    let arguments = select! {
      Parens(p) => p.into_iter().map(|(statement, filespan)| FileSpanned {
        _payload: statement,
        _span: filespan,
      }).collect::<Vec<_>>()
    };
    let line_sep = select! {
      Lone(Newline) => (),
      Lone(Semicolon) => (),
    };
    let statements = Statement::parser()
      .map_with(|statement, ex| FileSpanned {
        _payload: statement,
        _span: ex.span(),
      })
      .separated_by(line_sep.repeated().at_least(1))
      .allow_leading()
      .allow_trailing()
      .collect::<Vec<_>>()
      .nested_in(select_ref! {
        Braces(b) = ex => {
          b.spanned(ex.span())
        }
      })
      .labelled("fn_statements")
      .as_context();

    kw_fn
      .ignore_then(fn_name)
      .then(arguments)
      .then(statements)
      .map(|((name, args), statements)| Self { name, args, statements })
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
  /// `call LABEL`, unconditional call
  ///
  /// * Bytes: `0xCD, ADDR_LOW, ADDR_HIGH`
  Call { target: StrID, args: Vec<(TokenTree, FileSpan)> }, /* TODO: conditional calls */
  /// `ret`, unconditional return
  ///
  /// * Bytes: `0xC9`
  Return,
  /// Performs the inner statements and then jumps to the start of the loop
  Loop(Vec<FileSpanned<Statement>>),
  /// Jumps to the end of the loop
  Break,
  /// Jumps to the start of the loop
  Continue,
}
impl Statement {
  /// Parses one statement from a fn body.
  pub fn parser<'a>(
  ) -> impl Parser<'a, TokenTreeSliceInput<'a>, Self, ErrRichTokenTree<'a>> + Clone
  {
    recursive(|inner_self| {
      let return_p = select! {Lone(KwReturn) => Statement::Return};
      let break_p = select! {Lone(KwBreak) => ()}.to(Statement::Break);
      let continue_p = select! {Lone(KwContinue) => ()}.to(Statement::Continue);
      let call_p = {
        let call_target = select! {
          Lone(Ident(i)) => i
        };
        let arguments = select! {
          Parens(p) => p
        };
        call_target
          .then(arguments)
          .map(|(target, args)| Statement::Call { target, args })
      };
      let loop_p = {
        let kw_loop = select! {
          Lone(KwLoop) => ()
        };
        let line_sep = select! {
          Lone(Newline) => (),
          Lone(Semicolon) => (),
        };
        kw_loop
          .ignore_then(
            inner_self
              .clone()
              .map_with(|statement, ex| FileSpanned {
                _payload: statement,
                _span: ex.span(),
              })
              .separated_by(line_sep.repeated().at_least(1))
              .allow_leading()
              .allow_trailing()
              .collect::<Vec<_>>()
              .nested_in(select_ref! {
                Braces(b) = ex => {
                  b.spanned(ex.span())
                }
              }),
          )
          .map(Statement::Loop)
      };
      let x = choice((return_p, break_p, continue_p, call_p, loop_p));
      x
    })
  }
}
