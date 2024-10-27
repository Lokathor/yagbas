/// Parses one separator between statements.
pub fn statement_sep<'a>(
) -> impl Parser<'a, TokenTreeSliceInput<'a>, (), ErrRichTokenTree<'a>> + Clone
{
  select! {
    Lone(Newline) => (),
    Lone(Semicolon) => (),
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
