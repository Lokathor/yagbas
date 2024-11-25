use super::*;

/// Parses a `Lone(Newline)`, which is then discarded.
pub fn newline_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(Newline) => (),
  }
}

/// Parses a `Lone(KwFn)`, which is then discarded.
pub fn kw_fn_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(KwFn) => (),
  }
}

/// Parses a `Lone(KwConst)`, which is then discarded.
pub fn kw_const_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(KwConst) => (),
  }
}

/// Parses a `Lone(KwIf)`, which is then discarded.
pub fn kw_if_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(KwIf) => (),
  }
}

/// Parses a `Lone(KwElse)`, which is then discarded.
pub fn kw_else_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(KwElse) => (),
  }
}

/// Parses a `Lone(KwContinue)`, which is then discarded.
pub fn kw_continue_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(KwContinue) => (),
  }
}

/// Parses a `Lone(KwBreak)`, which is then discarded.
pub fn kw_break_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(KwBreak) => (),
  }
}

/// Parses a `Lone(KwLoop)`, which is then discarded.
pub fn kw_loop_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(KwLoop) => (),
  }
}

/// Parses a `Lone(Quote)`, which is then discarded.
pub fn quote_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(Quote) => (),
  }
}

/// Parses a `Lone(Colon)`, which is then discarded.
pub fn colon_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(Colon) => (),
  }
}

/// Parses a `Lone(Equal)`, which is then discarded.
pub fn equal_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(Equal) => (),
  }
}

/// Parses a `Lone(Exclamation)`, which is then discarded.
pub fn exclamation_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(Exclamation) => (),
  }
}

/// Parses a `Lone(Plus)`, which is then discarded.
pub fn plus_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(Plus) => (),
  }
}

/// Parses a `Lone(Minus)`, which is then discarded.
pub fn minus_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(Minus) => (),
  }
}

/// Parses a `Lone(LessThan)`, which is then discarded.
pub fn less_than_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(LessThan) => (),
  }
}

/// Parses a `Lone(GreaterThan)`, which is then discarded.
pub fn greater_than_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(GreaterThan) => (),
  }
}

/// Parses a `Lone(KwReturn)` and returns `Statement::Return` instead.
pub fn kw_return_p<'src, I>(
) -> impl Parser<'src, I, Statement, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(KwReturn) => Statement::Return,
  }
}

/// Parses a `Lone(KwA)`, which is then discarded.
pub fn kw_a_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(KwA) => (),
  }
}

/// Parses a `Lone(KwHL)`, which is then discarded.
pub fn kw_hl_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(KwHL) => (),
  }
}

/// Parses `Lone(Ident(i))` and returns `i`.
pub fn ident_p<'src, I>(
) -> impl Parser<'src, I, StrID, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(Ident(i)) => i,
  }
}

/// Parses `Lone(NumLit(x))`, returning `x`.
pub fn num_lit_p<'src, I>(
) -> impl Parser<'src, I, StrID, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(NumLit(str_id)) => str_id,
  }
}
