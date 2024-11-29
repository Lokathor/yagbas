//! Parsing of individual [TokenTree::Lone] token values.
//!
//! * Notably: [`num_lit_p`], [`ident_p`], and [`register_p`] can match against
//!   varying inputs, and the captured input is [FileSpanned] as their output.
//! * The rest of them only parse for exactly one value (eg: a single period or
//!   a single comma), and so all those parsers output nothing.

use super::*;

/// Parses a `Lone(NumLit(i))`, keeping `i`.
pub fn num_lit_p<'src, I>(
) -> impl Parser<'src, I, FileSpanned<StrID>, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(NumLit(i)) = ex => FileSpanned::new(i, ex.span()),
  }
  .labelled("num_lit")
  .as_context()
}

/// Parses a `Lone(Ident(i))`, keeping `i`.
pub fn ident_p<'src, I>(
) -> impl Parser<'src, I, FileSpanned<StrID>, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(Ident(i)) = ex => FileSpanned::new(i, ex.span()),
  }
  .labelled("ident")
  .as_context()
}

/// Parses any `Lone(register)` value.
pub fn register_p<'src, I>(
) -> impl Parser<'src, I, FileSpanned<Register>, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(KwA) = ex => FileSpanned::new(Register::A, ex.span()),
    Lone(KwF) = ex => FileSpanned::new(Register::F, ex.span()),
    Lone(KwB) = ex => FileSpanned::new(Register::B, ex.span()),
    Lone(KwC) = ex => FileSpanned::new(Register::C, ex.span()),
    Lone(KwD) = ex => FileSpanned::new(Register::D, ex.span()),
    Lone(KwE) = ex => FileSpanned::new(Register::E, ex.span()),
    Lone(KwH) = ex => FileSpanned::new(Register::H, ex.span()),
    Lone(KwL) = ex => FileSpanned::new(Register::L, ex.span()),
    Lone(KwAF) = ex => FileSpanned::new(Register::AF, ex.span()),
    Lone(KwBC) = ex => FileSpanned::new(Register::BC, ex.span()),
    Lone(KwDE) = ex => FileSpanned::new(Register::DE, ex.span()),
    Lone(KwHL) = ex => FileSpanned::new(Register::HL, ex.span()),
  }
  .labelled("register")
  .as_context()
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
  .labelled("`break`")
  .as_context()
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
  .labelled("`const`")
  .as_context()
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
  .labelled("`continue`")
  .as_context()
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
  .labelled("`fn`")
  .as_context()
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
  .labelled("`if`")
  .as_context()
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
  .labelled("`else`")
  .as_context()
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
  .labelled("`loop`")
  .as_context()
}

/// Parses a `Lone(KwReturn)`, which is then discarded.
pub fn kw_return_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(KwReturn) => (),
  }
  .labelled("`return`")
  .as_context()
}

/// Parses a `Lone(KwStatic)`, which is then discarded.
pub fn kw_static_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(KwStatic) => (),
  }
  .labelled("`static`")
  .as_context()
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
  .labelled("`:`")
  .as_context()
}

/// Parses a `Lone(Comma)`, which is then discarded.
pub fn comma_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(Comma) => (),
  }
  .labelled("`,`")
  .as_context()
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
  .labelled("`=`")
  .as_context()
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
  .labelled("`!`")
  .as_context()
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
  .labelled("`-`")
  .as_context()
}

/// Parses a `Lone(Newline)`, which is then discarded.
pub fn newline_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(Newline) => (),
  }
  .labelled("newline")
  .as_context()
}

/// Parses a `Lone(Pipe)`, which is then discarded.
pub fn pipe_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(Pipe) => (),
  }
  .labelled("`|`")
  .as_context()
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
  .labelled("`+`")
  .as_context()
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
  .labelled("`'`")
  .as_context()
}

/// Parses a `Lone(Semicolon)`, which is then discarded.
pub fn semicolon_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(Semicolon) => (),
  }
  .labelled("`;`")
  .as_context()
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
  .labelled("`>`")
  .as_context()
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
  .labelled("`<`")
  .as_context()
}

/// Parses a `Lone(Ampersand)`, which is then discarded.
pub fn ampersand_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(Ampersand) => (),
  }
  .labelled("`&`")
  .as_context()
}
