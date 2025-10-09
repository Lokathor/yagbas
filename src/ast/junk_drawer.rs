use super::*;

/// Makes a token tree slice into an [Input](chumsky::input::Input).
///
/// This is used by all our parsers that need to look at the content of a token
/// tree group.
///
/// * When making a parser that looks at the content of a token tree group, we
///   need to run a parser on that inner content.
/// * Running a parser on the content of a tree group uses `Input::map` to turn
///   our custom [S<T>] type (which chumsky doesn't know about) into `(T,
///   FileSpan)` (which is what chumsky is expecting).
/// * If any part of such a parser is recursive (eg, statements, expressions,
///   etc) then we'd get a type error because Rust doesn't see two calls to
///   `Input::map` with different closures as being the same type just because
///   the two closures have the exact same expression.
/// * So we force all our calls to any parser that uses grouped content to go
///   through this one specific function, which gives them all the exact same
///   mapping closure, which lets Rust see that they're all the same type.
pub fn make_tt_input<'src>(
  trees: &'src [(TokenTree, SimpleSpan)], eoi: SimpleSpan,
) -> impl BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>
{
  Input::map(trees, eoi, |(tree, span)| (tree, span))
}

pub type AstExtras<'src> =
  Full<Rich<'src, TokenTree, SimpleSpan>, SimpleState<&'static FileData>, ()>;

/// Parse a lone [Token::Ident] and get the [StrID] it's for.
pub(crate) fn ident_p<'src, I>()
-> impl Parser<'src, I, StrID, AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  // Note(Lokathor): All these intermediate steps with explicit types are
  // basically necessary otherwise rustc will fall over and cry.
  select! {
    TokenTree::Lone(Token::Ident) = ex => {
      let state: &mut SimpleState<&'static FileData> = ex.state();
      let file_content: &'static str = state.content();
      let span: SimpleSpan = ex.span();
      let range: Range<usize> = span.into();
      let str_id = StrID::from(&file_content[range]);
      str_id
    }
  }
}

/// Parse a lone [Token::NumLit] and get the [StrID] it's for.
pub(crate) fn numlit_p<'src, I>()
-> impl Parser<'src, I, StrID, AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  // Note(Lokathor): like ident_p, but for a number
  select! {
    TokenTree::Lone(Token::NumLit) = ex => {
      let state: &mut SimpleState<&'static FileData> = ex.state();
      let file_content: &'static str = state.content();
      let span: SimpleSpan = ex.span();
      let range: Range<usize> = span.into();
      let str_id = StrID::from(&file_content[range]);
      str_id
    }
  }
}

pub(crate) fn parens_p<'src, I>()
-> impl Parser<'src, I, Vec<(TokenTree, SimpleSpan)>, AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Parens(body) => { body }
  }
}

pub(crate) fn newline_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::Newline) => { () }
  }
}

pub(crate) fn equal_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::Equal) => { () }
  }
}

pub(crate) fn plus_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::Plus) => { () }
  }
}

pub(crate) fn minus_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::Minus) => { () }
  }
}

/// Lets you `select_ref!` the content out of some `Braces`
pub(crate) fn braces_content_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, I, AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
  M: Fn(&'src [(TokenTree, SimpleSpan)], SimpleSpan) -> I + Copy + 'src,
{
  select_ref! {
    TokenTree::Braces(b) = ex => make_input(b, ex.span()),
  }
}

/// Lets you `select_ref!` the content out of some `Parens`
pub(crate) fn parens_content_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, I, AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
  M: Fn(&'src [(TokenTree, SimpleSpan)], SimpleSpan) -> I + Copy + 'src,
{
  select_ref! {
    TokenTree::Parens(b) = ex => make_input(b, ex.span()),
  }
}

/// Lets you `select_ref!` the content out of some `Brackets`
pub(crate) fn brackets_content_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, I, AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
  M: Fn(&'src [(TokenTree, SimpleSpan)], SimpleSpan) -> I + Copy + 'src,
{
  select_ref! {
    TokenTree::Brackets(b) = ex => make_input(b, ex.span()),
  }
}

pub(crate) fn cmp_eq_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::Equal) => { () }
  }
  .ignore_then(select! {
    TokenTree::Lone(Token::Equal) => { () }
  })
}

pub(crate) fn cmp_ne_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::Exclamation) => { () }
  }
  .ignore_then(select! {
    TokenTree::Lone(Token::Equal) => { () }
  })
}

pub(crate) fn less_than_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::LessThan) => { () }
  }
}

pub(crate) fn greater_than_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::GreaterThan) => { () }
  }
}

pub(crate) fn cmp_le_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::LessThan) => { () }
  }
  .ignore_then(select! {
    TokenTree::Lone(Token::Equal) => { () }
  })
}

pub(crate) fn cmp_ge_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::GreaterThan) => { () }
  }
  .ignore_then(select! {
    TokenTree::Lone(Token::Equal) => { () }
  })
}

pub(crate) fn pipe_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::Pipe) => { () }
  }
}

pub(crate) fn caret_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::Caret) => { () }
  }
}

pub(crate) fn ampersand_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::Ampersand) => { () }
  }
}

pub(crate) fn double_lt_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::LessThan) => { () }
  }
  .ignore_then(select! {
    TokenTree::Lone(Token::LessThan) => { () }
  })
}

pub(crate) fn double_gt_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::GreaterThan) => { () }
  }
  .ignore_then(select! {
    TokenTree::Lone(Token::GreaterThan) => { () }
  })
}

pub(crate) fn asterisk_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::Asterisk) => { () }
  }
}

pub(crate) fn slash_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::Slash) => { () }
  }
}

pub(crate) fn percent_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::Percent) => { () }
  }
}

pub(crate) fn plusplus_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::Plus) => { () }
  }
  .ignore_then(select! {
    TokenTree::Lone(Token::Plus) => { () }
  })
}

pub(crate) fn minusminus_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::Minus) => { () }
  }
  .ignore_then(select! {
    TokenTree::Lone(Token::Minus) => { () }
  })
}

pub(crate) fn period_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::Period) => { () }
  }
}

pub(crate) fn quote_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::Quote) => { () }
  }
}

pub(crate) fn colon_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::Colon) => { () }
  }
}

pub(crate) fn comma_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::Comma) => { () }
  }
}

pub(crate) fn exclamation_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::Exclamation) => { () }
  }
}

pub(crate) fn kw_loop_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::KwLoop) => { () }
  }
}

pub(crate) fn kw_return_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::KwReturn) => { () }
  }
}

pub(crate) fn kw_break_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::KwBreak) => { () }
  }
}

pub(crate) fn kw_continue_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::KwContinue) => { () }
  }
}

pub(crate) fn kw_if_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::KwIf) => { () }
  }
}

pub(crate) fn kw_bit_struct_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::KwBitStruct) => { () }
  }
}

pub(crate) fn kw_struct_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::KwStruct) => { () }
  }
}

pub(crate) fn kw_else_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::KwElse) => { () }
  }
}

pub(crate) fn kw_static_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::KwStatic) => { () }
  }
}

pub(crate) fn bool_p<'src, I>()
-> impl Parser<'src, I, bool, AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::KwTrue) => { true },
    TokenTree::Lone(Token::KwFalse) => { false },
  }
}
