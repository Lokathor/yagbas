use super::*;

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

pub(crate) fn braces_p<'src, I>()
-> impl Parser<'src, I, Vec<(TokenTree, SimpleSpan)>, AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Braces(body) => { body }
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
