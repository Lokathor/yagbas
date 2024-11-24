use crate::{
  ast::{Function, Item, Loop, Reg8, Statement},
  src_files::{FileSpan, FileSpanned},
  str_id::StrID,
};
use chumsky::{
  extra::Err,
  input::{BorrowInput, ValueInput},
  prelude::*,
};

use super::{
  const_expr::ConstExpr,
  token::Token::{self, *},
  token_tree::TokenTree::{self, *},
};

pub type ErrRichToken<'src> = Err<Rich<'src, Token, FileSpan>>;
pub type ErrRichTokenTree<'src> = Err<Rich<'src, TokenTree, FileSpan>>;

/// Parses [Token] into [TokenTree].
pub fn token_tree_p<'src, I>(
) -> impl Parser<'src, I, TokenTree, ErrRichToken<'src>> + Clone
where
  I: BorrowInput<'src, Token = Token, Span = FileSpan> + ValueInput<'src>,
{
  recursive(|tt| {
    let base = tt.map_with(|tts, e| FileSpanned::new(tts, e.span())).repeated();

    // Looks like `{ ... }`
    let braces = {
      let open_brace = select! {
        OpBrace => (),
      }
      .labelled("open_brace")
      .as_context();
      let close_brace = select! {
        ClBrace => (),
      }
      .labelled("close_brace")
      .as_context();
      base
        .clone()
        .collect()
        .delimited_by(open_brace, close_brace)
        .map(TokenTree::Braces)
        .labelled("braces_group")
        .as_context()
    };

    // Looks like `[ ... ]`
    let brackets = {
      let open_bracket = select! {
        OpBracket => (),
      }
      .labelled("open_bracket")
      .as_context();
      let close_bracket = select! {
        ClBracket => (),
      }
      .labelled("close_bracket")
      .as_context();
      base
        .clone()
        .collect()
        .delimited_by(open_bracket, close_bracket)
        .map(TokenTree::Brackets)
        .labelled("brackets_group")
        .as_context()
    };

    // Looks like `( ... )`
    let parens = {
      let open_paren = select! {
        OpParen => (),
      }
      .labelled("open_paren")
      .as_context();
      let close_paren = select! {
        ClParen => (),
      }
      .labelled("close_paren")
      .as_context();
      base
        .clone()
        .collect()
        .delimited_by(open_paren, close_paren)
        .map(TokenTree::Parens)
        .labelled("parens_group")
        .as_context()
    };

    // Looks like something that does *NOT* open or close one of the other
    // types.
    let single =
      none_of([OpBracket, ClBracket, OpBrace, ClBrace, OpParen, ClParen])
        .map(TokenTree::Lone);

    // comments get stripped from the output.
    let comment = {
      // Looks like `//`
      let single_comment = select! {
        CommentSingle => (),
      };
      // Looks like `/* ... */`
      let block_start = select! {
        CommentBlockStart => (),
      };
      let block_end = select! {
        CommentBlockEnd => (),
      };
      let block_comment = base
        .clone()
        .delimited_by(block_start, block_end)
        .ignored()
        .labelled("block_comment")
        .as_context();

      single_comment.or(block_comment)
    };

    let x = choice((brackets, braces, parens, single))
      .padded_by(comment.repeated())
      .labelled("token_tree")
      .as_context();

    x
  })
}

/// Parses [TokenTree] into any [Item]
pub fn item_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, Item, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
  M: Fn(&'src [FileSpanned<TokenTree>], FileSpan) -> I + Copy + 'src,
{
  let function = function_p(make_input).map(Item::Function);

  let x = choice((function,));

  x
}

/// Parses [TokenTree] into specifically a [Function]
pub fn function_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, Function, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
  M: Fn(&'src [FileSpanned<TokenTree>], FileSpan) -> I + Copy + 'src,
{
  let name = ident_p()
    .map_with(|i, e| FileSpanned::new(i, e.span()))
    .labelled("fn_name")
    .as_context();
  let args = parenthesis_p().labelled("fn_arg_parens").as_context();
  let fn_body = statement_p(make_input)
    .map_with(|s: Statement, e| FileSpanned::new(s, e.span()))
    .separated_by(statement_sep_p().repeated().at_least(1))
    .allow_leading()
    .allow_trailing()
    .collect()
    .nested_in(nested_brace_content_p(make_input))
    .labelled("fn_body")
    .as_context();
  // Note(Lokathor): This stupid thing is because RA is weird sometimes.
  // https://github.com/rust-lang/rust-analyzer/issues/18542
  let x = Parser::map(
    kw_fn_p().ignore_then(name).then(args).then(fn_body),
    |((name, arguments), statements)| Function { name, arguments, statements },
  )
  .labelled("function")
  .as_context();

  x
}

/// Parses [TokenTree] into specifically a [Statement]
pub fn statement_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, Statement, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
  M: Fn(&'src [FileSpanned<TokenTree>], FileSpan) -> I + Copy + 'src,
{
  recursive(|statements| {
    let loop_stmt = {
      let opt_name = quote_p()
        .ignore_then(ident_p())
        .then_ignore(colon_p())
        .or_not()
        .labelled("loop_name")
        .as_context();
      let keyword = kw_loop_p();
      let loop_body = statements
        .clone()
        .map_with(|tts, e| FileSpanned::new(tts, e.span()))
        .separated_by(statement_sep_p().repeated().at_least(1))
        .allow_leading()
        .allow_trailing()
        .collect()
        .nested_in(nested_brace_content_p(make_input))
        .labelled("loop_body")
        .as_context();
      let x = opt_name
        .then_ignore(keyword)
        .then(loop_body)
        .map(|(opt_name, body)| {
          let name = opt_name.unwrap_or_default();
          Statement::Loop(Loop::new_with_name(name, body))
        })
        .labelled("loop_stmt")
        .as_context();
      x
    };

    let continue_stmt = {
      let keyword = kw_continue_p();
      let opt_name = quote_p()
        .ignore_then(ident_p())
        .or_not()
        .labelled("continue_target")
        .as_context();
      let x = keyword
        .ignore_then(opt_name)
        .map(|opt_n| Statement::Continue(opt_n.unwrap_or_default()))
        .labelled("continue_stmt")
        .as_context();
      x
    };

    let break_stmt = {
      let keyword = kw_break_p();
      let opt_name = quote_p()
        .ignore_then(ident_p())
        .or_not()
        .labelled("break_target")
        .as_context();
      let x = keyword
        .ignore_then(opt_name)
        .map(|opt_n| Statement::Break(opt_n.unwrap_or_default()))
        .labelled("break_stmt")
        .as_context();
      x
    };

    let call = ident_p()
      .then(parenthesis_p())
      .map(|(target, args)| Statement::Call { target, args })
      .labelled("call_stmt")
      .as_context();

    let assign8_const = reg8_p()
      .then_ignore(equal_p())
      .then(const_expr_p(make_input))
      .map(|(target, value)| Statement::AssignReg8Const { target, value })
      .labelled("assign8_const")
      .as_context();

    let x = choice((
      kw_return_p(),
      call,
      loop_stmt,
      continue_stmt,
      break_stmt,
      assign8_const,
    ));

    x
  })
}

/// Parses a single constant expression.
pub fn const_expr_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, FileSpanned<ConstExpr>, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
  M: Fn(&'src [FileSpanned<TokenTree>], FileSpan) -> I + Copy + 'src,
{
  use chumsky::pratt::*;

  recursive(|expr| {
    let atom = {
      let num_lit = num_lit_p().map_with(|n, extras| {
        FileSpanned::new(ConstExpr::Literal(n), extras.span())
      });
      let ident = ident_p().map_with(|i, extras| {
        FileSpanned::new(ConstExpr::Ident(i), extras.span())
      });
      let parens = expr
        .nested_in(nested_parens_content_p(make_input))
        .map_with(|x, extras| FileSpanned::new(x, extras.span()));

      choice((num_lit, ident, parens))
    };

    let with_pratt = atom.pratt((
      infix(left(1), plus_p(), |l, _op, r, extra| {
        FileSpanned::new(ConstExpr::Add(Box::new(l), Box::new(r)), extra.span())
      }),
      infix(left(1), minus_p(), |l, _op, r, extra| {
        FileSpanned::new(ConstExpr::Sub(Box::new(l), Box::new(r)), extra.span())
      }),
    ));

    with_pratt
  })
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
}

/// Parses a `Lone(Newline)` or `Lone(Semicolon)`, which is then discarded.
pub fn statement_sep_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(Newline) => (),
    Lone(Semicolon) => (),
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

/// Parses an 8-bit register keyword and returns it.
pub fn reg8_p<'src, I>(
) -> impl Parser<'src, I, Reg8, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(KwA) => Reg8::A,
    Lone(KwB) => Reg8::B,
    Lone(KwC) => Reg8::C,
    Lone(KwD) => Reg8::D,
    Lone(KwE) => Reg8::E,
    Lone(KwH) => Reg8::H,
    Lone(KwL) => Reg8::L,
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

/// Parses `Parens(p)` and returns `p`.
pub fn parenthesis_p<'src, I>(
) -> impl Parser<'src, I, Vec<FileSpanned<TokenTree>>, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Parens(p) = ex => p,
  }
}

/// Lets you `select_ref!` the content out of some `Braces`
pub fn nested_brace_content_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, I, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
  M: Fn(&'src [FileSpanned<TokenTree>], FileSpan) -> I + Copy + 'src,
{
  select_ref! {
    Braces(b) = ex => make_input(b, ex.span()),
  }
}

/// Lets you `select_ref!` the content out of some `Parens`
pub fn nested_parens_content_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, I, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
  M: Fn(&'src [FileSpanned<TokenTree>], FileSpan) -> I + Copy + 'src,
{
  select_ref! {
    Parens(b) = ex => make_input(b, ex.span()),
  }
}
