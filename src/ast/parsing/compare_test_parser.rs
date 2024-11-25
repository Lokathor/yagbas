use crate::ast::CompareTest;

use super::*;

pub fn compare_test_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, CompareTest, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
  M: Fn(&'src [FileSpanned<TokenTree>], FileSpan) -> I + Copy + 'src,
{
  let a_eq_imm = kw_a_p()
    .ignore_then(equal_p())
    .ignore_then(equal_p())
    .ignore_then(const_expr_p(make_input))
    .map_with(|x, e| CompareTest::AEqualsImm(x));
  let a_not_eq_imm = kw_a_p()
    .ignore_then(exclamation_p())
    .ignore_then(equal_p())
    .ignore_then(const_expr_p(make_input))
    .map_with(|x, e| CompareTest::ANotEqualsImm(x));
  let a_lt_imm = kw_a_p()
    .ignore_then(less_than_p())
    .ignore_then(const_expr_p(make_input))
    .map_with(|x, e| CompareTest::ALessThanImm(x));
  let a_ge_imm = kw_a_p()
    .ignore_then(greater_than_p())
    .ignore_then(equal_p())
    .ignore_then(const_expr_p(make_input))
    .map_with(|x, e| CompareTest::AGreaterThanOrEqualImm(x));

  //
  let a_eq_reg8 = kw_a_p()
    .ignore_then(equal_p())
    .ignore_then(equal_p())
    .ignore_then(reg8_p())
    .map_with(|r, e| CompareTest::AEqualsReg8(FileSpanned::new(r, e.span())));
  let a_not_eq_reg8 = kw_a_p()
    .ignore_then(exclamation_p())
    .ignore_then(equal_p())
    .ignore_then(reg8_p())
    .map_with(|r, e| {
      CompareTest::ANotEqualsReg8(FileSpanned::new(r, e.span()))
    });
  let a_lt_reg8 = kw_a_p()
    .ignore_then(less_than_p())
    .ignore_then(reg8_p())
    .map_with(|r, e| CompareTest::ALessThanReg8(FileSpanned::new(r, e.span())));
  let a_ge_reg8 = kw_a_p()
    .ignore_then(greater_than_p())
    .ignore_then(equal_p())
    .ignore_then(reg8_p())
    .map_with(|r, e| {
      CompareTest::AGreaterThanOrEqualReg8(FileSpanned::new(r, e.span()))
    });

  //
  let a_eq_hlt = kw_a_p()
    .ignore_then(equal_p())
    .ignore_then(equal_p())
    .ignore_then(kw_hl_p().nested_in(nested_bracket_content_p(make_input)))
    .to(CompareTest::AEqualsHlTarget);
  let a_not_eq_hlt = kw_a_p()
    .ignore_then(exclamation_p())
    .ignore_then(equal_p())
    .ignore_then(kw_hl_p().nested_in(nested_bracket_content_p(make_input)))
    .to(CompareTest::ANotEqualsHlTarget);
  let a_lt_hlt = kw_a_p()
    .ignore_then(less_than_p())
    .ignore_then(kw_hl_p().nested_in(nested_bracket_content_p(make_input)))
    .to(CompareTest::ALessThanHlTarget);
  let a_ge_hlt = kw_a_p()
    .ignore_then(greater_than_p())
    .ignore_then(equal_p())
    .ignore_then(kw_hl_p().nested_in(nested_bracket_content_p(make_input)))
    .to(CompareTest::AGreaterThanOrEqualHlTarget);

  let x = choice((
    a_eq_imm,
    a_not_eq_imm,
    a_lt_imm,
    a_ge_imm,
    //
    a_eq_reg8,
    a_not_eq_reg8,
    a_lt_reg8,
    a_ge_reg8,
    //
    a_eq_hlt,
    a_not_eq_hlt,
    a_lt_hlt,
    a_ge_hlt,
  ));

  x
}
