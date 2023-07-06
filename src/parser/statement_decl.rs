use super::*;

#[derive(Clone)]
pub enum StatementDecl {
  Load((TokenTree, SimpleSpan), (TokenTree, SimpleSpan)),
  Compare((TokenTree, SimpleSpan)),
  Jump((TokenTree, SimpleSpan), (TokenTree, SimpleSpan)),
  Inc((TokenTree, SimpleSpan)),
  Dec((TokenTree, SimpleSpan)),
}
impl core::fmt::Debug for StatementDecl {
  /// this cuts out some of the SimpleSpan junk from a debug print compared to
  /// using the derive.
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      StatementDecl::Load((dst, _), (src, _)) => write!(f, "ld {dst:?}, {src:?};"),
      StatementDecl::Compare((tt, _)) => write!(f, "cp a, {tt:?};"),
      StatementDecl::Jump((dst, _), (src, _)) => write!(f, "jp {dst:?}, {src:?};"),
      StatementDecl::Inc((tt, _)) => write!(f, "inc {tt:?};"),
      StatementDecl::Dec((tt, _)) => write!(f, "dec {tt:?};"),
    }
  }
}
impl StatementDecl {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    let load = {
      let instr = just(Lone(InstLD));
      let dest = select! {
        Brackets(tts) => Brackets(tts),
        Lone(RegA) => Lone(RegA),
        Lone(RegB) => Lone(RegB),
        Lone(RegC) => Lone(RegC),
        Lone(RegD) => Lone(RegD),
        Lone(RegE) => Lone(RegE),
        Lone(RegH) => Lone(RegH),
        Lone(RegL) => Lone(RegL),
        Lone(RegBC) => Lone(RegBC),
        Lone(RegDE) => Lone(RegDE),
        Lone(RegHL) => Lone(RegHL),
        Lone(RegSP) => Lone(RegSP),
      };
      let comma = just(Lone(Punct(',')));
      let macro_src = select! {
        Lone(Ident(i)) => Lone(Ident(i)),
      }
      .map_with_span(id2)
      .then_ignore(just(Lone(Punct('!'))))
      .then(
        select! {
          Parens(tts) => Parens(tts),
        }
        .map_with_span(id2),
      )
      .map(|((n, n_span), (p, p_span))| Parens(vec![(n, n_span), (p, p_span)]));
      let simple_src = select! {
        Brackets(tts) => Brackets(tts),
        Lone(NumLit(n)) => Lone(NumLit(n)),
        Lone(Ident(i)) => Lone(Ident(i)),
        Lone(RegA) => Lone(RegA),
        Lone(RegB) => Lone(RegB),
        Lone(RegC) => Lone(RegC),
        Lone(RegD) => Lone(RegD),
        Lone(RegE) => Lone(RegE),
        Lone(RegH) => Lone(RegH),
        Lone(RegL) => Lone(RegL),
        Lone(RegBC) => Lone(RegBC),
        Lone(RegDE) => Lone(RegDE),
        Lone(RegHL) => Lone(RegHL),
        Lone(RegSP) => Lone(RegSP),
      };
      let src = macro_src.or(simple_src);
      instr
        .ignore_then(dest.map_with_span(id2))
        .then_ignore(comma)
        .then(src.map_with_span(id2))
        .map(|(d, s)| StatementDecl::Load(d, s))
    };

    let compare = {
      let instr = just(Lone(InstCP));
      let reg_a = just(Lone(RegA));
      let comma = just(Lone(Punct(',')));
      let rhs = select! {
        Brackets(tts) => Brackets(tts),
        Lone(NumLit(n)) => Lone(NumLit(n)),
        Lone(RegA) => Lone(RegA),
        Lone(RegB) => Lone(RegB),
        Lone(RegC) => Lone(RegC),
        Lone(RegD) => Lone(RegD),
        Lone(RegE) => Lone(RegE),
        Lone(RegH) => Lone(RegH),
        Lone(RegL) => Lone(RegL),
      };

      let cp_a = instr
        .clone()
        .ignore_then(reg_a)
        .ignore_then(comma)
        .ignore_then(rhs.map_with_span(id2));
      let cp = instr.clone().ignore_then(rhs.map_with_span(id2));
      cp_a.or(cp).map(|(tt, span)| Self::Compare((tt, span)))
    };
    let or = {
      let instr = just(Lone(InstOR));
      let reg_a = just(Lone(RegA));
      let comma = just(Lone(Punct(',')));
      let rhs = select! {
        Brackets(tts) => Brackets(tts),
        Lone(NumLit(n)) => Lone(NumLit(n)),
        Lone(RegA) => Lone(RegA),
        Lone(RegB) => Lone(RegB),
        Lone(RegC) => Lone(RegC),
        Lone(RegD) => Lone(RegD),
        Lone(RegE) => Lone(RegE),
        Lone(RegH) => Lone(RegH),
        Lone(RegL) => Lone(RegL),
      };

      let or_a = instr
        .clone()
        .ignore_then(reg_a)
        .ignore_then(comma)
        .ignore_then(rhs.map_with_span(id2));
      let or = instr.clone().ignore_then(rhs.map_with_span(id2));
      or_a.or(or).map(|(tt, span)| Self::Compare((tt, span)))
    };

    let jump = {
      let instr = just(Lone(InstJP));
      let cond = select! {
        Lone(RegC) => Lone(RegC),
        Lone(Ident(i)) => Lone(Ident(i)),
      };
      let comma = just(Lone(Punct(',')));
      let target = select! {
        Lone(NumLit(n)) => Lone(NumLit(n)),
        Lone(Ident(i)) => Lone(Ident(i)),
      };

      let jp_cond = instr
        .clone()
        .ignore_then(cond.map_with_span(id2))
        .then_ignore(comma)
        .then(target.map_with_span(id2))
        .map(|(cond, target)| StatementDecl::Jump(cond, target));
      let jp_always =
        instr.clone().ignore_then(target.map_with_span(id2)).map(|target| {
          StatementDecl::Jump((Lone(Ident("al")), SimpleSpan::from(0..0)), target)
        });
      jp_cond.or(jp_always)
    };

    let inc = {
      let instr = just(Lone(InstINC));
      let target = select! {
        Brackets(tts) => Brackets(tts),
        Lone(RegA) => Lone(RegA),
        Lone(RegB) => Lone(RegB),
        Lone(RegC) => Lone(RegC),
        Lone(RegD) => Lone(RegD),
        Lone(RegE) => Lone(RegE),
        Lone(RegH) => Lone(RegH),
        Lone(RegL) => Lone(RegL),
        Lone(RegBC) => Lone(RegBC),
        Lone(RegDE) => Lone(RegDE),
        Lone(RegHL) => Lone(RegHL),
        Lone(RegSP) => Lone(RegSP),
      };
      instr.ignore_then(target).map_with_span(|t, span| Self::Inc((t, span)))
    };
    let dec = {
      let instr = just(Lone(InstDEC));
      let target = select! {
        Brackets(tts) => Brackets(tts),
        Lone(RegA) => Lone(RegA),
        Lone(RegB) => Lone(RegB),
        Lone(RegC) => Lone(RegC),
        Lone(RegD) => Lone(RegD),
        Lone(RegE) => Lone(RegE),
        Lone(RegH) => Lone(RegH),
        Lone(RegL) => Lone(RegL),
        Lone(RegBC) => Lone(RegBC),
        Lone(RegDE) => Lone(RegDE),
        Lone(RegHL) => Lone(RegHL),
        Lone(RegSP) => Lone(RegSP),
      };
      instr.ignore_then(target).map_with_span(|t, span| Self::Inc((t, span)))
    };

    load.or(compare).or(or).or(jump).or(inc).or(dec)
  }
}
