use core::ops::Range;

use crate::{static_str, StaticStr};
use logos::Logos;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Logos)]
#[logos(skip r#"[ \t\r\n\f]+"#)] // ignore this between tokens
pub enum Token {
  /// `//` starts a single-line comment.
  #[regex(r"//[^\r\n]*")]
  CommentSingle,

  /// `/*`, the start of a multi-line comment
  #[token(r"/*", priority = 2)]
  CommentBlockStart,

  /// `*/`, the end of a multi-line comment
  #[token(r"*/", priority = 2)]
  CommentBlockEnd,

  #[token("section")]
  KwSection,
  #[token("const")]
  KwConst,
  #[token("static")]
  KwStatic,
  #[token("if")]
  KwIf,

  #[token("a", priority = 3)]
  #[token("A", priority = 3)]
  RegA,
  #[token("b", priority = 3)]
  #[token("B", priority = 3)]
  RegB,
  #[token("c", priority = 3)]
  #[token("C", priority = 3)]
  RegC,
  #[token("d", priority = 3)]
  #[token("D", priority = 3)]
  RegD,
  #[token("e", priority = 3)]
  #[token("E", priority = 3)]
  RegE,
  #[token("h", priority = 3)]
  #[token("H", priority = 3)]
  RegH,
  #[token("l", priority = 3)]
  #[token("L", priority = 3)]
  RegL,
  #[token("af", priority = 3)]
  #[token("AF", priority = 3)]
  RegAF,
  #[token("bc", priority = 3)]
  #[token("BC", priority = 3)]
  RegBC,
  #[token("de", priority = 3)]
  #[token("DE", priority = 3)]
  RegDE,
  #[token("hl", priority = 3)]
  #[token("HL", priority = 3)]
  RegHL,
  #[token("sp", priority = 3)]
  #[token("SP", priority = 3)]
  RegSP,
  #[token("pc", priority = 3)]
  #[token("PC", priority = 3)]
  RegPC,

  /*
   * these tokens aren't Logos output, they can only be produced by the
   * TokenTree builder flattening specific Brackets tree sequences. Logos
   * doesn't support backtracking, so trying to lex these tokens directly would
   * cause more errors than desirable in the raw token stream. Not every sequence
   * gets a special replacement, but these are particularly common.
   */
  /// `[bc]`
  AddrBC,
  /// `[de]`
  AddrDE,
  /// `[hl]`
  AddrHL,
  /// `[hl++]`
  AddrHLInc,
  /// `[hl--]`
  AddrHLDec,

  #[token("adc", priority = 3)]
  #[token("ADC", priority = 3)]
  InstADC,
  #[token("add", priority = 3)]
  #[token("ADD", priority = 3)]
  InstADD,
  #[token("and", priority = 3)]
  #[token("AND", priority = 3)]
  InstAND,
  #[token("bit", priority = 3)]
  #[token("BIT", priority = 3)]
  InstBIT,
  #[token("call", priority = 3)]
  #[token("CALL", priority = 3)]
  InstCALL,
  #[token("ccf", priority = 3)]
  #[token("CCF", priority = 3)]
  InstCCF,
  #[token("cp", priority = 3)]
  #[token("CP", priority = 3)]
  InstCP,
  #[token("cpl", priority = 3)]
  #[token("CPL", priority = 3)]
  InstCPL,
  #[token("daa", priority = 3)]
  #[token("DAA", priority = 3)]
  InstDAA,
  #[token("dec", priority = 3)]
  #[token("DEC", priority = 3)]
  InstDEC,
  #[token("di", priority = 3)]
  #[token("DI", priority = 3)]
  InstDI,
  #[token("ei", priority = 3)]
  #[token("EI", priority = 3)]
  InstEI,
  #[token("halt", priority = 3)]
  #[token("HALT", priority = 3)]
  InstHALT,
  #[token("inc", priority = 3)]
  #[token("INC", priority = 3)]
  InstINC,
  #[token("jp", priority = 3)]
  #[token("JP", priority = 3)]
  InstJP,
  #[token("ld", priority = 3)]
  #[token("LD", priority = 3)]
  InstLD,
  #[token("ldh", priority = 3)]
  #[token("LDH", priority = 3)]
  InstLDH,
  #[token("nop", priority = 3)]
  #[token("NOP", priority = 3)]
  InstNOP,
  #[token("or", priority = 3)]
  #[token("OR", priority = 3)]
  InstOR,
  #[token("pop", priority = 3)]
  #[token("POP", priority = 3)]
  InstPOP,
  #[token("push", priority = 3)]
  #[token("PUSH", priority = 3)]
  InstPUSH,
  #[token("res", priority = 3)]
  #[token("RES", priority = 3)]
  InstRES,
  #[token("ret", priority = 3)]
  #[token("RET", priority = 3)]
  InstRET,
  #[token("reti", priority = 3)]
  #[token("RETI", priority = 3)]
  InstRETI,
  #[token("rl", priority = 3)]
  #[token("RL", priority = 3)]
  InstRL,
  #[token("rla", priority = 3)]
  #[token("RLA", priority = 3)]
  InstRLA,
  #[token("rlc", priority = 3)]
  #[token("RLC", priority = 3)]
  InstRLC,
  #[token("rlca", priority = 3)]
  #[token("RLCA", priority = 3)]
  InstRLCA,
  #[token("rr", priority = 3)]
  #[token("RR", priority = 3)]
  InstRR,
  #[token("rra", priority = 3)]
  #[token("RRA", priority = 3)]
  InstRRA,
  #[token("rrc", priority = 3)]
  #[token("RRC", priority = 3)]
  InstRRC,
  #[token("rrca", priority = 3)]
  #[token("RRCA", priority = 3)]
  InstRRCA,
  #[token("rst", priority = 3)]
  #[token("RST", priority = 3)]
  InstRST,
  #[token("sbc", priority = 3)]
  #[token("SBC", priority = 3)]
  InstSBC,
  #[token("scf", priority = 3)]
  #[token("SCF", priority = 3)]
  InstSCF,
  #[token("set", priority = 3)]
  #[token("SET", priority = 3)]
  InstSET,
  #[token("sla", priority = 3)]
  #[token("SLA", priority = 3)]
  InstSLA,
  #[token("sra", priority = 3)]
  #[token("SRA", priority = 3)]
  InstSRA,
  #[token("srl", priority = 3)]
  #[token("SRL", priority = 3)]
  InstSRL,
  #[token("stop", priority = 3)]
  #[token("STOP", priority = 3)]
  InstSTOP,
  #[token("sub", priority = 3)]
  #[token("SUB", priority = 3)]
  InstSUB,
  #[token("swap", priority = 3)]
  #[token("SWAP", priority = 3)]
  InstSWAP,
  #[token("xor", priority = 3)]
  #[token("XOR", priority = 3)]
  InstXOR,

  #[token("cy", priority = 3)]
  #[token("CY", priority = 3)]
  CondCY,
  #[token("nc", priority = 3)]
  #[token("NC", priority = 3)]
  CondNC,
  #[token("ze", priority = 3)]
  #[token("ZE", priority = 3)]
  CondZE,
  #[token("nz", priority = 3)]
  #[token("NZ", priority = 3)]
  CondNZ,
  #[token("al", priority = 3)]
  #[token("AL", priority = 3)]
  CondAL,

  /// A standard identifier in C-style langs: `[_a-zA-Z][_a-zA-Z0-9]*`
  ///
  /// The lone character `_` ends up matching as an Ident rather than a
  /// Punctuation.
  #[regex(r"[_a-zA-Z][_a-zA-Z0-9]*", |lex| static_str(lex.slice()), priority=2)]
  Ident(StaticStr),

  /// A punctuation character (using the `[:punct:]` regex class) that does
  /// *not* match any other case.
  #[regex(r"[[:punct:]]", |lex| lex.slice().chars().next().unwrap(), priority=1)]
  Punct(char),

  /// Something that's supposed to be a number.
  ///
  /// Interpreting the number is left for the parsing stage.
  #[regex(r"[-+]?((0x|\$)[_a-zA-Z0-9]+|(0b|%)[_a-zA-Z0-9]+|[0-9][_a-zA-Z0-9]*)", |lex| static_str(lex.slice()))]
  NumLit(StaticStr),

  /// Holds all the stuff *between* two `"`.
  ///
  /// This allows `\"` and `\\` within the string literal that's collected, but
  /// doesn't actually handle escape sequence processing.
  ///
  /// Thanks to `Quirl`, who made this regex: "works by specifying all of the
  /// escape sequences you allow (here just `\"`, and `\\` for a `\` itself) up
  /// front and then requiring the rest of the literal to not be a quote or
  /// escape (that would start with `\`)"
  #[regex(r#""((\\"|\\\\)|[^\\"])*""#, |lex| {let s = lex.slice(); static_str(&s[1..s.len()-1]) })]
  StrLit(StaticStr),
}
impl Token {
  #[inline]
  #[must_use]
  pub fn lexer(s: &str) -> logos::Lexer<'_, Token> {
    <Self as Logos>::lexer(s)
  }
}
impl core::fmt::Debug for Token {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match *self {
      Token::CommentSingle => write!(f, "//"),
      Token::CommentBlockStart => write!(f, "/*"),
      Token::CommentBlockEnd => write!(f, "*/"),
      Token::Ident(i) => write!(f, "{i}"),
      Token::Punct(p) => write!(f, "{p}"),
      Token::NumLit(n) => write!(f, "n{n:?}"),
      Token::StrLit(s) => write!(f, "s{s:?}"),
      Token::KwSection => write!(f, "section"),
      Token::KwConst => write!(f, "const"),
      Token::KwStatic => write!(f, "static"),
      Token::RegA => write!(f, "a"),
      Token::RegB => write!(f, "b"),
      Token::RegC => write!(f, "c"),
      Token::RegD => write!(f, "d"),
      Token::RegE => write!(f, "e"),
      Token::RegH => write!(f, "h"),
      Token::RegL => write!(f, "l"),
      Token::RegAF => write!(f, "af"),
      Token::RegBC => write!(f, "bc"),
      Token::RegDE => write!(f, "de"),
      Token::RegHL => write!(f, "hl"),
      Token::RegSP => write!(f, "sp"),
      Token::RegPC => write!(f, "pc"),
      Token::InstADC => write!(f, "adc"),
      Token::InstADD => write!(f, "add"),
      Token::InstAND => write!(f, "and"),
      Token::InstBIT => write!(f, "bit"),
      Token::InstCALL => write!(f, "call"),
      Token::InstCCF => write!(f, "ccf"),
      Token::InstCP => write!(f, "cp"),
      Token::InstCPL => write!(f, "cpl"),
      Token::InstDAA => write!(f, "daa"),
      Token::InstDEC => write!(f, "dec"),
      Token::InstDI => write!(f, "di"),
      Token::InstEI => write!(f, "ei"),
      Token::InstHALT => write!(f, "halt"),
      Token::InstINC => write!(f, "inc"),
      Token::InstJP => write!(f, "jp"),
      Token::InstLD => write!(f, "ld"),
      Token::InstLDH => write!(f, "ldh"),
      Token::InstNOP => write!(f, "nop"),
      Token::InstOR => write!(f, "or"),
      Token::InstPOP => write!(f, "pop"),
      Token::InstPUSH => write!(f, "push"),
      Token::InstRES => write!(f, "res"),
      Token::InstRET => write!(f, "ret"),
      Token::InstRETI => write!(f, "reti"),
      Token::InstRL => write!(f, "rl"),
      Token::InstRLA => write!(f, "rla"),
      Token::InstRLC => write!(f, "rlc"),
      Token::InstRLCA => write!(f, "rlca"),
      Token::InstRR => write!(f, "rr"),
      Token::InstRRA => write!(f, "rra"),
      Token::InstRRC => write!(f, "rrc"),
      Token::InstRRCA => write!(f, "rrca"),
      Token::InstRST => write!(f, "rst"),
      Token::InstSBC => write!(f, "sbc"),
      Token::InstSCF => write!(f, "scf"),
      Token::InstSET => write!(f, "set"),
      Token::InstSLA => write!(f, "sla"),
      Token::InstSRA => write!(f, "sra"),
      Token::InstSRL => write!(f, "srl"),
      Token::InstSTOP => write!(f, "stop"),
      Token::InstSUB => write!(f, "sub"),
      Token::InstSWAP => write!(f, "swap"),
      Token::InstXOR => write!(f, "xor"),
      Token::CondCY => write!(f, "cy"),
      Token::CondNC => write!(f, "nc"),
      Token::CondZE => write!(f, "ze"),
      Token::CondNZ => write!(f, "nz"),
      Token::CondAL => write!(f, "al"),
      Token::AddrHL => write!(f, "[hl]"),
      Token::AddrHLInc => write!(f, "[hl++]"),
      Token::AddrHLDec => write!(f, "[hl--]"),
      Token::AddrBC => write!(f, "[bc]"),
      Token::AddrDE => write!(f, "[de]"),
      Token::KwIf => write!(f, "if"),
    }
  }
}
