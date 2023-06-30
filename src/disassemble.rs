#[derive(Clone, Copy)]
pub enum OpToken {
  One(u8),
  Two(u8, u8),
  Three(u8, u16),
  WantedTwoGotOne(u8),
  WantedThreeGotOne(u8),
  WantedThreeGotTwo(u8, u8),
}
impl Default for OpToken {
  #[inline]
  fn default() -> Self {
    Self::One(0)
  }
}
impl core::fmt::Debug for OpToken {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match *self {
      OpToken::One(op) => write!(f, "${op:02X}"),
      OpToken::Two(op, i) => write!(f, "${op:02X}(${i:02X})"),
      OpToken::Three(op, i) => write!(f, "${op:02X}(${i:04X})"),
      OpToken::WantedTwoGotOne(err) => write!(f, "Err(${err:02X})"),
      OpToken::WantedThreeGotOne(err) => write!(f, "Err(${err:02X})"),
      OpToken::WantedThreeGotTwo(err1, err2) => write!(f, "Err(${err1:02X},${err2:02X})"),
    }
  }
}
impl core::fmt::Display for OpToken {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    if let Some(op) = self.op() {
      match op {
        0x00 => write!(f, "nop"),
        0x01 => write!(f, "ld bc, ${:04X}", self.u16()),
        0x02 => write!(f, "ld [bc], a"),
        0x03 => write!(f, "inc bc"),
        0x04 => write!(f, "inc b"),
        0x05 => write!(f, "dec b"),
        0x06 => write!(f, "ld b, {}", self.u8()),
        0x07 => write!(f, "rlca"),
        0x08 => write!(f, "ld [${:04X}], sp", self.u16()),
        0x09 => write!(f, "add hl, bc"),
        0x0A => write!(f, "ld a, [bc]"),
        0x0B => write!(f, "dec bc"),
        0x0C => write!(f, "inc c"),
        0x0D => write!(f, "dec c"),
        0x0E => write!(f, "ld c, {}", self.u8()),
        0x0F => write!(f, "rrca"),
        //
        0x10 => write!(f, "stop"),
        0x11 => write!(f, "ld de, ${:04X}", self.u16()),
        0x12 => write!(f, "ld [de], a"),
        0x13 => write!(f, "inc de"),
        0x14 => write!(f, "inc d"),
        0x15 => write!(f, "dec d"),
        0x16 => write!(f, "ld d, {}", self.u8()),
        0x17 => write!(f, "rla"),
        0x18 => write!(f, "jr {:+}", self.i8()),
        0x19 => write!(f, "add hl, de"),
        0x1A => write!(f, "ld a, [de]"),
        0x1B => write!(f, "dec de"),
        0x1C => write!(f, "inc e"),
        0x1D => write!(f, "dec e"),
        0x1E => write!(f, "ld d, {}", self.u8()),
        0x1F => write!(f, "rra"),
        // todo
        _ => core::fmt::Debug::fmt(self, f),
      }
    } else {
      match self {
        OpToken::WantedTwoGotOne(o) => write!(f, "raw_bytes!(${o:02X});"),
        OpToken::WantedThreeGotOne(o) => write!(f, "raw_bytes!(${o:02X});"),
        OpToken::WantedThreeGotTwo(o, t) => write!(f, "raw_bytes!(${o:02X}, ${t:02X});"),
        _ => core::fmt::Debug::fmt(self, f),
      }
    }
  }
}
impl OpToken {
  pub fn op(self) -> Option<u8> {
    match self {
      OpToken::One(op) => Some(op),
      OpToken::Two(op, _) => Some(op),
      OpToken::Three(op, _) => Some(op),
      OpToken::WantedTwoGotOne(_) => None,
      OpToken::WantedThreeGotOne(_) => None,
      OpToken::WantedThreeGotTwo(_, _) => None,
    }
  }
  pub fn u8(self) -> u8 {
    match self {
      OpToken::Two(_, u) => u,
      _ => panic!(),
    }
  }
  pub fn i8(self) -> i8 {
    match self {
      OpToken::Two(_, u) => u as i8,
      _ => panic!(),
    }
  }
  pub fn u16(self) -> u16 {
    match self {
      OpToken::Three(_, u) => u,
      _ => panic!(),
    }
  }
}

pub fn bytes_to_op_tokens(mut bytes: &[u8]) -> impl Iterator<Item = OpToken> + '_ {
  core::iter::from_fn(move || {
    let op: u8 = *bytes.first()?;
    let bytes_used = BYTES_PER_OP[usize::from(op)];
    match bytes_used {
      1 => match bytes {
        [one, rest @ ..] => {
          bytes = rest;
          Some(OpToken::One(*one))
        }
        [] => unimplemented!(),
      },
      2 => match bytes {
        [one, two, rest @ ..] => {
          bytes = rest;
          Some(OpToken::Two(*one, *two))
        }
        [one, rest @ ..] => {
          bytes = rest;
          Some(OpToken::WantedTwoGotOne(*one))
        }
        [] => unimplemented!(),
      },
      3 => match bytes {
        [one, two, three, rest @ ..] => {
          bytes = rest;
          let u = u16::from_le_bytes([*two, *three]);
          Some(OpToken::Three(*one, u))
        }
        [one, two, rest @ ..] => {
          bytes = rest;
          Some(OpToken::WantedThreeGotTwo(*one, *two))
        }
        [one, rest @ ..] => {
          bytes = rest;
          Some(OpToken::WantedThreeGotOne(*one))
        }
        [] => unimplemented!(),
      },
      _ => unimplemented!(),
    }
  })
}

static BYTES_PER_OP: [u8; 256] = [
  1, /* 0x00 */
  3, /* 0x01 */
  1, /* 0x02 */
  1, /* 0x03 */
  1, /* 0x04 */
  1, /* 0x05 */
  2, /* 0x06 */
  1, /* 0x07 */
  3, /* 0x08 */
  1, /* 0x09 */
  1, /* 0x0A */
  1, /* 0x0B */
  1, /* 0x0C */
  1, /* 0x0D */
  2, /* 0x0E */
  1, /* 0x0F */
  1, /* 0x10 */
  3, /* 0x11 */
  1, /* 0x12 */
  1, /* 0x13 */
  1, /* 0x14 */
  1, /* 0x15 */
  2, /* 0x16 */
  1, /* 0x17 */
  2, /* 0x18 */
  1, /* 0x19 */
  1, /* 0x1A */
  1, /* 0x1B */
  1, /* 0x1C */
  1, /* 0x1D */
  2, /* 0x1E */
  1, /* 0x1F */
  2, /* 0x20 */
  3, /* 0x21 */
  1, /* 0x22 */
  1, /* 0x23 */
  1, /* 0x24 */
  1, /* 0x25 */
  2, /* 0x26 */
  1, /* 0x27 */
  2, /* 0x28 */
  1, /* 0x29 */
  1, /* 0x2A */
  1, /* 0x2B */
  1, /* 0x2C */
  1, /* 0x2D */
  2, /* 0x2E */
  1, /* 0x2F */
  2, /* 0x30 */
  3, /* 0x31 */
  1, /* 0x32 */
  1, /* 0x33 */
  1, /* 0x34 */
  1, /* 0x35 */
  2, /* 0x36 */
  1, /* 0x37 */
  2, /* 0x38 */
  1, /* 0x39 */
  1, /* 0x3A */
  1, /* 0x3B */
  1, /* 0x3C */
  1, /* 0x3D */
  2, /* 0x3E */
  1, /* 0x3F */
  1, /* 0x40 */
  1, /* 0x41 */
  1, /* 0x42 */
  1, /* 0x43 */
  1, /* 0x44 */
  1, /* 0x45 */
  1, /* 0x46 */
  1, /* 0x47 */
  1, /* 0x48 */
  1, /* 0x49 */
  1, /* 0x4A */
  1, /* 0x4B */
  1, /* 0x4C */
  1, /* 0x4D */
  1, /* 0x4E */
  1, /* 0x4F */
  1, /* 0x50 */
  1, /* 0x51 */
  1, /* 0x52 */
  1, /* 0x53 */
  1, /* 0x54 */
  1, /* 0x55 */
  1, /* 0x56 */
  1, /* 0x57 */
  1, /* 0x58 */
  1, /* 0x59 */
  1, /* 0x5A */
  1, /* 0x5B */
  1, /* 0x5C */
  1, /* 0x5D */
  1, /* 0x5E */
  1, /* 0x5F */
  1, /* 0x60 */
  1, /* 0x61 */
  1, /* 0x62 */
  1, /* 0x63 */
  1, /* 0x64 */
  1, /* 0x65 */
  1, /* 0x66 */
  1, /* 0x67 */
  1, /* 0x68 */
  1, /* 0x69 */
  1, /* 0x6A */
  1, /* 0x6B */
  1, /* 0x6C */
  1, /* 0x6D */
  1, /* 0x6E */
  1, /* 0x6F */
  1, /* 0x70 */
  1, /* 0x71 */
  1, /* 0x72 */
  1, /* 0x73 */
  1, /* 0x74 */
  1, /* 0x75 */
  1, /* 0x76 */
  1, /* 0x77 */
  1, /* 0x78 */
  1, /* 0x79 */
  1, /* 0x7A */
  1, /* 0x7B */
  1, /* 0x7C */
  1, /* 0x7D */
  1, /* 0x7E */
  1, /* 0x7F */
  1, /* 0x80 */
  1, /* 0x81 */
  1, /* 0x82 */
  1, /* 0x83 */
  1, /* 0x84 */
  1, /* 0x85 */
  1, /* 0x86 */
  1, /* 0x87 */
  1, /* 0x88 */
  1, /* 0x89 */
  1, /* 0x8A */
  1, /* 0x8B */
  1, /* 0x8C */
  1, /* 0x8D */
  1, /* 0x8E */
  1, /* 0x8F */
  1, /* 0x90 */
  1, /* 0x91 */
  1, /* 0x92 */
  1, /* 0x93 */
  1, /* 0x94 */
  1, /* 0x95 */
  1, /* 0x96 */
  1, /* 0x97 */
  1, /* 0x98 */
  1, /* 0x99 */
  1, /* 0x9A */
  1, /* 0x9B */
  1, /* 0x9C */
  1, /* 0x9D */
  1, /* 0x9E */
  1, /* 0x9F */
  1, /* 0xA0 */
  1, /* 0xA1 */
  1, /* 0xA2 */
  1, /* 0xA3 */
  1, /* 0xA4 */
  1, /* 0xA5 */
  1, /* 0xA6 */
  1, /* 0xA7 */
  1, /* 0xA8 */
  1, /* 0xA9 */
  1, /* 0xAA */
  1, /* 0xAB */
  1, /* 0xAC */
  1, /* 0xAD */
  1, /* 0xAE */
  1, /* 0xAF */
  1, /* 0xB0 */
  1, /* 0xB1 */
  1, /* 0xB2 */
  1, /* 0xB3 */
  1, /* 0xB4 */
  1, /* 0xB5 */
  1, /* 0xB6 */
  1, /* 0xB7 */
  1, /* 0xB8 */
  1, /* 0xB9 */
  1, /* 0xBA */
  1, /* 0xBB */
  1, /* 0xBC */
  1, /* 0xBD */
  1, /* 0xBE */
  1, /* 0xBF */
  1, /* 0xC0 */
  1, /* 0xC1 */
  3, /* 0xC2 */
  3, /* 0xC3 */
  3, /* 0xC4 */
  1, /* 0xC5 */
  2, /* 0xC6 */
  1, /* 0xC7 */
  1, /* 0xC8 */
  1, /* 0xC9 */
  3, /* 0xCA */
  2, /* 0xCB */
  3, /* 0xCC */
  3, /* 0xCD */
  2, /* 0xCE */
  1, /* 0xCF */
  1, /* 0xD0 */
  1, /* 0xD1 */
  3, /* 0xD2 */
  1, /* 0xD3 */
  3, /* 0xD4 */
  1, /* 0xD5 */
  2, /* 0xD6 */
  1, /* 0xD7 */
  1, /* 0xD8 */
  1, /* 0xD9 */
  3, /* 0xDA */
  1, /* 0xDB */
  3, /* 0xDC */
  1, /* 0xDD */
  2, /* 0xDE */
  1, /* 0xDF */
  2, /* 0xE0 */
  1, /* 0xE1 */
  1, /* 0xE2 */
  1, /* 0xE3 */
  1, /* 0xE4 */
  1, /* 0xE5 */
  2, /* 0xE6 */
  1, /* 0xE7 */
  2, /* 0xE8 */
  1, /* 0xE9 */
  3, /* 0xEA */
  1, /* 0xEB */
  1, /* 0xEC */
  1, /* 0xED */
  2, /* 0xEE */
  1, /* 0xEF */
  2, /* 0xF0 */
  1, /* 0xF1 */
  1, /* 0xF2 */
  1, /* 0xF3 */
  1, /* 0xF4 */
  1, /* 0xF5 */
  2, /* 0xF6 */
  1, /* 0xF7 */
  2, /* 0xF8 */
  1, /* 0xF9 */
  3, /* 0xFA */
  1, /* 0xFB */
  1, /* 0xFC */
  1, /* 0xFD */
  2, /* 0xFE */
  1, /* 0xFF */
];
