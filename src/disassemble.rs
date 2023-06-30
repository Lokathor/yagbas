pub fn print_basic_disassembly(rom: &[u8]) {
  for (i, bank) in rom.chunks(8 * 1024).enumerate() {
    let mut zeros: u32 = 0;
    println!("/* Bank {i} */");
    for op in bytes_to_op_tokens(bank) {
      match op {
        OpToken::One(0) => zeros += 1,
        _ => {
          match zeros {
            0 => (),
            1 => {
              println!("    {};", OpToken::One(0x00));
              zeros = 0;
            }
            2.. => {
              println!("    zero_bytes!({zeros});");
              zeros = 0;
            }
          }
          println!("    {op};")
        }
      };
    }
    if zeros > 0 {
      println!("    zero_bytes!({zeros});");
    }
  }
}

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
        0x1E => write!(f, "ld e, {}", self.u8()),
        0x1F => write!(f, "rra"),
        //
        0x20 => write!(f, "jr nz, {:+}", self.i8()),
        0x21 => write!(f, "ld hl, ${:04X}", self.u16()),
        0x22 => write!(f, "ld [hl+] a"),
        0x23 => write!(f, "inc hl"),
        0x24 => write!(f, "inc h"),
        0x25 => write!(f, "dec h"),
        0x26 => write!(f, "ld h, {}", self.u8()),
        0x27 => write!(f, "daa"),
        0x28 => write!(f, "jr z, {:+}", self.i8()),
        0x29 => write!(f, "add hl, hl"),
        0x2A => write!(f, "ld a, [hl+]"),
        0x2B => write!(f, "dec hl"),
        0x2C => write!(f, "inc l"),
        0x2D => write!(f, "dec l"),
        0x2E => write!(f, "ld l, {}", self.u8()),
        0x2F => write!(f, "cpl"),
        //
        0x30 => write!(f, "jr nc, {:+}", self.i8()),
        0x31 => write!(f, "ld sp, ${:04X}", self.u16()),
        0x32 => write!(f, "ld [hl-] a"),
        0x33 => write!(f, "inc sp"),
        0x34 => write!(f, "inc [hl]"),
        0x35 => write!(f, "dec [hl]"),
        0x36 => write!(f, "ld [hl], {}", self.u8()),
        0x37 => write!(f, "scf"),
        0x38 => write!(f, "jr c, {:+}", self.i8()),
        0x39 => write!(f, "add hl, sp"),
        0x3A => write!(f, "ld a, [hl-]"),
        0x3B => write!(f, "dec sp"),
        0x3C => write!(f, "inc a"),
        0x3D => write!(f, "dec a"),
        0x3E => write!(f, "ld a, {}", self.u8()),
        0x3F => write!(f, "ccf"),
        //
        0x40 => write!(f, "ld b, b"),
        0x41 => write!(f, "ld b, c"),
        0x42 => write!(f, "ld b, d"),
        0x43 => write!(f, "ld b, e"),
        0x44 => write!(f, "ld b, h"),
        0x45 => write!(f, "ld b, l"),
        0x46 => write!(f, "ld b, [hl]"),
        0x47 => write!(f, "ld b, a"),
        0x48 => write!(f, "ld c, b"),
        0x49 => write!(f, "ld c, c"),
        0x4A => write!(f, "ld c, d"),
        0x4B => write!(f, "ld c, e"),
        0x4C => write!(f, "ld c, h"),
        0x4D => write!(f, "ld c, l"),
        0x4E => write!(f, "ld c, [hl]"),
        0x4F => write!(f, "ld c, a"),
        //
        0x50 => write!(f, "ld d, b"),
        0x51 => write!(f, "ld d, c"),
        0x52 => write!(f, "ld d, d"),
        0x53 => write!(f, "ld d, e"),
        0x54 => write!(f, "ld d, h"),
        0x55 => write!(f, "ld d, l"),
        0x56 => write!(f, "ld d, [hl]"),
        0x57 => write!(f, "ld d, a"),
        0x58 => write!(f, "ld e, b"),
        0x59 => write!(f, "ld e, c"),
        0x5A => write!(f, "ld e, d"),
        0x5B => write!(f, "ld e, e"),
        0x5C => write!(f, "ld e, h"),
        0x5D => write!(f, "ld e, l"),
        0x5E => write!(f, "ld e, [hl]"),
        0x5F => write!(f, "ld e, a"),
        //
        0x60 => write!(f, "ld h, b"),
        0x61 => write!(f, "ld h, c"),
        0x62 => write!(f, "ld h, d"),
        0x63 => write!(f, "ld h, e"),
        0x64 => write!(f, "ld h, h"),
        0x65 => write!(f, "ld h, l"),
        0x66 => write!(f, "ld h, [hl]"),
        0x67 => write!(f, "ld h, a"),
        0x68 => write!(f, "ld l, b"),
        0x69 => write!(f, "ld l, c"),
        0x6A => write!(f, "ld l, d"),
        0x6B => write!(f, "ld l, e"),
        0x6C => write!(f, "ld l, h"),
        0x6D => write!(f, "ld l, l"),
        0x6E => write!(f, "ld l, [hl]"),
        0x6F => write!(f, "ld l, a"),
        //
        0x70 => write!(f, "ld [hl], b"),
        0x71 => write!(f, "ld [hl], c"),
        0x72 => write!(f, "ld [hl], d"),
        0x73 => write!(f, "ld [hl], e"),
        0x74 => write!(f, "ld [hl], h"),
        0x75 => write!(f, "ld [hl], l"),
        0x76 => write!(f, "halt"),
        0x77 => write!(f, "ld [hl], a"),
        0x78 => write!(f, "ld a, b"),
        0x79 => write!(f, "ld a, c"),
        0x7A => write!(f, "ld a, d"),
        0x7B => write!(f, "ld a, e"),
        0x7C => write!(f, "ld a, h"),
        0x7D => write!(f, "ld a, l"),
        0x7E => write!(f, "ld a, [hl]"),
        0x7F => write!(f, "ld a, a"),
        //
        0x80 => write!(f, "add a, b"),
        0x81 => write!(f, "add a, c"),
        0x82 => write!(f, "add a, d"),
        0x83 => write!(f, "add a, e"),
        0x84 => write!(f, "add a, h"),
        0x85 => write!(f, "add a, l"),
        0x86 => write!(f, "add a, [hl]"),
        0x87 => write!(f, "add a, a"),
        0x88 => write!(f, "adc a, b"),
        0x89 => write!(f, "adc a, c"),
        0x8A => write!(f, "adc a, d"),
        0x8B => write!(f, "adc a, e"),
        0x8C => write!(f, "adc a, h"),
        0x8D => write!(f, "adc a, l"),
        0x8E => write!(f, "adc a, [hl]"),
        0x8F => write!(f, "adc a, a"),
        //
        0x90 => write!(f, "sub a, b"),
        0x91 => write!(f, "sub a, c"),
        0x92 => write!(f, "sub a, d"),
        0x93 => write!(f, "sub a, e"),
        0x94 => write!(f, "sub a, h"),
        0x95 => write!(f, "sub a, l"),
        0x96 => write!(f, "sub a, [hl]"),
        0x97 => write!(f, "sub a, a"),
        0x98 => write!(f, "sbc a, b"),
        0x99 => write!(f, "sbc a, c"),
        0x9A => write!(f, "sbc a, d"),
        0x9B => write!(f, "sbc a, e"),
        0x9C => write!(f, "sbc a, h"),
        0x9D => write!(f, "sbc a, l"),
        0x9E => write!(f, "sbc a, [hl]"),
        0x9F => write!(f, "sbc a, a"),
        //
        0xA0 => write!(f, "and a, b"),
        0xA1 => write!(f, "and a, c"),
        0xA2 => write!(f, "and a, d"),
        0xA3 => write!(f, "and a, e"),
        0xA4 => write!(f, "and a, h"),
        0xA5 => write!(f, "and a, l"),
        0xA6 => write!(f, "and a, [hl]"),
        0xA7 => write!(f, "and a, a"),
        0xA8 => write!(f, "xor a, b"),
        0xA9 => write!(f, "xor a, c"),
        0xAA => write!(f, "xor a, d"),
        0xAB => write!(f, "xor a, e"),
        0xAC => write!(f, "xor a, h"),
        0xAD => write!(f, "xor a, l"),
        0xAE => write!(f, "xor a, [hl]"),
        0xAF => write!(f, "xor a, a"),
        //
        0xB0 => write!(f, "or a, b"),
        0xB1 => write!(f, "or a, c"),
        0xB2 => write!(f, "or a, d"),
        0xB3 => write!(f, "or a, e"),
        0xB4 => write!(f, "or a, h"),
        0xB5 => write!(f, "or a, l"),
        0xB6 => write!(f, "or a, [hl]"),
        0xB7 => write!(f, "or a, a"),
        0xB8 => write!(f, "cp a, b"),
        0xB9 => write!(f, "cp a, c"),
        0xBA => write!(f, "cp a, d"),
        0xBB => write!(f, "cp a, e"),
        0xBC => write!(f, "cp a, h"),
        0xBD => write!(f, "cp a, l"),
        0xBE => write!(f, "cp a, [hl]"),
        0xBF => write!(f, "cp a, a"),
        //
        0xC0 => write!(f, "ret nz"),
        0xC1 => write!(f, "pop bc"),
        0xC2 => write!(f, "jp nz, ${:04X}", self.u16()),
        0xC3 => write!(f, "jp ${:04X}", self.u16()),
        0xC4 => write!(f, "call nz, ${:04X}", self.u16()),
        0xC5 => write!(f, "push bc"),
        0xC6 => write!(f, "add a, {}", self.u8()),
        0xC7 => write!(f, "rst $00"),
        0xC8 => write!(f, "ret z"),
        0xC9 => write!(f, "ret"),
        0xCA => write!(f, "jp z, ${:04X}", self.u16()),
        0xCB => write!(f, "{}", CB_MNEMONICS[usize::from(self.u8())]),
        0xCC => write!(f, "call z, ${:04X}", self.u16()),
        0xCD => write!(f, "call ${:04X}", self.u16()),
        0xCE => write!(f, "adc a, {}", self.u8()),
        0xCF => write!(f, "rst $08"),
        //
        0xD0 => write!(f, "ret nc"),
        0xD1 => write!(f, "pop de"),
        0xD2 => write!(f, "jp nc, ${:04X}", self.u16()),
        0xD3 => write!(f, "raw_bytes!($D3)"),
        0xD4 => write!(f, "call nc, ${:04X}", self.u16()),
        0xD5 => write!(f, "push de"),
        0xD6 => write!(f, "sub a, {}", self.u8()),
        0xD7 => write!(f, "rst $10"),
        0xD8 => write!(f, "ret c"),
        0xD9 => write!(f, "reti"),
        0xDA => write!(f, "jp c, ${:04X}", self.u16()),
        0xDB => write!(f, "raw_bytes!($DB)"),
        0xDC => write!(f, "call c, ${:04X}", self.u16()),
        0xDD => write!(f, "raw_bytes!($DD)"),
        0xDE => write!(f, "sbc a, {}", self.u8()),
        0xDF => write!(f, "rst $18"),
        //
        0xE0 => write!(f, "ldh [$FF{:02X}], a", self.u8()),
        0xE1 => write!(f, "pop hl"),
        0xE2 => write!(f, "ldh [c], a"),
        0xE3 => write!(f, "raw_bytes!($E3)"),
        0xE4 => write!(f, "raw_bytes!($E4)"),
        0xE5 => write!(f, "push hl"),
        0xE6 => write!(f, "and a, {}", self.u8()),
        0xE7 => write!(f, "rst $20"),
        0xE8 => write!(f, "add sp, {}", self.i8()),
        0xE9 => write!(f, "jp hl"),
        0xEA => write!(f, "ld [${:04X}], a", self.u16()),
        0xEB => write!(f, "raw_bytes!($EB)"),
        0xEC => write!(f, "raw_bytes!($EC)"),
        0xED => write!(f, "raw_bytes!($ED)"),
        0xEE => write!(f, "xor a, {}", self.u8()),
        0xEF => write!(f, "rst $28"),
        //
        0xF0 => write!(f, "ldh a, [$FF{:02X}]", self.u8()),
        0xF1 => write!(f, "pop af"),
        0xF2 => write!(f, "ldh a, [c]"),
        0xF3 => write!(f, "di"),
        0xF4 => write!(f, "raw_bytes!($F4)"),
        0xF5 => write!(f, "push af"),
        0xF6 => write!(f, "or a, {}", self.u8()),
        0xF7 => write!(f, "rst $30"),
        0xF8 => write!(f, "ld hl, sp{:+}", self.i8()),
        0xF9 => write!(f, "ld sp, hl"),
        0xFA => write!(f, "ld a, [${:04X}]", self.u16()),
        0xFB => write!(f, "ei"),
        0xFC => write!(f, "raw_bytes!($FC)"),
        0xFD => write!(f, "raw_bytes!($FD)"),
        0xFE => write!(f, "cp a, {}", self.u8()),
        0xFF => write!(f, "rst $38"),
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
  pub fn size(self) -> usize {
    match self {
      OpToken::One(_) => 1,
      OpToken::Two(_, _) => 2,
      OpToken::Three(_, _) => 3,
      OpToken::WantedTwoGotOne(_) => 1,
      OpToken::WantedThreeGotOne(_) => 1,
      OpToken::WantedThreeGotTwo(_, _) => 3,
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

static CB_MNEMONICS: [&str; 256] = [
  "rlc b",       /* 0x00 */
  "rlc c",       /* 0x01 */
  "rlc d",       /* 0x02 */
  "rlc e",       /* 0x03 */
  "rlc h",       /* 0x04 */
  "rlc l",       /* 0x05 */
  "rlc [hl]",    /* 0x06 */
  "rlc a",       /* 0x07 */
  "rrc b",       /* 0x08 */
  "rrc c",       /* 0x09 */
  "rrc d",       /* 0x0A */
  "rrc e",       /* 0x0B */
  "rrc h",       /* 0x0C */
  "rrc l",       /* 0x0D */
  "rrc [hl]",    /* 0x0E */
  "rrc a",       /* 0x0F */
  "rl b",        /* 0x10 */
  "rl c",        /* 0x11 */
  "rl d",        /* 0x12 */
  "rl e",        /* 0x13 */
  "rl h",        /* 0x14 */
  "rl l",        /* 0x15 */
  "rl [hl]",     /* 0x16 */
  "rl a",        /* 0x17 */
  "rr b",        /* 0x18 */
  "rr c",        /* 0x19 */
  "rr d",        /* 0x1A */
  "rr e",        /* 0x1B */
  "rr h",        /* 0x1C */
  "rr l",        /* 0x1D */
  "rr [hl]",     /* 0x1E */
  "rr a",        /* 0x1F */
  "sla b",       /* 0x20 */
  "sla c",       /* 0x21 */
  "sla d",       /* 0x22 */
  "sla e",       /* 0x23 */
  "sla h",       /* 0x24 */
  "sla l",       /* 0x25 */
  "sla [hl]",    /* 0x26 */
  "sla a",       /* 0x27 */
  "sra b",       /* 0x28 */
  "sra c",       /* 0x29 */
  "sra d",       /* 0x2A */
  "sra e",       /* 0x2B */
  "sra h",       /* 0x2C */
  "sra l",       /* 0x2D */
  "sra [hl]",    /* 0x2E */
  "sra a",       /* 0x2F */
  "swap b",      /* 0x30 */
  "swap c",      /* 0x31 */
  "swap d",      /* 0x32 */
  "swap e",      /* 0x33 */
  "swap h",      /* 0x34 */
  "swap l",      /* 0x35 */
  "swap [hl]",   /* 0x36 */
  "swap a",      /* 0x37 */
  "srl b",       /* 0x38 */
  "srl c",       /* 0x39 */
  "srl d",       /* 0x3A */
  "srl e",       /* 0x3B */
  "srl h",       /* 0x3C */
  "srl l",       /* 0x3D */
  "srl [hl]",    /* 0x3E */
  "srl a",       /* 0x3F */
  "bit 0, b",    /* 0x40 */
  "bit 0, c",    /* 0x41 */
  "bit 0, d",    /* 0x42 */
  "bit 0, e",    /* 0x43 */
  "bit 0, h",    /* 0x44 */
  "bit 0, l",    /* 0x45 */
  "bit 0, [hl]", /* 0x46 */
  "bit 0, a",    /* 0x47 */
  "bit 1, b",    /* 0x48 */
  "bit 1, c",    /* 0x49 */
  "bit 1, d",    /* 0x4A */
  "bit 1, e",    /* 0x4B */
  "bit 1, h",    /* 0x4C */
  "bit 1, l",    /* 0x4D */
  "bit 1, [hl]", /* 0x4E */
  "bit 1, a",    /* 0x4F */
  "bit 2, b",    /* 0x50 */
  "bit 2, c",    /* 0x51 */
  "bit 2, d",    /* 0x52 */
  "bit 2, e",    /* 0x53 */
  "bit 2, h",    /* 0x54 */
  "bit 2, l",    /* 0x55 */
  "bit 2, [hl]", /* 0x56 */
  "bit 2, a",    /* 0x57 */
  "bit 3, b",    /* 0x58 */
  "bit 3, c",    /* 0x59 */
  "bit 3, d",    /* 0x5A */
  "bit 3, e",    /* 0x5B */
  "bit 3, h",    /* 0x5C */
  "bit 3, l",    /* 0x5D */
  "bit 3, [hl]", /* 0x5E */
  "bit 3, a",    /* 0x5F */
  "bit 4, b",    /* 0x60 */
  "bit 4, c",    /* 0x61 */
  "bit 4, d",    /* 0x62 */
  "bit 4, e",    /* 0x63 */
  "bit 4, h",    /* 0x64 */
  "bit 4, l",    /* 0x65 */
  "bit 4, [hl]", /* 0x66 */
  "bit 4, a",    /* 0x67 */
  "bit 5, b",    /* 0x68 */
  "bit 5, c",    /* 0x69 */
  "bit 5, d",    /* 0x6A */
  "bit 5, e",    /* 0x6B */
  "bit 5, h",    /* 0x6C */
  "bit 5, l",    /* 0x6D */
  "bit 5, [hl]", /* 0x6E */
  "bit 5, a",    /* 0x6F */
  "bit 6, b",    /* 0x70 */
  "bit 6, c",    /* 0x71 */
  "bit 6, d",    /* 0x72 */
  "bit 6, e",    /* 0x73 */
  "bit 6, h",    /* 0x74 */
  "bit 6, l",    /* 0x75 */
  "bit 6, [hl]", /* 0x76 */
  "bit 6, a",    /* 0x77 */
  "bit 7, b",    /* 0x78 */
  "bit 7, c",    /* 0x79 */
  "bit 7, d",    /* 0x7A */
  "bit 7, e",    /* 0x7B */
  "bit 7, h",    /* 0x7C */
  "bit 7, l",    /* 0x7D */
  "bit 7, [hl]", /* 0x7E */
  "bit 7, a",    /* 0x7F */
  "res 0, b",    /* 0x80 */
  "res 0, c",    /* 0x81 */
  "res 0, d",    /* 0x82 */
  "res 0, e",    /* 0x83 */
  "res 0, h",    /* 0x84 */
  "res 0, l",    /* 0x85 */
  "res 0, [hl]", /* 0x86 */
  "res 0, a",    /* 0x87 */
  "res 1, b",    /* 0x88 */
  "res 1, c",    /* 0x89 */
  "res 1, d",    /* 0x8A */
  "res 1, e",    /* 0x8B */
  "res 1, h",    /* 0x8C */
  "res 1, l",    /* 0x8D */
  "res 1, [hl]", /* 0x8E */
  "res 1, a",    /* 0x8F */
  "res 2, b",    /* 0x90 */
  "res 2, c",    /* 0x91 */
  "res 2, d",    /* 0x92 */
  "res 2, e",    /* 0x93 */
  "res 2, h",    /* 0x94 */
  "res 2, l",    /* 0x95 */
  "res 2, [hl]", /* 0x96 */
  "res 2, a",    /* 0x97 */
  "res 3, b",    /* 0x98 */
  "res 3, c",    /* 0x99 */
  "res 3, d",    /* 0x9A */
  "res 3, e",    /* 0x9B */
  "res 3, h",    /* 0x9C */
  "res 3, l",    /* 0x9D */
  "res 3, [hl]", /* 0x9E */
  "res 3, a",    /* 0x9F */
  "res 4, b",    /* 0xA0 */
  "res 4, c",    /* 0xA1 */
  "res 4, d",    /* 0xA2 */
  "res 4, e",    /* 0xA3 */
  "res 4, h",    /* 0xA4 */
  "res 4, l",    /* 0xA5 */
  "res 4, [hl]", /* 0xA6 */
  "res 4, a",    /* 0xA7 */
  "res 5, b",    /* 0xA8 */
  "res 5, c",    /* 0xA9 */
  "res 5, d",    /* 0xAA */
  "res 5, e",    /* 0xAB */
  "res 5, h",    /* 0xAC */
  "res 5, l",    /* 0xAD */
  "res 5, [hl]", /* 0xAE */
  "res 5, a",    /* 0xAF */
  "res 6, b",    /* 0xB0 */
  "res 6, c",    /* 0xB1 */
  "res 6, d",    /* 0xB2 */
  "res 6, e",    /* 0xB3 */
  "res 6, h",    /* 0xB4 */
  "res 6, l",    /* 0xB5 */
  "res 6, [hl]", /* 0xB6 */
  "res 6, a",    /* 0xB7 */
  "res 7, b",    /* 0xB8 */
  "res 7, c",    /* 0xB9 */
  "res 7, d",    /* 0xBA */
  "res 7, e",    /* 0xBB */
  "res 7, h",    /* 0xBC */
  "res 7, l",    /* 0xBD */
  "res 7, [hl]", /* 0xBE */
  "res 7, a",    /* 0xBF */
  "set 0, b",    /* 0xC0 */
  "set 0, c",    /* 0xC1 */
  "set 0, d",    /* 0xC2 */
  "set 0, e",    /* 0xC3 */
  "set 0, h",    /* 0xC4 */
  "set 0, l",    /* 0xC5 */
  "set 0, [hl]", /* 0xC6 */
  "set 0, a",    /* 0xC7 */
  "set 1, b",    /* 0xC8 */
  "set 1, c",    /* 0xC9 */
  "set 1, d",    /* 0xCA */
  "set 1, e",    /* 0xCB */
  "set 1, h",    /* 0xCC */
  "set 1, l",    /* 0xCD */
  "set 1, [hl]", /* 0xCE */
  "set 1, a",    /* 0xCF */
  "set 2, b",    /* 0xD0 */
  "set 2, c",    /* 0xD1 */
  "set 2, d",    /* 0xD2 */
  "set 2, e",    /* 0xD3 */
  "set 2, h",    /* 0xD4 */
  "set 2, l",    /* 0xD5 */
  "set 2, [hl]", /* 0xD6 */
  "set 2, a",    /* 0xD7 */
  "set 3, b",    /* 0xD8 */
  "set 3, c",    /* 0xD9 */
  "set 3, d",    /* 0xDA */
  "set 3, e",    /* 0xDB */
  "set 3, h",    /* 0xDC */
  "set 3, l",    /* 0xDD */
  "set 3, [hl]", /* 0xDE */
  "set 3, a",    /* 0xDF */
  "set 4, b",    /* 0xE0 */
  "set 4, c",    /* 0xE1 */
  "set 4, d",    /* 0xE2 */
  "set 4, e",    /* 0xE3 */
  "set 4, h",    /* 0xE4 */
  "set 4, l",    /* 0xE5 */
  "set 4, [hl]", /* 0xE6 */
  "set 4, a",    /* 0xE7 */
  "set 5, b",    /* 0xE8 */
  "set 5, c",    /* 0xE9 */
  "set 5, d",    /* 0xEA */
  "set 5, e",    /* 0xEB */
  "set 5, h",    /* 0xEC */
  "set 5, l",    /* 0xED */
  "set 5, [hl]", /* 0xEE */
  "set 5, a",    /* 0xEF */
  "set 6, b",    /* 0xF0 */
  "set 6, c",    /* 0xF1 */
  "set 6, d",    /* 0xF2 */
  "set 6, e",    /* 0xF3 */
  "set 6, h",    /* 0xF4 */
  "set 6, l",    /* 0xF5 */
  "set 6, [hl]", /* 0xF6 */
  "set 6, a",    /* 0xF7 */
  "set 7, b",    /* 0xF8 */
  "set 7, c",    /* 0xF9 */
  "set 7, d",    /* 0xFA */
  "set 7, e",    /* 0xFB */
  "set 7, h",    /* 0xFC */
  "set 7, l",    /* 0xFD */
  "set 7, [hl]", /* 0xFE */
  "set 7, a",    /* 0xFF */
];
