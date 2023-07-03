#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InterpretNumErr {
  IllegalHexDigit(char),
  IllegalBinDigit(char),
  IllegalDecDigit(char),
  Overflow,
}

pub fn interpret_num(mut num: &str) -> Result<i32, InterpretNumErr> {
  let mut is_neg = false;
  if let Some(s) = num.strip_prefix('-') {
    is_neg = true;
    num = s;
  };
  fn interpret_hex(num: &str) -> Result<i32, InterpretNumErr> {
    let mut out: i32 = 0;
    for ch in num.chars() {
      let x: i32 = match ch {
        '_' => continue,
        '0'..='9' => (ch as i32) - ('0' as i32),
        'a'..='f' => 10 + (ch as i32) - ('a' as i32),
        'A'..='F' => 10 + (ch as i32) - ('A' as i32),
        other => return Err(InterpretNumErr::IllegalHexDigit(other)),
      };
      out = match out.checked_mul(16).and_then(|out| out.checked_add(x)) {
        Some(new_out) => new_out,
        None => return Err(InterpretNumErr::Overflow),
      };
    }
    Ok(out)
  }
  fn interpret_bin(num: &str) -> Result<i32, InterpretNumErr> {
    let mut out: i32 = 0;
    for ch in num.chars() {
      let x: i32 = match ch {
        '_' => continue,
        '0'..='1' => (ch as i32) - ('0' as i32),
        other => return Err(InterpretNumErr::IllegalBinDigit(other)),
      };
      out = match out.checked_mul(2).and_then(|out| out.checked_add(x)) {
        Some(new_out) => new_out,
        None => return Err(InterpretNumErr::Overflow),
      };
    }
    Ok(out)
  }
  fn interpret_dec(num: &str) -> Result<i32, InterpretNumErr> {
    let mut out: i32 = 0;
    for ch in num.chars() {
      let x: i32 = match ch {
        '_' => continue,
        '0'..='9' => (ch as i32) - ('0' as i32),
        other => return Err(InterpretNumErr::IllegalDecDigit(other)),
      };
      out = match out.checked_mul(10).and_then(|out| out.checked_add(x)) {
        Some(new_out) => new_out,
        None => return Err(InterpretNumErr::Overflow),
      };
    }
    Ok(out)
  }
  let val: i32 = if let Some(s) = num.strip_prefix('$') {
    interpret_hex(s)?
  } else if let Some(s) = num.strip_prefix("0x") {
    interpret_hex(s)?
  } else if let Some(s) = num.strip_prefix('%') {
    interpret_bin(s)?
  } else if let Some(s) = num.strip_prefix("0b") {
    interpret_bin(s)?
  } else {
    interpret_dec(num)?
  };
  Ok(if is_neg { -val } else { val })
}
