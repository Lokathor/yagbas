# Instructions

## Special Terms

The following terms are used for different groups of possible values

* Place8: `b`, `c`, `d`, `e`, `h`, `l`, `[hl]`, `a`
* Place16: `bc`, `de`, `hl`, `sp`
* PlaceIndirect: `[bc]`, `[de]`, `[hl++]`, `[hl--]`
* StackPair: `af`, `bc`, `de`, `hl`
* Cond (condition)
  * `cf`: carry-flag
  * `nc`: no-carry
  * `zf`: zero-flag
  * `nz`: not-zero
  * `al`: "always"

The "always" condition can be omitted from the source code. For example, writing `jp u16` will be interpreted as `jp al, u16`. This is generally how you're expected to do it. Writing the "always" condition into the source only handled for consistency.

## No Idents

These instruction forms can be resolved fully on their own, without the need to resolve any identifiers.

```
adc a, Place8
add a, Place8
add hl, Place16
and a, Place8
ccf
cp a, Place8
cpl
daa
di
ei
halt
dec Place8
dec Place16
inc Place8
inc Place16
jp hl
ld Place8, Place8 /* but not [hl] to [hl] */
ld PlaceIndirect, a
ld a, PlaceIndirect
ld sp, hl
ld [c], a
ld a, [c]
nop
or a, Place8
pop stackPair
push stackPair
ret cond
reti
rl Place8
rla
rlc Place8
rlca
rr Place8
rra
rrc Place8
rrca
sbc a, Place8
scf
sla Place8
sra Place8
srl Place8
stop
sub a, Place8
swap Place8
xor a, Place8
```

## Can Use Idents

The following forms can use `u3`, `u8`, `i8`, or `u16` values.
* `u3`, `u8`, and `u16` must evaluate to an unsigned 3, 8, or 16 bit value.
* `i8` must evaluate to a signed 8 bit value.

The source of the value can be
* a raw const expression
* a named const expression
* a label (including the name of a section or static), which causes the address of the label to be used as the value

The `jr` instruction is a "relative" jump. When a label is used, the distance (in bytes) between the *end* of the `jr` instruction and the labeled position is what becomes the `i8` value inserted. Also, `jr` to a label must refer to a label in the same section as the `jr` instruction itself (it is possible, but unlikely, that this restriction will be removed in the future).

The `ldh` instruction must be given a value where the high byte is `$FF`. Something in the range `$FF00` through `$FFFF`. Only the low byte of this value is actually included in the instruction encoding, and the high byte being `$FF` is implicit.

The `rst` instruction can only be use with one of these specific eight constant values (or something that evaluates to one of these values):
* RstValue: $0, $8, $10, $18, $20, $28, $30, $38

```
adc a, u8
add a, u8
add sp, i8
and a, u8
bit u3, Place8
call Cond, u16
cp a, u8
jp Cond, u16
jr Cond, i8
ld [u16], a
ld [u16], sp
ld Place8, u8
ld a, [u16]
ld Place16, u16
ld hl, sp + i8
ld sp, u16
ldh [u8], a
ldh a, [u8]
or a, u8
res u3, Place8
rst RstValue
sbc a, u8
set u3, Place8
sub a, u8
xor a, u8
```
