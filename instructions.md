# Instructions

## No Idents

The following instruction forms can be resolved fully without needing to resolve an ident.

```
adc a, place8
add a, place8
add hl, place16
and a, place8
ccf
cp a, place8
cpl
daa
di
ei
halt
dec place8
dec place16
inc place8
inc place16
jp hl
ld placeIndirect, a
ld place8, place8 /* but not [hl] to [hl] */
ld a, placeIndirect
ld sp, hl
ld [c], a
ld a, [c]
nop
or a, place8
pop stackPair
push stackPair
ret cond
reti
rl place8
rla
rlc place8
rlca
rr place8
rra
rrc place8
rrca
sbc a, place8
scf
sla place8
sra place8
srl place8
stop
sub a, place8
swap place8
xor a, place8
```

## Can Use Idents

These forms *can* use an ident in place of a value literal.
The ident can be:
* A declared constant
* A declared section
* A declared static
* A label

```
adc a, u8
add a, u8
add sp, i8
and a, u8
bit u3, place8
call cond, a16
cp a, u8
jp cond, a16
jr cond, i8
ld [a16], a
ld [a16], sp
ld place8, u8
ld a, [a16]
ld place16, u16
ld hl, sp + i8
ld sp, u16
ldh [a8], a
ldh a, [a8]
or a, u8
res u3, place8
rst {$0, $8, $10, $18, $20, $28, $30, $38}
sbc a, u8
set u3, place8
sub a, u8
xor a, u8
```
