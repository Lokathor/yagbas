; Define a new section and hard-code it to be at $0040.
SECTION "VBlank Interrupt", ROM0[$0040]
VBlankInterrupt:
	; This instruction is equivalent to `ret` and `ei`
	reti

SECTION "Init", ROM0
Init:
	; Place the following somewhere in your initiallization code:
	; hardware.inc defines a handy flag that we can use.
	ld a, IEF_VBLANK
	ldh [rIE], a
	; ...

	xor a, a ; This is equivalent to `ld a, 0`!
	ldh [rIF], a
	ei

.endlessLoop
	; Make sure to use `ldh` for HRAM and registers, and not a regular `ld`
	ldh a, [hFrameCounter]
	inc a
	ldh [hFrameCounter], a
	halt
	jr .endlessLoop

SECTION "Frame Counter", HRAM
hFrameCounter:
	db

SECTION "VBlank Interrupt", ROM0[$0040]
VBlankInterrupt:
	push af
	push bc
	push de
	push hl
	jp VBlankHandler

SECTION "VBlank Handler", ROM0
VBlankHandler:

	; Begin by loading the frame counter
	ldh a, [hFrameCounter]

	; Now check the 5th bit, causing it to set the zero flag for 32 frames,
	; every 32 frames. (about half a second on and off)
	bit 5, a

	; Now we're going to load a standard palette into `a`, but if the zero
	; flag is set we'll complement it, inverting every color.
	ld a, %11100100

	jr z, .skipCpl
	cpl ; ComPleMent `a`. Flips every bit in `a`
.skipCpl

	; Finally, load `a` into `rBGP`, the Game Boy's Background Palette register.
	ldh [rBGP], a


	; Now we just have to `pop` those registers and return!
	pop hl
	pop de
	pop bc
	pop af
	reti