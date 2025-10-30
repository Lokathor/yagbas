INCLUDE "hardware.inc"

SECTION "Header", ROM0[$0100]
	jp Main
	ds $150 - @, 0

SECTION "Main", ROM0
Main:
	ld bc, $2000
	ld hl, $8000
.clearVRAM
	ldh a, [rSTAT]
	and STATF_BUSY
	jr nz, .clearVRAM

	xor a, a
	ld [hli], a
	dec c
	jr nz, .clearVRAM
	dec b
	jr nz, .clearVRAM

	ld bc, SIZEOF("Font")
	ld de, $8000 + " " * 16
	ld hl, Font
	call VRAMCopy

	ld bc, Title.end - Title
	ld de, $9800
	ld hl, Title
	call VRAMCopy

	ld a, $0A
	ld [rRAMG], a

	FOR I, 15, -1, -1
		ld a, I
		call WriteBank
	ENDR

	FOR I, 16
		ld a, I
		call VerifyBank
		push af
		ld bc, BankString.end - BankString
		ld de, $9820 + SCRN_VX_B * I
		ld hl, BankString
		call VRAMCopy
		IF I > 9
			ld a, I - 10 + "A"
		ELSE
			ld a, I + "0"
		ENDC
		ld [$9820 + SCRN_VX_B * I + BankString.end - BankString + 1], a
		pop af
		jr nz, .fail\@
		ld bc, PassString.end - PassString
		ld de, $9820 + SCRN_VX_B * I + BankString.end - BankString + 3
		ld hl, PassString
		call VRAMCopy
		jr .next\@
	.fail\@
		ld bc, FailString.end - FailString
		ld de, $9820 + SCRN_VX_B * I + BankString.end - BankString + 3
		ld hl, FailString
		call VRAMCopy
	.next\@
	ENDR

	xor a, a
	ldh [rIE], a
	ei
	halt

SECTION "VRAM copy", ROM0
VRAMCopy::
	dec bc
	inc b
	inc c
.loop:
	ldh a, [rSTAT]
	and STATF_BUSY
	jr nz, .loop

	ld a, [hli]
	ld [de], a
	inc de
	dec c
	jr nz, .loop
	dec b
	jr nz, .loop
	ret

SECTION "Write Bank", ROM0
; a: bank to set
WriteBank:
	ld [rRAMB], a
	ld bc, $2000
	ld hl, $A000
.loop
	ld [hli], a
	inc a
	dec c
	jr nz, .loop
	dec b
	jr nz, .loop
	ret

SECTION "Verify Bank", ROM0
; a: bank to check
; Returns nz on failure.
VerifyBank:
	ld [rRAMB], a
	ld bc, $2000
	ld hl, $A000
.loop
	cp a, [hl]
	ret nz
	inc a
	inc hl
	dec c
	jr nz, .loop
	dec b
	jr nz, .loop
	ret

SECTION "Font", ROM0
Font:
	INCBIN "bin/font.2bpp"

SECTION "Strings", ROM0
Title: db "128KiB SRAM test ROM"
.end
BankString: db "Bank"
.end
PassString: db "Passed!"
.end
FailString: db "FAILED!"
.end
