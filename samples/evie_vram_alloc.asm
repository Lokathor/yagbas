;
; This file is a part of the VuiBui Standard Library.
; The VuiBui standard library is an attempt at creating a collection of short,
; common functions that are universally useful to Game Boy programs.
;
; stdmem.asm
; Common memory operations like Copy and Set, as well as faster variations for
; blocks of data under 256 bytes.
;
; Copyright 2021 Eievui
;
; This software is provided 'as-is', without any express or implied
; warranty.  In no event will the authors be held liable for any damages
; arising from the use of this software.
;
; Permission is granted to anyone to use this software for any purpose,
; including commercial applications, and to alter it and redistribute it
; freely, subject to the following restrictions:
;
; 1. The origin of this software must not be misrepresented; you must not
;    claim that you wrote the original software. If you use this software
;    in a product, an acknowledgment in the product documentation would be
;    appreciated but is not required.
; 2. Altered source versions must be plainly marked as such, and must not be
;    misrepresented as being the original software.
; 3. This notice may not be removed or altered from any source distribution.

SECTION "Memory Copy", ROM0

; Copies a certain amount of bytes from one location to another. Destination and
; source are both offset by length, in case you want to copy to or from multiple
; places.
; @ bc: length
; @ de: source
; @ hl: destination
MemCopy::
    dec bc
    inc b
    inc c
.loop:
    ld a, [de]
    ld [hli], a
    inc de
    dec c
    jr nz, .loop
    dec b
    jr nz, .loop
    ret

SECTION "Memory Set", ROM0

; Overwrites a certain amount of bytes with a single byte. Destination is offset
; by length, in case you want to overwrite with different values.
; @ a:  source (is preserved)
; @ bc: length
; @ hl: destination
MemSet::
    inc b
    inc c
    jr .decCounter
.loadByte
    ld [hli],a
.decCounter
    dec c
    jr nz, .loadByte
    dec b
    jr nz, .loadByte
    ret

;
; valloc_lib.asm
; A library for allocating VRAM memory at runtime.
;
; Copyright 2021 Eievui
;
; This software is provided 'as-is', without any express or implied
; warranty.  In no event will the authors be held liable for any damages
; arising from the use of this software.
;
; Permission is granted to anyone to use this software for any purpose,
; including commercial applications, and to alter it and redistribute it
; freely, subject to the following restrictions:
;
; 1. The origin of this software must not be misrepresented; you must not
;    claim that you wrote the original software. If you use this software
;    in a product, an acknowledgment in the product documentation would be
;    appreciated but is not required.
; 2. Altered source versions must be plainly marked as such, and must not be
;    misrepresented as being the original software.
; 3. This notice may not be removed or altered from any source distribution.


; --- Config ---
; Double the valloc memory and allow allocation in the CGB's second bank.
DEF ENABLE_CGB EQU 0
; How many tiles does each block take up? 1 tile is 16 bytes.
DEF BLOCK_SIZE EQU 4

ASSERT BLOCK_SIZE <= 16, "Valloc block size must be less or equal to 16"
ASSERT LOG(BLOCK_SIZE * 1.0, 2.0) & $FFFF == 0, "Valloc block size must be a power of 2"

DEF BLOCK_EXPO EQU LOG(BLOCK_SIZE * 1.0, 2.0) / 1.0
DEF REGION_SIZE EQU (128 / BLOCK_SIZE) * (1 + ENABLE_CGB)

SECTION "Allocate Video Memory", ROM0
VallocPurge::
    xor a, a
    ld bc, REGION_SIZE * 3
    ld hl, wVallocBackground
    ASSERT wVallocBackground + REGION_SIZE == wVallocShared
    ASSERT wVallocShared + REGION_SIZE == wVallocObjects
    jp MemSet

; Allocate a block of memory in VRAM.
; @ c:  Number of tiles to allocate.
; @ hl: Which tile type to allocate, either wVallocBackground, wVallocObjects,
;       or wValloxShared
; @ Returns the index in b.
; @ Returns 0 in the a register upon success, and a non-zero value on failure.
Valloc:
    ld b, 0 ; Tile Index.
    ; Find an available memory block.
.findBlock
    ld a, [hli]
    and a, a
    jr z, .foundBlock
    ; Skip reserved blocks (They appear free, so must be ignored!)
    ; a already contains the block's size, so muliply it by BLOCK_SIZE and add it
    ; to b
    dec hl
    ld d, 0
    ld e, a
    add hl, de
    ; And skip those indices.
    ;ld a, e (a already equals e)
    add a, b
    ld b, a
    ; If the tile index has overflowed, valloc should fail.
    cp a, 128 / BLOCK_SIZE
    jr c, .findBlock
.fail
    ret

.foundBlock
    ; Fail if there isn't enough room left here.
    ld a, b
    add a, c
    cp a, 128 / BLOCK_SIZE + 1
    jr nc, .fail
    ld d, c
.verifySize
    dec d
    jr z, .success
    ; Check if the following blocks are already reserved.
    ld a, [hli]
    and a, a
    jr nz, .findBlock
    jr .verifySize
.success
    ; Seek back the the first block found and reserve it.
    ld a, l
    sub a, c
    ld l, a
    ld a, h
    sbc a, 0
    ld h, a
    ; Set the reserved blocks.
    ld a, c
    ld [hl], a
    ; Abjust b to be an index rather than a block ID.
    ld a, b
    REPT BLOCK_EXPO
        add a, a
    ENDR
    ld b, a
    xor a, a ; a == 0 == success!
    ret

SECTION "Valloc Shared", ROM0
; Allocate a block of memory in VRAM which is usable as either background or
; object tiles. This will overflow into the shared area if needed.
; @ c:  Number of tiles to allocate.
; @ hl: Which tile type to allocate, either wVallocBackground or wVallocObjects.
; @ Returns the address of tile in de and the index in b.
VallocShared::
    push bc
    call Valloc
.hook
    and a, a ; zero means success!
    jr nz, .fail
    pop de
    ret
.fail
    pop bc
    ld hl, wVallocShared
    call Valloc
    add a, 128
    ld b, a
    ret

SECTION "Valloc Free", ROM0
; Free a previously allocated block of memory.
; @ a:  Tile index
; @ hl: Tile type, either wVallocBackground or wVallocObjects.
VallocFree::
    REPT BLOCK_EXPO
        rra
    ENDR
    IF BLOCK_EXPO
        and a, $7F >> BLOCK_EXPO
    ENDC
    cp a, 128 / BLOCK_SIZE
    jr c, .notShared
    ld hl, wVallocShared - REGION_SIZE
.notShared

    add a, l
    ld l, a
    adc a, h
    sub a, l
    ld h, a
    ld [hl], 0
    ret

SECTION "Get Index Address", ROM0
; Offsets a VRAM address by a tile index, switching to the shared section if
; needed.
; @ a:  Tile index.
; @ hl: Which area of VRAM is expected, either $8000 (Objects) or $9000
;       (Background)
VallocGetAddr::
    cp a, 128
    jr c, .notShared
    ld hl, $8000
.notShared

    add a, l
    ld l, a
    adc a, h
    sub a, l
    ld h, a
    ret

SECTION "Valloc Usage Map", WRAM0

/*
Each block contains a single size byte which is used to search
for open blocks in memory. A 0 means that the given block is free,
and subsequent blocks can be checked for enough available space.
*/

wVallocBackground::
    ds REGION_SIZE
wVallocShared::
    ds REGION_SIZE
wVallocObjects::
    ds REGION_SIZE

INCLUDE "hardware.inc"

SECTION "entry", ROM0[$100]
    jp Start
    DS $150 - @, 0

SECTION "main", ROM0
Start:
    ldh a, [rLY]
    cp a, SCRN_Y
    jr c, Start

    xor a, a
    ldh [rLCDC], a
    ld a, %11100100
    ldh [rBGP], a

    ; Before calling Valloc functions, you must call VallocPurge to prepare
    ; WRAM.
    call VallocPurge

    ; Now allocate a buncha VRAM...
    ; We'll loop 48 times, allocating 96 blocks.
    ld b, 48
.fillVRAM
        push bc
        ; This tile should be displayable on the background.
        ld hl, wVallocBackground
        ; Allocate one block. By default, this is 4 tiles.
        ld c, 1
        ; This will overflow into the shared area when needed.
        call VallocShared
        ; This tile should be displayable as an object.
        ld hl, wVallocObjects
        ld c, 1
        call VallocShared
        pop bc
        dec b
        jr nz, .fillVRAM

    ; ...and purge VRAM, effectively freeing every block!
    ; This is useful when closing a menu or unloading a map, and means you can
    ; forget about tile indices without worrying about a memory leak.
    call VallocPurge

    ; Valloc and VallocShared return the index of the tile they allocated.
    ld hl, wVallocBackground
    ; We'll allocate 24 blocks this time, which is 96 tiles.
    ld c, 24
    call VallocShared
    ; We need to store the resulting tile ID so that we can use it later.
    ld a, b ; The result is returned in the b register.
    ld [wLuvuiTileID], a

    ; Let's get the address of our tile so that we can load graphics to it!
    ; VallocGetAddr expects a tile ID in the a register, so we can just tell it
    ; which area of memory we expect ($8000 for background, $9000 for objects)
    ; and it'll give us the address.
    ld hl, $9000
    call VallocGetAddr
    ; Now we can copy a spritesheet into VRAM!
    ld de, LuvuiGraphics
    ld bc, LuvuiGraphics.end - LuvuiGraphics
    call MemCopy

    ; I also want a blank tile, to fill up the parts of the screen I'm not
    ; using.
    ld hl, wVallocBackground
    ; Here I'm allocating 1 block, which is 4 tiles, even though I only need
    ; one. While being able to allocate single tiles would be useful, it would
    ; also be a bit slower and use much more RAM. For this reason, the default
    ; block size is 4 tiles, but you can change this in valloc_lib.asm to fit
    ; your needs.
    ld c, 1
    call VallocShared
    ld a, b
    ld [wBlankTileID], a

    ld hl, $9000
    call VallocGetAddr
    xor a, a
    ld bc, 16
    call MemSet ; Make sure the tile is clear

    ; Finally, let's draw our graphics on the screen!
    ; First, clear the screen...
    ld a, [wBlankTileID]
    ld hl, $9800
    ld bc, 32 * 32
    call MemSet

    ; Then draw Luvui in the corner, facing towards us!
    ld a, [wLuvuiTileID]
    add a, 34
    ld [$9821], a
    inc a
    ld [$9822], a
    add a, 7
    ld [$9841], a
    inc a
    ld [$9842], a

    ld a, LCDCF_BGON | LCDCF_ON
    ld [rLCDC], a

.loop
    ei
    halt
    jr .loop

LuvuiGraphics:
    INCBIN "res/luvui.2bpp"
.end

SECTION "Tile IDs", WRAM0
wLuvuiTileID:
    db
wBlankTileID:
    db

