lzss_decompress
; input:
;    hl : data to be decompressed
;    de : destination place
;
; output
;    data decompressed to de
;    bc destroyed

	ld	c,(hl)
	inc	hl
	ld	b,(hl)
	inc	hl
; put size of compressed data in bc

main_loop
    jp  renew_bitmap

inner_loop
    ld  a,(bitmap)
	srl	a    ;carry = 1 : coded, else not
    ld  (bitmap),a
	jp	c,coded_part
	ldi	; it's not coded, just copy it
    jp  after_copying
coded_part
; first 10 bits : offset (max 1024), last 6 bits : length (max: 64)

; first byte : 8 weakest bits of offset (OOOOOOOO)
; second byte: 6 bits of length and 2 strongest bits of offset ( LLLLLLOO)
; ->  OOOOOOOOLLLLLLOO

    ld  a,(hl)
    ld  (offset),a
    inc hl
    dec bc
    ld  a,(hl)
    and %00000011
    ld  (offset+1),a
    ld  a,(hl)
    srl a
    srl a
    inc a
    inc a
    inc a    ;   because length - 3 is stored
    ld  (length),a
    inc hl
    dec bc

;hl: orig        de: dest     bc : remaining length of data
    push    hl
    push    bc

    ld  bc,(offset)
    ld  h,d
    ld  l,e
    or  a   ; clear carry flag
    sbc hl,bc

    ld  b,0
    ld  a,(length)
    ld  c,a

    ldir

    pop bc   ; counter
    pop hl   ; orig adress

after_copying
	ld	a,b
	or	c
	jp	z,lzss_decompress_end

	ld a,(bit_count)
    dec	a
    ld (bit_count),a
	jp	nz,inner_loop

renew_bitmap
	ld	a,8
	ld	(bit_count),a
	ld	a,(hl)
	ld	(bitmap),a
	inc hl
    dec	bc
    ld  a,b
    or  c
    jp  z,lzss_decompress_end
	jp	inner_loop


lzss_decompress_end
    ret



bitmap
    db 0
bit_count
    db 0
offset   ; < 2048
    dw 0
length   ; < 63
    db 0