GRAPH_MEM
gbuf
         ds #300
         ds #300

scrtable
        ds 4*256    ;4col systems use 2*256 (2bytes/8pixel), 16col systems use 4*256 (4bytes/8pixels)
        ds 4*256    ;inverted

vline_unrol
    ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de
    ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de
    ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de
    ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de
    ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de
    ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de
    ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de
    ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de
    ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de
    ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de
    ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de
    ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de
    ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de
    ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de
    ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de
    ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:add hl,de: ld a,(hl):or c:ld (hl),a:ret

tab_mulhla
        dw muln16,muln15,muln14,muln13,muln12,muln11,muln10,muln09,muln08,muln07,muln06,muln05,muln04,muln03,muln02,muln01
        dw mul000
        dw mul001,mul002,mul003,mul004,mul005,mul006,mul007,mul008,mul009,mul010,mul011,mul012,mul013,mul014,mul015,mul016
        ds 256-$+tab_mulhla

tab_sinus            ; 0 -> 120
        db 0,1,2,3,3,4,5,6,7,8,9,9,10,11,11,12,12,13,13,14,14,15,15,15,15,16,16,16,16,16
        db 16,16,16,16,16,15,15,15,15,14,14,13,13,12,12,11,11,10,9,9,8,7,7,6,5,4,3,3,2,1,0
        db -1,-2,-3,-3,-4,-5,-6,-7,-7,-8,-9,-9,-10,-11,-11,-12,-12,-13,-13,-14,-14,-15,-15,-15,-15,-16,-16,-16,-16
        db -16,-16,-16,-16,-16,-15,-15,-15,-15,-14,-14,-13,-13,-12,-12,-11,-11,-10,-9,-9,-8,-7,-7,-6,-5,-4,-3,-3,-2,-1,0
        db 1,2,3,3,4,5,6,7,8,9,9,10,11,11,12,12,13,13,14,14,15,15,15,15,16,16,16,16,16
        db 16,16
        ds 256-$+tab_sinus

tab_cosinus
        db 16,16,16,16,16,15,15,15,15,14,14,13,13,12,12,11,11,10,9,9,8,7,7,6,5,4,3,3,2,1,0
        db -1,-2,-3,-3,-4,-5,-6,-7,-7,-8,-9,-9,-10,-11,-11,-12,-12,-13,-13,-14,-14,-15,-15,-15,-15,-16,-16,-16,-16
        db -16,-16,-16,-16,-16,-15,-15,-15,-15,-14,-14,-13,-13,-12,-12,-11,-11,-10,-9,-9,-8,-7,-7,-6,-5,-4,-3,-3,-2,-1,0
        db 1,2,3,3,4,5,6,7,8,9,9,10,11,11,12,12,13,13,14,14,15,15,15,15,16,16,16,16,16
        db 16,16
        ds 256-$+tab_cosinus

tab_sinus_ns
        db 1,2,3,3,4,5,6,7,8,9,9,10,11,11,12,12,13,13,14,14,15,15,15,15,16,16,16,16,16
        db 16,16,16,16,16,15,15,15,15,14,14,13,13,12,12,11,11,10,9,9,8,7,7,6,5,4,3,3,2,1,0
        db 1,2,3,3,4,5,6,7,8,9,9,10,11,11,12,12,13,13,14,14,15,15,15,15,16,16,16,16,16
        db 16,16,16,16,16,15,15,15,15,14,14,13,13,12,12,11,11,10,9,9,8,7,7,6,5,4,3,3,2,1,0
        db 0,1
        ds 256-$+tab_sinus_ns

tab_cosinus_ns            ; 0 -> 120
        db 16,16,16,16,16,15,15,15,15,14,14,13,13,12,12,11,11,10,9,9,8,7,7,6,5,4,3,3,2,1,0
        db 1,2,3,3,4,5,6,7,8,9,9,10,11,11,12,12,13,13,14,14,15,15,15,15,16,16,16,16,16
        db 16,16,16,16,16,15,15,15,15,14,14,13,13,12,12,11,11,10,9,9,8,7,7,6,5,4,3,3,2,1,0
        db 1,2,3,3,4,5,6,7,8,9,9,10,11,11,12,12,13,13,14,14,15,15,15,15,16,16,16,16,16
        db 16,16,16,16,16,15,15,15,15,14,14,13,13,12,12,11,11,10,9,9,8,7,7,6,5,4,3,3,2,1,0
        db 0,1
        ds 256-$+tab_cosinus_ns

;### 16/8bit signed multiplication
;### Input      HL, A = multiplicators (-16 <= A <= 16)
;### Output     HL = HL * A
;### Destroyed  AF,BC,DE
multhl_by_a
        ex de,hl
mulhla0 ld h,0      ;*RELOC PATCH* tab_mulhla/256
        add 16
        add a
        ld l,a
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        jp (hl)

muln16  ex de,hl:add hl,hl:add hl,hl:add hl,hl:add hl,hl
        ld a,l:cpl:ld l,a:ld a,h:cpl:ld h,a:inc hl:ret
muln15  ld l,e:ld h,d:add hl,hl:add hl,hl:add hl,hl:add hl,hl:or a:sbc hl,de
        ld a,l:cpl:ld l,a:ld a,h:cpl:ld h,a:inc hl:ret
muln14  ex de,hl:add hl,hl:ld e,l:ld d,h:add hl,hl:add hl,hl:add hl,hl:or a:sbc hl,de
        ld a,l:cpl:ld l,a:ld a,h:cpl:ld h,a:inc hl:ret
muln13  ld l,e:ld h,d:add hl,hl:add hl,de:add hl,hl:add hl,hl:add hl,de
        ld a,l:cpl:ld l,a:ld a,h:cpl:ld h,a:inc hl:ret
muln12  ex de,hl:add hl,hl:add hl,hl:ld e,l:ld d,h:add hl,hl:add hl,de
        ld a,l:cpl:ld l,a:ld a,h:cpl:ld h,a:inc hl:ret
muln11  ld l,e:ld h,d:add hl,hl:ld c,l:ld b,h:add hl,hl:add hl,hl:add hl,de:add hl,bc
        ld a,l:cpl:ld l,a:ld a,h:cpl:ld h,a:inc hl:ret
muln10  ex de,hl:add hl,hl:ld e,l:ld d,h:add hl,hl:add hl,hl:add hl,de
        ld a,l:cpl:ld l,a:ld a,h:cpl:ld h,a:inc hl:ret
muln09  ld l,e:ld h,d:add hl,hl:add hl,hl:add hl,hl:add hl,de
        ld a,l:cpl:ld l,a:ld a,h:cpl:ld h,a:inc hl:ret
muln08  ex de,hl:add hl,hl:add hl,hl:add hl,hl
        ld a,l:cpl:ld l,a:ld a,h:cpl:ld h,a:inc hl:ret
muln07  ld l,e:ld h,d:add hl,hl:add hl,hl:add hl,hl:or a:sbc hl,de
        ld a,l:cpl:ld l,a:ld a,h:cpl:ld h,a:inc hl:ret
muln06  ex de,hl:add hl,hl:ld e,l:ld d,h:add hl,hl:add hl,de
        ld a,l:cpl:ld l,a:ld a,h:cpl:ld h,a:inc hl:ret
muln05  ld l,e:ld h,d:add hl,hl:add hl,hl:add hl,de
        ld a,l:cpl:ld l,a:ld a,h:cpl:ld h,a:inc hl:ret
muln04  ex de,hl:add hl,hl:add hl,hl
        ld a,l:cpl:ld l,a:ld a,h:cpl:ld h,a:inc hl:ret
muln03  ld l,e:ld h,d:add hl,hl:add hl,de
        ld a,l:cpl:ld l,a:ld a,h:cpl:ld h,a:inc hl:ret
muln02  ex de,hl:add hl,hl
        ld a,l:cpl:ld l,a:ld a,h:cpl:ld h,a:inc hl:ret
muln01  ex de,hl
        ld a,l:cpl:ld l,a:ld a,h:cpl:ld h,a:inc hl:ret
mul000  ld hl,0
        ret
mul001  ex de,hl
        ret
mul002  ex de,hl:add hl,hl
        ret
mul003  ld l,e:ld h,d:add hl,hl:add hl,de
        ret
mul004  ex de,hl:add hl,hl:add hl,hl
        ret
mul005  ld l,e:ld h,d:add hl,hl:add hl,hl:add hl,de
        ret
mul006  ex de,hl:add hl,hl:ld e,l:ld d,h:add hl,hl:add hl,de
        ret
mul007  ld l,e:ld h,d:add hl,hl:add hl,hl:add hl,hl:or a:sbc hl,de
        ret
mul008  ex de,hl:add hl,hl:add hl,hl:add hl,hl
        ret
mul009  ld l,e:ld h,d:add hl,hl:add hl,hl:add hl,hl:add hl,de
        ret
mul010  ex de,hl:add hl,hl:ld e,l:ld d,h:add hl,hl:add hl,hl:add hl,de
        ret
mul011  ld l,e:ld h,d:add hl,hl:ld c,l:ld b,h:add hl,hl:add hl,hl:add hl,de:add hl,bc
        ret
mul012  ex de,hl:add hl,hl:add hl,hl:ld e,l:ld d,h:add hl,hl:add hl,de
        ret
mul013  ld l,e:ld h,d:add hl,hl:add hl,de:add hl,hl:add hl,hl:add hl,de
        ret
mul014  ex de,hl:add hl,hl:ld e,l:ld d,h:add hl,hl:add hl,hl:add hl,hl:or a:sbc hl,de
        ret
mul015  ld l,e:ld h,d:add hl,hl:add hl,hl:add hl,hl:add hl,hl:or a:sbc hl,de
        ret
mul016  ex de,hl:add hl,hl:add hl,hl:add hl,hl:add hl,hl
        ret

;### ABS(hl)
;### Input      HL=signed value
;### Ouput      HL=abs(hl)
;### Destroyed  AF
abs_hl  bit 7,h
        ret z
        ld a,l  ;1
        cpl     ;1
        ld l,a  ;1
        ld a,h  ;1
        cpl     ;1
        ld h,a  ;1
        inc hl  ;2 8
        ret

;### HL fixed division (signed)
divhlby16
        sra h:rr l
        sra h:rr l
        sra h:rr l
        sra h:rr l
        ret

;### HL fixed division (unsigned)
divhlby32_pos
        srl h:rr l
divhlby16_pos
        srl h:rr l
        srl h:rr l
        srl h:rr l
        srl h:rr l
        ret

ReadSin
        ld hl,(orientation2)
readsi1 ld h,0
        ld a,(hl)
        ret

ReadCos
        ld hl,(orientation2)
readco1 ld h,0
        ld a,(hl)
        ret

;### patches routines with 256byte references for relocation
matinit dw tab_mulhla,  mulhla0+1
        dw tab_sinus,   readsi1+1
        dw tab_cosinus, readco1+1
        dw 0

matini  ld ix,matinit
        ld bc,4
matini1 ld a,(ix+1)
        or a
        ret z
        ld l,(ix+2)
        ld h,(ix+3)
        ld (hl),a
        add ix,bc
        jr matini1
