; -------------------- putScaledSpriteClip --------------------
; Draws a scaled sprite to the graph buffer, with clipping.
;
; Version: 1.1
; Author: Badja <http://move.to/badja> <badja@alphalink.com.au>
; Date: 21 December 1999
;
; Modified by Eric Piel : LD and smaller and quicker < Eric.Piel@etu.utc.fr >
; Date: 28 May 2000
; Size: 208 bytes (213 bytes if XORing the sprite, 205 bytes if just Loading it)
;
; Input:
;   HL -> sprite
;    D = x-coordinate
;    E = y-coordinate
;    B = height of unscaled sprite
;    C = width of unscaled sprite (in bytes, so divide by 8)
;    A = scale factor ($01 to $00 <=> 0.4% to 100%)
;        eg: $80 is 50%
;
; Output:
;   The sprite is ORed to the graph buffer. To XOR the sprite
;   instead, add the following line near the top of your program:
;   #define _SSC_XOR
;   Any parts of the sprite that are off-screen will be clipped.
;   To just load it insert:
;   #define _SSC_LD
;
; Destroys:
;   AF, BC, DE, HL;

;-----> Affiche une sprite dezoome de taille e/16e de la taille originale
;Input:    ix->sprite
;    a=x
;    l=y
;    b=height    (in pixels)
;    c=width        (in bytes, e.g. 2 would be 16)
;    e=ratio de dezoom de 0 a 15 (15:taille normale,0:minuscule)

scaled_sprite:
    ld    d,a    ; conversion des entrees, en attendant mieux :)
    ld    a,e
    inc    a
    add    a,a
    add    a,a
    add    a,a
    add    a,a

    ld    e,l

    push   ix
    pop    hl


putScaledSpriteClip:
    push    hl
    ld    (_SSC_SetScale1),a
    ld    (_SSC_SetScale2),a
    ld    a,d
    ld    (_SSC_SetInitXPos),a

    ld    a,e
    ld    (_SSC_YPos),a
    ld    a,d

    ld    d,0
    bit    7,e            ;is e <0?
    jr    z,_SSC_YIsPos
    dec    d            ;then de <0
_SSC_YIsPos:
    ld    h,d
    ld    l,e
    add    hl,de
    add    hl,de
    add    hl,hl
    add    hl,hl
    ld    de,GRAPH_MEM      ;plotscreen
    add    hl,de
    ld    d,0
    bit    7,a
    jr    z,_SSC_XIsPos
    dec    d
_SSC_XIsPos:
    ld    e,a
    sra    e
    sra    e
    sra    e
    add    hl,de
    and    %00000111
    ld    (_SSC_SetPreShift),a
    neg
    add    a,8
    ld    (_SSC_SetBitsLeft),a
    ex    de,hl

    ld    a,c
    ld    (_SSC_SetByteWidth),a
    sla    a
    sla    a
    sla    a
    ld    (_SSC_SetPixelWidth),a
    pop    hl

    ld    c,0
    push    bc
    jr    _SSC_DoRow

_SSC_SpriteLoop:
_SSC_SetScale1 equ $ + 1
    ld    a,0      ; scale factor (self-modified)
    add    a,c
    ld    c,a
    push    bc
    jr    z,_SSC_DoRow
    jr    c,_SSC_DoRow

_SSC_SetByteWidth equ $ + 1
    ld    bc,$0000    ; C = byte width of sprite data (self-modified)
    add    hl,bc
    jr    _SSC_SkipRow
_SSC_DoRow:
    push    de

_SSC_SetInitXPos equ $ + 1
    ld    a,$00        ; x-position of start of row (self-modified)
    ld    (_SSC_XPos),a

_SSC_SetPreShift equ $ + 2
    ld    bc,$0000    ; B = # bits before start of row (self-modified)
    ld    a,b
    or    a
    jr    z,_SSC_PutBitsLeft
    ld    a,(de)
_SSC_PreShift:
    rlca
    djnz    _SSC_PreShift
    ld    (de),a
_SSC_PutBitsLeft:
_SSC_SetBitsLeft equ $ + 1
    ld    b,$00        ; # bits to copy into first byte (self-modified)
_SSC_SetPixelWidth equ $ + 1
    ld    a,$00        ; pixel width of sprite data (self-modified)
    push    af
    jr    _SSC_DoPixel
_SSC_RowLoop:
    and    %00000111
    jr    nz,_SSC_SameByte
    inc    hl
_SSC_SameByte:
_SSC_SetScale2 equ $ + 1
    ld    a,0      ; scale factor (self-modified)
    add    a,c
    ld    c,a
    jr    z,_SSC_DoPixel
    jr    nc,_SSC_SkipPixel
_SSC_DoPixel:
_SSC_XPos    equ $ + 1
    ld    a,00        ; current x-position (self-modified)
    inc    a
    ld    (_SSC_XPos),a
    dec    a
    cp    96
    jr    nc,_SSC_OffScreen
_SSC_YPos    equ $ + 1
    ld    a,00        ; current y-position (self-modified)
    cp    64
    jr    nc,_SSC_OffScreen

    ld    a,(de)
    rlc    (hl)
 IFNDEF _SSC_LD
    jr    nc,_SSC_LeaveBit
 IFDEF _SSC_XOR
    bit    7,a
    jr    z,_SSC_LeaveCarry
    ccf
_SSC_LeaveCarry:

 ENDIF
 ENDIF
    rla
    jr    _SSC_DoneBit

_SSC_OffScreen:
    ld    a,(de)
    rlc    (hl)
_SSC_LeaveBit:
    rlca
_SSC_DoneBit:
    ld    (de),a

    djnz    _SSC_DoneByte
    ld    b,8
    inc    de
    jr    _SSC_DoneByte
_SSC_SkipPixel:
    rlc    (hl)
_SSC_DoneByte:
    pop    af
    dec    a
    push    af
    jr    nz,_SSC_RowLoop
_SSC_DoneRow:
    inc    hl
    bit    3,b
    jr    nz,_SSC_RowComplete
    ld    a,(de)
_SSC_PostShift:
    rlca
    djnz    _SSC_PostShift
    ld    (de),a
    inc    de
_SSC_RowComplete:
    pop    af
    ex    (sp),hl        ;pop    de\ push hl
    ld    de,12
    add    hl,de
    ex    de,hl
    pop    hl

    ld    a,(_SSC_YPos)
    inc    a
    ld    (_SSC_YPos),a

_SSC_SkipRow:
    pop    bc
    dec    b
    jp    nz,_SSC_SpriteLoop
    ret


; -------------------- getScaledSize --------------------
; Calculates the scaled size of a sprite.
;
; Version: 1.1
; Author: Badja <http://move.to/badja> <badja@alphalink.com.au>
; Date: 21 December 1999
; Size: 19 bytes
;
; Modified by Eric Piel : quicker and registers fit the 1.1 of scale routine < Eric.Piel@etu.utc.fr >
; Date: 28 May 2000
; Size: 27 bytes
;
; Input:
;    B = height of unscaled sprite
;    C = width of unscaled sprite (in pixels)
;    A = scale factor ($01 to $00 <=> 0.4% to 100%)
;        eg: $80 is 50%
;
; Output:
;    A = height of scaled sprite
;    H = width of scaled sprite
;
; Destroys:
;    AF, B, DE, HL

; UNTESTED
getScaledSize_adapted:
;-----> Affiche une sprite dezoome de taille e/16e de la taille originale
;Input: ix->sprite
;   b=height    (in pixels)
;   c=width     (in bytes, e.g. 2 would be 16)
;   e=ratio de dezoom de 0 a 15 (15:taille normale,0:minuscule)
    ld  a,e
    inc a
    add a,a
    add a,a
    add a,a
    add a,a

getScaledSize:
    or    a
    jr    nz,gss_Compute
    ld    a,b
    ld    h,c
    ret
gss_Compute:
    ld    l,a
    ld    h,b
    call    MUL_HL_T
    ld    l,a
    ld    a,h
    ld    h,c
    jp    MUL_HL_T

; -------------------- lineDraw --------------------
;
; Draws to the graph buffer a line between two points.
;
; Version: 1.0
; Author: Badja <http://badja.calc.org> <badja@calc.org>
; Date: 19 June 2001
; Size: 234 bytes
;
; Input:
;   (D, E) = (x0, y0), the first point
;   (H, L) = (x1, y1), the second point
;
; Input constraints:
;   Both points must be within the bounds of the screen.
;
; Output:
;   A line between the two points is ORed to the graph buffer.
;
; Destroys:
;   AF, BC, DE, HL, IX
;
; Requires:
;   Ion's getPixel routine.
;
; Comments:
;   This routine is based on the classic algorithm by Bresenham.
;   It produces the best possible pixel representation of a line,
;   and is very fast because it uses only integer arithmetic.

lineDraw:
      ld    a,h
      sub   d
      jr    nc,ld_noAbsX
      neg
ld_noAbsX:
      ld    b,a               ; B = |dx|
      ld    a,l
      sub   e
      ld    c,a               ; C = dy
      jr    nc,ld_noAbsY
      neg
ld_noAbsY:                    ; A = |dy|
      cp    b
      jr    nc,ld_rotated

      ld    a,h
      sub   d                 ; A = dx
      bit   7,a
      jr    z,ld_noSwap1
      ex    de,hl
      ld    a,b               ; A = |dx|
ld_noSwap1:
      push  af
      push  de
      neg
      ld    d,a               ; D = -dx
      ld    bc,12
      ld    a,e
      cp    l
      jr    c,ld_noReflectX
      ld    e,l
      ld    l,a
      ld    bc,-12
ld_noReflectX:
      ld    (ld_setIncrY),bc
      ld    b,$ff
      ld    c,d               ; BC = -dx
      ld    h,0               ; HL = y1
      ld    d,h               ; DE = y0
      and   a                 ; set C flag to zero
      sbc   hl,de             ; HL = dy
      add   hl,hl
      ld    (ld_setIncrE),hl
      add   hl,bc
      ld    d,h
      ld    e,l               ; DE = d
      add   hl,bc
      ld    (ld_setIncrESE),hl
      pop   bc                ; B = x0, C = y0
      push  de
      ld    a,b
      ld    e,c
      call  getpixel       ; HL -> graphbuffer offset, A = pixel mask
      push  hl
      pop   ix                ; IX -> graphbuffer offset
      pop   hl                ; HL = d
      pop   bc                ; B = |dx|
      ld    d,a
      or    (ix)
      ld    (ix),a
      ld    a,b
      or    a
      ret   z
ld_lineLoopE:
      ld    a,d
      bit   7,h
      jr    z,ld_goESE
ld_setIncrE equ $ + 1
      ld    de,$0000
      add   hl,de
      jr    ld_incrementX
ld_goESE:
ld_setIncrESE equ $ + 1
      ld    de,$0000
      add   hl,de
ld_setIncrY equ $ + 1
      ld    de,$0000
      add   ix,de
ld_incrementX:
      rrca
      jr    nc,ld_sameByte1
      inc   ix
ld_sameByte1:
      ld    d,a
      or    (ix)
      ld    (ix),a
      djnz  ld_lineLoopE
      ret

ld_rotated:
      bit   7,c               ; C = dy
      jr    z,ld_noSwap2
      ex    de,hl
      ld    c,a               ; C = |dy|
ld_noSwap2:
      ld    a,c
      push  af
      push  de
      neg
      ld    c,a               ; C = -dy
      ld    l,h
      ld    e,d
      ld    a,e
      cp    l
      ld    a,$0f             ; opcode for RRCA
      ld    b,$23             ; second byte of opcode for INC IX
      jr    c,ld_noReflectY
      ld    e,l
      ld    l,d
      ld    a,$07             ; opcode for RLCA
      ld    b,$2b             ; second byte of opcode for DEC IX
ld_noReflectY:
      ld    (ld_setIncrX1),a
      ld    a,b
      ld    (ld_setIncrX2),a
      ld    b,$ff             ; BC = -dy
      ld    h,0               ; HL = x1
      ld    d,h               ; DE = x0
      and   a                 ; set C flag to zero
      sbc   hl,de             ; HL = dx
      add   hl,hl
      ld    (ld_setIncrS),hl
      add   hl,bc
      ld    d,h
      ld    e,l               ; DE = d
      add   hl,bc
      ld    (ld_setIncrSSE),hl
      pop   bc                ; B = x0, C = y0
      push  de
      ld    a,b
      ld    e,c
      call  getpixel       ; HL -> graphbuffer offset, A = pixel mask
      push  hl
      pop   ix                ; IX -> graphbuffer offset
      pop   hl                ; HL = d
      pop   bc                ; B = |dy|
      ld    d,a
      or    (ix)
      ld    (ix),a
      ld    a,b
      or    a
      ret   z
ld_lineLoopS:
      ld    a,d
      bit   7,h
      jr    z,ld_goSSE
ld_setIncrS equ $ + 1
      ld    de,$0000
      add   hl,de
      jr    ld_incrementY
ld_goSSE:
ld_setIncrSSE equ $ + 1
      ld    de,$0000
      add   hl,de
ld_setIncrX1
      rrca
      jr    nc,ld_sameByte2
ld_setIncrX2 equ $ + 1
      inc   ix
ld_sameByte2:
ld_incrementY:
      ld    de,12
      add   ix,de
      ld    d,a
      or    (ix)
      ld    (ix),a
      djnz  ld_lineLoopS
      ret


; -------------------- lineClipAndDraw     --------------------
; -------------------- lineClipAndDrawLong --------------------
;
; Draws to the graph buffer a line between two points.
; Parts of the line that are off-screen will be clipped.
;
; Version: 1.0
; Author: Badja <http://badja.calc.org> <badja@calc.org>
; Date: 19 June 2001
; Size: 251 bytes (169 bytes if not using long lines)
;
; Input:
;   (D, E) = (x0, y0), the first point
;   (H, L) = (x1, y1), the second point
;   Values are interpreted as signed bytes, i.e. they can range
;   from -128 to 127.
;
; Input constraints:
;   Use the lineClipAndDraw routine if you are drawing lines
;   whose width and height don't exceed 128 pixels, i.e.:
;     |x1 - x0| < 128  and  |y1 - y0| < 128
;
;   Use the lineClipAndDrawLong routine if your lines are longer.
;   Note that this routine is slower. If you don't use this
;   routine, add to the start of your program the line
;   "#define lcd_noLongLines" to save 82 bytes.
;
; Output:
;   A line between the two points is clipped at the boundary of
;   the screen and ORed to the graph buffer.
;
; Destroys:
;   AF, BC, DE, HL, IX
;
; Requires:
;   The lineDraw routine (in lineDraw.inc)
;
; Comments:
;   This routine is based on the Cohen-Sutherland line-clipping
;   algorithm. If a line is entirely on (or entirely off) the
;   screen, the routine quickly realises this and immediately
;   draws the line (or no line) without trying to calculate any
;   boundary intersection points.

lineClipAndDraw:
      call  lcd_compOutCode
      ex    de,hl
      ld    c,b               ; C = ????, outcode0
lcd_subdivide:
      call  lcd_compOutCode   ; B = outcode0, outcode1
      ld    a,b
      or    a                 ; are both outcodes zero?
      jp    z,lineDraw        ; if so, trivial accept and draw line
      ld    a,b
      and   c                 ; is logical AND of outcodes non-zero?
      and   $0f               ; (mask off irrelevant bits)
      ret   nz                ; if so, trivial reject and return
      ld    a,b               ; failed both tests, so clip the line segment...
      and   $0f               ; at least one endpoint is off-screen, is point 1 off-screen?
      jr    nz,lcd_clip       ; if so, pick this one
      ex    de,hl             ; otherwise, the other endpoint must be off-screen, so exchange them
      ld    b,c
      ld    c,a
lcd_clip:
      push  hl
      bit   0,b
      jr    z,lcd_notBottom
      ld    b,63              ; divide line at bottom of screen (y = 63)
      jr    lcd_divideHoriz
lcd_notBottom:
      bit   1,b
      jr    z,lcd_notTop
      ld    b,0               ; divide line at top of screen (y = 0)
lcd_divideHoriz:
      push  bc
      ld    a,h
      sub   d
      ld    h,a               ; H = x1 - x0
      ld    a,d
      jr    z,lcd_vertical    ; if x1 - x0 == 0, then intersection point is x0 == A
      ld    a,l
      sub   e
      ld    c,a               ; C = y1 - y0
      ld    a,b
      sub   e
      ld    l,a               ; L = y - y0
      ld    b,c               ; B = y1 - y0
      ld    c,d               ; C = x0
      call  lcd_divideLine    ; A = x
lcd_vertical:
      pop   bc                ; B = y
      ld    d,a
      ld    e,b               ; DE = intersection point
      jr    lcd_nextPass
lcd_notTop:
      bit   2,b
      ld    b,95              ; divide line at right edge of screen (x = 95)
      jr    nz,lcd_divideVert
      ld    b,0               ; divide line at left edge of screen (x = 0)
lcd_divideVert:
      push  bc
      ld    a,l
      sub   e
      ld    l,a               ; L = y1 - y0
      ld    a,e
      jr    z,lcd_horizontal  ; if y1 - y0 == 0, then intersection point is y0 == A
      ld    a,h
      sub   d
      ld    c,a               ; C = x1 - x0
      ld    a,b
      sub   d
      ld    h,a               ; H = x - x0
      ld    b,c               ; B = x1 - x0
      ld    c,e               ; C = y0
      call  lcd_divideLine    ; A = y
lcd_horizontal:
      pop   bc                ; B = x
      ld    d,b
      ld    e,a               ; DE = intersection point
lcd_nextPass:
      ld    b,c
      pop   hl
      jr    lcd_subdivide     ; repeat process for new line segment

lcd_divideLine:               ; return A = C + H * L / B
      ld    a,h
      xor   l
      xor   b                 ; bit 7 of A = sign of H * L / B
      push  af
      bit   7,h
      jr    z,lcd_positive1
      ld    a,h
      neg
      ld    h,a
lcd_positive1:                ; H = |H|
      bit   7,l
      jr    z,lcd_positive2
      ld    a,l
      neg
      ld    l,a
lcd_positive2:                ; L = |L|
      ld    a,b
      bit   7,a
      jr    z,lcd_positive3
      neg
lcd_positive3:                ; A = |B|
      call    MUL_HL_T    ; HL = H * L
      call    divhlbya        ; HL = HL / A
      pop   af                ; bit 7 of A = sign of H * L / B
      and   $80               ; mask off irrelevent bits
      ld    a,l               ; A = result of |H| * |L| / |B|
      jr    z,lcd_positive4
      neg                     ; make result negative if necessary
lcd_positive4:
      add   a,c               ; A = C + result
      ret

lcd_compOutCode:              ; left-rotate 4-bit outcode for point DE into B
      ld    a,d
      rla
      jr    c,lcd_negative1   ; if D < 0, then D must be < 96
      rl    b
      ld    a,95
      sub   d
      rla
      jr    lcd_notNegative1
lcd_negative1:
      rl    b
      and   a
lcd_notNegative1:
      rl    b

      ld    a,e
      rla
      jr    c,lcd_negative2   ; if E < 0, then E must be < 64
      rl    b
      ld    a,63
      sub   e
      rla
      jr    lcd_notNegative2
lcd_negative2:
      rl    b
      and   a
lcd_notNegative2:
      rl    b
      ret


cp_hl_de
         push  hl
         or    a
         sbc   hl,de
         pop   hl
         ret

cp_hl_de_approx_b
         push  hl
         or    a
         sbc   hl,de
         pop   hl
         ret z
         jp c,__de_bigger
__hl_bigger
        push hl
        or a
        sbc hl,de
        ld d,0
        ld e,b
        or a
        sbc hl,de  ; HL - DE - B = ?
        pop hl
        jp c,_force_z
        scf  ; otherwise, HL was bigger enough
        ccf
        ret

__de_bigger
        push de
        push hl
        ex de,hl
        or a
        sbc hl,de
        ld d,0
        ld e,b
        or a
        sbc hl,de  ; DE - HL - B = ?
        pop hl
        pop de
        jp c,_force_z
        scf  ; otherwise, DE was bigger enough
        ret

_force_z
    xor a
    ret





abs_hl
    bit    7,h
    ret    z
    push    de
    ex    de,hl
    ld    hl,0
    or    a
    sbc    hl,de
    pop    de
    ret

divhlby16:
    bit    7,h        ;test if hl<0
    jp    nz,hl_neg16
divhlby16_pos
    srl    h
    rr    l
    srl    h
    rr    l
    srl    h
    rr    l
    srl    h
    rr    l
    ret

divhlby32_pos
    srl h
    rr  l
    srl h
    rr  l
    srl h
    rr  l
    srl h
    rr  l
    srl h
    rr  l
    ret

hl_neg16:
    ex    de,hl
    ld    hl,0
    sbc    hl,de
    call    divhlby16_pos
    ex    de,hl
    ld    hl,0
    sbc    hl,de
    ret


ld_hl_mhl
    ld    a,(hl)
    inc    hl
    ld    h,(hl)
    ld    l,a
    ret

keyboard_pause
   di
   in a,(#aa)
   and #f0
   or 7
   ld b,a
.wait1
   ld a,b
   out (#aa),a
   in a,(#a9)
   ei
   push af
   push bc
   call checkmessages
   rst #30
   pop bc
   pop af
   and 4
   jr z,.wait1
   ld a,b
   inc a
   di
   out (#aa),a
   in a,(#a9)
   CP #FF
   jr nz,.wait1

.wait2
   ld a,b
   out (#aa),a
   in a,(#a9)
   or %11111011
   cp #FF
   ei

   push af
   push bc
   call checkmessages
   rst #30
   pop bc
   pop af

   ret nz
   di
   ld a,b
   inc a
   out (#aa),a
   in a,(#a9)
   or 4
   CP #FF
   jr z,.wait2
   ei
   ret


/*
  call ReadKeyboard
  call ValidateKeys
  ld    b,7
  ld    hl,ValidKey
  ld    a,0
loop_wait_keys:
  and   (hl)
  inc   hl
  djnz loop_wait_keys
  ei
  halt
  cp    %11111111
  jr z,loop_wait_keys
  ret
*/

divhlby10
    ld    a,10
divhlbya
    push    bc
    ld    c,a
    sub    a
    ld    b,16
loop_divhlbya
    add    hl,hl
    rla
    cp    c
    jr    c,jump_divhlbya
    sub    c
    inc    l
jump_divhlbya
    djnz    loop_divhlbya
    pop    bc
    ret


clrgbuf_fast        ; not really fast
    ld    hl,gbuf
    ld    de,gbuf+1
    ld    (hl),0
    ld    bc,767
    ldir
    ret

Scroll_U:                       ; [16228 Clock Cycles]
  PUSH HL                       ; Save Registers
  PUSH DE
  PUSH BC
  LD  HL, GRAPH_MEM+12          ; Copy from one row below top
  LD  DE, GRAPH_MEM             ;   to top row
  LD  BC, 756                   ;   756 bytes
  LDIR
  LD  H, D                      ; Fill Blanks
  LD  L, E
  INC E
  LD  (HL), B
  LD  C, 11
  LDIR
  POP BC                        ; Restore Registers
  POP DE
  POP HL
  RET

;SCREEN OUTPUT

nodifastcopy

    ld    a,(interrupts_active)
    dec   a
    jp z,0
.gohere equ $-2

   call checkmessages
 rst #30


  ld hl,gbuf
  ld de,gamescreen+3
  ld bc,scrtable+256
  ld ix,#300
.loop
  ld a,(hl)
  ld (hl),0
  inc hl
.mod nop
  ld c,a
  ld a,(bc)
  ld (de),a
  inc de
  inc b
  ld a,(bc)
  ld (de),a
  inc de
  dec b
  dec ixl
  jp nz,.loop
  dec ixh
  jp nz,.loop

.ohi
        ld c,MSC_DSK_WINDIN
        ld a,(prgwin)
        ld b,a
        ld de,0
        call msgsnd

  ret

fastCopyinv equ nodifastcopy.mod
gohere      equ nodifastcopy.gohere



; A = x , E = y
getpixel:
    ld    d,0
    ld    h,d
    ld    l,e
    add    hl,de
    add    hl,de
    add    hl,hl
    add    hl,hl
    ld    de,gbuf
    add    hl,de
    ld    b,0
    ld    c,a
    and    %00000111
    srl    c
    srl    c
    srl    c
    add    hl,bc
    ld    b,a
    inc    b
    ld    a,%00000001
getPixelLoop:
    rrca
    djnz    getPixelLoop
    ret


; vertical line centered on the horizon
; not anymore since the dying/dead state
; H = X, L = starting Y (<32)
vlinecentered:
    ld  a,h
    or  a
    ret m ; lateral clipping
    cp  96
    ret nc ; lateral clipping

    ld     a,32
    sub    l
    add    a,a
    ld     b,a
    push   bc ; save length of line

    ld      a,(walls_delta_height2)
    ld      b,a
    ld      a,l
    sub     b
    ld      l,a
    jp      p,vlc_noclip
; clip upper part
    ld      l,0 ; start drawing line at top of screen
    pop     bc; reduce length of line
    add     a,b ; a<0
    ld      b,a
;now test if it is short enough
    cp      63
    jp      c,vlc_shortenough
    ld      b,63
vlc_shortenough
    push    bc
vlc_noclip
    ld     a,h
    ld     e,l
    call   getpixel
    pop    bc
    ld     c,a
    ld     de, 12 ; screen width in bytes
vlinecenteredloop
    ld     a,(hl)
    or     c
    ld     (hl),a
    add    hl,de
    djnz   vlinecenteredloop
    ret


largesprite_masked_clipped_display_nothing
    pop af
    pop af
    ret
largesprite_masked_clipped_down
    push    af
    push    hl
    push    bc
    ; if l+b>63  then b -= l+b-64
    ld      a,l
    add     a,b
    sub     63
    jp      c,largesprite_masked_clipped_do_nothing
    ld      b,a
    pop     af ; was bc before
    sub     b
    jp      z,largesprite_masked_clipped_display_nothing
    jp      c,largesprite_masked_clipped_display_nothing
    ld      b,a
    push    bc
largesprite_masked_clipped_do_nothing
    pop     bc
    pop     hl
    pop     af
;    jp      largesprite_masked


largesprite_masked
    jp    largesprite_AND_XOR



; tmplsa_width db 0
; tmplsa_width_count db 0
; largesprite_AND:
;     push    af
;     ld  a,c
;     ld  (tmplsa_width),a
;     ld  (tmplsa_width_count),a
;     pop af
;     ld  e,l
;     ld  h,0
;     ld  d,h
;     add hl,de
;     add hl,de
;     add hl,hl
;     add hl,hl
;     ld  e,a
;     and %00000111
;     ld  c,a
;     srl e
;     srl e
;     srl e
;     add hl,de
;     ld  de,gbuf
;     add hl,de
; largeSpriteLoop1_AND:
;     push    hl
; largeSpriteLoop2_AND
;     ld  d,(ix)
;     ld  e,%11111111
;     ld  a,c
;     or  a
;     jr  z,largeSpriteSkip1_AND
; largeSpriteLoop3_AND
;     scf
;     rr  d
;     rr  e
;     dec a
;     jr  nz,largeSpriteLoop3_AND
; largeSpriteSkip1_AND
;     ld  a,(hl)
;     and d
;     ld  (hl),a
;     inc hl
;     ld  a,(hl)
;     and e
;     ld  (hl),a
;     inc ix
;     ld  a,(tmplsa_width_count)
;     dec a
;     ld  (tmplsa_width_count),a
;     jr  nz,largeSpriteLoop2_AND
;     ld  a,(tmplsa_width)
;     ld  (tmplsa_width_count),a
;     pop hl
;     ld  de,12
;     add hl,de
;     djnz    largeSpriteLoop1_AND
;     ret

tmpls_width db 0
tmpls_width_count db 0
;-----> Draw a picture modded so that it doesn't use shadow regsisters and don't stop interrupts
;Input: ix->sprite
;   a=x
;   l=y
;   b=height    (in pixels)
;   c=width     (in bytes, e.g. 2 would be 16)
;Output: nothing   ; All registers are destroyed except bc', de', hl'
largesprite:
    push    af
    ld  a,c
    ld  (tmpls_width),a
    ld  (tmpls_width_count),a
    pop af
    ld  e,l
    ld  h,0
    ld  d,h
    add hl,de
    add hl,de
    add hl,hl
    add hl,hl
    ld  e,a
    and %00000111
    ld  c,a
    srl e
    srl e
    srl e
    add hl,de
    ld  de,gbuf
    add hl,de
largeSpriteLoop1:
    push    hl
largeSpriteLoop2:
    ld  d,(ix)
    ld  e,0
    ld  a,c
    or  a
    jr  z,largeSpriteSkip1
largeSpriteLoop3:
    srl d
    rr  e
    dec a
    jr  nz,largeSpriteLoop3
largeSpriteSkip1:
    ld  a,(hl)
    or  d
    ld  (hl),a
    inc hl
    ld  a,(hl)
    or  e
    ld  (hl),a
    inc ix
    ld  a,(tmpls_width_count)
    dec a
    ld  (tmpls_width_count),a
    jr  nz,largeSpriteLoop2
    ld  a,(tmpls_width)
    ld  (tmpls_width_count),a
    pop hl
    ld  de,12
    add hl,de
    djnz    largeSpriteLoop1
    ret



tmplsax_width db 0
tmplsax_width_count db 0
;-----> Draw a picture modded so that it doesn't use shadow regsisters and don't stop interrupts
;Input: ix->sprite
;   a=x
;   l=y
;   b=height    (in pixels)
;   c=width     (in bytes, e.g. 2 would be 16)
;Output: nothing   ; All registers are destroyed except bc', de', hl'
largesprite_AND_XOR:
    push    af
    ld  a,c
    ld  (tmplsax_width),a
    ld  (tmplsax_width_count),a
    pop af
    ld  e,l
    ld  h,0
    ld  d,h
    add hl,de
    add hl,de
    add hl,hl
    add hl,hl
    ld  e,a
    and %00000111
    ld  c,a
    srl e
    srl e
    srl e
    add hl,de
    ld  de,gbuf
    add hl,de
largeSpriteLoop1ax:
    push    hl

largeSpriteLoop2ax_AND
    ld  d,(ix)
    ld  e,%11111111
    ld  a,c
    or  a
    jr  z,largeSpriteSkip1ax_AND
largeSpriteLoop3ax_AND
    scf
    rr  d
    rr  e
    dec a
    jr  nz,largeSpriteLoop3ax_AND
largeSpriteSkip1ax_AND
    ld  a,(hl)
    and d
    ld  (hl),a
    inc hl
    ld  a,(hl)
    and e
    ld  (hl),a
    inc ix

    dec hl

largeSpriteLoop2ax:
    ld  d,(ix)
    ld  e,0
    ld  a,c
    or  a
    jr  z,largeSpriteSkip1ax
largeSpriteLoop3ax:
    srl d
    rr  e
    dec a
    jr  nz,largeSpriteLoop3ax
largeSpriteSkip1ax:
    ld  a,(hl)
    xor  d
    ld  (hl),a
    inc hl
    ld  a,(hl)
    xor  e
    ld  (hl),a
    inc ix

    ld  a,(tmplsax_width_count)
    dec a
    ld  (tmplsax_width_count),a
    jr  nz,largeSpriteLoop2ax_AND
    ld  a,(tmplsax_width)
    ld  (tmplsax_width_count),a
    pop hl
    ld  de,12
    add hl,de
    djnz    largeSpriteLoop1ax
    ret


DisplayHud
    ld    hl,gbuf+12*50
    ld    (hl),%11111111
    inc    hl
    ld    a,(hl)
    or    %11111110
    ld    (hl),a

    ld    hl,gbuf+12*50+10
    ld    a,(hl)
    or    %01111111
    ld    (hl),a
    inc    hl
    ld    (hl),%11111111

    ld    hl,hud_sprite
    ld    a,13
    ld    de,gbuf+12*51

disp_hud_loop
    ldi
    ldi
    push    bc
    ex    de,hl
    ld    bc,8
    add    hl,bc
    ex    de,hl
    pop    bc
    ldi
    ldi
    dec    a
    jp    nz,disp_hud_loop

display_hud2
    ld    a,(health)
    ld    de,58*256+0
    ld    b,3
    call    vdispa_s

    ld    a,(weapon_selected)
    cp    GUN_SELECTED
    jp    z,dh2_gun
    cp    CHAINGUN_SELECTED
    jp    z,dh2_chaingun
    cp    SHOTGUN_SELECTED
    jp    z,dh2_shotgun
    cp    CHAINSAW_SELECTED
    ret    z

dh2_gun
    ld    a,(q_gun_ammo)
    jp    dh2_end

dh2_chaingun
    ld    a,(q_chaingun_ammo)
    jp    dh2_end
dh2_shotgun
    ld    a,(q_shotgun_ammo)

dh2_end

    ld    de,58*256+82
    ld    b,3
    call    vdispa_s
    ret





;----------------------------------------------------------------------------
;[ MUL_HL_T ] by Hideaki Omuro
; optimized for time (36 bytes, 198-255 clocks)
;
;parameters:
; H = 8-bit multiplicand, L = 8-bit multiplier
;returns:
; HL = 16-bit product
;modifies:
; DEFHL
;----------------------------------------------------------------------------
MUL_HL_T:
   LD    D, 0                          ; DE = multiplier, H = multiplicand
   LD    E, L
   LD    L, D                          ; result = 0
   ADD   HL, HL                        ; shift/test bit
   JR    NC, _CMHT_noadd1              ; if shifted bit is set then
   ADD   HL, DE                        ; add the multiplier
_CMHT_noadd1:
   ADD   HL, HL                        ; shift/test bit
   JR    NC, _CMHT_noadd2              ; if shifted bit is set then
   ADD   HL, DE                        ; add the multiplier
_CMHT_noadd2:
   ADD   HL, HL                        ; shift/test bit
   JR    NC, _CMHT_noadd3              ; if shifted bit is set then
   ADD   HL, DE                        ; add the multiplier
_CMHT_noadd3:
   ADD   HL, HL                        ; shift/test bit
   JR    NC, _CMHT_noadd4              ; if shifted bit is set then
   ADD   HL, DE                        ; add the multiplier
_CMHT_noadd4:
   ADD   HL, HL                        ; shift/test bit
   JR    NC, _CMHT_noadd5              ; if shifted bit is set then
   ADD   HL, DE                        ; add the multiplier
_CMHT_noadd5:
   ADD   HL, HL                        ; shift/test bit
   JR    NC, _CMHT_noadd6              ; if shifted bit is set then
   ADD   HL, DE                        ; add the multiplier
_CMHT_noadd6:
   ADD   HL, HL                        ; shift/test bit
   JR    NC, _CMHT_noadd7              ; if shifted bit is set then
   ADD   HL, DE                        ; add the multiplier
_CMHT_noadd7:
   ADD   HL, HL                        ; shift/test bit
   RET   NC                            ; if shifted bit is set then
   ADD   HL, DE                        ; add the multiplier
   RET



 IFNDEF TI83
vRandom
// http://zilog.sh.cvut.cz/~baze/misc/z80bits.html
    ld    de,(randseed)
    ld    a,d
    ld    h,e
    ld    l,253
    or    a
    sbc    hl,de
    sbc    a,0
    sbc    hl,de
    ld    d,0
    sbc    a,d
    ld    e,a
    sbc    hl,de
    jr    nc,$+3
    inc    hl
    ld    (randseed),hl
    ret
randseed    dw    0
 ENDIF



; the following routine is used in the file newlvl.z80,
; speed is not important here
; input = a
; output = carry flag set if a <-> opaque tile, clear otherwise

is_a_opaque
    cp    1 ; wall
    jp    z,is_a_opaque_yes
    cp    14 ; column
    jp    z,is_a_opaque_yes
is_a_opaque_no
    scf
    ccf
    ret

is_a_opaque_yes
    scf
    ret




;*******************************************************

WIDTH_SPACE equ 4  ; it was 12 in the original routine, (12 bytes = width of the screen)

; -------------------- lineDraw2 --------------------
;
;
; modified for a buffer that is 4 bytes wide and 32 bytes high
; doesn't draw anything
; OUTPUT : carry flag set if wall hit between the 2 points
;       carry flag clear otherwise
;
; original routine :

; Author: Badja <http://badja.calc.org> <badja@calc.org>
; Date: 19 June 2001
; Size: 234 bytes
;
; Input:
;   (D, E) = (x0, y0), the first point
;   (H, L) = (x1, y1), the second point

lineDraw2:
      ld    a,h
      sub   d
      jr    nc,ld2_noAbsX
      neg
ld2_noAbsX:
      ld    b,a               ; B = |dx|
      ld    a,l
      sub   e
      ld    c,a               ; C = dy
      jr    nc,ld2_noAbsY
      neg
ld2_noAbsY:                    ; A = |dy|
      cp    b
      jr    nc,ld2_rotated
      ld    a,h
      sub   d                 ; A = dx
      bit   7,a
      jr    z,ld2_noSwap1
      ex    de,hl
      ld    a,b               ; A = |dx|
ld2_noSwap1:
      push  af
      push  de
      neg
      ld    d,a               ; D = -dx
      ld    bc,WIDTH_SPACE
      ld    a,e
      cp    l
      jr    c,ld2_noReflectX
      ld    e,l
      ld    l,a
      ld    bc,-WIDTH_SPACE
ld2_noReflectX:
      ld    (ld2_setIncrY),bc
      ld    b,$ff
      ld    c,d               ; BC = -dx
      ld    h,0               ; HL = y1
      ld    d,h               ; DE = y0
      and   a                 ; set C flag to zero
      sbc   hl,de             ; HL = dy
      add   hl,hl
      ld    (ld2_setIncrE),hl
      add   hl,bc
      ld    d,h
      ld    e,l               ; DE = d
      add   hl,bc
      ld    (ld2_setIncrESE),hl
      pop   bc                ; B = x0, C = y0
      push  de
      ld    a,b
      ld    e,c
      call  access_opaque_array       ; HL -> graphbuffer offset, A = pixel mask
      push  hl
      pop   ix                ; IX -> graphbuffer offset
      pop   hl                ; HL = d
      pop   bc                ; B = |dx|

; stack is at level

      ld    d,a
      and   (ix)
    jp    nz,linedraw2_wall_met

      ld    a,b
      or    a
          jp    z,linedraw2_no_wall_met
ld2_lineLoopE:
      ld    a,d
      bit   7,h
      jr    z,ld2_goESE
ld2_setIncrE equ $ + 1
      ld    de,$0000
      add   hl,de
      jr    ld2_incrementX
ld2_goESE:
ld2_setIncrESE equ $ + 1
      ld    de,$0000
      add   hl,de
ld2_setIncrY equ $ + 1
      ld    de,$0000
      add   ix,de
ld2_incrementX:
      rrca
      jr    nc,ld2_sameByte1
      inc   ix
ld2_sameByte1:
      ld    d,a
      and   (ix)
    jp    nz,linedraw2_wall_met

      djnz  ld2_lineLoopE
; no wall was met
    scf
    ccf
      ret





ld2_rotated:
      bit   7,c               ; C = dy
      jr    z,ld2_noSwap2
      ex    de,hl
      ld    c,a               ; C = |dy|
ld2_noSwap2:
      ld    a,c
      push  af
      push  de
      neg
      ld    c,a               ; C = -dy
      ld    l,h
      ld    e,d
      ld    a,e
      cp    l
      ld    a,$0f             ; opcode for RRCA
      ld    b,$23             ; second byte of opcode for INC IX
      jr    c,ld2_noReflectY
      ld    e,l
      ld    l,d
      ld    a,$07             ; opcode for RLCA
      ld    b,$2b             ; second byte of opcode for DEC IX
ld2_noReflectY:
      ld    (ld2_setIncrX1),a
      ld    a,b
      ld    (ld2_setIncrX2),a
      ld    b,$ff             ; BC = -dy
      ld    h,0               ; HL = x1
      ld    d,h               ; DE = x0
      and   a                 ; set C flag to zero
      sbc   hl,de             ; HL = dx
      add   hl,hl
      ld    (ld2_setIncrS),hl
      add   hl,bc
      ld    d,h
      ld    e,l               ; DE = d
      add   hl,bc
      ld    (ld2_setIncrSSE),hl
      pop   bc                ; B = x0, C = y0
      push  de
      ld    a,b
      ld    e,c
      call  access_opaque_array       ; HL -> graphbuffer offset, A = pixel mask
      push  hl
      pop   ix                ; IX -> graphbuffer offset
      pop   hl                ; HL = d
      pop   bc                ; B = |dy|

; stack is at level
    ld    d,a
    and    (ix)
    jp    nz,linedraw2_wall_met
    ld    a,b
    or    a
    jp    z,linedraw2_no_wall_met
ld2_lineLoopS:
      ld    a,d
      bit   7,h
      jr    z,ld2_goSSE
ld2_setIncrS equ $ + 1
      ld    de,$0000
      add   hl,de
      jr    ld2_incrementY
ld2_goSSE:
ld2_setIncrSSE equ $ + 1
      ld    de,$0000
      add   hl,de
ld2_setIncrX1
      rrca
      jr    nc,ld2_sameByte2
ld2_setIncrX2 equ $ + 1
      inc   ix
ld2_sameByte2:
ld2_incrementY:
      ld    de,WIDTH_SPACE
      add   ix,de
      ld    d,a
      and   (ix)
    jp    nz,linedraw2_wall_met

      djnz  ld2_lineLoopS
; no wall was met
linedraw2_no_wall_met
    scf
    ccf
    ret

linedraw2_wall_met
    scf
    ret
;*******************************************************




; disp a with a cetain number (b) of digits
vdispa_s
    ld    h,0
    ld    l,a
vdisphl_s
    push    de
    ld    de,op1+5
    xor    a
    ld    (de),a
vdisphlsloop
    call    divhlby10
    add    a,'0'
    dec    de
    ld    (de),a
    djnz    vdisphlsloop
    ex    de,hl
    pop    de
    jp    fastvputs


vdispa
    ld    h,0
    ld    l,a
vdisphl
    push    de
    ld    de,op1+5
    xor    a
    ld    (de),a
vdisphlloop
    call    divhlby10
    add    a,'0'
    dec    de
    ld    (de),a
    ld    a,h
    or    l
    jr    nz,vdisphlloop
    ex    de,hl
    pop    de
fastvputs
    ld    (pencol),de

; Dwedit's font routine

vputs
    ld a,(hl)
    inc hl

    or a
    ret z
    push hl
        call vputmap
    pop hl
    jr vputs

vputmap
    sub 32
    ld l,a
    srl l
    ld h,0
    ld e,l
    ld d,h
    add hl,hl
    add hl,hl
    add hl,de
    ld de,fontdata
    add hl,de
    ld de,bytes5
    ld b,5
    and 1
    jr z,unshiftedchar
shiftedcharloop:
    ld a,(hl)
    add a,a
    add a,a
    add a,a
    add a,a
    ld (de),a
    inc hl
    inc de
    djnz shiftedcharloop
    jr fontskip
unshiftedchar:
    ld a,(hl)
    and %11110000
    ld (de),a
    inc hl
    inc de
    djnz unshiftedchar
fontskip:
    ld ix,bytes5
    ld hl,(pencol)
    ld a,l
    ld l,h
    inc l
    ld bc,$0501
    call largesprite ; TODO call something less massive ?
    ;advance
    ld hl,pencol
    ld a,(hl)
    add a,6
    ld (hl),a
    ld c,%00001000
findwidthloop:
    dec (hl)
    ld a,c
    add a,a
    ret z
    ld c,a
    ld de,bytes5
    ld b,5
findwidthloop2:
    ld a,(de)
    and c
    ret nz
    inc de
    djnz findwidthloop2
    jr findwidthloop


bytes5:
    db 0,0,0,0,0


;2 wide: flst(),;]
;1 wide:  !'.:i


fontdata:
 db %00001000
 db %00001000
 db %00001000
 db %00000000
 db %00001000

 db %10100101
 db %10101111
 db %10100101
 db %00001111
 db %00000101

 db %01111010
 db %10100010
 db %01110100
 db %00101000
 db %11111010

 db %01001000
 db %10101000
 db %01001000
 db %10100000
 db %01010000

 db %01001000
 db %10000100
 db %10000100
 db %10000100
 db %01001000

 db %10100000
 db %01000100
 db %11101110
 db %01000100
 db %10100000

 db %00000000
 db %00000000
 db %01001110
 db %01000000
 db %10000000

 db %00000010
 db %00000010
 db %00000100
 db %00001000
 db %10001000

 db %11100110
 db %10010010
 db %10010010
 db %10010011
 db %01110111

 db %01101110
 db %10010001
 db %00100010
 db %01000001
 db %11111110

 db %10001111
 db %10101000
 db %11111110
 db %00100001
 db %00101110

 db %10001111
 db %10001001
 db %11110001
 db %10010001
 db %11110001

 db %11101111
 db %10011001
 db %11111111
 db %10010001
 db %01110001

 db %00000000
 db %10000100
 db %00000000
 db %10000100
 db %00001000

 db %00100000
 db %01001110
 db %10000000
 db %01001110
 db %00100000

 db %10001100
 db %01000010
 db %00100100
 db %01000000
 db %10000100

 db %11100100
 db %00011010
 db %11011110
 db %10111010
 db %01111010

 db %11000110
 db %10101000
 db %11001000
 db %10101000
 db %11000110

 db %11001110
 db %10101000
 db %10101100
 db %10101000
 db %11001110

 db %11100110
 db %10001000
 db %11001010
 db %10001010
 db %10000110

 db %10101110
 db %10100100
 db %11100100
 db %10100100
 db %10101110

 db %00101010
 db %00101010
 db %00101100
 db %10101010
 db %11101010

 db %10001010
 db %10001110
 db %10001110
 db %10001010
 db %11101010

 db %11001110
 db %10101010
 db %10101010
 db %10101010
 db %10101110

 db %11001110
 db %10101010
 db %11001010
 db %10001110
 db %10000110

 db %11000110
 db %10101000
 db %11000100
 db %10100010
 db %10101100

 db %11101010
 db %01001010
 db %01001010
 db %01001010
 db %01001110

 db %10101010
 db %10101010
 db %10101010
 db %01001110
 db %01001010

 db %10101010
 db %10101010
 db %01000100
 db %10100100
 db %10100100

 db %11100100
 db %00101010
 db %01001110
 db %10001010
 db %11100100

 db %10001100
 db %10000100
 db %01000100
 db %00100100
 db %00101100

 db %01000000
 db %10100000
 db %00000000
 db %00000000
 db %00001110

 db %10000000
 db %01000110
 db %00001010
 db %00001010
 db %00000110

 db %10000000
 db %11000110
 db %10101000
 db %10101000
 db %11000110

 db %00100000
 db %01100100
 db %10101010
 db %10101100
 db %01100110

 db %01000110
 db %10001010
 db %11000110
 db %10000010
 db %10001100

 db %10001000
 db %11000000
 db %10101000
 db %10101000
 db %10101000

 db %00101000
 db %00001000
 db %00101010
 db %10101100
 db %01001010

 db %11000000
 db %01001010
 db %01001110
 db %01001010
 db %01001010

 db %00000000
 db %11000100
 db %10101010
 db %10101010
 db %10100100

 db %00000000
 db %11000110
 db %10101010
 db %11000110
 db %10000010

 db %00000000
 db %10101100
 db %11001000
 db %10000100
 db %10001100

 db %10000000
 db %11001010
 db %10001010
 db %10001010
 db %01001110

 db %00000000
 db %10101010
 db %10101010
 db %01001110
 db %01001010

 db %00000000
 db %10101010
 db %01001010
 db %01000100
 db %10101000

 db %00000110
 db %11110100
 db %00101000
 db %01000100
 db %11110110      ; ends at $7B : '{'

