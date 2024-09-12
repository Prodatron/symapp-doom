new_level:
    ;im    1
    
    xor a
    ld (interrupts_active),a

    ld    sp,(sp_2)
    ld    hl,(tempreturn)
    push    hl
    
    ;------------

    call    clrgbuf_fast

    ld    hl,level
    inc    (hl)

    ld    hl,levels_list
    ld    a,(level)
    dec    a
    add    a,a
    ld    e,a
    ld    d,0
    add    hl,de

    ld    a,(hl)  ;
    inc    hl      ;
    ld    h,(hl)  ;
    ld    l,a     ; ld hl,(hl)


    or     h
    jp     nz,next_level    ; if hl != 0

    ld     hl,t_won
    ld     de,256*29+30
    call   fastvputs
    call   nodifastcopy
    call   temporisateur
    call   keyboard_pause
    jp     game_won



next_level
    push   hl
    ld     de,256*28+32
    ld     hl,t_level
    call   fastvputs
    ld     a,(level)
    ld     de,(pencol)
    inc    de
    call   vdispa
    ld     a,$2f    ; CPL
    ld     (fastCopyinv),a    ; invert display

    call   nodifastcopy

; erase previous temporary data
    xor    a
    ld     (player_is_hit),a

    ld     hl,listex_sprite
    ld     de,listex_sprite+1
    ld     (hl),0
    ld     bc,fin_listes_sprite-listex_sprite-1
    ldir

    pop    hl

    ld    e,(hl)
    inc    hl
    ld    d,(hl)
    inc    hl
    ld    (xperso_v),de

    ld    e,(hl)
    inc    hl
    ld    d,(hl)
    inc    hl
    ld    (zperso_v),de

    ld    a,(hl)
    ld    (orientation),a
    inc    hl

; *******************
;     load map
; *******************

    call   decompress_level


; *******************
;     load enemies
; *******************

    ld    de,enemiesdata

    ld    a,(hl)
    ld    (de),a    ; amount of enemies
    ld    b,a
    inc    hl
    inc de

newlvl_enemies_copy_loop
    push   bc
    ld     bc,5
    ldir
    ld     a,(hl) ; get enemy type
    inc    hl
    push   hl
    push   de
    ld     hl,EnemyType
    ld     d,0
    add    a,a ;
    add    a,a ;
    add    a,a ; a*8 because currently the EnemyType table holds 8 bytes per enemy type
    ld     e,a
    add    hl,de
    pop    de
    ld     bc,6
    ldir    ; copy enemy type, life, ai, time before re-attacking, initial sprite

    xor  a
    ld   (de),a
    inc  de
    ld   (de),a
    inc  de
    ld   (de),a
    inc  de
    ld   (de),a
    inc  de
    ld   (de),a
    inc  de


    pop    hl
    pop    bc
    djnz    newlvl_enemies_copy_loop

; now enemies' data is stocked

    call    update_xperso_zperso

;****************** creation of the opaque array :*************
    ld     hl,level_buffer
    ld     de,opaque
    ld     b,128

opaque_create_loop_1
    push   bc

    ld     c,0
    ld     b,8
opaque_create_loop_2
    ld     a,(hl)
    inc    hl
    call   is_a_opaque
    jp     c,a_is_opaque
; a is not opaque :
    sla    c
    jp     end_opaque_create_loop_2
a_is_opaque
    scf
    rl    c
end_opaque_create_loop_2
    djnz    opaque_create_loop_2


    ld    a,c
    ld    (de),a
    inc    de

    pop    bc
    djnz    opaque_create_loop_1


;****************** opaque array created ******************

 IFDEF DISPLAYOPAQUEARRAY

    ld    hl,opaque
    ld    de,grbuf

    ld    a,32
loop_disp_opq
    ldi
    ldi
    ldi
    ldi
    inc    de
    inc    de
    inc    de
    inc    de
    inc    de
    inc    de
    inc    de
    inc    de

    dec    a
    jp    nz,loop_disp_opq

    xor    a
    ld    (fastCopyinv),a
    call    nodifastcopy

    call    getcsc_pause

 ENDIF

; *****************************

    call    temporisateur

    jp    PreMainLoop


temporisateur
    xor    a
    ei
tempo
    push af
tempo1
    call checkmessages
    or a
    jr nz,tempo1
    pop af
    rst #30
    dec    a
    jr    nz,tempo
    ret



; *****************************


decompress_level    ; TOTALLY UNTESTED
    ld      de,level_buffer
    call    lzss_decompress

    push    hl  ; save pointer to enemy data
    ; now unpack data (1 byte -> 2 bytes)

; debug code :
;     ld  hl,uncompressed_data
;     ld  de,level_buffer
;     ld  bc,900
; loop_test_lzss
;     ld  a,(de)
;     cp  (hl)
;     jp  nz,error_lzss
;     dec bc
;     inc hl
;     inc de
;     ld  a,b
;     or  c
;     jp  nz,loop_test_lzss
;     ld  hl,107
;     call _disphl
;     call getcsc_pause
; error_lzss
;     ld  hl,666
;     call _disphl
;     call getcsc_pause

    call    fill_frame_walls
    pop     hl
    ret

;*******************************
remaining_length dw 0
last_byte   dw 0

fill_frame_walls
    ld      ix,level_buffer
    ld      hl,900
    ld      (remaining_length),hl
    ld      hl,level_buffer + 900 - 1
    ld      (last_byte),hl

    ;begin
    ld      a,32
    call    insert_walls

    ; loop 30 times minus the last time
    ld      b,29
decomp_body_loop
; in this loop :
; ix : pointer to the current level_buffer byte
    push    bc

    ld      a,1
    call    insert_walls
    ld      a,30
    call    advance_without_moving
    ld      a,1
    call    insert_walls

    pop     bc
    djnz    decomp_body_loop
    ; 31th and 32th line
     ld      a,1
    call    insert_walls
    ld      a,30
    call    advance_without_moving
    ld      a,33
    call    put_walls
    ret

insert_walls
    call    move_further
    call    put_walls
    ret

move_further
;input : a = space
    ld  hl,(last_byte)
    ld  d,0
    ld  e,a
    add hl,de
    ex  de,hl
    ld  hl,(last_byte)
    ld  (last_byte),de
    ld  bc,(remaining_length)
    lddr
    ret

put_walls
; a = number of walls to insert
    ld  b,a
loop_put_walls_a
    ld      (ix),1   ; code for 'wall'
    inc     ix
    djnz    loop_put_walls_a
    ret

advance_without_moving
;input: a = how many to advance
    ld      d,0
    ld      e,a
    add     ix,de
    ld      hl,(remaining_length)
    or      a
    sbc     hl,de
    ld      (remaining_length),hl
    ret
