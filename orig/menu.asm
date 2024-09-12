disp_menu
	xor	a
	ld	(nmenu),a
	ld	a,1
	ld	(tempdifficulty),a

	ld	(sp_2),sp	; save stack pointer address
	ld	hl,menu_loop
	ld	(tempreturn),hl	; save return address

menu_loop
	ld	a,$2f	; CPL
	ld	(fastCopyinv),a	; invert display

	call	clrgbuf_fast
	ld	a,24
	ld	l,0
	ld	b,20
	ld	c,6
	ld	ix,spritemenu
	call	largesprite

	ld	hl,t_z
	ld	de,18+256*2
	call	fastvputs

	ld	hl,version
	ld	de,70+256*56
	call	fastvputs

	ld	hl,t_continue
	ld	de,30+256*25
	call	fastvputs

	ld	hl,t_newgame
	ld	de,30+256*33
	call	fastvputs

	ld	hl,t_options
	ld	de,30+256*41
	call	fastvputs

	ld	hl,t_quit
	ld	de,30+256*49
	call	fastvputs

	ld	a,(nmenu)
	add	a,a
	add	a,a			;a=17+8*nmenu
	add	a,a
	add	a,25
	ld	(penrow),a
	ld	a,25
	ld	(pencol),a

	ld	a,'>'
	call	vputmap

	call	nodifastcopy
	call	keyboard_pause

	bit 5,a        ; up
	jr	z,menumoins
        bit 6,a        ; down
	jr	z,menuplus
        bit 0,a        ; space
	jp	z,menuok
        bit 2,a        ; esc
	jp	nz,menu_loop
	ret	; leaves the menu, thus the program

/*
    ld a,(ValidKey+0)          ; up
    and %00001000
	jr	z,menumoins
    ld a,(ValidKey+0)          ; down
    and %00000001
	jp	z,menuplus
    ld a,(ValidKey+6)          ; 2nd
    and %00100000
	jp	z,menuok
    ld a,(ValidKey+6)          ; mode
    and %01000000
	jp	nz,menu_loop
	ret	; leaves the menu, thus the program
*/
menuplus:
	ld	a,(nmenu)
	cp	3
	jr	z,retourmenuplus
	inc	a
	ld	(nmenu),a
	jp	menu_loop
retourmenuplus:
	xor	a
	ld	(nmenu),a
	jp	menu_loop

menumoins:
	ld	a,(nmenu)
	or	a
	jp	z,retourmenumoins
	dec	a
	ld	(nmenu),a
	jp	menu_loop
retourmenumoins:
	ld	a,3
	ld	(nmenu),a
	jp	menu_loop

menuok:
	ld	a,(nmenu)
	or	a
	jp	z,continue
	dec	a
	jp	z,init_game
	dec	a
	jr	z,options
	ret


continue
	ld	a,(savegame)
	or	a
	jp	z,menu_loop
	jp	ContinueGame


options:
	xor	a
	ld	(moptions),a

options_b:
	call	clrgbuf_fast

	ld	a,24
	ld	l,0
	ld	b,20
	ld	c,6
	ld	ix,spritemenu
	call	largesprite

	ld	hl,t_z
	ld	de,18+256*2
	call	fastvputs

	ld	hl,t_options2
	ld	de,30+256*25
	call	fastvputs


	ld	de,30+256*41
	ld	hl,t_difficulty
	call	fastvputs

	ld	de,30+256*49

	ld	a,(tempdifficulty)
	or	a
	jr	z,facile
	dec	a
	jr	z,moyen
	jr	dur
facile:
	ld	hl,t_tooyoung
	jr	options_loop_end

moyen:
	ld	hl,t_hurtme
	jr	options_loop_end

dur:
	ld	hl,t_ultraviolence

options_loop_end
	call	fastvputs

	ld	a,(moptions)
	add	a,a
	add	a,a
	add	a,a
	add	a,33+8			;a=a*2+2
	ld	(penrow),a
	ld	a,25
	ld	(pencol),a
	ld	a,'>'
	call	vputmap

	call	nodifastcopy
	call	keyboard_pause

	bit 7,a        ; right
	jr	z,optioninc
        bit 4,a        ; left
	jr	z,optiondec
        bit 0,a        ; space
	jp	z,menu_loop
        bit 2,a        ; esc
	jp	z,menu_loop
	jp	options_b

 /*
    ld a,(ValidKey+0)  ; right
    and %00000100
	jr	z,optioninc
    ld a,(ValidKey+0)  ; left
    and %00000010
	jr	z,optiondec
;   ld a,(ValidKey+0)  ; up
;   and %00001000
;	jp	z,optionplus
;   ld a,(ValidKey+0)  ; down
;   and %00000001
;	jp	z,optionmoins
    ld a,(ValidKey+6)  ; 2nd
    and %00100000
	jp	z,menu_loop
    ld a,(ValidKey+6)  ; mode
    and %01000000
	jp	z,menu_loop
	jp	options_b
*/

optioninc:
	ld	a,(moptions)
;	or	a
;	jr	z,inccont

incdiff:
	ld	a,(tempdifficulty)
	cp	2
	jp	z,options_b
	inc	a
	ld	(tempdifficulty),a
	jp	options_b


optiondec:
	ld	a,(moptions)
;	or	a
;	jp	z,deccont
decdiff:
	ld	a,(tempdifficulty)
	or	a
	jp	z,options_b
	dec	a
	ld	(tempdifficulty),a
	jp	options_b


optionplus:
	ld	a,(moptions)
	or	a
	jp	z,options_b
	dec	a
	ld	(moptions),a
	jp	options_b

optionmoins:
	ld	a,(moptions)
	cp	1
	jp	z,options_b
	inc	a
	ld	(moptions),a
	jp	options_b
