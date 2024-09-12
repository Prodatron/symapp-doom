disp_menu
    	xor	a
    	ld	(nmenu),a
    	ld	a,1
    	ld	(tempdifficulty),a
    	ld	(sp_2),sp	        ; save stack pointer address
    	ld	hl,menshw
    	ld	(tempreturn),hl	    ; save return address

menshw  ld hl,winmengrp         ;MAIN MENU
        call gamshw1

menlop  ld a,(nmenu)            ;main menu loop
        ld c,4
        ld hl,56
        ld de,winmen_cur
        call menchs
        ld (nmenu),a
        dec l
        jr nz,menlop1
        sub 1
        jr c,newshw
        jp z,optshw
        dec a
        jp z,readthis
        ret
menlop1 ld a,(savegame)         ;esc
	    or a
	    jp nz,gamcnt
        ld a,3
        ld (nmenu),a
        ld de,56
        ld ix,winmen_cur
        call mencur
        jr menlop

newshw  ld hl,winsklgrp         ;SKILL MENU
        call newlop1

newlop  ld a,(tempdifficulty)   ;skill menu loop
        ld c,3
        ld hl,66
        ld de,winskl_cur
        call menchs
        ld (tempdifficulty),a
        dec l
        jp z,gamnew
newlop2 ld hl,winmengrp         ;esc -> go back to main menu
        call newlop1
        jr menlop
newlop1 ld (syswininf+36),hl
        ld e,-1
        ld c,MSC_DSK_WINPIN
        ld hl,56
        ld (prgmsgb+06),hl
        ld hl,112
        ld (prgmsgb+08),hl
        ld hl,39
        ld (prgmsgb+10),hl
        ld hl,0
        jp msgsnd2

optsel  db 0
optcolb db 8    ;background colour
optcolf db 1    ;foreground colour

optshw  ld hl,winoptgrp         ;OPTIONS MENU
        call newlop1

optlop  ld a,(optsel)
        ld c,3
        ld hl,66
        ld de,winopt_cur
        call menchs
        ld (optsel),a
        dec l
        jr nz,optlop1
        cp 1
        jr c,optfor
        jr z,optbck
        ld hl,(optcolb)
        ld a,l
        ld l,h
        ld h,a
        ld (optcolb),hl
        call optupd
        jr optlop
optlop1 call syschk
        jr newlop2
optfor  ld hl,optcolf
        jr optbck1
optbck  ld hl,optcolb
optbck1 inc (hl)
        res 4,(hl)
        call optupd
        jr optlop

optupd  ld hl,(optcolb)
        ld ix,winopt_col
        ld a,l
        add 192
        ld (winopt_col+4+16),a
        ld a,h
        add 192
        ld (winopt_col+4),a
        ld de,winopt_col_num*256+256-2
        ld c,MSC_DSK_WINDIN
        jp msgsnd2


;### choose a menu item
;### A=current item, C=number of items, HL=y offset, DE=control data record of skull cursor
;### -> A=new item, L=action (0=esc, 1=select)
menchsn db 0

menchs  ld (menchsn),a
        ld a,c
        ld (menchs3+1),a
        ld (menchs5+1),hl
        ld (menchs6+2),de
menchs0 call keyboard_pause
        cp 136              ;up
    	jr	z,menchs1
    	cp 137              ;down
    	jr	z,menchs2
    	cp 32               ;space
    	jr	z,menchs7
    	cp 13               ;return
    	jr	z,menchs7
        cp 27               ;esc
        jr z,menchs8
        jr menchs0
menchs1 ld a,(menchsn)
        sub 1
        jr c,menchs0
        jr menchs4
menchs2 ld a,(menchsn)
        inc a
menchs3 cp 0
        jr nc,menchs0
menchs4 ld (menchsn),a
menchs5 ld de,0
menchs6 ld ix,0
        call mencur
        jr menchs0
menchs7 ld l,1
        jr menchs9
menchs8 ld l,0
menchs9 ld a,(menchsn)
        ret

mencur  ld l,(ix+8+16+0)     ;refresh cursor
        ld h,(ix+8+16+1)
        ld (ix+8+0),l
        ld (ix+8+1),h
        add a
        ld c,a
        add a:add a
        add c
        ld l,a
        ld h,0
        add hl,de
        ld (ix+8+16+0),l
        ld (ix+8+16+1),h
        ld de,256*winmen_cur_num+256-2
        ld c,MSC_DSK_WINDIN
        jp msgsnd2


;### actions
gamnew  ld a,50                 ;start new game
        ld ix,hud_ammo          ;init hud
        call dspnum0
        ld a,HEALTH_MAX
        ld ix,hud_hlth
        call dspnum0
        ld hl,face0
        ld (hud_face+4),hl
        ld a,-1
        call clrgbuf_fast1      ;prevent display of an old game
        call nodifastcopy
        call gamshw
        jp init_game
gamcnt  call gamshw             ;continue game
        jp ContinueGame
gamshw  ld hl,wingamgrp         ;show game screen
gamshw1 ld (syswininf+36),hl
        ld e,-1
        ld c,MSC_DSK_WINDIN
        jp msgsnd2

readthis
        call SySystem_HLPOPN
        jp menlop
