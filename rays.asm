RayCast
	xor	a
	ld	(dimension),a
	ld	(n_sprite),a
	ld	(n_spritea),a

	ld	a,(xperso_v+1)
	ld	(x_player_int),a
	ld	a,(zperso_v+1)
	ld	(z_player_int),a

	ld	a,48		; how many rays to cast
	ld	(numray),a

	ld	a,(orientation)
	sub	24
	jp	p,not_ret_angle
	add	a,120
not_ret_angle
	ld	(ray_angle),a	; init ray angle

; clear visited array
	ld	hl,visited
	ld	de,visited+1
	ld	bc,127
	ld	(hl),0
	ldir

; mark player in the visited array
	ld	hl,visited
	ld	a,(z_player_int)
	add	a,a
	add	a,a
	ld	c,a
	ld	b,0
	add	hl,bc
	
	ld	a,(x_player_int)
	ld	c,a
	srl	c
	srl	c
	srl	c
	add	hl,bc
	ex	de,hl
	ld	hl,masks
	and	%00000111
	ld	c,a
	add	hl,bc
	ld	a,(hl)
	ex	de,hl
	or	(hl)
	ld	(hl),a

ChooseRoutine
	ld	a,(ray_angle)
	ld	hl,Cast
	cp	15
	jp	m,routine1
	cp	30
	jp	m,routine2
	ld	hl,Cast60
	cp	45
	jp	m,routine2
	cp	60
	jp	m,routine1
	ld	hl,Cast90
	cp	75
	jp	m,routine1
	cp	90
	jp	m,routine2
	ld	hl,Cast120
	cp	105
	jp	m,routine2
;	jp	routine1

routine1:
	ld	(incx_15+1),hl
	xor	a
	ld	(zray),a
	ld	(xray),a
	ld	a,(ray_angle)
	ld	hl,dxrayon_l
	ld	d,0
	ld	e,a
	add	hl,de
	ld	a,(hl)
	ld	(dx),a
	sra	a
	neg
	ld	(S),a
	ld	hl,dzrayon_l
	sbc	hl,de
	ld	a,(hl)
	ld	(dz),a
trace15:
	ld	a,(xray)
	ld	b,a
	ld	a,(dx)		;while xray<dx do goto sub_30
	cp	b
	jp	p,incx_15
	jp	NewRay
incx_15:
	call	$0000
	ld	hl,xray
	inc	(hl)
	ld	a,(dz)		;S=S+dz
	ld	hl,S
	add	a,(hl)
	ld	(hl),a
	jp	m,trace15
incy_15:
	ld	hl,zray
	inc	(hl)		;zray=zray+1
	ld	a,(dx)
	ld	b,a
	ld	a,(S)		;S=S-dx
	sub	b
	ld	(S),a
	jp	trace15

routine2:
	ld	(incz_30+1),hl
	xor	a		;initialise z et xray a 0
	ld	(zray),a
	ld	(xray),a
	ld	a,(ray_angle)
	ld	hl,dxrayon_l
	ld	d,0
	ld	e,a
	add	hl,de
	ld	a,(hl)
	ld	(dx),a
	ld	hl,dzrayon_l-1
	sbc	hl,de
	ld	a,(hl)
	ld	(dz),a
	sra	a
	neg
	ld	(S),a
trace30:
	ld	a,(zray)
	ld	b,a
	ld	a,(dz)		;while zray<dz do goto sub_30
	cp	b
	jp	p,incz_30
	jp	NewRay
incz_30:
	call	$0000
	ld	hl,zray
	inc	(hl)
	ld	a,(dx)		;S=S+dx
	ld	hl,S
	add	a,(hl)
	ld	(hl),a
	jp	m,trace30
incx_30:
	ld	hl,xray
	inc	(hl)		;xray=xray+1
	ld	a,(dz)
	ld	b,a
	ld	a,(S)		;S=S-dz
	sub	b
	ld	(S),a
	jp	trace30

;^.^.^.^.miscellaneous routines ^.^.^.^.
NewRay
	ld	hl,numray
	dec	(hl)
	ret	z


	ld	a,(n_sprite)	;reinitialise l'ordonnee de la liste des
	ld	(n_spritea),a	;sprites (pour ne pas afficher 2* la meme)
	or	a
	jp	z,suite_newray
	ld	bc,4
	push	bc
	ld	hl,listex_sprite
	ld	de,listex_sprite2
	ldir		;copie les coordonnees des sprites du rayon actuel
	pop	bc	;dans la liste 2 (dernier rayon)
	push	bc
	ld	hl,listez_sprite
	ld	de,listez_sprite2
	ldir
	pop	bc			;dans la liste 2 (dernier rayon)
	ld	hl,listesprite_ident
	ld	de,listesprite_ident2
	ldir

suite_newray
	xor	a
	ld	(n_sprite),a
	ld	hl,ray_angle
	inc	(hl)
	ld	a,120
	cp	(hl)
	jp	z,newray_2		    ;incremente l'angle sauf si il est egal a 120, dans ce
	jp	ChooseRoutine		;cas, anglerayon=1
newray_2:
	xor	a
	ld	(hl),a			;reset anglerayon=0
	ld	hl,Cast
	jp	routine2

Cast
	ld	a,(x_player_int)
	ld	hl,xray
	add	a,(hl)
	ld	b,a
	ld	a,(z_player_int)
	ld	hl,zray
	add	a,(hl)
	ld	c,a
	call	ReadMap
	jp	checkwall

Cast60
	ld	a,(xray)
	ld	b,a
	ld	a,(x_player_int)
	sub	b
	ld	b,a
	ld	a,(z_player_int)
	ld	hl,zray
	add	a,(hl)
	ld	c,a
	call	ReadMap
	jp	checkwall

Cast90
	ld	a,(xray)
	ld	b,a
	ld	a,(x_player_int)
	sub	b
	ld	b,a
	ld	a,(zray)
	ld	c,a
	ld	a,(z_player_int)
	sub	c
	ld	c,a
	call	ReadMap
	jp	checkwall

Cast120
	ld	a,(x_player_int)
	ld	hl,xray
	add	a,(hl)
	ld	b,a
	ld	a,(zray)
	ld	c,a
	ld	a,(z_player_int)
	sub	c
	ld	c,a
	call	ReadMap
	jp	checkwall



ReadMap
	push	bc		; hl = monde+ c*32+b
	ld	h,0		; taille d'une ligne = 32
	ld	l,c
	add	hl,hl
	add	hl,hl
	add	hl,hl
	add	hl,hl
	add	hl,hl ; hl=hl*32
	ld	e,b
	ld	d,0
	add	hl,de
	ld	de,level_buffer
	add	hl,de
	ld	a,(hl)
	pop	de
	ret

checkwall:
	cp	1
	jp	z,aff_mur

	push	af
	push	de
; mark the current coordinates into the visited array
	call	access_visited_array
	or	(hl)
	ld	(hl),a
	pop	   de
	pop    af
	
	or     a
	ret    z
	ld     (object_coords),de
	ld     (matnum),a
	jp     sprite_aff

aff_mur
	ld     hl,(tempcoo)
	call   cp_hl_de
	jp     z,fin_aff_mur
suiteaff_mur
	ld     (tempcoo),de
	ld     hl,listex
	ld     a,(dimension)
	ld     b,0
	ld     c,a
	add    hl,bc
	ld     a,d
	ld     (hl),a
	ld     hl,listez
	add    hl,bc
	ld     a,e
	ld     (hl),a
	ld     hl,dimension
	inc    (hl)
fin_aff_mur
	pop    hl
	jp     NewRay

sprite_aff:
	ld     a,(n_spritea)
	or     a
	jp     z,suiteaffsprite
	ld     hl,listex_sprite2
	ld     b,a
	ld     a,d
sprite_aff2:
	cp     (hl)
	jp     z,suiteverifsprite
sprite_aff22:
	inc    hl
	djnz   sprite_aff2
suiteaffsprite:
	call   ecriturelistesprite
	jp     spritesaff
suiteverifsprite:
	push   hl
	push   bc
	ld     bc,listez_sprite2-listex_sprite2
	add    hl,bc
	ld     a,e
	cp     (hl)
	pop    bc
	pop    hl
	jp     z,ecriturelistesprite
	ld     a,d
	jp     sprite_aff22
ecriturelistesprite:
	ld     a,(n_sprite)
	ld     hl,listex_sprite
	ld     c,a
	ld     b,0
	add    hl,bc
	ld     (hl),d
	ld     hl,listez_sprite
	add    hl,bc
	ld     (hl),e
	ld     hl,listesprite_ident
	add    hl,bc
	ld     (hl),b			;(hl)=b=0
	inc    a
	ld     (n_sprite),a
	ret
	
access_opaque_array
; input : a=X, e=Y ; called in linedraw2
	ld     hl,opaque
	push   hl
	jp     access_128_array_opaque

access_visited_array
; input : d=X, e=Z
	ld	hl,visited
	push	hl
access_128_array
; input : d=X, e=Z
; output : hl = address of the point, a = mask
; destroyed : ABCDEHL
	ld	a,d
access_128_array_opaque
	ld	d,0
	sla	e
	sla	e
	ld	h,0
	ld	l,a
	srl	l
	srl	l
	srl	l
	add	hl,de
	pop	de
	add	hl,de
	ex	de,hl
	and	%00000111
	ld	b,0
	ld	c,a
	ld	hl,masks
	add	hl,bc
	ld	a,(hl)
	ex	de,hl
	ret
