DisplayWalls
    ld    a,120
    ld    hl,orientation
    sub    (hl)
    ld    (orientation2),a    ;makes trig calculations easier

    xor    a
    ld    (increment),a

;///////////////////////////////////////
; find coordinates of the 1st point
;//////////////////////////////////////

; loop until it finds a point before the player
find_first_point_to_display
    ld      a,(increment)
    ld       hl,listex
    call       ReadList
    ld       bc,8
    add       hl,bc
    ld       de,(x_player)
    sbc       hl,de
    ld       (variationx),hl

    ld       a,(increment)
    ld       hl,listez
    call       ReadList
    ld       bc,8
    add        hl,bc
    ld       de,(z_player)
    sbc       hl,de
    ld       (variationz),hl

    call    is_object_before_player
    jp      c,first_point_found
    ld      hl,increment
    inc     (hl)
    jp      find_first_point_to_display

first_point_found
    call       calcul_x_y

    ld       hl,(temp_wall)
    ld       (old_wall_pos),hl
    ld       hl,increment
    inc       (hl)

;//////////////////////////////
; display the lines
;//////////////////////////////

draw_lines_loop
    call    decouilleur
    ld    a,(increment)
    ld    hl,listex
    call    ReadList
    ld    de,8
    add    hl,de
    ld    de,(x_player)
    sbc    hl,de
    ld    (variationx),hl

    ld    a,(increment)
    ld    hl,listez
    call    ReadList
    ld    de,8
    add    hl,de
    ld    de,(z_player)
    sbc    hl,de
    ld    (variationz),hl

    call    is_object_before_player
    ret     nc ; stop once we find a point behind the player. otherwise, it creates line bugs


    call    calcul_x_y

    ld    de,(old_wall_pos)
    ld    hl,(temp_wall)
    ld    (old_wall_pos),hl

    xor     a
    ld      (walls_delta_height1),a
    ld      (walls_delta_height2),a
    ld      a,(game_state)
    cp      STATE_DYING
    jp      z,modify_wall_dying_dead
    cp      STATE_DEAD
    jp      z,modify_wall_dying_dead
    jp      dont_modify_wall_pos

modify_wall_dying_dead
    push    de
    push    hl
    ld      a,32
    sub     e
    ld      h,a
    ld      a,(falling_counter)
    ld      l,a
    call    MUL_HL_T
    call    divhlby32_pos
    ld      a,l
    ld      (walls_delta_height1),a

    pop     hl
    push    hl

    ld      a,32
    sub     l
    ld      h,a
    ld      a,(falling_counter)
    ld      l,a
    call    MUL_HL_T
    call    divhlby32_pos
    ld      a,l

    ld      (walls_delta_height2),a
    pop     hl
    pop     de
dont_modify_wall_pos

    ld  a,(tempcouille)
    or  a
    jp  nz,couilleaff

    push    de
    push    hl

    ld      a,(walls_delta_height1)
    ld      b,a
    ld      a,e
    sub     b
    ld      e,a

    ld      a,(walls_delta_height2)
    ld      b,a
    ld      a,l
    sub     b
    ld      l,a

;   (D, E) = (x0, y0), the first point  (H, L) = (x1, y1), the second point
    call    lineClipAndDraw ; display the higher line

    pop    hl
    pop    de

    push    de
    push    hl
couilleaff

    call    vlinecentered ; vertical centered lines routine

    ld    a,(tempcouille)
    or    a
    jp    nz,couilleaff2

    pop    hl
    pop    de

    ld    a,63
    sub    l
    ld    l,a

    ld    a,63
    sub    e
    ld    e,a

    ld      a,(walls_delta_height1)
    ld      b,a
    ld      a,e
    sub     b
    ld      e,a

    ld      a,(walls_delta_height2)
    ld      b,a
    ld      a,l
    sub     b
    ld      l,a

    call    lineClipAndDraw ; display the lower line

couilleaff2
    ld    hl,increment
    inc    (hl)
    ld    a,(dimension)
    cp    (hl)
    ret    z        ; end of loop
    jp    draw_lines_loop

;////////////////////////////////////////////

; this routine lets know if we need or not to make a line with the
; last 2 points
; if no : (tempcouille) !=0
decouilleur
    xor    a
    ld    (tempcouille),a
    ld    a,(increment)
    cp    1
    ret    m
    ld    hl,listex
    ld    e,a
    ld    d,0
    add    hl,de
    ld    c,(hl)
    dec    hl
    ld    a,(hl)
    sub    c
    jp    p,no_neg_01
    neg
no_neg_01
    ld    b,a

    ld    hl,listez
    ld    a,(increment)
    ld    e,a
    ld    d,0
    add    hl,de
    ld    c,(hl)

    dec    hl
    ld    a,(hl)
    sub    c
    jp    p,no_neg_02
    neg
no_neg_02
    add    a,b

    cp    2
    jp    z,couille_verif
    jp    p,couille
    ret
couille_verif    ;il faut savoir si c'est sur x ou sur z qu'on a une couille
    ld    hl,listex
    ld    a,(increment)
    ld    e,a
    ld    d,0
    add    hl,de
    ld    c,(hl)
    dec    hl
    ld    a,(hl)
    ld    e,a
    sub    c
    jp    p,no_neg_03
    neg
no_neg_03
    cp    2
    jp    z,couille_x
    ld    a,c
    ld    b,a
    ld    hl,listez
    ld    a,(increment)
    ld    e,a
    ld    d,0
    add    hl,de
    ld    a,(hl)
    dec    hl
    ld    c,(hl)
    ld    e,a
    sub    c        ;on verifie si ca couille sur z
    jp    p,no_neg_04
    neg
no_neg_04
    cp    2
    ret    nz
    ld    a,e
    add    a,c
    sra    a
    ld    c,a
suite_couille:
    call    ReadMap        ;bc = coordonnees
    cp    1
    ret    z
couille
    ld    a,1
    ld    (tempcouille),a
    ret


couille_x:
    ld    a,e
    add    a,c
    sra    a
    ld    b,a
    ld    hl,listez
    ld    a,(increment)
    ld    e,a
    ld    d,0
    add    hl,de
    ld    a,(hl)
    ld    c,a
    jp    suite_couille

;////////////////////////////
;///////////////////////
;//////////////////

DisplayEnemies
; we check in the "visited" array
; if an enemy has been seen though the raycaster,
; if yes, it is displayed.
    ld    ix,enemiesdata
    ld    b,(ix)    ; number of enemies
    inc    ix

d_e_loop
    push    bc
        ld    l,(ix+0)
        ld    h,(ix+1) ; enemy_X
        ld    de,8
        add    hl,de
        ld    (tempmonstrex),hl
        ld    a,h

        ld    l,(ix+2)
        ld    h,(ix+3); enemy_Z
        ld    de,8
        add    hl,de
        ld    (tempmonstrey),hl
        ld    e,h
        ld    d,a ; DE = coordinates for access_visited_array

;        ld    (tempmonstre),de; not necessary

; TODO manage enemy's type, animation and orientation
        push ix
        pop hl
        ld bc,ENEMY_ORIENTATION_OFFSET
        add hl,bc
        ld b,(hl)
        ; substract with player orientation
        ld a,(orientation)
        sub b
        jp p,d_e_loop_no_neg_a
        neg
d_e_loop_no_neg_a
        cp 15
        jp m,d_e_loop_sprite_back
        cp 44                                  ; makes it easier to see
        jp m,d_e_loop_sprite_left              ; enemies as facing, because of
        cp 76                                  ; set_enemy_orientation in ai.z80
        jp m,d_e_loop_sprite_front
        cp 105
        jp m,d_e_loop_sprite_right
d_e_loop_sprite_back
        ld bc,BACK
        jp d_e_loop_sprite_chosen
d_e_loop_sprite_left
        ld bc,LEFT
        jp d_e_loop_sprite_chosen
d_e_loop_sprite_front
        ld bc,FACE
        jp d_e_loop_sprite_chosen
d_e_loop_sprite_right
        ld bc,RIGHT
        jp d_e_loop_sprite_chosen
d_e_loop_sprite_chosen
        push bc
            push ix
            pop hl
            ld  bc,ENEMY_SPRITE_SET_OFFSET
            add hl,bc
            call ld_hl_mhl ; now hl->sprite set (face, left, right, back)    ; MAYBE MOVE THIS..... (*)
        pop bc
        add hl,bc
        call ld_hl_mhl ; get sprite
        ld    (CurrentEnemySpriteAddress),hl
        push ix
        pop hl
        ld bc,ENEMY_LIFE_OFFSET
        add hl,bc
        ld    (CurrentEnemyLifeAddress),hl

; verify if the monster is seen through the raycaster
        push    ix
            call    access_visited_array
            and    (hl)
            call    nz,spritesaff_monster
        pop    ix
    ld  de,16
    add ix,de
    pop    bc
    ;djnz    d_e_loop
    dec b
    jp nz,d_e_loop
    ret

CurrentEnemySpriteAddress dw 0
CurrentEnemyLifeAddress   dw 0

spritesaff_monster

    ld    a,15
    ld    (matnum),a // crappy hack

; this way, the enemies' offset are taken into account
tempmonstrex equ $+1
    ld    hl,0
    call    divhlby16_pos
    ld    bc,8
    add    hl,bc
    ld    bc,(x_player)
    sbc    hl,bc        ;x-x_player=variationx(16)
    ld    (variationx),hl

tempmonstrey equ $+1
    ld    hl,0
    call    divhlby16_pos
    ld    bc,8
    add    hl,bc
    ld    bc,(z_player)
    sbc    hl,bc        ;z-z_player=variationz(16)
    ld    (variationz),hl
    jp    monstres

spritesaff
    ld    de,(object_coords)
    ld    h,0
    ld    l,d
    add    hl,hl
    add    hl,hl
    add    hl,hl
    add    hl,hl        ; put x*16 into hl
    ld    bc,8
    add    hl,bc
    ld    bc,(x_player)
    sbc    hl,bc        ;x-x_player=variationx(16)
    ld    (variationx),hl

    ld    h,0
    ld    l,e
    add    hl,hl
    add    hl,hl
    add    hl,hl
    add    hl,hl        ; put z*16 into hl
    ld    bc,8
    add    hl,bc
    ld    bc,(z_player)
    sbc    hl,bc        ;z-z_player=variationz(16)
    ld    (variationz),hl

    ld    a,(matnum)

; problem here : there is no test in case there is a forbidden value un (matnum)
; so changing the map by hand can drive to horrible bugs
    dec    a
    dec    a
    ld     h,0
    add    a,a
    ld     l,a
    ld     de,sprites_table
    add    hl,de
    call    ld_hl_mhl
    ld      a,l
    or      h
    ret     z ; if there's no sprite, display nothing
    jp      suitesaff2

sprites_table
    dw        soins1 ; 2
    dw      full_life ; 3
    dw      gun_ammo ; 4
    dw      chaingun_ammo ; 5
    dw        shotgun_ammo ; 6
    dw        full_ammo ; 7
    dw      chaingun_on_the_ground ; 8
    dw      shotgun_on_the_ground ; 9
    dw      0 ; chainsaw_on_the_ground ; 10 ;
    dw      berserk_ring ; 11
    dw        end_of_level_flag ; 12
    dw      0 ; 13 = respawn spot
    dw      column ; 14
    dw        barrel ; 15

monstres
; handles living, dying and dead enemies

    ld    hl,(CurrentEnemySpriteAddress)

suitesaff2:
    ld        (temp_sprite_address),hl

    call is_object_before_player
    ret nc

    call    calcul_x_y

    ld      (temp_x_center),a
    ld        hl,(temp_wall)

     ld        a,l


; this test is here to prevent displaying sprites whose
; Z coordinate on screen is too low
      cp        -41 ; why ?

      ret        m

    ld        e,15 ; full size
    or        a
    jp        m,spritesuiteaff
    ld        e,2  ; tiny size
    cp        25
    jp        p,spritesuiteaff

    push    hl
    push    bc
    ld        hl,LUT_dezoom_sprites
    ld        c,a
    ld        b,0
    add        hl,bc
    ld        e,(hl)
    pop        bc
    pop        hl

spritesuiteaff
    ld        a,e
    ld        (spritedezoom),a

    call    read_sprite_data ; b hauteur, c largeur, l decalageY, a posX, ix addr. sprite
    ld        (tempxt),a

; Pour calculer largeur_en_pixel/2 et posY en fct du dezoom :
    call    rectifie_selon_dezoom

    ld      a,(temp_x_center)
    ld      d,a

    ld        a,(tempxt)    ;largeur_en_pixels/2
    sub      d              ;
    neg            ;a=left coord of the sprite

    push    hl
    ld      hl,spritedezoom
    ld      e,(hl)
    pop     hl

    push    af   ; a modifier pour le tombage a terre
    ld        a,32     ; horizon
    sub        l
    ld        l,a
    pop        af

; only enter this section if player is dead or dying
    push    af    ; -> pop af
    ld      a,(game_state)
    cp      STATE_DYING
    jp      z,correct_spr_disp_if_dead_or_dying
    cp      STATE_DEAD
    jp      z,correct_spr_disp_if_dead_or_dying
    pop     af  ; indispensable
    jp      dont_correct_spr_disp

correct_spr_disp_if_dead_or_dying
    pop     af  ; indispensable
    push    af
    push    bc
    push    de
    push    hl
    call    getScaledSize_adapted;b,c,e->a,h
    ld      (scaled_sprite_height),a
    pop     hl
    add     a,l ; a = height of scaled sprite, l = posY
    sub     32 ; get distance below horizon
    push    af
    ld      a,(falling_counter)
    ld      b,a
    ld      a,32
    sub     b
    ld      b,a
    pop     af
    ld      h,a
    ld      l,b
    call    MUL_HL_T
    call    divhlby32_pos ; new distance below horizon is in l
    ; add sprite height
    ld      a,(scaled_sprite_height)
    ld      b,a
    ld      a,l
    sub     b
    add     a,32
    ld      l,a ; corrected posY
    pop     de
    pop     bc
    pop     af
dont_correct_spr_disp

    push    hl
    push    af
    push    bc
    call    scaled_sprite   ; a = x , l = y ; b=height (in pixels) c=width (in bytes, e.g. 2 would be 16)
    ; e=ratio de dezoom de 0 a 15 (15:taille normale,0:minuscule)
    pop        bc
    pop        af
    pop        hl

    ld        b,a    ; save the sprite's left coord beginning
    ld        a,(power)
    dec     a    ; cp    1
    call    p,shoot_enemy
    ret

shoot_enemy
    ld    a,(matnum)    ; sprite.X is in b
    cp    15
    ret    nz

    push    hl
    ld    hl,(CurrentEnemyLifeAddress)
    ld    a,(hl)
    pop    hl
    or    a
    ret    z    ; if enemy dead, it doesn't "take shots"

    ld    a,(tempxt)
     sla    a        ; sprite width
    add    a,b
    ld    c,a
    cp    SCREEN_WIDTH/2
    ret    c

    ld    a,b
    cp    SCREEN_WIDTH/2
    ret    nc

    ld    a,c
    sub    3
    inc    l
    inc    l
    inc    l

    ld    b,5
    ld    c,1
    ld    ix,sang1
    call    largesprite

    ld    hl,(CurrentEnemyLifeAddress)


    ld    a,(power)
    ld    e,a
    sub    6        ; each time a monster takes the shot, the power of the shot loses 6
    jp    p,monstre_2
    xor    a
monstre_2
    ld    (power),a

    ld    a,(hl)
    cp    ENEMY_DYING+1
    ret    m
    sub    e        ;health-puissance
    cp    ENEMY_DYING+1
    jp    m,KillEnemy
    ld    (hl),a

 IFDEF ULTRAPOWER
    ld  (hl),ENEMY_DYING
 ENDIF

    inc    hl
    ld    (hl),1        ; acticate AI flag
    ret

KillEnemy
    ld    (hl),ENEMY_DYING
    ret

;ecrituremat:
;    ld    bc,(tempmonstre)
;    ld    h,0
;    ld    l,c
;    add    hl,hl
;    add    hl,hl
;    add    hl,hl
;    add    hl,hl
;    add    hl,hl    ; HL = HL*32
;    ld    e,b
;    ld    d,0
;    add    hl,de
;    ld    de,monde
;    add    hl,de
;    ld    (hl),a
;    ret

;///////////////////////////////
read_sprite_data
    ld     hl,(temp_sprite_address)    ; adresse du sprite
    ld     b,(hl)    ; hauteur
    inc    hl
    ld     c,(hl)    ; largeur
    inc    hl
    ld     e,(hl)    ; decalageY
    inc    hl
    ld     a,(hl)    ; posX (= largeur en pixels/2)
    inc    hl
    push   hl
    pop    ix    ; (ix) = sprite
    ld     l,e
    ret
;///////////////////////////////
rectifie_selon_dezoom
;    push   de
;    push   af

    push    hl
    push    bc

    ld        a,(spritedezoom)
    cp        15
    jp        z,don_t_modify_pos_X

    inc        a
    ld        l,a

    ld        a,(tempxt)    ; posX
    ld        b,a

    ld        h,0
    ld        d,h
    ld        e,h
    ex        de,hl
loop_b_times_l
    add        hl,de
    djnz    loop_b_times_l

    call    divhlby16_pos

    ld        a,l    ; l = posX adjusted
    ld        (tempxt),a
don_t_modify_pos_X

; now we adjust posY

    pop        bc
    ld        a,b
    srl        a    ; hauteur/2
    pop        hl
    push    bc

    ld        c,0
    sub        l
    jp        m,rect_posY_marque_negatif
    jp        suite_rect_posY
rect_posY_marque_negatif
    set        0,c
    neg
suite_rect_posY
    ld        l,a

    ld        a,(spritedezoom)
    cp        15
    jp        z,verifie_negatif_rect_pos_Y
    inc        a
    ld        b,a

    ld        h,0
    ld        d,h
    ld        e,h
    ex        de,hl
loop_b_times_l_bis
    add        hl,de
    djnz    loop_b_times_l_bis

    call    divhlby16_pos    ; l = posY corrige'

verifie_negatif_rect_pos_Y
    bit        0,c
    jp        z,fin_rectification_posY
    ld        a,l
    neg
    ld        l,a

fin_rectification_posY
    pop        bc

;    pop af
;    pop   de


    ret



calcul_x_y
    call    ReadSin
    ld      hl,(variationz)
    call    multhl_by_a    ;variationz*sin(orientation2)

    push    hl
    call    ReadCos
    ld        hl,(variationx)
    call    multhl_by_a    ;variationx*cos(orientation2)
    pop        bc

    or        a
    sbc        hl,bc        ;variationx*cos(orientation2)-variationz*sin(orientation2) = distance from player
    call    divhlby16

    push    hl
    call    hauteur_tab
    ld        (temp_wall),hl

    call    ReadCos        ;dans a
    ld        hl,(variationz)
    call    multhl_by_a        ;variationz*cos(orientation2)dans bc

    push    hl
    call    ReadSin
    ld        hl,(variationx)
    call    multhl_by_a        ;variationx*sin(orientation2)
    pop        bc

    add        hl,bc    ;variationx*sin(orientation2)+variationz*cos(orientation2)

    call    divhlby16

    ex        de,hl
    pop        bc

    call    div_bc_by_de

    add        a,SCREEN_WIDTH/2

    ld        hl,(temp_wall)
    ld        h,a
    ld        (temp_wall),hl

    ret


;////////////////////////////
;-------HL*A-signed----------
; A,H,L,D,E,B destroyed
; output : HL = HL*A (signed)
;----------------------------
;****************************
; en pratique, quand cette routine
; est employee : -16 < A < 16

multhl_by_a
    cp    1
    ret    z    ; HL*1=HL
    or    a
    jp    z,hl_zero; HL*0 = 0
    cp    -1
    jp    z,hl_is_neg ; HL*-1=-HL

    ld    b,0    ; B = flag saying if the result has to be <0

    bit    7,h    ; if HL<0 ...
    call    nz,hl_is_neg
    or    a    ; if A<0 ...
    call    m,a_is_neg

    ld    d,h
    ld    e,l
    dec    a
multhl_by_a_loop
    add    hl,de
    dec    a
    jp    nz,multhl_by_a_loop

    bit    0,b    ; does the result have to be negative ?
    ret    z
            ; if yes, invert it
hl_is_neg
    ex    de,hl
    ld    hl,0
    or    a
    sbc    hl,de
    inc    b
    ret

a_is_neg
    neg
    inc    b
    ret

hl_zero
    ld    hl,0
    ret

;/////////////////////////////////////
; calculates BC/DE*40
; BC and DE are between -160 and 160
; result A is between -79 and 79

; before, max was -80 and 80, but it made bugs
; with the line draw routine so now it's 79

DIVBCBYDE_MAX equ 79

div_bc_by_de
    ld    ix,0

    push    bc
    push    de
    pop    bc
    pop    de

    bit    7,b
    call    nz,bc_is_neg
    bit    7,d
    call    nz,de_is_neg

    ld    a,d
    or    e
    ld    a,DIVBCBYDE_MAX
    jp    z,end_div_bc_by_de ; avoid divide by zero

    ld    h,b
    ld    l,c
    add    hl,hl ;
    add    hl,hl ;
    add    hl,hl ;
    push    hl    ;
    add    hl,hl ;
    add    hl,hl ;
    pop    bc    ;
    add    hl,bc ; HL = HL * 40
Divide
    ld    c,l
    ld    a,h
    ld    hl,0 ; HL = remainder
    ld    b,16 ; 16 bits in dividend
    or    a ;clear carry
DivLoop
    rl    c
    rla
    rl    l
    rl    h
    push    hl
    sbc    hl,de
    ccf
    jr    c, Drop
    ex    (sp),hl
Drop
    inc    sp
    inc    sp
    djnz    DivLoop

    ex    de,hl
    rl    c
    ld    l,c
    rla
    ld    h,a  ; can we put ld h,0 ?

    push    hl
        ld    de,DIVBCBYDE_MAX
        or    a
        sbc    hl,de
    pop    hl

    call    p,divmax
    ld    a,l

end_div_bc_by_de
    push    ix
    pop    hl
    bit    0,l
    ret    z
    neg
    ret

divmax
    ld    l,DIVBCBYDE_MAX
    ret

bc_is_neg
    ld    hl,0
    or    a
    sbc    hl,bc
    ld    b,h
    ld    c,l
    inc    ix
    ret

de_is_neg
    ld    hl,0
    or    a
    sbc    hl,de
    ld    d,h
    ld    e,l
    inc    ix
    ret

;////////////////////////////////////////////////////////////////
ReadCos
    ld    hl,cosinus
    ld    a,(orientation2)
    ld    e,a          ;a position//utilise e,d,h,l,a
    ld    d,0
    add    hl,de       ; ajoute la position dans la liste
    ld    a,(hl)       ; va chercher la valeur et la met dans l
    ret

ReadSin
    ld    hl,sinus
    ld    a,(orientation2)
    ld    e,a          ;a position//utilise e,d,h,l,a
    ld    d,0
    add    hl,de       ; ajoute la position dans la liste
    ld    a,(hl)       ; va chercher la valeur et la met dans l
    ret

ReadList
    ld    e,a          ;a position//utilise e,d,h,l,a
    ld    d,0
    add    hl,de       ; ajoute la position dans la liste
    ld    l,(hl)       ; va chercher la valeur et la met dans l
    ld    h,0        ;hl*16 44 cycles!
    add    hl,hl
    add    hl,hl
    add    hl,hl
    add    hl,hl        ;on a la valeur dans hl
    ret

hauteur_tab:
    call    abs_hl        ; hl = |hl|
    ld    e,l          ;a position//utilise e,d,h,l,a
    ld    d,0
    ld    hl,walls_height
    add    hl,de       ; ajoute la position dans la liste
    ld    l,(hl)       ; va chercher la valeur et la met dans l
    ld  h,0
    ret



is_object_before_player
; out :  c : yes
;       nc : no (behind)
    call    ReadSin
    ld      hl,(variationz)
    call    multhl_by_a ;variationz*sin(orientation2)

    push    hl
    call    ReadCos
    ld      hl,(variationx)
    call    multhl_by_a ;variationx*cos(orientation2)
    pop     bc

    or      a
    sbc     hl,bc       ;variationx*cos(orientation2)-variationz*sin(orientation2) = distance from player


    bit 7,h
    jp  nz,iobf_hlneg
    scf
    ret
iobf_hlneg
    scf
    ccf
    ret

