

intcnt2 equ 10;13      ; AI speed
intcnt1 equ 4;10
intcntrun equ 2



InterruptRoutine

    rst #30

    ld a,0
interrupts_active equ $-1
    and a
    jp z,InterruptRoutine

    ld a,1
.cnt equ $-1
    dec a
    jr nz,.skip
    ld    a,(shoot_time)
    dec    a
    ld (shoot_time),a
    ld a,5
.skip
    ld (.cnt),a
;======================
; Enemy IA & moves
;======================

    ld    hl,interrupt_count2
    dec    (hl)
    jp    nz,PlayerInput
; this way it's a little faster

    call    HandleEnemyAI    ; see ai.z80

    ld    a,intcnt2
    ld    (interrupt_count2),a

;=================
; Player input
;=================

;===
; TODO : change to take account of different modes:
; 0 : playing
; 1 : dying
; 2 : dead (lying on ground waiting for keypress)
; 3 : demo playing ?  |
; 4 : AI playing ?    | later
;===

PlayerInput


    ld  hl,interrupt_count
    dec (hl)
    jp  nz,findep2

    ld  a,(game_state)
    cp  STATE_PLAYING
    jp  z,NormalPlayerInput
    cp  STATE_DYING
    jp  z,FakeInputPlayerDying
    cp  STATE_DEAD
    jp  z,FakeInputPlayerDead

FakeInputPlayerDead
; do nothing
    ld a,(statfocus)
    or a
    jr z,skip_key_dead
    ld e,66:ld hl,jmp_keytst:rst #28:dec e  ;Key [ESC]
    jp  z,QuitPlayerDead
skip_key_dead
    ld  a,intcnt1
    ld  (interrupt_count),a
    jp  findep2


FakeInputPlayerDying
; decrease the fall counter (31 -> 0)
    ld a,(statfocus)
    or a
    jr z,skip_key_dying
    ld e,66:ld hl,jmp_keytst:rst #28:dec e  ;Key [ESC]
    jp  z,QuitPlayerDead
skip_key_dying
    ld  a,(falling_counter)
    inc a
    ld  (falling_counter),a
    cp  32
    jp  nz,player_not_dead_yet

    ld  a,STATE_DEAD
    ld  (game_state),a

player_not_dead_yet
    ld  a,intcnt1
    ld  (interrupt_count),a
    jp  findep2


keys_esc123 db 0
keys_zxudlr db 0

NormalPlayerInput
    ld    hl,(xperso_v)
    ld    (x_player_old),hl
    ld    hl,(zperso_v)
    ld    (z_player_old),hl

    ld a,(statfocus)
    or a
    jp z,skip_key_normal1

    ld de,256*66+57     ;  e="3",  d=esc - b05
    ld ix,256*64+65     ;ixl="2",ixh="1" - b12
    ld iy,256*21+23     ;iyl=ctr,iyh=shf - b34
    ld hl,jmp_keymul:rst #28
    ld a,e
    ld (keys_esc123),a

    ld de,256*71+01     ;  e=rgt,  d="Z" - b05
    ld ix,256*02+08     ;ixl=lft,ixh=dwn - b12
    ld iy,256*63+00     ;iyl= up,iyh="X" - b34
    ld hl,jmp_keymul:rst #28
    ld a,e
    ld (keys_zxudlr),a

    ld hl,#8148:rst #28
    ld a,e
    ld (run1+1),a
    bit 1,a
    call nz,shoot
    ld a,(keys_esc123)  ; Key [1]
    bit 2,a
    call nz,select_gun
    ld a,(keys_esc123)  ; Key [2]
    bit 1,a
    call nz,select_chaingun
    ld a,(keys_esc123)  ; Key [3]
    bit 0,a
    call nz,select_shotgun

skip_key_normal1
        ld      a,(orientation)
        ld      (entity_orientation),a
        ld      hl,(xperso_v)
        ld      (entity_x),hl
        ld      hl,(zperso_v)
        ld      (entity_z),hl
        ld      hl,(x_player_old)
        ld      (entity_x_old),hl
        ld      hl,(z_player_old)
        ld      (entity_z_old),hl
        ld      a,ENTITY_TYPE_PLAYER
        ld      (entity_type),a

    ld a,(statfocus)
    or a
    jr z,skip_key_normal2

    ld a,(keys_zxudlr)  ; Key [Z]
    bit 5,a
    call nz,straff_left

    ld a,(keys_zxudlr)  ; Key [X]
    bit 4,a
    call nz,straff_right

    ld a,(keys_zxudlr)  ; Key [UP]
    bit 3,a
    call nz,forward

    ld a,(keys_zxudlr)  ; Key [DOWN]
    bit 2,a
    call nz,backward

    ld a,(keys_zxudlr)  ; Key [LEFT]
    bit 1,a
    call nz,turn_right

    ld a,(keys_zxudlr)  ; Key [RIGHT]
    bit 0,a
    call nz,turn_left

skip_key_normal2

        ld      a,(entity_orientation)
        ld      (orientation),a
        ld      hl,(entity_x)
        ld      (xperso_v),hl
        ld      hl,(entity_z)
        ld      (zperso_v),hl

    call    update_xperso_zperso

    ld a,(statfocus)
    or a
    jr z,skip_key_normal3

    ld a,(keys_esc123)  ; Key [ESC]
    bit 5,a
    jp nz,QuitSave
run1
    ld a,0              ; Key [SHIFT]
    rra
    ld  a,intcntrun
    jr c,run


skip_key_normal3
    ld    a,intcnt1
run
    ld    (interrupt_count),a

findep2

    jp InterruptRoutine
;    ret

;==========================
; End of Interrupt Routine
;==========================


shoot
    ld    a,(weapon_selected)
    cp    GUN_SELECTED
    jp    z,sht_gun
    cp    CHAINGUN_SELECTED
    jp    z,sht_chaingun
    cp    SHOTGUN_SELECTED
    jp    z,sht_shotgun
    ret

sht_gun
    ld    hl,q_gun_ammo
    jp    sht_common
sht_chaingun
    ld    hl,q_chaingun_ammo
    jp    sht_common
sht_shotgun
    ld    hl,q_shotgun_ammo
    jp    sht_common

sht_common

    ld    a,(hl)
    or    a
    ret    z
    ld  ix,(weapon_data)
    ld    a,(shoot_time)
    cp     (ix+0)
    ret    p
    ld    a,(ix+1)
    ld    (shoot_time),a
    dec    (hl)
    ld    a,(ix+2)
    ld    (power),a
    ret

select_gun
    ld    a,GUN_SELECTED
    ld    (weapon_selected),a
    ld    de,gun_data
    ld  (weapon_data),de
    ret


select_shotgun
    ld    a,(has_shotgun)
    or    a
    ret    z
    ld    a,SHOTGUN_SELECTED
    ld    (weapon_selected),a
    ld    de,shotgun_data
    ld  (weapon_data),de
    ret

select_chaingun
    ld    a,(has_chaingun)
    or    a
    ret    z
    ld    a,CHAINGUN_SELECTED
    ld    (weapon_selected),a
    ld    de,chaingun_data
    ld  (weapon_data),de
    ret



;*****************************
; entity deplacement routines
;*****************************

forward
    ld    a,(entity_orientation)
    cp    30
    jp    m,haut30
    cp    60
    jp    m,haut60
    cp    90
    jp    m,haut90
    jp    haut120

backward
    ld    a,(entity_orientation)
    cp    30
    jp    m,haut90
    cp    60
    jp    m,haut120
    cp    90
    jp    m,haut30
    jp    haut60

straff_right
    ld    a,(entity_orientation)
    add    a,30
    cp    120
    call    p,retangle_straff
    cp    30
    jp    m,haut30
    cp    60
    jp    m,haut60
    cp    90
    jp    m,haut90
    jp    haut120

straff_left
    ld    a,(entity_orientation)
    add    a,30
    cp    120
    call    p,retangle_straff
    cp    30
    jp    m,haut90
    cp    60
    jp    m,haut120
    cp    90
    jp    m,haut30
    jp    haut60


haut30
    ld    hl,cosinus_ns
    ld    c,a
    ld    b,0
    add    hl,bc
    ld    a,(hl)
    ld    d,b
    ld    e,a
    ld    hl,(entity_x)
    add    hl,de
    ld    (entity_x),hl
    ld    hl,sinus_ns
    add    hl,bc
    ld    a,(hl)
    ld    e,a
    ld    hl,(entity_z)
    add    hl,de
    ld    (entity_z),hl
    jp    verifdeplacement

haut60
    ld    hl,cosinus_ns
    ld    c,a
    ld    b,0
    add    hl,bc
    ld    a,(hl)
    ld    d,b
    ld    e,a
    ld    hl,(entity_x)
    sbc    hl,de
    ld    (entity_x),hl
    ld    hl,sinus_ns
    add    hl,bc
    ld    a,(hl)
    ld    e,a
    ld    hl,(entity_z)
    add    hl,de
    ld    (entity_z),hl
    jp    verifdeplacement

haut90
    ld    hl,cosinus_ns
    ld    c,a
    ld    b,0
    add    hl,bc
    ld    a,(hl)
    ld    d,b
    ld    e,a
    ld    hl,(entity_x)
    sbc    hl,de
    ld    (entity_x),hl
    ld    hl,sinus_ns
    add    hl,bc
    ld    a,(hl)
    ld    e,a
    ld    hl,(entity_z)
    sbc    hl,de
    ld    (entity_z),hl
    jp    verifdeplacement

haut120
    ld    hl,cosinus_ns
    ld    c,a
    ld    b,0
    add    hl,bc
    ld    a,(hl)
    ld    d,b
    ld    e,a
    ld    hl,(entity_x)
    add    hl,de
    ld    (entity_x),hl
    ld    hl,sinus_ns
    add    hl,bc
    ld    a,(hl)
    ld    e,a
    ld    hl,(entity_z)
    sbc    hl,de
    ld    (entity_z),hl
    jp    verifdeplacement


turn_left
    ld    a,(entity_orientation)
    cp    120
    jp    z,retorientation_1
    jp    p,retorientation_1
    inc    a
    ld    (entity_orientation),a
    ret

turn_right
    ld    a,(entity_orientation)
    dec    a
    jp    z,retorientation_2
    jp    m,retorientation_2
    ld    (entity_orientation),a
    ret


retorientation_1:
    ld    a,2 ; why not 1 ?
    ld    (entity_orientation),a
    ret

retorientation_2:
    ld    a,120
    ld    (entity_orientation),a
    ret

retangle_straff:
    sub    120
    ret

verifdeplacement
    ld    e,0
    ld    a,(entity_z+1)
    ld    c,a
    ld    a,(entity_z_old+1)
    cp    c
    jp    z,verifdepl2
    set    2,e    ; if we've moved in the z direction, we mark it in e
verifdepl2:
    ld    a,(entity_x+1)
    ld    b,a
    ld    a,(entity_x_old+1)
    cp    b
    jp    z,verifdepl3
    set    1,e    ; if we've moved in the x direction, we mark it in e
verifdepl3:
    ld     a,e
    or     a
    ret    z ; if we've not moved out of a tile, nothing to test
    push   de
    call   ReadMap
    pop    de
    or     a
    ret    z

    cp     1 ; if wall ...
    jp     z,retcoordonnees

OBJECT_TYPE_LIMIT    equ    13  ; limit between non/walkable

    cp     OBJECT_TYPE_LIMIT+1
    jp     c,non_retcoordonnees

retcoordonnees
    bit    1,e        ; read e to see in which direction we're blocked
    call   nz,retxperso
    bit    2,e
    call   nz,retzperso
    ret

non_retcoordonnees
        ld      (n_r_c_save),a

        ld      a,(entity_type)
        cp      ENTITY_TYPE_ENEMY ; enemies don't pick objects
        ret     z

n_r_c_save      equ $+1
    ld  a,0
    cp  2
    jp    z,add_health
    cp  3
    jp  z,add_full_health
    cp    4
    jp    z,add_gun_ammo
    cp    5
    jp    z,add_chaingun_ammo
    cp    6
    jp    z,add_shotgun_ammo
    cp  7
    jp  z,add_full_ammo
    cp  8
    jp  z,pick_chaingun
    cp  9
    jp  z,pick_shotgun
    cp    12
    jp    z,gagner
    ret


add_health:
    ld    b,HEALTH_MAX
    ld    a,(health)
    cp    b    ; vie max
    ret    z
    ld    (hl),0    ; on consomme l'item, qui disparait de la carte
    add    a,5
    cp    b
    jp    c,don_t_adjust_max_health
    ld    a,b
don_t_adjust_max_health
    ld    (health),a
    ret

add_full_health
    ld  b,HEALTH_MAX
    ld  a,(health)
    cp  b
    ret z
    ld  (hl),0
    ld  a,HEALTH_MAX
    ld  (health),a
    ret

add_gun_ammo
    ld    b,GUN_AMMO_MAX
    ld    a,(q_gun_ammo)
    cp    b    ; munitions max
    ret    z
    ld    (hl),0    ; on consomme l'item, qui disparait de la carte
    add    a,20
    cp    b
    jp    c,don_t_adjust_max_gun_ammo
    ld    a,b
don_t_adjust_max_gun_ammo
    ld    (q_gun_ammo),a
    ret

add_chaingun_ammo
        ld      a,(has_chaingun)
        or      a
        ret     z


    ld    b,CHAINGUN_AMMO_MAX
    ld    a,(q_chaingun_ammo)
    cp    b    ; munitions max
    ret    z
    ld    (hl),0    ; on consomme l'item, qui disparait de la carte
    add    a,40
    cp    b
    jp    c,don_t_adjust_max_chaingun_ammo
    ld    a,b
don_t_adjust_max_chaingun_ammo
    ld    (q_chaingun_ammo),a
    ret

add_shotgun_ammo
    ld    b,SHOTGUN_AMMO_MAX
    ld    a,(q_shotgun_ammo)
    cp    b    ; munitions max
    ret    z
    ld    (hl),0    ; on consomme l'item, qui disparait de la carte
    add    a,5
    cp    b
    jp    c,don_t_adjust_max_shotgun_ammo
    ld    a,b
don_t_adjust_max_shotgun_ammo
    ld    (q_shotgun_ammo),a
    ret

add_full_ammo
; todo : manage it better
; is not all ammo at max : consume it
    ld    (hl),0
    ld    a,GUN_AMMO_MAX
    ld    (q_gun_ammo),a
    ld    a,(has_chaingun)
    or    a
    jp    z,afa2
    ld    a,CHAINGUN_AMMO_MAX
    ld    (q_chaingun_ammo),a
afa2
    ld    a,(has_shotgun)
    or    a
    ret    z
    ld    a,SHOTGUN_AMMO_MAX
    ld    (q_shotgun_ammo),a
    ret

pick_chaingun
    ld    a,1
    ld    (has_chaingun),a
    call    add_chaingun_ammo
    jp    select_chaingun

pick_shotgun
    ld    a,1
    ld    (has_shotgun),a
    call    add_shotgun_ammo
    jp    select_shotgun

; /// gestion du glissage contre les murs

;TODO comment on sait si on a bougé à gauche ou à droite, et en haut ou en bas?

retxperso:
    ld    hl,(entity_x_old)
    ld    a,l
    and    %11110000    ;ne garde que les 4 premiers bits de la virgule
    ld    l,a
    ld    bc,(entity_x)
    ld    a,c
    and    %00001111
    add    a,l
    ld    l,a
    ld    (entity_x),hl
    ret

retzperso:
    ld    hl,(entity_z_old)
    ld    a,l
    and    %11110000    ;ne garde que les 4 premiers bits de la virgule
    ld    l,a
    ld    bc,(entity_z)
    ld    a,c
    and    %00001111
    add    a,l
    ld    l,a
    ld    (entity_z),hl
    ret

;************************************
; end of entity deplacement routines
;************************************


entity_orientation
    db 0
entity_x
    dw 0
entity_z
    dw 0
entity_x_old
    dw 0
entity_z_old
    dw 0
entity_type
    db 0

ENTITY_TYPE_PLAYER equ 0
ENTITY_TYPE_ENEMY equ 1

gagner:
; pour etre sur que les pages memoire entre 4000 et cfff
; sont bien la ram....

    ld    hl,new_level
    ld    (gohere),hl
    ld    hl,interrupts_active
    dec   (hl)
    jp    InterruptRoutine

;    ex    (sp),hl
;    reti

KillPlayer
    xor a
    ld  (health),a

    ld  a,(game_state)
    cp  STATE_PLAYING
    ret nz

    ld  a,STATE_DYING
    ld  (game_state),a
    ret

QuitPlayerDead
    ld    hl,game_lost
    ld    (gohere),hl
    ld    hl,interrupts_active
    dec   (hl)
    jp    InterruptRoutine
;    ex    (sp),hl
;    reti

QuitSave
    ld  hl,QuitSave_out_of_interrupt
    ld    (gohere),hl
    ld    hl,interrupts_active
    dec   (hl)
    jp    InterruptRoutine
;    ex  (sp),hl
;    reti

update_xperso_zperso
    ld    hl,(xperso_v)
    srl    h
    rr    l
    srl    h
    rr    l
    srl    h
    rr    l
    srl    h
    rr    l
    ld    (x_player),hl        ;updates xperso and zperso for
    ld    hl,(zperso_v)        ;the display routine
    srl    h
    rr    l
    srl    h
    rr    l
    srl    h
    rr    l
    srl    h
    rr    l
    ld    (z_player),hl
    ret


gun_data
    db -5,4,2 ; time between shots ; time of shot sprite display ; power
        db 37,64-20,20,3
    dw pistol
    db 34,64-34,34,3
    dw pistol2
chaingun_data
    db -1,2,4
        db 28,64-20,20,5
    dw mitra
    db 28,64-34,34,5
    dw mitrafeu
shotgun_data
    db -14,4,13
        db 33,64-21,21,4
    dw shotgun_idle_sprite
    db 20,64-42,42,6
    dw shotgun_fire_sprite
