; dw enemy_x
; dw enemy_z
ENEMY_ORIENTATION_OFFSET equ 4
ENEMY_TYPE_OFFSET equ 5
ENEMY_LIFE_OFFSET equ 6
ENEMY_AI_OFFSET equ 7
ENEMY_TIME_OFFSET equ 8
ENEMY_SPRITE_SET_OFFSET equ 9


HandleEnemyAI
    push    ix
    ld    ix,enemiesdata
    ld    b,(ix)    ; number of enemies
; don't verify if b>0
    inc    ix

EnemyAILoop
    push    bc

    ld    a,(ix+ENEMY_LIFE_OFFSET) ; get enemy life
    cp    ENEMY_DYING+1
    jp    c,DyingOrDeadEnemy
; the enemy is alive, has it noticed the player ?


    ld    a,(ix+ENEMY_AI_OFFSET)
    cp    1
    jp    z,EnemyActive
    cp    2
    jp    z,EnemyToShoot
    cp    3
    jp    z,EnemyShooting
    cp    4
    jp    z,EnemyHasShot

; so, ai state = 0 = idle

EnemyIdle
; it has not seen the player, but is the
; player close enough for it to see ?

    ld    d,(ix+1) ; enemyX
    ld    e,(ix+3) ; enemyZ

    call    access_visited_array
    and    (hl)
    jp    z,CheckNextEnemy

    call    ManhattanDistanceEnemyPlayer

    cp    8    ; enemy vision range = 8 tiles
    jp    nc,CheckNextEnemy ; player is not close enough


; if the player can't see the monster, then it can't see the player
; (as long as enemy orientation is not taken in account, it won't be done well)

; if this piece of code happens when the
; visited array has just been erased and not completely yet written in,
; it is as if the monster hasn't seen the player,
; so it reduces the difficulty :)

EnemyHasSeenPlayer

ENEMYSHOOTDELAY equ 10

    ld    (ix+ENEMY_AI_OFFSET),1 ; activate "AI flag"
    ld    (ix+ENEMY_TIME_OFFSET),ENEMYSHOOTDELAY
    jp    CheckNextEnemy

EnemyActive
; each enemy either attacks or move at the same time

    ld    a,(ix+ENEMY_TIME_OFFSET)
    dec   a
    ld    (ix+ENEMY_TIME_OFFSET),a
    jp    nz,EnemyWalk
    ld    a,ENEMYSHOOTDELAY
    ld    (ix+ENEMY_TIME_OFFSET),a

    ld    (ix+ENEMY_AI_OFFSET),2 ; state "prepare to shoot"

    ld    hl,enemy_shoot_ready_sprite_set
    ld    (ix+ENEMY_SPRITE_SET_OFFSET),l
    ld    (ix+ENEMY_SPRITE_SET_OFFSET+1),h
    jp    CheckNextEnemy



EnemyToShoot
    ld    (ix+ENEMY_AI_OFFSET),3 ; state "shooting"

    ld    hl,enemy_shoot_fire_sprite_set
    ld    (ix+ENEMY_SPRITE_SET_OFFSET),l
    ld    (ix+ENEMY_SPRITE_SET_OFFSET+1),h

    ld    a,(difficulty)

    ld    d,0
    ld    e,a
    ld    hl,DifficultyScale
    add   hl,de
    ld    b,(hl)
    push  bc
    call  ManhattanDistanceEnemyPlayer
    pop   bc
    cp    7
    jp    nc,enemy_too_far_to_aim

    ld    d,0
    ld    e,a
    ld    hl,EnemyDistanceAimTable
    add   hl,de
    ld    a,(hl)
    add   a,b
    ld    b,a
enemy_too_far_to_aim

    push  bc
    call  vRandom
    pop   bc
    ld    a,h

    cp    b
    jp    nc,CheckNextEnemy

; now, we draw an imaginary line between the enemy and the player
; on the opaque array to prevent shooting through the walls

    ld    d,(ix+1) ; enemyX
    ld    e,(ix+3) ; enemyZ

    ld    a,(xperso_v+1)
    ld    h,a
    ld    a,(zperso_v+1)
    ld    l,a

    push    ix
    call    lineDraw2
    pop    ix
; draws an imaginary line between enmy and player on the opaque array
; if a wall is met -> carry flag is set
    jp    c,CheckNextEnemy

PlayerHit
    ld    a,(health)
; TODO : depends on the enemy type
    sub    2

 IFNDEF INVINCIBLE
    ld    (health),a
    call  z,KillPlayer
    call  m,KillPlayer
 ENDIF

    ld    a,1
    ld    (player_is_hit),a

    jp    CheckNextEnemy


EnemyShooting
    ld    (ix+ENEMY_AI_OFFSET),4 ; state "has shot"

    ld    hl,enemy_shoot_ready_sprite_set
    ld    (ix+ENEMY_SPRITE_SET_OFFSET),l
    ld    (ix+ENEMY_SPRITE_SET_OFFSET+1),h
    jp    CheckNextEnemy

EnemyHasShot
    ld    (ix+ENEMY_AI_OFFSET),1 ; state "active"

    ld    hl,enemy_walk1_sprite_set
    ld    (ix+ENEMY_SPRITE_SET_OFFSET),l
    ld    (ix+ENEMY_SPRITE_SET_OFFSET+1),h

    ld    (ix+ENEMY_TIME_OFFSET),ENEMYSHOOTDELAY

    jp    CheckNextEnemy

EnemyWalk
    ld    a,(ix+ENEMY_TIME_OFFSET)
    and   %1
    jp    z,EnemyWalk_sprite2
    ld    hl,enemy_walk1_sprite_set
    jp    EnemyWalk_sprite_put
EnemyWalk_sprite2
    ld    hl,enemy_walk2_sprite_set
EnemyWalk_sprite_put
    ld    (ix+ENEMY_SPRITE_SET_OFFSET),l
    ld    (ix+ENEMY_SPRITE_SET_OFFSET+1),h


; 1 : calculate enemy's orientation towards the player
; 2 : make it move forward using entity move routines

    ;ld    a,(ix+ENEMY_TIME_OFFSET)
    ;and   %111
    ;call  z,set_enemy_orientation

    call  set_enemy_orientation

    call  check_distance
    jp    c,CheckNextEnemy ; if monster too close
                            ; then don't move it forward

    ld    l,(ix)
    ld    h,(ix+1)

    ld      (entity_x),hl
    ld      (entity_x_old),hl

    ld    (x_monster_old),hl
    ld    (x_monster),hl

    ld    l,(ix+2)
    ld    h,(ix+3)
    ld      (entity_z),hl
    ld      (entity_z_old),hl

    ld      a,(ix+ENEMY_ORIENTATION_OFFSET)
    ld      (entity_orientation),a
    ld      a,ENTITY_TYPE_ENEMY
    ld      (entity_type),a

    call    forward

    ld      hl,(entity_x)
    ld    (ix),l
    ld    (ix+1),h
    ld      hl,(entity_z)
    ld    (ix+2),l
    ld    (ix+3),h

    jp CheckNextEnemy



DyingOrDeadEnemy
    ld a,(ix+ENEMY_LIFE_OFFSET) ; get enemy life
    or a
    jp z,DeadEnemy
    dec a
    ld (ix+ENEMY_LIFE_OFFSET),a


    ld hl,enemy_die_sprite_set
    ld (ix+ENEMY_SPRITE_SET_OFFSET),l
    ld (ix+ENEMY_SPRITE_SET_OFFSET+1),h
    jp CheckNextEnemy

DeadEnemy
    ld hl,enemy_dead_sprite_set
    ld (ix+ENEMY_SPRITE_SET_OFFSET),l
    ld (ix+ENEMY_SPRITE_SET_OFFSET+1),h
    jp CheckNextEnemy


CheckNextEnemy
    ld    de,16
    add    ix,de
    pop    bc
    dec    b        ;    djnz    EnemyIALoop
    jp    nz,EnemyAILoop    ;

    pop    ix

    ret



;==================== SUBROUTINES ======================

ManhattanDistanceEnemyPlayer
    ld    e,(ix+1) ; enemyX
    ld    d,(ix+3) ; enemyZ
    ld    a,(xperso_v+1)
    sub    e
    jp    p,blablalab
    neg
blablalab
    ld    e,a
    ld    a,(zperso_v+1)
    sub    d
    jp    p,blahblahlab2
    neg
blahblahlab2
    add    a,e
    ret



EnemyOrientationTable
    db    105,90,75,0
    db    0,0,60,0
    db    15,30,45,0


;=====================
set_enemy_orientation
;=====================
    ld    e,(ix)
    ld    d,(ix+1) ; enemyX
    ld    hl,(xperso_v)
    push bc
    ld    b,9
    call  cp_hl_de_approx_b
    pop bc
    ;call  cp_hl_de
    ld    b,1
    jp    z,suite_s_e_o
    ld    b,2
    jp    c,suite_s_e_o
    ld    b,0

suite_s_e_o
    ld    e,(ix+2)
    ld    d,(ix+3) ; enemyZ
    ld    hl,(zperso_v)
    push bc
    ld    b,9
    call  cp_hl_de_approx_b
    pop bc
    ;call  cp_hl_de
    jp    z,same_z
    ld    c,0
    jp    c,end_s_e_o
monster_z_inf
    ld    c,2
    jp    end_s_e_o

same_z
    ld    a,b
    dec    a    ; cp 1  ; if enemy and player are close enough
    jp    z,do_not_set_enemy_orientation ; then do nothing

    ld    c,1
    jp    end_s_e_o

end_s_e_o
    ld    hl,EnemyOrientationTable
    ld    d,0
    ld    e,b
    add    hl,de
    ld    e,c
    sla    e
    sla    e ; e*4
    add    hl,de
    ld    a,(hl)

    ;push    af
    ;call    vRandom
    ;ld    a,%00001111
    ;and    h
    ;sub    8    ; a contains a number between -8 and 7
    ;pop    bc
    ;add    a,b    ; add randomness to orientation

    ;jp    m,s_e_o_lower

    ;cp    120-1
    ;jp    nc,s_e_o_greater

    ld    (ix+ENEMY_ORIENTATION_OFFSET),a
    scf
    ret

;s_e_o_greater
    ;sub    120
    ;ld    (ix+ENEMY_ORIENTATION_OFFSET),a
    ;scf
    ;ret

;s_e_o_lower
    ;add    a,120
    ;ld    (ix+ENEMY_ORIENTATION_OFFSET),a
    ;scf
    ;ret

do_not_set_enemy_orientation
    scf
    ccf
    ret


;=====================
check_distance;check if enemy not too close. return C if too close. NC if not.
;=====================
    ld    e,(ix)
    ld    d,(ix+1) ; enemyX
    ld    hl,(xperso_v)

    push bc
    ld    b,100
    call  cp_hl_de_approx_b
    pop bc

    jp nz,_check_distance_ret_nc


    ld    e,(ix+2)
    ld    d,(ix+3) ; enemyZ
    ld    hl,(zperso_v)

    push bc
    ld    b,100
    call  cp_hl_de_approx_b
    pop bc

    jp nz,_check_distance_ret_nc

    scf
    ret


_check_distance_ret_nc
    scf
    ccf
    ret
