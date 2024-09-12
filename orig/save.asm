; Contains all saved data

savegame    db 0

xperso_v    dw 0
zperso_v    dw 0
orientation    db 0

health          db 0
q_gun_ammo      db 0
q_chaingun_ammo db 0
q_shotgun_ammo  db 0

shoot_time    db 0
power        db 0

level        db 0

difficulty    db 1    ; medium (0,1 or 2)

game_state  db 0    ; 0 : playing, 1: dying 2: dead
falling_counter db 0

player_is_hit    db 0

has_chaingun    db 0
has_shotgun     db 0
has_chainsaw    db 0
berserk_time    db 0

weapon_selected db 0
weapon_data dw 0

GUN_SELECTED equ 0
CHAINGUN_SELECTED equ 1
SHOTGUN_SELECTED equ 2
CHAINSAW_SELECTED equ 3

;    current level: size = 32*32=1024


level_buffer
 REPT 1024
     db 1
 ENDR

enemiesdata
 REPT 512 ; arbitrary size : 511/16 = 31 enemies max
    db 0
 ENDR
; structure of data for one enemy :
; dw enemy_x
; dw enemy_z


; ENEMY_ORIENTATION_OFFSET equ 4
; ENEMY_TYPE_OFFSET equ 5
; ENEMY_LIFE_OFFSET equ 6
; ENEMY_AI_OFFSET equ 7
; ENEMY_TIME_OFFSET equ 8
; ENEMY_SPRITE_OFFSET equ 9


; this array stores which points of the map are seen throught the raycaster
visited
 REPT 128
     db 0
 ENDR


 ; this array stores which points of the map are opaque
opaque
 REPT 128
     db 0
 ENDR

