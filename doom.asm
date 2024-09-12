;DEBUG ; to display the number of frames when the game stops + raycaster during game
;VTI ; player move is faster, because it appears slower on vti
;INVINCIBLE
;ULTRAPOWER ; kill enemies in one shot

 DEFINE TI83P
 ;DEFINE DEBUG
 ;DEFINE VTI

;/////////////
; zDoom 0.12
;/////////////

;mysaferam equ $c000 ; appBackupScreen, 768 bytes


x_player_old    equ    mysaferam
z_player_old     equ    x_player_old+2
x_player    equ    z_player_old+2
z_player    equ    x_player+2
ray_angle     equ    z_player+2
dx         equ    ray_angle+1
dz         equ    dx+1
xray         equ    dz+1
zray         equ    xray+1
S         equ    zray+1
x_player_int    equ    S+1
z_player_int    equ    x_player_int+1
numray        equ    z_player_int+1
increment    equ    numray+1
variationx    equ    increment+1
variationz    equ    variationx+2
dimension    equ    variationz+2
old_wall_pos    equ    dimension+1
temp_wall    equ    old_wall_pos+2
orientation2    equ    temp_wall+2
tempcoo        equ    orientation2+1
n_sprite    equ    tempcoo+2
n_spritea    equ    n_sprite+1
interrupt_count equ    n_spritea+2
sp_2        equ    interrupt_count+1
matnum        equ    sp_2+2
spritedezoom    equ    matnum+1
tempxt        equ    spritedezoom+1
object_coords    equ    tempxt+1
temp_sprite_address    equ    object_coords+2
tempreturn    equ    temp_sprite_address+2
tempcouille    equ    tempreturn+2
nmenu        equ    tempcouille+1
moptions    equ    nmenu+1
interrupt_count2 equ    moptions+1
tempdifficulty    equ    interrupt_count2+1; 1 byte
x_monster_old    equ    tempdifficulty+1
z_monster_old    equ    x_monster_old+2 ; 2 bytes
x_monster    equ    z_monster_old+2
z_monster    equ    x_monster+2 ;  2 bytes
temp_x_center equ z_monster+2 ; 1 byte
scaled_sprite_height equ temp_x_center+1 ; 1 byte
walls_delta_height1 equ scaled_sprite_height+1 ; 1 byte
walls_delta_height2 equ walls_delta_height1+1 ; 1 byte

; 51 bytes used (space available = 128)

ENEMY_DYING    equ    4
SCREEN_WIDTH    equ    96

STATE_PLAYING equ 0
STATE_DYING equ 1
STATE_DEAD equ 2


;interrupt_table        equ    $9900
;interrupt_table2    equ    $99
;interrupt        equ    $9A
;interrupt_adresse    equ    $9A9A



HEALTH_MAX  equ 100
GUN_AMMO_MAX        equ 100
CHAINGUN_AMMO_MAX   equ 200
SHOTGUN_AMMO_MAX    equ 50


CPL_OPCODE equ $2f

; interruption at address $8383: only the first 286 bytes of APD_BUF are available


;    INCLUDE <vion.inc>
;beginnnig_of_program
;    db "zDoom "


;    call   disp_menu

;    ret           ;Exit

version
    db "v0.12",0
    ;db "MSX",0
;#########################################

init_game
    xor a
    ld  (shoot_time),a
    ld  (level),a
    ld  (player_is_hit),a
    ld  (has_chaingun),a
    ld  (has_chainsaw),a
    ld  (has_shotgun),a
    ld  (q_chaingun_ammo),a
    ld  (q_shotgun_ammo),a
    ld  (berserk_time),a
    ld  (power),a
    ld  (falling_counter),a
    ld  a,STATE_PLAYING
    ld  (game_state),a

    call select_gun


    ld    a,(tempdifficulty)
    ld    (difficulty),a

    ld    a,HEALTH_MAX
    ld    (health),a
    ld    a,50
    ld    (q_gun_ammo),a

    ld    a,1
    ld    (savegame),a ; tell there's a running game


;initialisation
    call    system_init
    jp        new_level

ContinueGame
    call    system_init
    jp        PreMainLoop


system_init

    ld      a,r
    ld      (randseed),a
    ret
/*
    ld    hl,interrupt_table
    ld    de,interrupt_table+1
    ld    bc,256        ; *NOT* 256-1 :)
    ld    (hl),interrupt
    ldir            ;installation of interrupt

    ld    hl,interrupt_adresse
    ld    (hl),$C3    ; opcode of JP
    ld    de,InterruptRoutine
    inc    hl
    ld    (hl),e
    inc    hl
    ld    (hl),d        ; address of the interrupt routine

    di

    ld    a,interrupt_table2
    ld    i,a
    ret
*/
PreMainLoop
    xor    a
    ld    (fastCopyinv),a
    ld    a,10
    ld    (interrupt_count),a
    ld    a,2
    ld    (interrupt_count2),a
    ld (interrupts_active),a
    ;im    2
    ei

    call    RayCast
;=================
MainLoop
;=================
    call    DisplayWalls        ;60000
    call    DisplayEnemies      ; 5038
    call    DisplayWeapon       ; 9632
    call    DisplayHud          ; 7718
    call    RayCast             ;70000      also display of map-objects

;    call    DisplayDebug

 IFDEF DEBUG
     call    showvisited
 ENDIF

    ld    a,(player_is_hit)
    or    a
    jp    z,NoNotifyHit
    xor    a
    ld    (player_is_hit),a

; display hit notification by shifting the screen one line upwards
    call    Scroll_U
    xor    a
    ld    (fastCopyinv),a
NoNotifyHit
    call    nodifastcopy        ;24000
    call    clear_keys
    jp      MainLoop

clear_keys
    call    checkmessages
    or a
    jr nz,clear_keys
    ret


;/////////////////////////////////////
;Display Weapon Sprite
;2 sprites : 1 normal and 1 firing
;/////////////////////////////////////

DisplayWeapon
    ld  ix,(weapon_data)
    ld    a,(shoot_time)
    dec    a
    ;ld (shoot_time),a
    jp    m,idle
    jp    p,fire

idle
    cp     (ix+0)
    jp     m,idle_floor
    xor    a
    ld     (power),a
idle2
    ld    a,(ix+3)
    ld    l,(ix+4)
    ld    b,(ix+5)
    ld    c,(ix+6)

    ld  e,(ix+7)
    ld  d,(ix+8)
    push    de
    pop     ix

    push    af
    ld  a,(game_state)
    cp  STATE_DYING
    jp  z,dw_lowering
    cp  STATE_DEAD
    jp  z,dw_lowering
    pop af
    jp    largesprite_masked ;largesprite_masked_clipped_down

idle_floor
    ld    a,(ix+0)
    dec a
    ld    (shoot_time),a
    jp    idle2

fire
    ld    a,(ix+9)
    ld    l,(ix+10)
    ld    b,(ix+11)
    ld    c,(ix+12)

    ld  e,(ix+13)
    ld  d,(ix+14)
    push    de
    pop ix

    push    af
    ld  a,(game_state)
    cp  STATE_DYING
    jp  z,dw_lowering
    cp  STATE_DEAD
    jp  z,dw_lowering
    pop af
    jp    largesprite_masked ;largesprite_masked_clipped_down

dw_lowering
    ld      a,(falling_counter)
    add     a,l
    ld      l,a
    pop     af
    jp     largesprite_masked_clipped_down




QuitSave_out_of_interrupt
    xor a
    ld (interrupts_active),a

    ld    sp,(sp_2)
    ld    hl,(tempreturn)
    push    hl

    call    clrgbuf_fast

    ld    a,$2f            ; CPL opcode
    ld    (fastCopyinv),a        ; invert display
    call    clear_keys
    ret                ;  back to menu

game_lost
game_won
    xor    a
    ld    (savegame),a
    ld (interrupts_active),a

    ;im    1
    ld    sp,(sp_2)
    ld    hl,(tempreturn)
    push    hl

    call    clrgbuf_fast

    ld    hl,t_game_over
    ld    de,256*29+30
    call    fastvputs

 IFDEF DEBUG
    ld    hl,(frames)
    ld    de,256*52+17
    call    vdisphl
    ld    hl,t_frames
    call    vputs
 ENDIF

    ld    a,$2f    ; CPL opcode
    ld    (fastCopyinv),a        ; invert display

    call    nodifastcopy
    call    keyboard_pause

    ret            ; back to menu

 IFDEF DEBUG
showvisited
    ld    hl,visited
    ld    de,grbuf
    ld    a,32
showvisited_loop
    ldi
    ldi
    ldi
    ldi
    ex    de,hl
    push    de
    ld    de,8
    add    hl,de
    pop    de
    ex    de,hl
    dec    a
    jr    nz,showvisited_loop
    ret
 ENDIF

    INCLUDE "menu.asm"
    INCLUDE "newlvl.asm"
    INCLUDE "intrrpt.asm"
    INCLUDE "rays.asm"
    INCLUDE "display.asm"
    INCLUDE "doomrout.asm"
    INCLUDE "ai.asm"
    INCLUDE "lzss.asm"

; data at the end, beyond TI-83+'s 8.8KB executable code limit

; ~6100 / 8800 bytes of code so far

data_start

    INCLUDE "lvl.asm"
    INCLUDE "tables.asm"
    INCLUDE "sprites.asm"
    INCLUDE "save.asm"



t_lost          db "You Lose",0
t_won           db "You Win",0
t_level        db "Level ",0
t_z        db "z",0
t_continue    db "Continue",0
t_newgame    db "New game",0
t_options    db "Options",0
t_quit        db "Quit",0
t_options2    db "Options :",0
t_difficulty    db "Difficulty",0
t_tooyoung    db "Too young to die",0
t_hurtme    db "Hurt me plenty",0
t_ultraviolence    db "Ultra-Violence",0
t_game_over    db "Game Over",0

 IFDEF DEBUG
t_frames    db " frames displayed",0
frames         dw 0
 ENDIF

DispCoordWalls    db 0

pencol db 0
penrow db 0
op1 ds 6

data_end
