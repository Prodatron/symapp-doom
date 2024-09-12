;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                           S y m b O S    D O O M                           @
;@                                                                            @
;@   (c) 2002/2007 by Alexis Guinamard, Guillaume Hoffmann, Raphael Siryani   @
;@                     (c) 2021 by NYYRIKKI and Prodatron                     @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;*** use SJASMPLUS for assembling! ***

;BUGS

;TODO
;- try to optimize
;  - sprite routines?
;  - raycasting?
;  - overhead in drawing routines?
;- multicolour support (1byte/pixel)


 org #1000

 output "e:\symbos\apps\DOOM.EXE"
 include "SymbOS-Constants.asm"

 relocate_start
 
use_SyKernel_MTADDP     equ 0   ;Adds a new process and starts it
use_SyKernel_MTDELP     equ 0   ;Stops an existing process and deletes it
use_SyKernel_MTADDT     equ 1   ;Adds a new timer and starts it
use_SyKernel_MTDELT     equ 1   ;Stops an existing timer and deletes it
use_SyKernel_MTSLPP     equ 0   ;Puts an existing process into sleep mode
use_SyKernel_MTWAKP     equ 0   ;Wakes up a process, which was sleeping
use_SyKernel_TMADDT     equ 0   ;Adds a counter for a process
use_SyKernel_TMDELT     equ 0   ;Stops a counter of a process
use_SyKernel_TMDELP     equ 0   ;Stops all counters of one process
use_SyKernel_MTPRIO     equ 0   ;Changes the priority of a process

use_SySystem_PRGRUN     equ 1   ;Starts an application or opens a document
use_SySystem_PRGEND     equ 0   ;Stops an application and frees its resources
use_SySystem_PRGSRV     equ 0   ;Manages shared services or finds applications
use_SySystem_SYSWRN     equ 0   ;Opens an info, warning or confirm box
use_SySystem_SELOPN     equ 0   ;Opens the file selection dialogue
use_SySystem_HLPOPN     equ 1   ;Opens the help file for this application (##!!## missing doc)

;==============================================================================
;### CODE AREA ################################################################
;==============================================================================

;### APPLICATION HEADER #######################################################

;header structure
prgdatcod       equ 0           ;Length of the code area (OS will place this area everywhere)
prgdatdat       equ 2           ;Length of the data area (screen manager data; OS will place this area inside a 16k block of one 64K bank)
prgdattra       equ 4           ;Length of the transfer area (stack, message buffer, desktop manager data; placed between #c000 and #ffff of a 64K bank)
prgdatorg       equ 6           ;Original origin of the assembler code
prgdatrel       equ 8           ;Number of entries in the relocator table
prgdatstk       equ 10          ;Length of the stack in bytes
prgdatcrn       equ 12          ;Length of crunched data      (*NOT YET SUPPORTED*)
prgdatctp       equ 14          ;Cruncher type (0=uncrunched) (*NOT YET SUPPORTED*)
prgdatnam       equ 15          ;Application name. The end of the string must be filled with 0.
prgdatidn       equ 48          ;"SymExe10" SymbOS executable file identification
prgdatcex       equ 56          ;additional memory for code area (will be reserved directly behind the loaded code area)
prgdatdex       equ 58          ;additional memory for data area (see above)
prgdattex       equ 60          ;additional memory for transfer area (see above)
prgdatres       equ 62          ;*reserved* (28 bytes)
prgdatism       equ 90          ;Application icon (small version), 8x8 pixel, SymbOS graphic format
prgdatibg       equ 109         ;Application icon (big version), 24x24 pixel, SymbOS graphic format
prgdatlen       equ 256         ;length of header

prgpstdat       equ 6           ;start address of the data area
prgpsttra       equ 8           ;start address of the transfer area
prgpstspz       equ 10          ;additional sub process or timer IDs (4*1)
prgpstbnk       equ 14          ;64K ram bank (1-8), where the application is located
prgpstmem       equ 48          ;additional memory areas; 8 memory areas can be registered here, each entry consists of 5 bytes
                                ;00  1B  Ram bank number (1-8; if 0, the entry will be ignored)
                                ;01  1W  Address
                                ;03  1W  Length
prgpstnum       equ 88          ;Application ID
prgpstprz       equ 89          ;Main process ID

App_BegCode
prgcodbeg    dw prgdatbeg-prgcodbeg  ;length of code area
             dw prgtrnbeg-prgdatbeg  ;length of data area
             dw prgtrnend-prgtrnbeg  ;length of transfer area
prgdatadr    dw #1000                ;original origin                    POST address data area
prgtrnadr    dw relocate_count       ;number of relocator table entries  POST address transfer area
prgprztab    dw prgstk-prgtrnbeg     ;stack length                       POST table processes
             dw 0                    ;crunched data length
App_BnkNum
prgbnknum    db 0                    ;crunched data type                 POST bank number
             db "DOOM":ds 20:db 0 ;Name
             db 1                    ;flags (+1=16c icon)
             dw prgicn16c-prgcodbeg  ;16 colour icon offset
             ds 5                    ;*reserved*
prgmemtab    db "SymExe10"           ;SymbOS-EXE-identifier              POST table reserved memory areas
             dw 0                    ;additional code memory
             dw 0                    ;additional data memory
             dw 0                    ;additional transfer memory
             ds 26                   ;*reserved*
             db 1,3                  ;required OS version (3.1)

prgicnsml   db 2,8,8,#5F,#AF,#7F,#AF,#7F,#AF,#37,#EF,#27,#EF,#03,#CE,#00,#4E,#00,#04
prgicnbig   db 6,24,24
            db #F0,#F0,#F0,#F0,#F0,#F0,#F0,#F1,#FF,#DF,#F8,#F0,#F0,#F7,#EE,#0F,#FE,#F0,#F0,#F7,#01,#3F,#FA,#F0,#F0,#E7,#0F,#BF,#FA,#F0,#F0,#FF,#7F,#8F,#7F,#F0,#F0,#FF,#FF,#0F,#7F,#F0,#F0,#FF,#07,#0E,#37,#F0
            db #F0,#CE,#07,#0E,#37,#F0,#F0,#CE,#07,#0E,#37,#F0,#F0,#CE,#00,#01,#FF,#F0,#E1,#9E,#FB,#7C,#97,#78,#E1,#2F,#F3,#F8,#1F,#78,#F1,#0E,#4F,#27,#07,#F8,#E1,#08,#00,#00,#01,#78,#F0,#0E,#00,#00,#07,#F0
            db #F0,#0A,#77,#6E,#17,#F0,#F0,#8A,#03,#09,#15,#F0,#F0,#8E,#33,#EE,#17,#F0,#F0,#C6,#77,#88,#36,#F0,#F0,#E2,#07,#0E,#74,#F0,#F0,#E1,#00,#00,#78,#F0,#F0,#F0,#87,#1E,#F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0

 include "math.asm"

;### PRGPRZ -> Application process
dskprzn      db 2
sysprzn      db 3
windatprz   equ 3   ;Process ID
prgwin       db 0    ;main window ID

prgprz  ld a,(prgprzn)
        ld (syswininf+windatprz),a

        ld hl,gamtims           ;Game-Timer
        ld a,(prgbnknum)
        call SyKernel_MTADDT
        jp c,prgend
	    ld (timerid),a

        call SySystem_HLPINI
        call syschk
        call matini

        ld c,MSC_DSK_WINOPN
        ld a,(prgbnknum)
        ld b,a
        ld de,syswininf
        call msgsnd             ;open window
prgprz1 call msgdsk             ;get message -> IXL=Status, IXH=sender ID
        cp MSR_DSK_WOPNER
        jp z,prgend1            ;memory full -> quit process
        cp MSR_DSK_WOPNOK
        jr nz,prgprz1           ;message is not "window has been opened" -> ignore
        ld a,(prgmsgb+4)
        ld (prgwin),a           ;window has been opened -> store ID
        
;---------- START TIMER

        call disp_menu


;### PRGEND -> quit application
prgend  ld a,(timerid)
        ld c,MSC_KRL_MTDELT
        ld l,a
        call SyKernel_Message
prgend1 ld a,(prgprzn)
        db #dd:ld l,a
        ld a,(sysprzn)
        db #dd:ld h,a
        ld iy,prgmsgb
        ld (iy+0),MSC_SYS_PRGEND    ;send "please kill me" message to system manager
        ld a,(prgcodbeg+prgpstnum)
        ld (iy+1),a
        rst #10
prgend0 rst #30                     ;wait for death
        jr prgend0

;A=char from keyboard (0=no char received)
checkmessages:
        call msgget             ;*** check for messages
        jr nc,check0
        cp MSR_DSK_CFOCUS
        jr z,resetfocus
        cp MSR_DSK_WFOCUS
        jr z,savefocus
        cp MSR_DSK_WCLICK       ;* window has been clicked?
        jr nz,checkmessages
        ld a,(iy+2)             ;* yes, check what exactly
        cp DSK_ACT_KEY          ;*** Taste wurde gedrückt
        jr z,prgkey
        cp DSK_ACT_CLOSE        ;* close clicked
        jr z,prgend
        jp checkmessages
prgkey  ld a,(iy+4)
        ret
check0  xor a
        ret

savefocus
        ld a,(iy+2)
        ld (statfocus),a
resetfocus
        xor a
        ld (winmengrp+14),a
        ld (winsklgrp+14),a
        jr checkmessages


statfocus db 1
timerid db 0


;==============================================================================
;### SUB-ROUTINES #############################################################
;==============================================================================

;### MSGGET -> check for message for application
;### Output     CF=0 -> keine Message vorhanden, CF=1 -> IXH=Absender, (recmsgb)=Message, A=(recmsgb+0), IY=recmsgb
msgget  ld a,(prgprzn)
         db #dd:ld l,a           ;IXL=our own process ID
         db #dd:ld h,-1          ;IYL=sender ID (-1 = receive messages from any sender)
        ld iy,prgmsgb           ;IY=Messagebuffer
        rst #18                 ;get Message -> IXL=Status, IXH=sender ID
        or a
         db #dd:dec l
        ret nz
        ld iy,prgmsgb
        ld a,(iy+0)
        or a
        jp z,prgend
        scf
        ret

;### MSGDSK -> wait for a message from the desktop manager
;### Ausgabe    (recmsgb)=Message, A=(recmsgb+0), IY=recmsgb
;### Veraendert
msgdsk  call msgget
        jr nc,msgdsk            ;no Message
        ld a,(dskprzn)
         db #dd:cp h
        jr nz,msgdsk            ;Message from someone else -> ignore
        ld a,(prgmsgb)
        ret

;### MSGSND -> send message to desktop process
;### Eingabe    C=command, B/E/D/L/H=Parameter1/2/3/4/5
msgsnd2 ld a,(prgwin)
        ld b,a
msgsnd  ld a,(dskprzn)
msgsnd1 db #dd:ld h,a
        ld a,(prgprzn)
        db #dd:ld l,a
        ld iy,prgmsgb
        ld (iy+0),c
        ld (iy+1),b
        ld (iy+2),e
        ld (iy+3),d
        ld (iy+4),l
        ld (iy+5),h
        rst #10
        ret

;### SYSCHK -> get computer type and adjust screen output table
syschk  ld hl,jmp_sysinf        ;*** get Computer Type
        ld de,256*1+5
        ld ix,cfgcpctyp
        ld iy,66+2+6+8
        rst #28
        ld a,(cfgcpctyp)
        bit 7,a             ;0=cpc like encoding, 1=msx like encoding

        ld hl,grpm16            ;msx
        ld de,nodifastcopy.loopg
        jr nz,grpcnv1
        and 31
        cp 7        ;7-10=msx for old symbos versions
        jr c,syschk1
        cp 10+1
        jr c,grpcnv1

syschk1 ld hl,gamescreen_head4  ;cpc
        ld de,gamescreen
        ld (wingam_scr+4),de
        ldi:ldi:ldi
        ld a,8
        ld (wingam_scr+2),a
        ld hl,grpcpc
        ld de,nodifastcopy.loope

grpcnv1 ld (grpcnv3+1),hl
        ld (grpcnv4+1),hl
        ld (nodifastcopy.loope-2),de

        ld hl,(optcolb)
        ld a,l              ;a=background
        and 3
        ld l,a
        ld a,h              ;a=foreground
        and 3
        add a:add a
        or l
        add a:add a:add a:add a
        ld c,a
        ld b,0
        ld hl,grpcpctab
        add hl,bc
        ld (grpcpc1+2),hl

        ld hl,scrtable
grpcnv2 ld a,l
grpcnv3 call 0
        inc h:inc h:inc h:inc h
        ld a,l
        cpl
grpcnv4 call 0
        dec h:dec h:dec h:dec h
        inc l
        jr nz,grpcnv2
        ret

grpmsx  ld de,(optcolb)     ;e=background, d=foreground
        ld c,a
        ld a,e
        and 3
        ld e,a
        ld a,d
        and 3
        ld d,a
        call grpmsx1
        inc h
        call grpmsx1
        dec h
        ret
grpmsx1 ld ixl,4
        xor a
grpmsx2 add a
        add a
        rlc c
        ld b,e
        jr nc,grpmsx3
        ld b,d
grpmsx3 or b
        dec ixl
        jr nz,grpmsx2
        ld (hl),a
        ret

grpm16  ld de,(optcolb)     ;e=background, d=foreground
        ld c,a
        ld ixh,4
grpm161 ld ixl,2
        xor a
grpm162 add a:add a:add a:add a
        rlc c
        ld b,e
        jr nc,grpm163
        ld b,d
grpm163 or b
        dec ixl
        jr nz,grpm162
        ld (hl),a
        inc h
        dec ixh
        jr nz,grpm161
        dec h:dec h:dec h:dec h
        ret

grpcpc  ld c,a
        and #f0
        rrca:rrca:rrca:rrca
        call grpcpc1
        inc h
        ld a,c
        and #0f
        call grpcpc1
        dec h
        ret
grpcpc1 ld ix,0
        ld (grpcpc2+2),a
grpcpc2 ld a,(ix+0)
        ld (hl),a
        ret

grpcpctab
    db #00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00 ;pen0pap0
    db #F0,#E0,#D0,#C0,#B0,#A0,#90,#80,#70,#60,#50,#40,#30,#20,#10,#00 ;pen0pap1
    db #0F,#0E,#0D,#0C,#0B,#0A,#09,#08,#07,#06,#05,#04,#03,#02,#01,#00 ;pen0pap2
    db #FF,#EE,#DD,#CC,#BB,#AA,#99,#88,#77,#66,#55,#44,#33,#22,#11,#00 ;pen0pap3
    db #00,#10,#20,#30,#40,#50,#60,#70,#80,#90,#A0,#B0,#C0,#D0,#E0,#F0 ;pen1pap0
    db #F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0,#F0 ;pen1pap1
    db #0F,#1E,#2D,#3C,#4B,#5A,#69,#78,#87,#96,#A5,#B4,#C3,#D2,#E1,#F0 ;pen1pap2
    db #FF,#FE,#FD,#FC,#FB,#FA,#F9,#F8,#F7,#F6,#F5,#F4,#F3,#F2,#F1,#F0 ;pen1pap3
    db #00,#01,#02,#03,#04,#05,#06,#07,#08,#09,#0A,#0B,#0C,#0D,#0E,#0F ;pen2pap0
    db #F0,#E1,#D2,#C3,#B4,#A5,#96,#87,#78,#69,#5A,#4B,#3C,#2D,#1E,#0F ;pen2pap1
    db #0F,#0F,#0F,#0F,#0F,#0F,#0F,#0F,#0F,#0F,#0F,#0F,#0F,#0F,#0F,#0F ;pen2pap2
    db #FF,#EF,#DF,#CF,#BF,#AF,#9F,#8F,#7F,#6F,#5F,#4F,#3F,#2F,#1F,#0F ;pen2pap3
    db #00,#11,#22,#33,#44,#55,#66,#77,#88,#99,#AA,#BB,#CC,#DD,#EE,#FF ;pen3pap0
    db #F0,#F1,#F2,#F3,#F4,#F5,#F6,#F7,#F8,#F9,#FA,#FB,#FC,#FD,#FE,#FF ;pen3pap1
    db #0F,#1F,#2F,#3F,#4F,#5F,#6F,#7F,#8F,#9F,#AF,#BF,#CF,#DF,#EF,#FF ;pen3pap2
    db #FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF ;pen3pap3

        
    include "symbos_lib-Kernel.asm"
    include "symbos_lib-SystemManager.asm"

mysaferam   ds 100

    include "doom.asm"


;==============================================================================
;### DATA AREA ################################################################
;==============================================================================

prgdatbeg

            ds 32-10               ;32byte alignment
gamescr16   db 48,96,64:dw $+7:dw $+4,48*64:db 5
gamescreen  equ $-3
            ds 96/2*64

gamescreen_head4
            db 24,96,64

systitinft   db "DOOM",0

    include "bitmaps.asm"


;==============================================================================
;### TRANSFER AREA ############################################################
;==============================================================================

prgtrnbeg

prgicn16c db 12,24,24:dw $+7:dw $+4,12*24:db 5
    db #88,#D8,#88,#88,#88,#88,#88,#88,#8D,#88,#88,#88,#88,#D8,#88,#D8,#88,#88,#88,#88,#8D,#88,#8D,#88,#88,#8D,#88,#D8,#88,#88,#88,#88,#8D,#88,#DD,#88,#88,#8D,#D8,#DD,#EE,#EF,#FF,#EE,#DD,#8D,#D8,#88
    db #88,#88,#DD,#DD,#FF,#FF,#FF,#FF,#3D,#DD,#D8,#88,#8D,#80,#DD,#DD,#F3,#3F,#F3,#3F,#DD,#DD,#E8,#D8,#88,#DD,#33,#DD,#FF,#33,#33,#FF,#DD,#33,#DD,#88,#88,#DD,#33,#33,#3F,#F3,#33,#FF,#33,#33,#D3,#88
    db #8E,#33,#33,#33,#33,#F3,#3F,#F3,#33,#3F,#33,#E8,#83,#33,#33,#33,#33,#33,#33,#33,#33,#33,#33,#30,#E3,#33,#3D,#D3,#33,#99,#99,#33,#3D,#DD,#33,#3E,#F3,#33,#3D,#EE,#D3,#96,#69,#3D,#EE,#DD,#33,#3F
    db #33,#3D,#D3,#DE,#ED,#E9,#9E,#DE,#ED,#33,#D3,#33,#33,#31,#DD,#3D,#EE,#EE,#EE,#EE,#D3,#3D,#13,#33,#33,#D1,#1D,#D3,#3D,#EE,#DE,#ED,#3D,#DD,#3D,#33,#E3,#F3,#D1,#5D,#3D,#33,#D3,#D3,#DD,#1D,#3F,#FE
    db #83,#33,#D3,#15,#5D,#5D,#D5,#51,#51,#3D,#DF,#3E,#8E,#33,#33,#31,#11,#51,#11,#1D,#13,#33,#33,#38,#8D,#DD,#3F,#F3,#DD,#DD,#3D,#3D,#3F,#F3,#33,#D8,#8D,#D3,#33,#FF,#33,#FF,#FF,#F3,#FF,#33,#3D,#D8
    db #88,#ED,#D3,#33,#33,#3F,#F3,#3F,#33,#33,#DE,#88,#88,#8D,#D3,#33,#3F,#E3,#3D,#E3,#33,#3D,#D8,#88,#88,#88,#88,#ED,#ED,#DD,#3D,#DD,#DD,#88,#88,#88,#88,#88,#88,#88,#ED,#D8,#8D,#DE,#88,#88,#88,#88

;### PRGPRZS -> Stack for application process
        ds 128
        ds 128
prgstk  ds 6*2
         dw prgprz

App_PrcID
prgprzn  db 0

App_MsgBuf
prgmsgb ds 14

;### GAMTIMS -> stack for timer process
        ds 128
gamtims ds 6*2
        dw InterruptRoutine
;gamtimn db 0

cfgcpctyp    db 0

;### Game screen ##############################################################

syswininf    dw #1501,0,79,20,112,104,0,0,112,104,112,104,112,104,prgicnsml,systitinft,0,0,winempgrp,0,0:ds 136+14

winempgrp    db  1,0: dw winempdat,0,0,0,0,0,0          ;empty form for startup
winempdat    dw  0,255*256+0, 128+1, 0,0, 1,1, 0

wingamgrp    db 12,0: dw wingamdat,0,0,0,0,0,0          ;gameplay form
wingamdat
    dw 00,255*256+10, gameframe1,     0, 0, 112,  8,0
wingam_scr
    dw 01,255*256+10, gamescr16,      8, 8,  96, 64,0
    dw 02,255*256+10, gameframe2,     0, 8,   8, 64,0
    dw 03,255*256+10, gameframe3,   104, 8,   8, 64,0
    dw 04,255*256+10, gameframe4,     0,72, 112, 32,0
hud_ammo_num = 5
hud_ammo
    dw 05,255*256+10, fntnume,        4,84,  10, 11,0
    dw 06,255*256+10, fntnume,       15,84,  10, 11,0
    dw 07,255*256+10, fntnume,       26,84,  10, 11,0
hud_hlth_num = 8
hud_hlth
    dw 08,255*256+10, fntnume,    72+ 4,84,  10, 11,0
    dw 09,255*256+10, fntnume,    72+15,84,  10, 11,0
    dw 10,255*256+10, fntnume,    72+26,84,  10, 11,0
hud_face_num = 11
hud_face
    dw 11,255*256+10, face0,         44,82,  24, 21,0

winmengrp    db  8,0: dw winmendat,0,0,0,0,0,0          ;menu form
winmendat
    dw 00,255*256+ 0, 128+1,          0, 0, 112,104,0
    dw 01,255*256+10, menu_logo,     12, 1,  88, 55,0
winmen_cur_num = 2
winmen_cur
    dw 02,255*256+ 0, 128+1,         14,56,  12,  9,0
    dw 03,255*256+10, menu_skull,    14,56,  12,  9,0
    dw 04,255*256+ 8, menu_new,      29,57,  60,  8,0
    dw 05,255*256+ 8, menu_options,  29,67,  60,  8,0
    dw 06,255*256+ 8, menu_read,     29,77,  60,  8,0
    dw 07,255*256+ 8, menu_quit,     29,87,  60,  8,0

winsklgrp    db  8,0: dw winskldat,0,0,0,0,0,0          ;skill form
winskldat
    dw 00,255*256+ 0, 128+1,          0, 0, 112,104,0
    dw 01,255*256+10, menu_logo,     12, 1,  88, 55,0
winskl_cur
    dw 02,255*256+ 0, 128+1,          2,76,  12,  9,0
    dw 03,255*256+10, menu_skull,     2,76,  12,  9,0
    dw 04,255*256+ 8, menu_skill,    17,57,  60,  8,0
    dw 05,255*256+ 8, menu_skill1,   17,67,  60,  8,0
    dw 06,255*256+ 8, menu_skill2,   17,77,  60,  8,0
    dw 07,255*256+ 8, menu_skill3,   17,87,  60,  8,0

winoptgrp    db 12,0: dw winoptdat,0,0,0,0,0,0          ;options form
winoptdat
    dw 00,255*256+ 0, 128+1,          0, 0, 112,104,0
    dw 01,255*256+10, menu_logo,     12, 1,  88, 55,0
winopt_cur
    dw 02,255*256+ 0, 128+1,          2,66,  12,  9,0
    dw 03,255*256+10, menu_skull,     2,66,  12,  9,0
    dw 04,255*256+ 8, opt_colours,   17,57,  48,  7,0
    dw 05,255*256+ 8, opt_fore,      17,67,  28,  7,0
    dw 06,255*256+ 8, opt_back,      17,77,  28,  7,0
    dw 07,255*256+ 8, opt_ground,    44,69,  44,  5,0
    dw 08,255*256+ 8, opt_ground,    45,79,  44,  5,0
    dw 09,255*256+ 8, opt_swap,      17,87,  32,  7,0
winopt_col_num = 10
winopt_col
    dw 10,255*256+ 2, 256*103+192+1, 90,67,   7,  7,0
    dw 11,255*256+ 2, 256*103+192+8, 90,77,   7,  7,0

prgtrnend

 relocate_table
 relocate_end
