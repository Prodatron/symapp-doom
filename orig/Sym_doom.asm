;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                           S y m b O S    D O O M                           @
;@                                                                            @
;@                            (c) 2021 by NYYRIKKI                            @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

; 96x64

 org #1000

 output "bin\DOOM.EXE"
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

prgcodbeg    dw prgdatbeg-prgcodbeg  ;length of code area
             dw prgtrnbeg-prgdatbeg  ;length of data area
             dw prgtrnend-prgtrnbeg  ;length of transfer area
prgdatadr    dw #1000                ;original origin                    POST address data area
prgtrnadr    dw relocate_count       ;number of relocator table entries  POST address transfer area
prgprztab    dw prgstk-prgtrnbeg     ;stack length                       POST table processes
             dw 0                    ;crunched data length
prgbnknum    db 0                    ;crunched data type                 POST bank number
             db "DOOM"
            ds 32-4: db 0            ;Name
prgmemtab    db "SymExe10"           ;SymbOS-EXE-identifier              POST table reserved memory areas
             dw 0                    ;additional code memory
             dw 0                    ;additional data memory
             dw 0                    ;additional transfer memory
            ds 28                   ;*reserved*

prgicnsml    db 2,8,8
             ;db #00,#00,#00,#46,#00,#8C,#00,#46,#23,#8C,#33,#08,#33,#8C,#00,#00
              ds 2*8,255
prgicnbig    db 6,24,24
            ds 6*24,#FF


;### PRGPRZ -> Application process
dskprzn      db 2
sysprzn      db 3
windatprz   equ 3   ;Process ID
prgwin       db 0    ;main window ID

prgprz  ld a,(prgprzn)
        ld (syswininf+windatprz),a

        ld c,MSC_DSK_WINOPN
        ld a,(prgbnknum)
        ld b,a
        ld de,syswininf
        call msgsnd             ;open window
prgprz1 call msgdsk             ;get message -> IXL=Status, IXH=sender ID
        cp MSR_DSK_WOPNER
        jp z,prgend             ;memory full -> quit process
        cp MSR_DSK_WOPNOK
        jr nz,prgprz1           ;message is not "window has been opened" -> ignore
        ld a,(prgmsgb+4)
        ld (prgwin),a           ;window has been opened -> store ID
        
        call syschk
        
;---------- START TIMER

        ld hl,gamtims           ;Game-Timer
        ld a,(prgbnknum)
        call SyKernel_MTADDT
        jp c,prgend
	ld (timerid),a

        call disp_menu


;### PRGEND -> quit application
prgend  

	ld a,(timerid)
        ld c,MSC_KRL_MTDELT
        ld l,a
        call SyKernel_Message
        

        ld a,(prgprzn)
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

checkmessages:

        call msgget             ;*** check for messages
        ret nc
        cp MSR_DSK_WFOCUS
        jr z,savefocus
        cp MSR_DSK_WCLICK       ;* window has been clicked?
        jr nz,checkmessages
        ld a,(iy+2)             ;* yes, check what exactly
;        cp DSK_ACT_KEY          ;*** Taste wurde gedrückt
;        jr z,prgkey
        cp DSK_ACT_CLOSE        ;* close clicked
        jr z,prgend
        jp checkmessages
;prgkey
;        ld a,(iy+4)

savefocus
         ld a,(iy+2)
         ld (statfocus),a
         jp checkmessages

statfocus db 0
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
msgsnd  ld a,(dskprzn)
msgsnd1  db #dd:ld h,a
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
        and 15
        cp 8
        ; This is MSX
        ld hl,scrtable+256
        ld l,0
        jp c,cpcgraphloop

msxgraphloop
        xor a
        ld c,l
        rlc c
        adc a,a
        add a,a
        rlc c
        adc a,a
        add a,a
        rlc c
        adc a,a
        add a,a
        rlc c
        adc a,a
        ld (hl),a

        xor a
        inc h
        rlc c
        adc a,a
        add a,a
        rlc c
        adc a,a
        add a,a
        rlc c
        adc a,a
        add a,a
        rlc c
        adc a,a
        ld (hl),a
        dec h
        inc l
        jp nz,msxgraphloop

        ret

cpcgraphloop
        ld a,l
        and #f0
        ld (hl),a
        inc h
        ld a,l
        rlca
        rlca
        rlca
        rlca
        and #f0
        ld (hl),a
        dec h
        inc l
        jp nz,cpcgraphloop
        ret
        
        include "symbos_lib-Kernel.asm"

        include "doom.asm"


scrtable:   ds 512+256

mysaferam   ds 100


;==============================================================================
;### DATA AREA ################################################################
;==============================================================================

prgdatbeg

;### SYMBOS INFO ##############################################################

systiticn    db 2,8,8
             ;db #00,#00,#00,#46,#00,#8C,#00,#46,#23,#8C,#33,#08,#33,#8C,#00,#00
              db #26,#4D,#2B,#46,#FF,#FF,#4D,#9B,#46,#9D,#FF,#FF,#26,#4D,#2B,#46
             ;ds 2*8,255

systitinft   db "zDOOM",0


gamescreen   db 96/4,96,64        ;40?
             ds 96/4*64,240

;==============================================================================
;### TRANSFER AREA ############################################################
;==============================================================================

prgtrnbeg
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

syswininf    dw #1501,0,75,02,96,64,0,0,96,64,96,64,96,64,systiticn,systitinft,0,0,sysgrpinf,0,0:ds 136+14
sysgrpinf    db 1,0: dw sysdatinf,0,0,11*256+11,0,0,11
sysdatinf     ; |_ = count
; dw 00,255*256+0 ,2,           0, 0,1000,1000,0         ;00 Background
  dw 00,255*256+8, gamescreen,    0, 0,   96,64,0

prgtrnend

 relocate_table
 relocate_end
