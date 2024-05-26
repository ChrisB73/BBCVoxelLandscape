; Voxel Asm
; By Chris Bradburne
; Based on https://github.com/s-macke/VoxelSpace/tree/master?tab=readme-ov-file
; Tube Low Res Version 

codestart=&800     ; Code load address
maxpos=&17        ; Position of landscape data

oswrch=&ffee
osbyte=&fff4
osword=&fff1
osfile=&ffdd

cpu 1

scale=&0
left_x=&2
dx=&4
oy=&6          ; 2 bytes - allow 4
horizon=&d
height=&e
player_x=&16
player_y=&18
drawdistance=&2a
zpointer=&2b
drawloopvalue=&2e

startplot=&32
screenheight=200

scalestore=&35

key1=&58
key2=&59

lastvalue=&60
lastmultiplier=&61
drawvalue=&62
drawheight=&63

renderstatus=&70
drawingflag=&71
crtcflag=&72

iscaled=&75         ; 2 bytes
screenstart=&77 ; 2 bytes
multiplier=&79
multresult=&7a  ; 3 bytes
value=&7d

drawingparamblock=&7e
linelength=&7e
plotposition=&7f    ; 2 Bytes
colour=&81
topposition=&82
plotposition2=&83
tubestatus=&87

temp=&88            ; 3 bytes
tmpa=&8c

size=128
screenwidth=32

org codestart
guard maxpos*256

; Square table for multiply 
.sqrLow
FOR i, 0, 511
equb lo((i*i)/4)
NEXT    
.sqrHigh
FOR i, 0, 511
equb hi((i*i)/4)
NEXT    

;Lookup data
include "voxlookup8.asm"

; Colour table for quick access
.colourtable
; Right hand pixels
FOR i, 0, 127
IF i=0
equb %00110000  ; Blue
elif i<5
equb %00001111  ; Yellow
elif i<19
equb %00001100  ; Green
elif i<&37
equb %00000011  ;Red
elif i<&57
equb %00110011 ; Magenta
else
equb %00111111 ; White
ENDIF
NEXT    


.yb
skip &20             ; Buffer for max hight drawn
.lb
skip &20             ; Buffer for last drawn
.cb
skip &20             ; Buffer for colour
.yb2
skip &20             ; Buffer for max hight drawn
.lb2
skip &20             ; Buffer for last drawn
.cb2
skip &20             ; Buffer for colour

.entrypoint
    ldx #1
    ldy #0
    lda #200
    jsr osbyte      ; Prevent Escape - removes the need for escape handling on input.

    ; Implode character definitions
    lda #20
    ldx #0
    ldy #0
    jsr osbyte


    lda #lo(instructions)
    ldx #hi(instructions)
    jsr printtext

    lda #lo(instructions2)
    ldx #hi(instructions2)
    jsr printtext

; Load IO Code
    lda #hi(iocodestart)
    sta fileblock+3
    lda #0
    sta fileblock+2
    sta fileblock+6
    lda #255
    sta fileblock+4
    sta fileblock+5

    lda #lo(bootfilename)
    sta fileblock
    lda #hi(bootfilename)
    sta fileblock+1

    ldx #lo(fileblock)
    ldy #hi(fileblock)
    lda #&ff
    jsr osfile           ; Osfile - Load

    jsr loadfile

    lda #lo(presskeytxt)
    ldx #hi(presskeytxt)
    jsr printtext

    ldx #0
    ldy #25
    lda #129
    jsr osbyte          ; Wait for key press

    cli

    lda #0
    ldx #lo(renderstatus)
    ldy #hi(renderstatus)
    jsr setmem           ; Ensure no redirect

    ; Redirect OSWRCH to my routine
    ldx #lo(&20e)
    ldy #hi(&20e)
    jsr getmem
    ldx #lo(drawstatepassthrough+1)
    ldy #hi(drawstatepassthrough+1)
    jsr setmem

    ldx #lo(&20f)
    ldy #hi(&20f)
    jsr getmem
    ldx #lo(drawstatepassthrough+2)
    ldy #hi(drawstatepassthrough+2)
    jsr setmem

    ldx #lo(&20e)
    ldy #hi(&20e)
    lda #lo(drawstatemachine)
    jsr setmem
    ldx #lo(&20f)
    ldy #hi(&20f)
    lda #hi(drawstatemachine+1)
    jsr setmem

.voxelstart
; Main entry point after initialisation.

; Set screen mode and background colour
    lda #22
    jsr oswrch
    lda #2
    jsr oswrch

    lda #&09
    sta crtcregs+12
    lda #&c0
    sta crtcregs+13

; Resize screen mode
   ; Set CRTC
    ldx #13
.crtcloop
    jsr setcrtc

    dex
    bpl crtcloop

; Set initial variables
    lda #16:sta player_x+1
    lda #0:sta player_x:sta player_y
    sta player_y+1
    lda #4:sta startplot

    lda #223:sta height
    lda #32:sta horizon
    lda #48:sta drawdistance            ;160 mark

    lda #&4e
    sta screenstart

    lda #4
    ldx #lo(drawingflag)
    ldy #hi(drawingflag)
    jsr setmem

    lda #0
    ldx #lo(crtcflag)
    ldy #hi(crtcflag)
    jsr setmem


; Main draw procedure
.mainloop

    lda #128
    ldx #lo(renderstatus)
    ldy #hi(renderstatus)
    jsr setmem
;    sta renderstatus            ; Set render flag to intercept oswrch

;  Reset heigh buffer
    ldx #screenwidth-1
    lda #screenheight            ; 200 in lowest line (25*8)
.resetlp
    sta yb,x
    sta lb,x
    sta yb2,x
    sta lb2,x
    stz cb,x                    ; No colour
    stz cb2,x                    ; No colour
    dex
    bpl resetlp

    ldx startplot ; Starting location for scale. Stting this too high means
           ; some of the very closest rows might not be drawn
           ; Too low and unnecessary calculations.

;Z loop
.rowloop
    ;     left_x% = (player_x - z)*256     
    stx zpointer
    sec
    lda player_x
    sbc zlo,X
    sta left_x
    lda player_x+1
    sbc zhi,X
    sta left_x+1
    ;dx%=(right_x% - left_x%)/(SW%/2)
    lda zlo,X
    sta dx
    lda zhi,X
    ; Divide by 16
    lsr a:ror dx
    lsr a:ror dx
    lsr a:ror dx
    lsr a:ror dx
    sta dx+1

    ;left_y = player_y + z
    ; Store into temp
    lda zlo,X
    asl A
    sta temp
    lda zhi,X
    rol A
    sta temp+1

    clc
    lda player_y
    adc temp            ; Not stored as we don't care about low result
    lda player_y+1
    adc temp+1

    ;      oy%=((INT(left_y/2)MOD size%)*size%)+outpic
    ; A contains Y hi
;    lsr A       ; lst_y/2 MOD side% : Size%=128
    and #127
    ldy #0
    sty temp
    lsr A
    ror temp
    lsr A
    ror temp
    sta temp+1  ; * 32 by moving right 3

    lda temp
    sta oy      ; data offset start page aligned
    lda temp+1
    clc
    adc #maxpos    ; Location of landscape data
    sta datasource+1

    ldy #0

; Shift scale down to allow 8x8 multiply
    ldx zpointer
    lda scalelo,x
    sta scale
    lda scalehi,X

    beq noshiftdown
.shiftdownlp
    lsr a
    ror scale
    iny
    cmp #0
    bne shiftdownlp
.noshiftdown
    sta scale+1
    sty scalestore

    ldx #0
    lda #8
    sta iscaled

    lda screenstart            ; Screen start
    sta iscaled+1       ; iscaled = i*8 (x*8) + screenstart



.columnloop
    ;      B%=(INT(left_x%DIV256)MODsize%)+oy%
    lda left_x+1
    and #127
    ora oy
    sta datasource
datasource=P%+1
    lda datasource
    ;      V%=?B%
    sta value

    phx
    ;      L% = ((H%-V%) * scale%)DIV256 + Z%
    lda height
    sec
    sbc value
    php
    bcs notneg
    eor #&FF
    sec
    adc #0              ; Negate for multiply.

.notneg
    sta multiplier
 ; *scale


; Do multiply
    ldx scale
    lda multiplier
    sta getLow+1
    sta getHigh+1

    stx msubpointer+1
    sec
.msubpointer
    sbc #0
    bcs doMult
    sbc #0
    eor #255
.doMult
    tay
.getLow
    lda sqrLow,x
    sbc sqrLow,y
    sta multresult
.getHigh
    lda sqrHigh,x
    sbc sqrHigh,y

    ldy scalestore
    beq noshiftup

    ; Shift result back up
.shiftuplp
    asl multresult
    rol a
    bcs clearstack              ; Overflow so no draw
    dey
    bne shiftuplp

.noshiftup
    sta multiplier+1
    plp
    bcs adddiff             ; Positive from before multiply

    ; Subtract from horizon
    sec
    lda horizon
    sbc multiplier+1
    bcs drawcarryon     ; No overflow
    lda #0               ; Set to min
    bcc drawcarryon     ; Always taken

.clearstack
    lda #255                          ; Set to max
    bne noshiftup                   ; Always taken 

; DIV 256 means just using multiplier+1 = length
.adddiff
    ; Add Add to horizon
    clc
    lda horizon
    adc multiplier+1                 ; Add Z
    bcc drawcarryon                        ; no Overflow 
    lda #255                          ; Set to max

.drawcarryon
    plx
    cmp yb,x 
    sta multiplier+1
    bcs nodraw

;    sta drawheight
    lda cb,x
    bne notfirstcolour                ; Not first colour
    ldy value
    lda colourtable,y
    sta cb,X
    bne nodrawthistime              ; Always taken 

.notfirstcolour
    ldy value
    lda colourtable,y
    cmp cb,X
    beq nodrawthistime ; Colour is same as previous line - don't doraw yet

.linetodraw
    tay
    lda cb,X    ; Get prebious colour
    jsr oswrch  ; Set colour  
    tya
    sta cb,x    ; Store new colour

    ; Have a line to draw
        ; Calculate vertical position
    lda yb,x
    and #%11111000              ; Remove bottom bits
    lsr a:lsr a         ; Convert to 512*lines
    ;clc                    ; Not necessary because bottom bits cleared
    adc iscaled+1
    jsr oswrch                  ; Plot position+1
    lda yb,x
    and #%111                   ; Bottom bits
    ora iscaled
    jsr oswrch                  ; Plot position

    lda lb,x
    sec
    sbc yb,x
    jsr oswrch                  ; Line length

    lda yb,x
    sta lb,x                    ; Store new start position
    
.nodrawthistime
    lda multiplier+1
    sta yb,X        ; Store new max height

;      IF L% < yb%?I% THEN PROCdraw_vline(I%, L%, yb%?I%):yb%?I%=L%
.nodraw
    lda iscaled
    sec
    sbc #8
    sta iscaled
    lda iscaled+1
    sbc #0
    sta iscaled+1
    ; Do odd column
    lda value           ; Load currnt value and store
    tay                
    clc
    adc lastvalue           ; Add to previous value
    sty lastvalue       ; Store current to last
    ror A               ; Average
    sta value           ; Store to new value

    lda multiplier+1    ; Current top position
    tay
    clc
    adc lastmultiplier  ; LAst top position
    sty lastmultiplier  ; Store current to last
    ror a
    sta multiplier+1

    cpx #0
    beq nodraw2

    cmp yb2,x 
    bcs nodraw2

;    sta drawheight
    lda cb2,x
    bne notfirstcolour2                ; Not first colour
    ldy value
    lda colourtable,y
    sta cb2,X
    bne nodrawthistime2              ; Always taken 

.notfirstcolour2
    ldy value
    lda colourtable,y
    cmp cb2,X
    beq nodrawthistime2 ; Colour is same as previous line - don't doraw yet

.linetodraw2
    tay
    lda cb2,X    ; Get prebious colour
    jsr oswrch  ; Set colour  
    tya
    sta cb2,x    ; Store new colour

    ; Have a line to draw
        ; Calculate vertical position
    lda yb2,x
    and #%11111000              ; Remove bottom bits
    lsr a:lsr a         ; Convert to 512*lines
    ;clc                    ; Not necessary because bottom bits cleared
    adc iscaled+1
    jsr oswrch                  ; Plot position+1
    lda yb2,x
    and #%111                   ; Bottom bits
    ora iscaled
    jsr oswrch                  ; Plot position

    lda lb2,x
    sec
    sbc yb2,x
    jsr oswrch                  ; Line length

    lda yb2,x
    sta lb2,x                    ; Store new start position
    
.nodrawthistime2
    lda multiplier+1
    sta yb2,X        ; Store new max height

;      IF L% < yb%?I% THEN PROCdraw_vline(I%, L%, yb%?I%):yb%?I%=L%
.nodraw2

;      left_x%=left_x%+ dx%
    lda iscaled
    clc
    adc #24
    sta iscaled
    lda iscaled+1
    adc #0
    sta iscaled+1

;    clc
    lda left_x
    adc dx
    sta left_x
    lda left_x+1
    adc dx+1
    sta left_x+1        

    inx
    cpx #32             ; Screen width
    beq columnloopfinished
    jmp columnloop      ; Branch > 127
.columnloopfinished

    ;  UNTIL z>=distance

    ldx zpointer
    inx
    cpx drawdistance
    bcs doneframe
    jmp rowloop

.doneframe

.drawsky
    ; now do final cyan sky
    ldx #screenwidth-1

    lda #lo(512-8)
    sta iscaled
    lda screenstart
    clc
    adc #hi(512-8)
    sta iscaled+1

.skyloop
    ; Draw final line
    lda cb,X    ; Get prebious colour
    jsr oswrch  ; Set colour  
    lda yb,x
    and #%11111000              ; Remove bottom bits
    lsr a:lsr a;:lsr a         ; Convert to 512*lines
    ;clc                    ; Not necessary because bottom bits cleared
    adc iscaled+1
    jsr oswrch                  ; Plot position+1
    lda yb,x
    and #%111                   ; Bottom bits
    ora iscaled
    jsr oswrch                  ; Plot position

    lda lb,x
    sec
    sbc yb,x
    jsr oswrch                  ; Line length

.skynofinaldraw
    lda yb,X
    beq skipsky1

; Start pos =0
    lda #%00111100
    jsr oswrch              ; Colour
    lda iscaled+1
    jsr oswrch              ; plotpos+1
    lda iscaled
    jsr oswrch              ; plotpos

    lda yb,X
    jsr oswrch              ; Linelength

.skipsky1
    sec
    lda iscaled
    sbc #8
    sta iscaled
    lda iscaled+1
    sbc #0
    sta iscaled+1

    cpx #0
    beq skipsky2

;Do odd lines
    lda cb2,X    ; Get prebious colour
    jsr oswrch  ; Set colour  
    lda yb2,x
    and #%11111000              ; Remove bottom bits
    lsr a:lsr a;:lsr a         ; Convert to 512*lines
    ;clc                    ; Not necessary because bottom bits cleared
    adc iscaled+1
    jsr oswrch                  ; Plot position+1
    lda yb2,x
    and #%111                   ; Bottom bits
    ora iscaled
    jsr oswrch                  ; Plot position

    lda lb2,x
    sec
    sbc yb2,x
    jsr oswrch                  ; Line length

.skynofinaldraw2
    lda yb2,X
    beq skipsky2

; Start pos =0
    lda #%00111100
    jsr oswrch              ; Colour
    lda iscaled+1
    jsr oswrch              ; plotpos+1
    lda iscaled
    jsr oswrch              ; plotpos

    lda yb2,X
    jsr oswrch              ; Linelength

.skipsky2
    sec
    lda iscaled
    sbc #8
    sta iscaled
    lda iscaled+1
    sbc #0
    sta iscaled+1

    dex
    bmi waitforio
    jmp skyloop

.waitforio
    dex
    bne waitforio         ; Give IO processor some breathing space
    ldx #lo(tubestatus)
    ldy #hi(tubestatus)
    jsr getmem
    bmi waitforio

    lda #64
    ldx #lo(renderstatus)
    ldy #hi(renderstatus)
    jsr setmem

    lda crtcregs+12
    jsr oswrch          ; Set screen start
    lda crtcregs+13
    jsr oswrch          ; And swap screen

    lda crtcregs+12
    eor #&A             ; Swap between 09 and 03
    sta crtcregs+12
    lda crtcregs+13
    eor #&40            ; Swap between &C and &9
    sta crtcregs+13

    lda screenstart
    eor #&52            ; Change between &4e &1c
    sta screenstart

.skipcrtc

    ; Process keys
.checkkeylp
    ldx #&Ed
    ldy #0
    jsr getmem
;    lda &Ed
    bmi gotkey

;    lda &Ec
 
    ldx #&Ec
    ldy #0
    jsr getmem
    bpl checkkeylp          ; Wait for key.

.gotkey
    ldx #&Ed
    ldy #0
    jsr getmem
    sta key1

    ldx #&Ec
    ldy #0
    jsr getmem
    sta key2

    lda #33+128             ;W
    jsr keycheck
    bne notw
    inc player_y+1
.notw
    lda #81+128             ;s
    jsr keycheck
    bne nots
    dec player_y+1
.nots
    lda #65+128             ;A
    jsr keycheck
    bne nota
    dec player_x+1
.nota
    lda #50+128             ;D
    jsr keycheck
    bne notd
    inc player_x+1
.notd

    lda #69+128             ;J
    jsr keycheck
    bne notj
    ldx horizon
    inx:inx:inx
    bmi notj
    stx horizon
.notj
    lda #85+128             ;N
    jsr keycheck
    bne notn
    ldx horizon
    dex:dex:dex
    bmi notn
    stx horizon
.notn


    lda #70+128             ;K
    jsr keycheck
    bne notk
    lda height
    clc
    adc #8
    cmp #8
    bcc notk
    sta height
.notk
    lda #101+128             ;M
    jsr keycheck
    bne notm
    lda height
    sec
    sbc #8
    cmp #8
    bcc notm
    sta height
.notm

    lda #84+128             ;H
    jsr keycheck
    bne noth
    lda drawdistance
    cmp #128-4
    bcs noth
    clc
    adc #4
    sta drawdistance

.noth
    lda #100+128             ;B
    jsr keycheck
    bne notb
    lda drawdistance
    cmp #16+4
    bcc notb
    sec
    sbc #4
    sta drawdistance
.notb

    lda #83+128             ;G
    jsr keycheck
    bne notg
    ldx startplot
    inx
    cpx #16
    bcs notg
    stx startplot

.notg
    lda #99+128             ;V
    jsr keycheck
    bne notv
    ldx startplot
    dex
    cpx #16
    bcs notv
    stx startplot
.notv

    lda #86+128             ;L
    jsr keycheck
    bne notl
    jmp loadnew

.notl

;    inc player_y+1
;    dec height:dec height
;    inc horizon
    jmp mainloop


.keycheck
    cmp key1
    beq keycheckend
    cmp key2
.keycheckend
    rts

.dodraw
;    lda multiplier+1
.docolour
;    lda drawheight
;    jsr oswrch              ; Toppoistion 
    ; Select colour
    ldy #%00110000           ;4
    lda drawvalue
    beq dogcol
    ldy #%00001111  ;3
    cmp #5
    bcc dogcol
    ldy #%00001100         ;2
    cmp #19
    bcc dogcol
    ldy #%00111111           ;7
    cmp #&57
    bcs dogcol
    ldy #%00110011            ;5
    cmp #&37
    bcs dogcol
    ldy #%00000011               ;1
.dogcol
    tya
    ;sta colour
    jmp oswrch

.setcrtc
    ; sets crtc variable X
    lda #23
    jsr oswrch
    lda #0
    jsr oswrch
    txa
    jsr oswrch
    lda crtcregs,X
    jsr oswrch

    ldy #6
    lda #0
.sendzerolp
    jsr oswrch
    dey
    bne sendzerolp
    rts

.crtcregs
equb &7F ; R0  horizontal total
equb 64 ; R1  horizontal displayed
equb 90 ; R2  horizontal position
equb &28 ; R3  sync width
equb &28 ; R4  vertical total
equb 0 ; R5  vertical total adjust
equb 25 ; R6  vertical displayed
equb 33 ; R7  vertical position
equb &00 ; R8  interlace
equb &7 ; R9  scanlines per row
equb &32 ; R10 cursor start
equb &0 ; R11 cursor end
equb &0c  ;R12 Screen start hi 
equb &e0 ;R13 Screen Start lo

.loadnew
    lda #0
    ldx #lo(renderstatus)
    ldy #hi(renderstatus)
    jsr setmem

    lda #lo(loadprompt)
    ldx #hi(loadprompt)
    jsr printtext

.loadkeylp2
    lda &Ec
    bne loadkeylp2
    lda &Ed
    bne loadkeylp2

    ; Clear buffers
    ldx #0
    lda #15
    jsr osbyte


.loadkeylp
    ldx #255
    ldy #120
    lda #129
    jsr osbyte          ; inkey
    bcs loadkeylp
    cpx #'A'
    bcc loadkeylp
    cpx #'N'
    bcs loadkeylp       ; Chek range
    txa
    jsr oswrch          ; Print character
    sta filename+2      ; Store in filename

    lda #lo(loadingtext)
    ldx #hi(loadingtext)
    jsr printtext       ; Print loading message 

    jsr loadfile        ; Load file and decompress
    jmp voxelstart      ; Go to start

.printtext
    ; Print text - takes A,X as text loction
    ; Zero terminated
    sta printtextlp+1
    stx printtextlp+2
    ldx #0
.printtextlp
    lda &3000,x
    jsr oswrch
    inx
    cmp #0
    bne printtextlp
    rts

.loadfile
; Load file
    lda #maxpos
    sta fileblock+3
    lda #0
    sta fileblock+2
    sta fileblock+4
    sta fileblock+5
    sta fileblock+6

    lda #lo(filename)
    sta fileblock
    lda #hi(filename)
    sta fileblock+1

    ldx #lo(fileblock)
    ldy #hi(fileblock)
    lda #&ff
    jmp osfile           ; Osfile - Load

.getmem
    ; Routin to get memory from io processor
    ; Takes X,Y as memory to get
    ; Returns memory in A
    stx memaccessblock
    sty memaccessblock+1
    lda #5
    ldx #lo(memaccessblock)
    ldy #hi(memaccessblock)
    jsr osword
    lda memaccessblock+4
    rts

.setmem
    ; Routin to set memory in io processor
    ; Takes X,Y as memory to set
    ; A is value to set
    stx memaccessblock
    sty memaccessblock+1
    sta memaccessblock+4
    lda #6
    ldx #lo(memaccessblock)
    ldy #hi(memaccessblock)
    jmp osword

.fileblock
equw filename
equd &4e00          ; Load address
equd 0              ; Execution Addres
equd 0
equd 0

.memaccessblock
equd 0
equb 0

.filename
equb "M.A",13

.bootfilename
equb "IOCode",13

.loadprompt
equb 22,7,13,10,"Which Landscape (A-M)?",0
.loadingtext
equb 13,10,10,"Loading...",0


.instructions
equs 22,7,10,"Voxel Landscape Demo",13,10
equs      "====================",13,10
equs "Mode 2 Tube Low Resoution",13,10,13,10
equs "WASD : Move Horizontally",13,10
equs "K/M : Move Up/Down",13,10
equs "J/N : Pitch Up/Down",13,10
equs "H/B : Draw Distance Further/Closer",13,10,0
.instructions2
equs "G/V : Draw Start Further/Closer",13,10
equs "L : Load New Landscape",13,10,10
equs "Loading initial landscape...",0
.presskeytxt
equb "Done!",13,10,10,"Press a key to start.",0

print "Tube Code  ",~codestart,~P%,~entrypoint
save "!Boot",codestart,P%,entrypoint

cpu 0

iocodestart=&1a00
clear iocodestart,iocodestart+512
org iocodestart
guard iocodestart+512

.drawstatemachine
    bit renderstatus
    bmi drawentry
    bvs doscreenset
.drawstatepassthrough
    jmp &3000                     ; Should return to the OS.

.doscreenset
    ldy crtcflag
    bne dsssecond
    sta crtcflag
    rts
.dsssecond
    pha 
    lda #19
    jsr osbyte
    sei
    pla
    ldx #13
    jsr setcrtcio
    ldx #12
    lda crtcflag
    jsr setcrtcio
    lda #0
    sta crtcflag
    sta renderstatus
    cli
    rts

.drawentry
    ldx drawingflag
    sta drawingparamblock-1,X
    dex 
    stx drawingflag
    beq drawline              ; All parameters loaded
    rts

.drawline
    ; Takes Multiplier as line length
    ; Multiplier+1 as start location
    ; colour as colour 
    ; iscaled as start location
;    lda topposition
;    and #7              ; Invert 

    lda plotposition
    and #7
    sta topposition                 ; save start offset
    lda plotposition
    and #%11111000
    sta plotposition                ; Strip bottom bits

    ; Draw first group of 8
    ; Work out if there are less bytes than a single group
    lda linelength
    cmp #8
    bcs dlmorethantop    ; More than 8 length so must be to end of first group

    lda topposition
    adc linelength
    cmp #8              ; More than 8 so is to end of first group
    bcs dlmorethantop
    ; Not more than top

    ldy linelength
    lda branchtabler,y
    sta topbranch+1     ; Store as branch
    ldy topposition

    lda colour
.topbranch
    bpl dllessstart     ; Always taken
.dllessstart
    ; Line drawing. Code is branched into to save loop checking 
    sta (plotposition),Y:iny
    sta (plotposition),Y:iny
    sta (plotposition),Y:iny
    sta (plotposition),Y:iny
    sta (plotposition),Y:iny
    sta (plotposition),Y:iny
    sta (plotposition),Y
    jmp dlfinished                 ; Finish as it's only single byte

.dlmorethantop
    ; Write a "complete" top byte

    ldy topposition
    lda branchtable,y
    sta topwholebranch+1     ; Write start branch

    lda colour          ; Get colour to plot
.topwholebranch
    bpl dlwholestart     ; Always taken
.dlwholestart
    ; Line drawing. Code is branched into to save loop checking
    sta (plotposition),Y:iny
    sta (plotposition),Y:iny
    sta (plotposition),Y:iny
    sta (plotposition),Y:iny
    sta (plotposition),Y:iny
    sta (plotposition),Y:iny
    sta (plotposition),Y:iny
    sta (plotposition),Y

    ldy topposition
    lda linelength  
    sec
    sbc revtable,y
    sta linelength      ; Reduce by number we just plotted.
    bne dlcarryon
    jmp dlfinished                 ; If zero then finished

.dlcarryon
    cmp #8              ; Do we have whole block to finish?
    bcc lastblock

    lda plotposition
    ora #4              ; Always byte aligned so we can assume or for +4
    sta plotposition2    ; Multresult is used here to save zp usage - not necessary
    lda plotposition+1
    sta plotposition2+1

.dlmainlp
    ldy plotposition+1
    iny:iny
    sty plotposition+1
    sty plotposition2+1

    ldy #0
    lda colour
    ; Line drawing. Draw a block of 8
    sta (plotposition2),Y
    sta (plotposition),Y:iny
    sta (plotposition2),Y
    sta (plotposition),Y:iny
    sta (plotposition2),Y
    sta (plotposition),Y:iny
    sta (plotposition2),Y
    sta (plotposition),Y

    lda linelength
    sec
    sbc #8
    sta linelength      ; Store value
    cmp #8
    bcs dlmainlp        ; More than 8 loop whole block

.lastblock
    ; Draw remaining lines

    ldy linelength      ; Remainder
    beq dlfinished

    inc plotposition+1  ; Increment screen position
    inc plotposition+1  ; Increment screen position

    lda branchtabler,y
    sta bottombranch+1     ; Write last branch

    ldy #0              ; Start at top of block
    lda colour          ; Get colour to plot
.bottombranch
    bpl dlbottomblock     ; Always taken
.dlbottomblock
    sta (plotposition),Y:iny
    sta (plotposition),Y:iny
    sta (plotposition),Y:iny
    sta (plotposition),Y:iny
    sta (plotposition),Y:iny
    sta (plotposition),Y:iny
    sta (plotposition),Y

.dlfinished
    lda #4
    sta drawingflag
    lda &fee0
    sta tubestatus 
    rts

; Table for branch calculation - Reversed
.branchtabler
FOR i, 0, 7
equb 3*(7-i)
NEXT    

; Table for branch calculation - Normal
.branchtable
FOR i, 0, 7
equb 3*i
NEXT    

.revtable
equb 8,7,6,5,4,3,2,1

.setcrtcio
    ; sets crtc variable X to value A
    pha
    lda #23
    jsr drawstatepassthrough
    lda #0
    jsr drawstatepassthrough
    txa
    jsr drawstatepassthrough
    pla
    jsr drawstatepassthrough

    ldy #6
    lda #0
.sendzerolpio
    jsr drawstatepassthrough
    dey
    bne sendzerolpio
    rts


print "io processor code ",~iocodestart,"space",iocodestart+512-P%
save "IOCode",iocodestart,P%,&30000+iocodestart,&30000+iocodestart

putfile "maps\map64x128_11.bin","M.A",&3000
putfile "maps\map64x128_9.bin","M.B",&3000
putfile "maps\map64x128_16.bin","M.C",&3000
putfile "maps\map64x128_15.bin","M.D",&3000
putfile "maps\map64x128_22.bin","M.E",&3000
putfile "maps\map64x128_21.bin","M.F",&3000
putfile "maps\map64x128_1.bin","M.G",&3000
putfile "maps\map64x128_2.bin","M.H",&3000
putfile "maps\map64x128_5.bin","M.I",&3000
putfile "maps\map64x128_6.bin","M.J",&3000
putfile "maps\map64x128_7.bin","M.K",&3000
putfile "maps\map64x128_10.bin","M.L",&3000
putfile "maps\map64x128_25.bin","M.M",&3000
