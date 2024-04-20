; Voxel Asm
; By Chris Bradburne
; Based on https://github.com/s-macke/VoxelSpace/tree/master?tab=readme-ov-file
; With zx02 compressor and multiplication by 

codestart=&1900     ; Code load address
maxpos=&2e00        ; Position of landscape data

oswrch=&ffee
osbyte=&fff4
osword=&fff1
osfile=&ffdd

yb=&400             ; Buffer for max hight drawn
yb2=&480            ; Buffer for max average height

scale=&0
left_x=&2
dx=&4
oy=&6          ; 2 bytes - allow 4
;datasource=&8  ; Overwritten by oy
value=&a
iscaled=&b         ; 2 bytes
horizon=&d
height=&e
colour=&f
multiplier=&10
multresult=&11  ; 2 bytes
player_x=&16
player_y=&18
drawdistance=&2a
zpointer=&2b
screenstart=&2c ; 2 bytes
drawloopvalue=&2e
maskedcolour=&2f
plotposition=&30    ; 2 Bytes

temp=&40            ; 4 bytes

ZX0_src=&80
ZX0_dst=&92
offset=&84      ; ZX02
bitr=&86        ; ZX02
pntr=&88           ; ZX02 - 2 bytes

size=128
screenwidth=32

org codestart
guard maxpos

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

; Table for branch calculation
.branchtable
FOR i, 0, 7
equb 9*i
NEXT    

.voxelstart
; Main entry point after initialisation.

; Set screen mode and background colour
    lda #22
    jsr oswrch
    lda #5
    jsr oswrch          ; Change to mode 5

    ldy #0
    ldx #224
    lda #154
    jsr osbyte          ; ULA for Mode 8

    lda #&f
    sta &360
    lda #&1
    sta &361
    lda #20
    jsr oswrch          ; Mode 8 setup.

    lda #&0c
    sta crtcregs+12     ; Reset crtc screen start
    lda #&e0
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
    lda #0:sta player_y+1
    lda #0:sta player_x:sta player_y

    lda #224:sta height
    lda #32:sta horizon
    lda #48:sta drawdistance            ;160 mark

    lda #&67
    sta screenstart             ; Initialise screen location in memory

; Main draw procedure
.mainloop

;  Reset heigh buffer
    ldx #screenwidth/2
    lda #200           ; 200 in lowest line (25*8)
.ybresetlp
    sta yb-1,x
    sta yb-1+screenwidth/2,x
    sta yb2-1,x
    sta yb2-1+screenwidth/2,x
    dex
    bne ybresetlp

    ldx #4 ; Starting location for scale. Stting this too high means
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
    adc #&2e    ; Location of landscape data
    sta oy+1

    ;FOR I%=1 TO SW%
    ldx #0
.columnloop
    ;      B%=(INT(left_x%DIV256)MODsize%)+oy%
    lda left_x+1
    and #63
    clc
    adc oy
    sta datasource
    ldy oy+1
    adc #0
    sty datasource+1
datasource=P%+1
    lda datasource
    ;      V%=?B%
    sta value
    stx temp
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
;    lda #0
;    sta multiplier+1
 ; *scale

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
    sty temp+1

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

    ldx temp                    ; Restore X
    ldy temp+1
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
    pla
    lda #255                          ; Set to max
    sta &501,X
    sta &581,x
    jmp nodraw

; DIV 256 means just using multiplier+1 = length
.adddiff
    ; Add Add to horizon
    clc
    lda horizon
    adc multiplier+1                 ; Add Z
    bcc drawcarryon                        ; no Overflow 
    lda #255                          ; Set to max

.drawcarryon
    sta &501,X          ; Calculated values
    clc
    adc &500,x          ; Previous calculation
    ror a               ; Average position
    sta &580,x          ; Odd numbered columns
    lda value
    sta &601,X          ; Store height value for colour
    clc
    adc &600,X
    ror a               ; Average height value
    sta &680,X          ; Store height value for colour

;      IF L% < yb%?I% THEN PROCdraw_vline(I%, L%, yb%?I%):yb%?I%=L%
.nodraw
;      left_x%=left_x%+ dx%
    clc
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

    ; We have now calculated all our heights
    ; Step through and draw them all
    ldx #screenwidth-1
.drawrowlp
    ; Multiply X to horizontal position
    stx iscaled
    lda #0
    asl iscaled
    rol A
    asl iscaled
    rol A
    asl iscaled
    rol A
    adc screenstart            ; Screen start
    sta iscaled+1       ; iscaled = i*8 (x*8) + screenstart

    lda &601,X
    sta value
    lda &501,x
    sta multiplier+1


    cmp yb,x 
    bcs drawskip1
    ;PROCdraw_vline(I%, L%, yb%?I%)
    jsr dodrawr

.drawskip1
    cpx #0
    beq drawskip2           ; Don't draw leftmost line as average bad
    lda &680,X
    sta value
    lda &580,x
    sta multiplier+1

    cmp yb2,x 
    bcs drawskip2
    ;PROCdraw_vline(I%, L%, yb%?I%)
    jsr dodrawl            ; Draw lef hand line


.drawskip2
    dex
    bpl drawrowlp

    ;  UNTIL z>=distance

    ldx zpointer
    inx
    cpx drawdistance
    bcs doneframe
    jmp rowloop

.doneframe

    ; now do final cyan sky
    ldx #31
    lda #%00111100      ; Colour 6
    sta colour
.skyloop
    stx iscaled
    lda #0
    sta multiplier+1
    asl iscaled
    rol A
    asl iscaled
    rol A
    asl iscaled
    rol A

    adc screenstart            ; Screen Start
    sta plotposition+1
    lda iscaled
    sta plotposition

    lda yb,X
    sta multiplier
    dec multiplier

    jsr drawliner

    cpx #0
    bne notlastcol
    stx colour              ; Draw leftmost column whole size as zero

.notlastcol
    lda iscaled
    sta plotposition
    lda iscaled+1
    sta plotposition+1
    lda #0
    sta multiplier+1

    lda yb2,X
    sta multiplier
    dec multiplier

    jsr drawlinel

    dex
    bpl skyloop

; Swap screens and update screen starts
    lda #19
    jsr osbyte          ; Wait vsync

    sei
    ldx #13
    jsr setcrtc
    ldx #12
    jsr setcrtc
    cli
    lda screenstart
    eor #&29            ; Change between &4e &1c
    sta screenstart
    lda crtcregs+12
    eor #&05             ; Swap between 09 and 03
    sta crtcregs+12
    lda crtcregs+13
    eor #&20            ; Swap between &C and &9
    sta crtcregs+13

    ; Process keys
.checkkeylp
    lda &Ed
    bmi gotkey
    lda &Ec
    bpl checkkeylp          ; Wait for key.
.gotkey
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
    inx:inx
    bmi notj
    stx horizon
.notj
    lda #85+128             ;N
    jsr keycheck
    bne notn
    ldx horizon
    dex:dex
    bmi notn
    stx horizon
.notn


    lda #70+128             ;K
    jsr keycheck
    bne notk
    lda height
    clc
    adc #6
    cmp #8
    bcc notk
    sta height
.notk
    lda #101+128             ;M
    jsr keycheck
    bne notm
    lda height
    sec
    sbc #6
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
    cmp &Ec
    beq keycheckend
    cmp &Ed
.keycheckend
    rts

.dodrawr
{    ; Select colour
    ldy #%00110000           ;4
    lda value
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
    sta colour

    ; Calculate vertical position
    lda multiplier+1    ; Value
    lsr a:lsr a:lsr a         ; Convert to 256*lines
    clc
    adc iscaled+1
    sta plotposition+1       ; Iscaled is now top of line
    lda iscaled
    sta plotposition

    lda yb,x
    sec
    sbc multiplier+1            ; Total line length
    sta multiplier      ; MUltiplier is total length
    dec multiplier

    lda multiplier+1
    sta yb,x
}
;    jsr drawliner
; Runs into drawline
.drawliner
{    ; Takes Multiplier as line length
    ; Multiplier+1 as start location
    ; colour as colour 
    ; iscaled as start location
    lda colour
    and #%01010101
    sta maskedcolour


    ; Draw first group of 8
    ; Work out if there are less bytes than a single group
    lda multiplier
    cmp #8
    bcs dlmorethantop    ; More than 8 length so must be to end of first group

    lda multiplier+1
    and #7
    sta temp+2                 ; save start offset
    adc multiplier
    cmp #8              ; More than 8 so is to end of first group
    bcs dlmorethantop
    ; Not more than top

    lda multiplier
    eor #7              ; Invert 
    tay
    lda branchtable,y
    sta topbranch+1     ; Store as branch
    ldy temp+2

.topbranch
    bpl dllessstart     ; Always taken
.dllessstart
    ; Line drawing. Code is branched into to save loop checking 
    lda (plotposition),y:and #%10101010:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%10101010:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%10101010:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%10101010:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%10101010:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%10101010:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%10101010:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%10101010:ora maskedcolour:sta (plotposition),Y
    rts                 ; Finish as it's only single byte

.dlmorethantop
    ; Write a "complete" top byte

    lda multiplier+1
    and #7              ; Strip bits
    tay
    lda branchtable,y
    sta topwholebranch+1     ; Write start branch

    lda colour          ; Get colour to plot
.topwholebranch
    bpl dlwholestart     ; Always taken
.dlwholestart
    ; Line drawing. Code is branched into to save loop checking
    lda (plotposition),y:and #%10101010:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%10101010:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%10101010:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%10101010:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%10101010:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%10101010:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%10101010:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%10101010:ora maskedcolour:sta (plotposition),Y


    lda multiplier+1
    and #7
    eor #&f8        ; 07 eor ff
    sec
    adc multiplier  ; Reverse Subtract
    sta multiplier      ; Reduce by number we just plotted.
    bne dlcarryon
    jmp dlfinished      ; If zero then done

.dlcarryon
    cmp #8              ; Do we have whole block to finish?
    bcc lastblock

    lda plotposition
    ora #4              ; Always byte aligned so we can assume or for +4
    sta multresult+1    ; Multresult is used here to save zp usage - not necessary
    lda plotposition+1
    sta multresult+2

.dlmainlp
    clc
    lda plotposition+1
    adc #1
    sta plotposition+1
    sta multresult+2

    ldy #0
    lda colour
    ; Line drawing. Draw a block of 8
    lda (multresult+1),y:and #%10101010:ora maskedcolour:sta (multresult+1),Y
    lda (plotposition),y:and #%10101010:ora maskedcolour:sta (plotposition),Y:iny
    lda (multresult+1),y:and #%10101010:ora maskedcolour:sta (multresult+1),Y
    lda (plotposition),y:and #%10101010:ora maskedcolour:sta (plotposition),Y:iny
    lda (multresult+1),y:and #%10101010:ora maskedcolour:sta (multresult+1),Y
    lda (plotposition),y:and #%10101010:ora maskedcolour:sta (plotposition),Y:iny
    lda (multresult+1),y:and #%10101010:ora maskedcolour:sta (multresult+1),Y
    lda (plotposition),y:and #%10101010:ora maskedcolour:sta (plotposition),Y

    lda multiplier
    sec
    sbc #8
    sta multiplier      ; Store value
    cmp #8
    bcs dlmainlp        ; More than 8 loop whole block

.lastblock
    ; Draw remaining line
    clc
    lda plotposition+1
    adc #1
    sta plotposition+1       ; Increment screen position

    lda multiplier      ; Remainder
    beq dlfinished
    eor #7
    tay
    lda branchtable,y
    sta bottombranch+1     ; Write last branch

    ldy #0              ; Start at top of block
    lda colour          ; Get colour to plot
.bottombranch
    bpl dlbottomblock     ; Always taken
.dlbottomblock
    lda (plotposition),y:and #%10101010:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%10101010:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%10101010:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%10101010:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%10101010:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%10101010:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%10101010:ora maskedcolour:sta (plotposition),Y

.dlfinished
}
    rts

; Drawl is the same cod as drawr but with opposite masks. Also iscaled is not recalculated
.dodrawl
{    ; Select colour
    ldy #%00110000           ;4
    lda value
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
    sta colour

    ; Calculate vertical position
    lda multiplier+1    ; Value
    lsr a:lsr a:lsr a         ; Convert to 256*lines
    clc
    adc iscaled+1
    sta plotposition+1       ; Iscaled is now top of line
    lda iscaled
    sta plotposition

    lda yb2,x
    sec
    sbc multiplier+1            ; Total line length
    sta multiplier      ; MUltiplier is total length
    dec multiplier

    lda multiplier+1
    sta yb2,x
}

; Runs into drawline
.drawlinel
{    ; Takes Multiplier as line length
    ; Multiplier+1 as start location
    ; colour as colour 
    ; iscaled as start location
    lda colour
    and #%10101010
    sta maskedcolour


    ; Draw first group of 8
    ; Work out if there are less bytes than a single group
    lda multiplier
    cmp #8
    bcs dlmorethantop    ; More than 8 length so must be to end of first group

    lda multiplier+1
    and #7
    sta temp+2                 ; save start offset
    adc multiplier
    cmp #8              ; More than 8 so is to end of first group
    bcs dlmorethantop
    ; Not more than top

    lda multiplier
    eor #7              ; Invert 
    tay
    lda branchtable,y
    sta topbranch+1     ; Store as branch
    ldy temp+2

.topbranch
    bpl dllessstart     ; Always taken
.dllessstart
    lda (plotposition),y:and #%01010101:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%01010101:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%01010101:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%01010101:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%01010101:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%01010101:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%01010101:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%01010101:ora maskedcolour:sta (plotposition),Y
    rts                 ; Finish as it's only single byte

.dlmorethantop
    ; Write a "complete" top byte

    lda multiplier+1
    and #7              ; Strip bits
    tay
    lda branchtable,y
    sta topwholebranch+1     ; Write start branch

    lda colour          ; Get colour to plot
.topwholebranch
    bpl dlwholestart     ; Always taken
.dlwholestart
    lda (plotposition),y:and #%01010101:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%01010101:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%01010101:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%01010101:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%01010101:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%01010101:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%01010101:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%01010101:ora maskedcolour:sta (plotposition),Y


    lda multiplier+1
    and #7
    eor #&f8        ; 07 eor ff
    sec
    adc multiplier  ; Reverse Subtract
    sta multiplier      ; Reduce by number we just plotted.
    bne dlcarryon
    jmp dlfinished      ; If zero then done

.dlcarryon
    cmp #8              ; Do we have whole block to finish?
    bcc lastblock

    lda plotposition
    ora #4
    sta multresult+1
    lda plotposition+1
    sta multresult+2

.dlmainlp
    clc
    lda plotposition+1
    adc #1
    sta plotposition+1
    sta multresult+2

    ldy #0
    lda colour
    lda (multresult+1),y:and #%01010101:ora maskedcolour:sta (multresult+1),Y
    lda (plotposition),y:and #%01010101:ora maskedcolour:sta (plotposition),Y:iny
    lda (multresult+1),y:and #%01010101:ora maskedcolour:sta (multresult+1),Y
    lda (plotposition),y:and #%01010101:ora maskedcolour:sta (plotposition),Y:iny
    lda (multresult+1),y:and #%01010101:ora maskedcolour:sta (multresult+1),Y
    lda (plotposition),y:and #%01010101:ora maskedcolour:sta (plotposition),Y:iny
    lda (multresult+1),y:and #%01010101:ora maskedcolour:sta (multresult+1),Y
    lda (plotposition),y:and #%01010101:ora maskedcolour:sta (plotposition),Y

    lda multiplier
    sec
    sbc #8
    sta multiplier      ; Store value
    cmp #8
    bcs dlmainlp        ; More than 8 do whole block

.lastblock
    clc
    lda plotposition+1
    adc #1
    sta plotposition+1       ; Increment screen position

    lda multiplier      ; Remainder
    beq dlfinished
    eor #7
    tay
    lda branchtable,y
    sta bottombranch+1     ; Write last branch

    ldy #0              ; Start at top of block
    lda colour          ; Get colour to plot
.bottombranch
    bpl dlbottomblock     ; Always taken
.dlbottomblock
    lda (plotposition),y:and #%01010101:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%01010101:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%01010101:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%01010101:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%01010101:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%01010101:ora maskedcolour:sta (plotposition),Y:iny
    lda (plotposition),y:and #%01010101:ora maskedcolour:sta (plotposition),Y

.dlfinished
}
    rts

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
equb &3F ; R0  horizontal total
equb 32 ; R1  horizontal displayed
equb 45 ; R2  horizontal position
equb &24 ; R3  sync width
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
; Load and de-compress file
    lda #&4e
    sta ZX0_src+1
    sta fileblock+3
    lda #&2e
    sta ZX0_dst+1
    lda #0
    sta ZX0_dst
    sta ZX0_src
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
    jsr &ffdd           ; Osfile - Load

;runs into decompression
    include "zx02.asm" ; Includes RTS

.fileblock
equw filename
equd &4e00          ; Load address
equd 0              ; Execution Addres
equd 0
equd 0

.filename
equb "M.A",13

.loadprompt
equb 22,7,13,10,"Which Landscape (A-M)?",0
.loadingtext
equb 13,10,10,"Loading...",0

print "Space",maxpos-P%,"End",~P%,"Length ",P%-codestart

clear maxpos,&8000

.entrypoint
    ldx #1
    ldy #0
    lda #200
    jsr osbyte

    lda #lo(instructions)
    ldx #hi(instructions)
    jsr printtext

    jsr loadfile

    lda #lo(presskeytxt)
    ldx #hi(presskeytxt)
    jsr printtext

    ldx #0
    ldy #25
    lda #129
    jsr osbyte

    jmp voxelstart

.instructions
equs 22,7,10,"Voxel Landscape Demo",13,10
equs      "====================",13,10
equs "Mode 8 32K Version",13,10,13,10
equs "WASD : Move Horizontally",13,10
equs "K/M : Move Up/Down",13,10
equs "J/N : Pitch Up/Down",13,10
equs "H/B : Draw Distance Further/Closer",13,10
equs "L : Load New Landscape",13,10,10
equs "Loading initial landscape...",0
.presskeytxt
equb "Done!",13,10,10,"Press a key to start.",0


save "!Boot",codestart,P%,entrypoint
putfile "maps\map64x128_11.bin.zx02","M.A",&3000
putfile "maps\map64x128_9.bin.zx02","M.B",&3000
putfile "maps\map64x128_16.bin.zx02","M.C",&3000
putfile "maps\map64x128_15.bin.zx02","M.D",&3000
putfile "maps\map64x128_22.bin.zx02","M.E",&3000
putfile "maps\map64x128_21.bin.zx02","M.F",&3000
putfile "maps\map64x128_1.bin.zx02","M.G",&3000
putfile "maps\map64x128_2.bin.zx02","M.H",&3000
putfile "maps\map64x128_5.bin.zx02","M.I",&3000
putfile "maps\map64x128_6.bin.zx02","M.J",&3000
putfile "maps\map64x128_7.bin.zx02","M.K",&3000
putfile "maps\map64x128_10.bin.zx02","M.L",&3000
putfile "maps\map64x128_25.bin.zx02","M.M",&3000