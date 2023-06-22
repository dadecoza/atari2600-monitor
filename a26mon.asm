;-------------------------------------------------------------------------
;
; 2600 Monitor
; https://github.com/dadecoza/atari2600-monitor
;
; This is a machine language monitor for the Atari 2600.
; (https://en.wikipedia.org/wiki/Machine_code_monitor)
;
; It is an implementation of Steve Wozniak's "Wozmon".
; (https://www.sbprojects.net/projects/apple1/wozmon.php)
; The Wozmon code has only been slightly modified to cater to the custom
; character map.
;
; The heart of this cartridge is the 12 characters per line text kernel.
; Unfortunately, I don't know who the original author is to credit them. 
; I extracted it from https://github.com/robinhedwards/UnoCart-2600, 
; who in turn got it from https://github.com/tdididit/a26-flashcart.
; Looking at tdididit's code, it seems that they also obtained it from
; somewhere else, as it does not fit with the rest of their code style 
; (I might be wrong though).
;
; In any case, I extracted the kernel base code and uploaded it to my
; GitHub page for anyone else looking for a text kernel: 
; https://github.com/dadecoza/atari2600-text-kernel.
;
; As for the keyboard controller scanning routine, I got that from the
; AtariAge Forums: 
; https://forums.atariage.com/topic/204852-reading-keyboard-controllers/
;
; It should be clear by now that I know very little about 6502 assembly,
; but I do know how to copy and paste. ;)
;
; Okay, I think that is all of the credits done... except maybe for the
; "Vintage Computing and Console Group of South Africa" members who share
; in my excitement and share ideas while I work on these wacky projects.
;
; Now for some technical details...
; The cartridge consists of 2K of ROM and 1K of RAM (1K of RAM because we
; are using one address line as a write enable). I thought I came up with
; this clever idea to map RAM and ROM into the 4K of available memory
; space, but nope, a company named CommaVid
; (https://atariage.com/software_page.php?SoftwareLabelID=281) also did it
; back in the '80s, and that is a good thing because our favorite Stella
; Emulator supports it!
;
; Requirements:
; - 1x Atari 2600
; - 1x Standard Keyboard controller
; - 1x Multi-Cart that supports the CommaVid format...
;   (or 1x SRAM IC, 1x (E)EPROM, 1x NAND gate, a breadboard, a lot of jumper
;     cables, and a way to connect all of that to your cartridge port)
;  or or just use an emulator :)
;
; The memory map is as follows...
; F000-F3FF RAM Read
; F400-F7FF RAM Write
; F800-FFFF ROM
;
; And the keyboard layout is as follows (use the "ALT" key to switch between
; layouts)...
;
; +---+---+---+   +---+---+---+
; | 1 | 2 | 3 |   | A | B | C |
; +---+---+---+   +---+---+---+
; | 4 | 5 | 6 |   | D | E | F |
; +---+---+---+   +---+---+---+
; | 7 | 8 | 9 |   | . | : | R |
; +---+---+---+   +---+---+---+
; |ALT| 0 | CR|   |ALT|BS |CR |
; +---+---+---+   +---+---+---+
;
; For details on how to use Wozmon, I will refer you again to ...
; https://www.sbprojects.net/projects/apple1/wozmon.php
;
; And maybe finally as a reminder to myself ... this is the script I used to
; assemble this file ...
; dasm a26mon.asm -oa26mon.bin -la26mon.lst -f3 -I$HOME/lib/dasm/machines/atari2600 && \
; stella a26mon.bin -tv.phosphor always -tv.phosblend 50
;
; Enjoy!
; Johannes le Roux (@dadecoza)- 2023
;;-------------------------------------------------------------------------

        PROCESSOR 6502
        INCLUDE "vcs.h"
        INCLUDE "macro.h"
PAL = 1
NTSC = 0
SYSTEM = PAL
;-------------------------------------------------------------------------
;  Variables
;-------------------------------------------------------------------------
        SEG.U Variables
        ORG $80
State                ds 1
KeyMode              ds 1
KeyRow               ds 1
Key                  ds 1
Temp                 ds 1
CursorPos            ds 1
Frame                ds 1
Debounce             ds 1
Counter              ds 2
TextLineCounter      ds 1
TextBlockPointer     ds 2
Char1Ptr             ds 2
Char2Ptr             ds 2
Char3Ptr             ds 2
Char4Ptr             ds 2
Char5Ptr             ds 2
Char6Ptr             ds 2
Char7Ptr             ds 2
Char8Ptr             ds 2
Char9Ptr             ds 2
Char10Ptr            ds 2
Char11Ptr            ds 2
Char12Ptr            ds 2
MsgPtr               ds 2
;-------------------------------------------------------------------------
;  Wozmon Variables
;-------------------------------------------------------------------------
MODE                 ds 1     ; $00=XAM, $7F=STOR, $AE=BLOCK XAM
XAML                 ds 1     ; Last "opened" location Low
XAMH                 ds 1     ; Last "opened" location High
STL                  ds 1     ; Store address Low
STH                  ds 1     ; Store address High
HL                   ds 1     ; Hex value parsing Low
HH                   ds 1     ; Hex value parsing High
YSAV                 ds 1     ; Used to see if hex value is given
KBINX                ds 1
KBBUFF               ds 12
;-------------------------------------------------------------------------
;  Constants
;-------------------------------------------------------------------------
CR                EQU $04     ; Carriage Return
StateGetInput     EQU #$00
StateProcessInput EQU #$01
;-------------------------------------------------------------------------
;  Memory Setup
;-------------------------------------------------------------------------
        SEG RAMREAD
        ORG $F000
RamRead
        SEG DISPLAYREAD
        ORG $F387
DisplayRead
        SEG RAMWRITE
        ORG $F400
RamWrite
        SEG DISPLAYWRITE
        ORG $F787
DisplayWrite
        SEG ROM
        ORG $F800
;-------------------------------------------------------------------------
;  Initialize
;-------------------------------------------------------------------------
Start
        CLEAN_START
        CLD
        CLI
        #if SYSTEM = NTSC
        LDA #$EE
        #endif
        #if SYSTEM = PAL
        LDA #$FE
        #endif
        STA COLUP0
        STA COLUP1
        #if SYSTEM = NTSC
        LDA #$90
        #endif
        #if SYSTEM = PAL
        LDA #$D0
        #endif
        STA COLUBK
        LDA #$70
        STA PF0
        LDA #$00
        STA PF1
        STA PF2
        STA KeyMode
        STA KBINX
        LDA #1
        STA CTRLPF
        STA VDELP0
        STA VDELP1
        LDY #144
        LDA #$F0
        STA SWACNT            ; output for 4 bits for left port
        LDA #%110
        STA NUSIZ0
        STA NUSIZ1
        LDA #%11101111        ; Top Row
        STA KeyRow
        LDY #$00
ClearRam
        LDA #$00
        DEY
        STA RamWrite,y
        BNE ClearRam
        LDY #$78
ClearDisplay
        STA DisplayWrite,y
        DEY
        BNE ClearDisplay
        LDA #<CharacterTable
        STA TextBlockPointer
        LDA #>CharacterTable
        STA TextBlockPointer+1
        LDA #<MsgWelcome
        STA MsgPtr
        LDA #>MsgWelcome
        STA MsgPtr+1
        JSR PrintMessage
        LDA #12
        STA CursorPos
;*************************************************************************
;-------------------------------------------------------------------------
;  MAIN PROGRAM LOOP
;-------------------------------------------------------------------------
MainProgramLoop
        JSR VBLANKRoutine
        JSR KernelRoutine
        JSR OverscanRoutine
        JMP MainProgramLoop
;*************************************************************************
;-------------------------------------------------------------------------
;  VBLANK Routine
;-------------------------------------------------------------------------
VBLANKRoutine
        LDA #%00000111
VSYNCLoop
        STA WSYNC
        STA VSYNC
        LSR
        BCS VSYNCLoop
        #if SYSTEM = NTSC
        LDA #47               ; ((40*76)-12) / 64
        #endif
        #if SYSTEM = PAL
        LDA #56               ; ((48*76)-12) / 64; ((48*76)-12) / 64
        #endif
        STA TIM64T
        DEC Counter
        BNE NoHiClick
        DEC Counter+1
NoHiClick
        LDA #0
        STA TextLineCounter
        JSR SetTextPointersSubroutine
        LDA #$15
        LDX #0
        JSR PositionASpriteSubroutine
        LDA #$25
        LDX #1
        JSR PositionASpriteSubroutine
;-------------------------------------------------------------------------
;  State Machine
;-------------------------------------------------------------------------
        LDA State
        BEQ CallGetInput
        CMP StateProcessInput
        BEQ CallProcessInput
CallProcessInput
        JSR ProcessInput
        JMP WaitForVblankEnd
CallGetInput
        JSR GetInput
        LDA Debounce
        BEQ WaitForVblankEnd
        DEC Debounce
;-------------------------------------------------------------------------
WaitForVblankEnd
        LDA INTIM
        BNE WaitForVblankEnd
        STA WSYNC
        STA VBLANK            ;turn off VBLANK - it was turned on by overscan
        RTS
;*************************************************************************
;-------------------------------------------------------------------------
;  Kernel Routine
;-------------------------------------------------------------------------
KernelRoutine
        #if SYSTEM = NTSC
        LDA #227              ; ((scanlines*76)-12) / 64
        #endif
        #if SYSTEM = PAL
        LDA #255              ; ((scanlines*76)-12) / 64
        #endif
        STA TIM64T
        LDA #0
TextLoop
        STA TextLineCounter
        JSR SetTextPointersSubroutine
        JSR DrawLineOfTextSubroutine
        LDA TextLineCounter
        CLC
        ADC #12
        CMP #120
        BNE TextLoop
WaitForKernelEnd
        LDA INTIM
        BNE WaitForKernelEnd
        #if SYSTEM = PAL
        LDA #15
        STA TIM64T
WaitForPalKernelEnd
        LDA INTIM
        BNE WaitForPalKernelEnd
        #endif
        RTS
;*************************************************************************
;-------------------------------------------------------------------------
;  Overscan Routine
;-------------------------------------------------------------------------
OverscanRoutine
        LDA #2
        STA WSYNC
        STA VBLANK            ; turn on VBLANK
        #if SYSTEM = NTSC
        LDA #35               ; ((30*76)-12) / 64
        #endif
        #if SYSTEM = PAL
        LDA #42               ; ((scanlines*76)-12) / 64
        #endif
        STA TIM64T
;-------------------------------------------------------------------------
;  Flash Cursor
;-------------------------------------------------------------------------
        LDY CursorPos
        LDA Frame
        ASL
        ASL
        ASL
        BCC NoCursor
        LDX KeyMode
        INX
        TXA
        JMP WriteCursor
NoCursor
        LDA #0
WriteCursor
        STA DisplayWrite,y
        INC Frame
WaitForOverscanEnd
        LDA INTIM
        BNE WaitForOverscanEnd
        RTS
;*************************************************************************
;-------------------------------------------------------------------------
;  END MAIN SUBROUTINES
;-------------------------------------------------------------------------
;*************************************************************************
;-------------------------------------------------------------------------
;  Begin Subroutines
;-------------------------------------------------------------------------
;*************************************************************************
;-------------------------------------------------------------------------
;  Position Sprites
;-------------------------------------------------------------------------
        ALIGN 256
PositionASpriteSubroutine
        SEC
        STA HMCLR
        STA WSYNC             ; begin line 1
DivideLoop
        SBC #15
        BCS DivideLoop        ; +4/5   4/ 9.../54
        EOR #7                ; +2     6/11.../56
        ASL
        ASL
        ASL
        ASL                   ; +8      14/19.../64
        STA.wx HMP0,X         ; +5      19/24.../69
        STA RESP0,X           ; +4      23/28/33/38/43/48/53/58/63/68/73
        STA WSYNC             ; +3      0      begin line 2
        STA HMOVE             ; +3
Ret
        RTS ;+6    9
;*************************************************************************
;-------------------------------------------------------------------------
;  Set Text pointers
;-------------------------------------------------------------------------
SetTextPointersSubroutine
        LDY TextLineCounter
        LDX #0
SetCharPtrsLoop
        TYA
        PHA
        LDA DisplayRead,Y
        TAY
        LDA (TextBlockPointer),Y
        STA Char1Ptr,X
        LSR
        BCC OnCharSetPageOne
        LDA #>CharSetPageTwo
        STA Char1Ptr+1,X
        BNE DoneSettingCharPtrHi
OnCharSetPageOne
        LDA #>CharSetPageOne
        STA Char1Ptr+1,X
        NOP
DoneSettingCharPtrHi
        INX
        INX
        PLA
        TAY
        INY
        CPX #24
        BNE SetCharPtrsLoop
        RTS
;*************************************************************************
;-------------------------------------------------------------------------
;  Draw a line of text
;-------------------------------------------------------------------------
DrawLineOfTextSubroutine
LF303
        STA HMCLR
        STA WSYNC
        SLEEP 36
        LDX #$90
        LDY #8
        LDA Counter
        AND #1
        BEQ SpritesLeft
        JMP SpritesRight
LF327
        STA GRP1
        LDA (Char5Ptr),Y
        STA GRP0
        LDA (Char7Ptr),Y
        STX HMP0
        STX HMP1
        STA GRP1
        LDA (Char9Ptr),Y
        STA GRP0
        LDA (Char11Ptr),Y
        STA GRP1
        STA GRP0
SpritesRight
        DEY
        BEQ LF37D
        LDA (Char2Ptr),Y
        LSR
        STA GRP0
        LDA (Char4Ptr),Y
        LSR
        STA.w  $001C
        STA HMOVE
        LDA (Char6Ptr),Y
        LSR
        STA GRP0
        LDA (Char10Ptr),Y
        LSR
        STA Temp
        LDA (Char8Ptr),Y
        LSR
        STA GRP1
        LDA Temp
        STA GRP0
        LDA (Char12Ptr),Y
        LSR
        STA GRP1
SpritesLeft
        STA GRP0
        LDA #$70
        STA HMP0
        STA HMP1
        DEY
        BEQ LF387
        LDA (Char1Ptr),Y
        STA GRP0
        LDA (Char3Ptr),Y
        STA HMOVE
        JMP LF327
LF37D
        STX HMP0
        STX HMP1
        STA WSYNC
        STA HMOVE
        BEQ LF38C
LF387
        STA WSYNC
        NOP
        NOP
        NOP
LF38C
        LDA #0
        STA GRP0
        STA GRP1
        STA GRP0
        RTS
;*************************************************************************
;-------------------------------------------------------------------------
;  Scan keypad for keypresses
;-------------------------------------------------------------------------
ReadKeypadSubroutine
        LDA Debounce
        BEQ ReadKeyPad
        RTS
ReadKeyPad
        LDA KeyRow
ReadCol
        STA SWCHA
        SLEEP 477
        BIT INPT4
        BMI NotKP1
        EOR #%11110011
        LSR
        LSR
        STA Key
NotKP1
        BIT INPT1
        BMI NotKP2
        EOR #%11110111
        LSR
        LSR
        STA Key
NotKP2
        BIT INPT0
        BMI NotKP3
        EOR #%11111011
        LSR
        LSR
        STA Key
NotKP3
        CLC
        LDA KeyRow
        ROL
        BCS KeyDone
        LDA #%11101111
KeyDone
        STA KeyRow
        RTS
;*************************************************************************
;-------------------------------------------------------------------------
;  Map the keypress
;-------------------------------------------------------------------------
ProcessKeyPress
        CMP #$20
        BEQ ChangeMode
        CMP #$22
        BEQ Enter
        TAX
        LDA KeyMode
        BNE AltKey
        LDA KeyMapping,X
        JMP PrintKey
AltKey
        CPX #$23
        BEQ CallBackSpace
        LDA KeyMappingAlt,X
PrintKey
        STA Temp
        LDA KBINX
        CMP #10
        BCC AddToBuffer
        JMP KeyDoneDone
AddToBuffer
        LDX KBINX
        LDA Temp
        STA KBBUFF,X
        INC KBINX
        JSR Putch
        JMP KeyDoneDone
ChangeMode
        LDA KeyMode
        BEQ ModeOne
        LDA #0
        STA KeyMode
        JMP KeyDoneDone
ModeOne
        LDA #1
        STA KeyMode
        JMP KeyDoneDone
Enter
        LDA KBINX
        TAY
        LDA #0
        STA KBBUFF,Y
        STA KBINX
        LDA StateProcessInput
        STA State
        LDA CR
        STA Putch
        JMP KeyDoneDone
CallBackSpace
        LDA KBINX
        BEQ KeyDoneDone
        DEC KBINX
        LDA CursorPos
        TAY
        LDA #0
        STA DisplayWrite,Y
        DEC CursorPos
KeyDoneDone
        LDA #5
        STA Debounce
        LDA #0
        STA Key
        RTS
;*************************************************************************
;-------------------------------------------------------------------------
;  Put a character at the cursor position in memory
;-------------------------------------------------------------------------
Putch
        STA Temp
        PHA
        TYA
        PHA
        TXA
        PHA
        LDA Temp
        CMP CR
        BEQ NewLine
        LDY CursorPos
        STA DisplayWrite,Y
        INC CursorPos
        LDA CursorPos
        JMP Scroll
NewLine
        LDA CursorPos
        TAY
        LDA #$00
        STA DisplayWrite,Y
        TYA
        CMP #12
        BCC LessThan12
Mod
        SEC
Modu
        SBC #12
        BCS Modu
        ADC #12
        STA Temp
        LDA CursorPos
        SBC Temp
        JMP ModDone
LessThan12:
        LDA #00
ModDone:
        CLC
        ADC #12
        STA CursorPos
Scroll
        CMP #120
        BCC Putchrts
        LDX #12
MoveLine
        LDY #1
MoveDisplayByte
        LDA DisplayRead,Y
        STA DisplayWrite-1,Y
        INY
        CPY #120
        BNE MoveDisplayByte
        LDA #0
        STA DisplayWrite-1,Y
        DEX
        BNE MoveLine
        LDA #108
        STA CursorPos
Putchrts
        PLA
        TAX
        PLA
        TAY
        PLA
        RTS
;*************************************************************************
;-------------------------------------------------------------------------
;  Manage input routines
;-------------------------------------------------------------------------
GetInput
        LDA Key
        BNE KeyPressed
        JSR ReadKeypadSubroutine
        JMP GetInputRts
KeyPressed
        JSR ProcessKeyPress
GetInputRts
        Rts
;*************************************************************************
;-------------------------------------------------------------------------
;  Print Message
;-------------------------------------------------------------------------
PrintMessage
        LDY #0
MsgLoop
        LDA (MsgPtr),Y
        BEQ MsgRts
        JSR Putch
        INY
        JMP MsgLoop
MsgRts
        RTS
;*************************************************************************
;-------------------------------------------------------------------------
;  Wozmon
;-------------------------------------------------------------------------
        ALIGN 256
ProcessInput
        LDY #-1               ; Reset text index
        LDA #0                ; Default mode is XAM
        TAX                   ; X=0
SetStorr
        ASL                   ; Leaves $7B if setting STOR mode
SetMode
        STA MODE              ; Set mode flags
BlSkip
        INY                   ; Advance text index
NextItem
        BMI BlSkip
        LDA KBBUFF,Y          ; Get character
        BEQ Escape            ; We're done if it's 0
        CMP #$05              ; "."
        BCC BlSkip            ; Ignore everything below "."
        BEQ SetBlk            ; Set BLOCK XAM mode ("." = $AE)
        CMP #$06              ; ":"
        BEQ SetStor           ; Set STOR mode. $BA will become $7B
        CMP #$23              ; "R"
        BEQ Run               ; Run the program. Forget the rest.
        STX HL                ; Clear input value (X=0)
        STX HH
        STY YSAV              ; Save Y for comparison
        JMP NextHex
SetBlk
        LDA #$AE
        JMP SetMode
SetStor
        LDA #$BA
        JMP SetStorr
NextHex                       ; Here we're trying to parse a new hex value
        LDA KBBUFF,Y          ; Get character for hex test
        SEC
        SBC #$08
        CMP #10
        BCC Dig
        ADC #239
        CMP #$FA              ; Hex letter?
        BCC NotHex            ; No! Character not hex
Dig
        ASL
        ASL                   ; Hex digit to MSD of A
        ASL
        ASL
        LDX #4                ; Shift count
HexShift
        ASL                   ; Hex digit left, MSB to carry
        ROL HL                ; Rotate into LSD
        ROL HH                ; Rotate into MSD
        DEX                   ; Done 4 shifts?
        BNE HexShift          ; No, loop
        INY                   ; Advance text index
        BNE NextHex           ; Always taken
NotHex
        CPY YSAV              ; Was at least 1 hex digit given?
        BEQ NotHexErr         ; No! Ignore all, start from scratch
        BIT MODE              ; Test MODE byte
        BVC NotStor           ; B6=0 is STOR, 1 is XAM or BLOCK XAM
; STOR mode, save LSD of new hex byte
        LDA HL                ; LSDs of hex data
        STA (STL,X)           ; Store current 'store index'(X=0)
        INC STL               ; Increment store index.
        BNE NextItem          ; No carry!
        INC STH               ; Add carry to 'store index' high
ToNextItem
        JMP NextItem          ; Get next command item.
NotHexErr
        LDA #<MsgError
        STA MsgPtr
        LDA #>MsgError
        STA MsgPtr+1
        JSR PrintMessage
Escape
        JMP ProcessDone
;-------------------------------------------------------------------------
;  RUN user's program from last opened location
;-------------------------------------------------------------------------
Run
        JMP (XAML)            ; Run user program
;-------------------------------------------------------------------------
;  We're not in Store mode
;-------------------------------------------------------------------------
NotStor
        BMI XamNext           ; B7 = 0 for XAM, 1 for BLOCK XAM
; We're in XAM mode now
        LDX #2                ; Copy 2 bytes
SetAdr
        LDA HL-1,X            ; Copy hex data to
        STA STL-1,X           ; 'store index'
        STA XAML-1,X          ; and to 'XAM index'
        DEX                   ; Next of 2 bytes
        BNE SetAdr            ; Loop unless X = 0
; Print address and data from this address, fall through next BNE.
NxtPrnt
        BNE PrData            ; NE means no address to print
        LDA CR
        JSR Putch             ; Print CR first
        LDA XAMH              ; Output high-order byte of address
        JSR PrByte
        LDA XAML              ; Output low-order byte of address
        JSR PrByte
        LDA #$06              ; Print colon
        JSR Putch
PrData
        LDA #$07              ; Print space
        JSR Putch
        LDA (XAML,X)          ; Get data from address (X=0)
        JSR PrByte            ; Output it in hex format
XamNext
        STX MODE              ; 0 -> MODE (XAM mode).
        LDA XAML              ; See if there is more to print
        CMP HL
        LDA XAMH
        SBC HH
        BCS ToNextItem        ; Not less! No more data to output
        INC XAML              ; Increment 'examine index'
        BNE Mod8Chk           ; No carry!
        INC XAMH
Mod8Chk
        LDA XAML              ; If address MOD 8 = 0 start new line
        AND #%00000001
        BPL NxtPrnt           ; Always taken.
        JSR PrByte
ProcessDone
        LDA #0
        STA KBINX
        STA KBBUFF
        LDA StateGetInput
        STA State
        LDA CR
        JSR Putch
        RTS
;-------------------------------------------------------------------------
;  Subroutine to print a byte in A in hex form (destructive)
;-------------------------------------------------------------------------
PrByte
        PHA                   ; Save A for LSD
        LSR
        LSR
        LSR                   ; MSD to LSD position
        LSR
        JSR PrHex             ; Output hex digit
        PLA                   ; Restore A
; Fall through to print hex routine
;-------------------------------------------------------------------------
;  Subroutine to print a hexadecimal digit
;-------------------------------------------------------------------------
PrHex
        CLC
        AND #%00001111; Mask LSD for hex print; Mask LSD for hex print
        ADC #$08
        JSR Putch
        RTS
;*************************************************************************
;-------------------------------------------------------------------------
;  Data Below
;-------------------------------------------------------------------------
        ORG $FE00
        ALIGN 256
CharSetPageOne
Char00  .byte $00, $FE, $FE, $FE, $FE, $FE, $FE, $FE; Cursor
Char01  .byte $00, $C0, $C0, $C0, $C0, $C0, $C0, $C0; Alt Cursor
Char02  .byte $00, $00, $00, $00, $00, $00, $00, $00; Space
Char03  .byte $00, $C6, $C6, $FE, $C6, $C6, $6C, $38; A
Char04  .byte $00, $FC, $C6, $C6, $FC, $C6, $C6, $FC; B
Char05  .byte $00, $3C, $66, $C0, $C0, $C0, $66, $3C; C
Char06  .byte $00, $F8, $CC, $C6, $C6, $C6, $CC, $F8; D
Char07  .byte $00, $FE, $C0, $C0, $F8, $C0, $C0, $FE; E
Char08  .byte $00, $C0, $C0, $C0, $FC, $C0, $C0, $FE; F
Char09  .byte $00, $3E, $66, $C6, $CE, $C0, $60, $3E; G
Char10  .byte $00, $C6, $C6, $C6, $FE, $C6, $C6, $C6; H
Char11  .byte $00, $78, $30, $30, $30, $30, $30, $78; I
Char12  .byte $00, $7C, $C6, $06, $06, $06, $06, $06; J
Char13  .byte $00, $CE, $DC, $F8, $F0, $D8, $CC, $C6; K
Char14  .byte $00, $FE, $C0, $C0, $C0, $C0, $C0, $C0; L
Char15  .byte $00, $C6, $C6, $D6, $FE, $FE, $EE, $C6; M
Char16  .byte $00, $C6, $CE, $DE, $FE, $F6, $E6, $C6; N
Char17  .byte $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C; O
Char18  .byte $00, $C0, $C0, $FC, $C6, $C6, $C6, $FC; P
Char19  .byte $00, $76, $CC, $DA, $C6, $C6, $C6, $7C; Q
Char20  .byte $00, $CE, $DC, $F8, $CE, $C6, $C6, $FC; R
Char21  .byte $00, $7C, $C6, $06, $7C, $C0, $CC, $78; S
Char22  .byte $00, $30, $30, $30, $30, $30, $30, $FC; T
Char23  .byte $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6; U
Char24  .byte $00, $10, $38, $7C, $EE, $C6, $C6, $C6; V
Char25  .byte $00, $C6, $EE, $FE, $FE, $D6, $C6, $C6; W
Char26  .byte $00, $C6, $EE, $7C, $38, $7C, $EE, $C6; X
Char27  .byte $00, $30, $30, $30, $78, $CC, $CC, $CC; Y
Char28  .byte $00, $FE, $E0, $70, $38, $1C, $0E, $FE; Z
Char29  .byte $00, $7C, $C6, $E6, $D6, $CE, $C6, $7C; 0
Char30  .byte $00, $FC, $30, $30, $30, $30, $70, $30; 1
Char31  .byte $00, $FE, $E0, $78, $3C, $0E, $C6, $7C; 2
        ALIGN 256
        .byte 0; the following data is offset by 1 byte
CharSetPageTwo
Char32  .byte $00, $7C, $C6, $06, $3C, $18, $0C, $7E; 3
Char33  .byte $00, $0C, $0C, $FE, $CC, $6C, $3C, $1C; 4
Char34  .byte $00, $7C, $C6, $06, $06, $FC, $C0, $FC; 5
Char35  .byte $00, $7C, $C6, $C6, $FC, $C0, $60, $3C; 6
Char36  .byte $00, $30, $30, $30, $18, $0C, $C6, $FE; 7
Char37  .byte $00, $7C, $C6, $C6, $7C, $C6, $C6, $7C; 8
Char38  .byte $00, $78, $0C, $06, $7E, $C6, $C6, $7C; 9
Char39  .byte $00, $30, $30, $00, $00, $00, $00, $00; .
Char40  .byte $00, $30, $30, $00, $00, $00, $30, $30; :
CharacterTable
        .byte Char02, Char00, Char01, Char02, Char02, Char39
        .byte Char40, Char02, Char29, Char30, Char31, Char32
        .byte Char33, Char34, Char35, Char36, Char37, Char38
        .byte Char03, Char04, Char05, Char06, Char07, Char08
        .byte Char09, Char10, Char11, Char12, Char13, Char14
        .byte Char15, Char16, Char17, Char18, Char19, Char20
        .byte Char21, Char22, Char23, Char24, Char25, Char26
        .byte Char27, Char28
KeyMapping
        .byte #$00, #$00, #$00, #$00, #$00, #$09; 5
        .byte #$0a, #$0b, #$00, #$0c, #$0d, #$0e; 11
        .byte #$00, #$00, #$00, #$00, #$00, #$0f; 17
        .byte #$10, #$11, #$00, #$00, #$00, #$00; 23
        .byte #$00, #$00, #$00, #$00, #$00, #$00; 29
        .byte #$00, #$00, #$01, #$00, #$04, #$08; 35
KeyMappingAlt
        .byte #$00, #$00, #$00, #$00, #$00, #$12; 5
        .byte #$13, #$14, #$00, #$15, #$16, #$17; 11
        .byte #$00, #$00, #$00, #$00, #$00, #$05; 17
        .byte #$06, #$23, #$00, #$00, #$00, #$00; 23
        .byte #$00, #$00, #$00, #$00, #$00, #$00; 29
        .byte #$00, #$00, #$01, #$00, #$04, #$03; 35
MsgError
        .byte #$16, #$23, #$23, #$20, #$23, #$00; ERROR
MsgWelcome
        .byte #$0a, #$0e, #$08, #$08, #$07, #$1e; 2600 Monitor
        .byte #$20, #$1f, #$1a, #$25, #$20, #$23
        .byte #$00
;-------------------------------------------------------------------------
;-------------------------End Data----------------------------------------
;-------------------------------------------------------------------------
        ORG $FFFC
        .word Start
        .word Start
