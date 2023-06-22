# atari2600-monitor
a Machine Language Monitor for the Atari 2600

![Screenshot](https://raw.githubusercontent.com/dadecoza/atari2600-monitor/main/docs/a26mon.gif)


This is Steve Wozniaks Woz Monitor ported to the Atari 2600. For general information and usage of Wozmon you can checkout the excelent post on [SB-Projects page](https://www.sbprojects.net/projects/apple1/wozmon.php).

## Cartridge
The cartridge makes use of the Commavid (CV) mapper.
```
CV Commavid
-----

This was used by Commavid.  It allowed for both ROM and RAM on the cartridge,
without using bankswitching.  There's 2K of ROM and 1K of RAM.

2K of ROM is mapped at 1800-1FFF.
1K of RAM is mapped in at 1000-17FF.

The read port is at 1000-13FF.
The write port is at 1400-17FF.
```
from: http://blog.kevtris.org/blogfiles/Atari%202600%20Mappers.txt

## Keyboard controller layout
The atari2600-monitor makes use of a standard CX50 keyboard controller. <br>
<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/5/50/Atari-2600-Keyboard-Controller-FL.jpg/1024px-Atari-2600-Keyboard-Controller-FL.jpg" alt="Keyboard Controller" width="100"><br>
The Layouts is as follows ...
### Normal
|   |   |   |
|---|---|---|
| 1 | 2 | 3 |
| 4 | 5 | 6 |
| 7 | 8 | 9 |
|ALT| 0 |CR |
### Alt
|   |   |   |
|---|---|---|
| A | B | C |
| D | E | F |
| . | : | R |
|ALT|BS |CR |

### Key functions
* 0123456789ABCDEF hex digits
* *.* Examine memory location
* *:* Store to memory Location
* *R* Run from  last memory location
* *ALT* Switch between keyboard layouts
* *BS* Backspace
* *CR* Enter

## Examples
### Change the background colour by writing to the TIA COLUBK register ...
```
009:30
```
Keyboard controller key presses ...
```
0 0 9 * 8 * 3 0 #
```
Stella Emulator key presses ...
```
x x d z s z 3 x c
```

### Print HELLO WORLD
This example makes use of the monitors subroutines to print "HELLO WORLD"

```
      1  f015					      PROCESSOR	6502
      2  f015		       00 a5	   MsgPtr     EQU	$a5
      3  f015		       fc d6	   PrintMsg   EQU	$fcd6
      4  f015		       fd c0	   Done       EQU	$fdc0
      5  f000					      org	$F000
      6  f000		       a9 0e		      LDA	#<data
      7  f002		       85 a5		      STA	MsgPtr
      8  f004		       a9 f0		      LDA	#>data
      9  f006		       85 a6		      STA	MsgPtr+1
     10  f008		       20 d6 fc 	      JSR	PrintMsg
     11  f00b		       4c c0 fd 	      JMP	Done
     12  f00e		       04 19 16 1d*data       .byte.b	#$04, #$19, #$16, #$1d, #$1d, #$20, #$07	; \nHELLO WORLD\0
     13  f015		       28 20 23 1d*	      .byte.b	#$28, #$20, #$23, #$1d, #$15, #$00
```
Although we will be running the example from memory location F000 we will have to enter the program at memory location F400 the reason for this is that we are using
the A10 address line as "write enable" on the SRAM IC (we do not have the luxury of a write pin on the cartridge port). After the program is typed in we will then examine memory location F400 followed by the "R" command. ...

```
F400
:A9
:0E
:85
:A5
:A9
:F0
:85
:A6
:20
:D6
:FC
:4C
:C0
:FD
:04
:19
:16
:1D
:1D
:20
:07
:28
:20
:23
:1D
:15
:00
F000
R
```
