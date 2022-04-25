.model SMALL 
.STACK 256 
.386
.data
INMESS1 DB "PLEASE ENTER YOUR NAME: $" 
INMESS2 DB "INTIAL POINTS: $"
INMESS3 DB "PRESS ENTER KEY TO CONTINUE $"   
INMESS4 DB "TO STATRT CHATTING PRESS F1$"  
INMESS5 DB "TO START THE GAME PESS F2 $" 
INMESS6 DB "TO END THE PROGRAM PRESS ESC $"  
PLAYER1MESS DB "PLAYER 1 $"
PLAYER2MESS DB "PLAYER 2 $"
LINE    DB 80 DUP('-'),'$'  
CHATMESS DB "YOU SENT CHAT INVITATION TO $"                   ;CHAT INVIT. MESSAGE WHERE NAME IS OTHER PLAYER NAME SEND
CHATMESSR DB ' SENT CHAT INVITATION TO START PRESS F1 $'      ;CHAT INVIT. MESSAGE WHERE NAME IS OTHER PLAYER NAME RECIEVE
GAMEINVS DB "YOU SENT GAME INVITATION TO $"                   ;GAME INVIT. MESSAGE  SEND
GAMEINVR DB " SENT YOU GAME INVITATION TO START PRESS F2 $"   ;GAME INVIT. MESSAGE  RECIEVE
LEVELSEL DB "FOR LEVEL 1 PRESS [1] FOR LEVEL 2 PRESS [2]: $"  ;LEVEL SELECTION MESSAGE
FORB_MESS DB 'PLEASE CHOOSE YOUR FORBIDDEN CHARACTER (0-9 A-Z)','$'  ;FORBIDEN CHAR MESSAGE
MESS_F4 DB    "PRESS F4 TO START $"
GAMELEVEL   DB  0                                               ;LEVEL OF THE GAME
CHAR1       DB '$','$'                                   ;FORRBIDEN CHAR OF PLAYER 2 CHOSSEN BY 1
CHAR1_CHANGE DB 0
CHAR2       DB '$','$'                                       ;FORRBIDEN CHAR OF PLAYER 1 CHOSSEN BY 2
CHAR2_CHANGE DB 0
MESS10       DB ' PLAYER 1 CHOOSE: ','$'
MESS11       DB ' PLAYER 2 CHOOSE: ','$'  
NAMEMESS DB 15,?,15 DUP('$')                                  ;PLAYER 1 NAME
PMESS DB 3,?, 3 dup(?)                                        ;PLAYER 1 POINTS
P1_POINTS DB 0                                                ;PLAYER 1 POINTS IN DECIMAL
NAMEMESS2 DB 15,?,15 DUP('$')                                 ;PLAYER 2 NAME
PMESS2 DB 3,?, 3 dup(?)                                       ;PLAYER 2 POINTS
P2_POINTS DB 0                                                ;PLAYER 2 POINTS IN DECIMAL
POINTS_GAME DB 0                                              ;POINTS OF THE GAME 
TEMP_POINTS DB 0                                              ;FOR PRINTING POINTS
AVA_POWER  DB 'AVAILABLE  POWER UPS IN THE GAME','$'
POWERUP   DB 'FOR POWER UP PRESS (F2)','$'
PLAYER_USE_PUP_MESS DB 'P_UP MODE','$'
CHOSSEN_PUP_MESS DB 'CHOOSE P_UP','$'
NEW_FB DB 'NEW FB: ','$'                                     ;NEW FB MESSAGE 
NEW_TV DB 'NEW TV: ','$'                                     ;NEW TV MESSAGE
MESS_L2 DB 'OWNP:1 OP:2','$'
LP2 DB 0

PU_1       DB 'Executing a command on your own processor(5 POINT)----------->1','$'
PU_2       DB 'Executing a command on your own processor+Opponent(8 POINT)-->2','$'  
PU_3       DB 'Changing the forbidden character only once(8 POINT)---------->3','$'
PU_4       DB 'Clearing all registers at once(30 POINT)--------------------->4','$' 
PU_5       DB 'Change the target value(8 POINTS)---------------------------->5','$'

PUP_MESSAGE DB 'CHOICE IS:','$'                     ;MESS OF POWER UP CHOICE

KASBAN_MESS1 DB 'PLAYER 1 KASBAN ','$'
KASBAN_MESS2 DB 'PLAYER 2 KASBAN ','$'

TEN DB 10
ONE DB 1
TWO DB 2
THREE DB 3
FOUR DB 4
FIVE DB 5

NUMBER_SIZE EQU 2 
TARG_VAL DW 01EFAH                                           ;TARGET VALUE
ELDOOR DB 1                                                  ;EH DOOOR PLAYER 1 WALA 2
KASBAN DB ?                                                  ; 1:PLAYER 1_______ 2:PLAYER 2___________ 3:DRAW
PUP_INUSE DB 0
VAR DW ?
C_REG3_P1 DB 0
C_REG2_P2 DB 0
TARG_CHAGE_P1 DB 0
TARG_CHAGE_P2 DB 0
NEW_TVAL DB 4 DUP('$'),'$'                                                ;NEW TARGET VAL 

;FOUR DB 0

WHAT_PROCC DB 0





;------------------------------------------------------PLAYERS CHOICE MODE DATA----------------------------------------------------------------------
CHOICE_P1 DB ?
CHOICE_P2 DB ?
  
;-------------------------------------------------------IMAGE DATA------------------------------------------------------------------------------------

filehandle DW ?          
ErrorMsg db 'error$' 
TOP_Y                      dw  9                  ;Y on line drawing in "CleanTopLine" procedure
ScrLine db  320 dup (0)
filename db 'Game.bmp',0
Header                     db  54 dup (0)         ;BMP
 
SET_PIXEL_COLOR            equ 0ch   
MAX_X                      equ 319
Palette                    db  256*4 dup (14)                ;maximum x cordinate             ;set pixel color with interrupt     ;BMP

;--------------------------------------------GAME DATA(REG,FLAGS,COMMANDS)-------------------------------------------------------------------------
;GENERAL REGISTERS
RAX1 DW 0000H
RBX1 DW 0000H
RCX1 DW 0000H
RDX1 DW 0000H
ISI1 DW 0000H
IDI1 DW 0000H
SSP1 DW 0000H
BBP1 DW 0000H

;PLAYER 2 REGISTERS
RAX2 DW 0000H
RBX2 DW 0000H
RCX2 DW 0000H
RDX2 DW 0000H
ISI2 DW 0000H
IDI2 DW 0000H
SSP2 DW 0000H
BBP2 DW 0000H

;PLAYER 1 REGISTERS
RAX3 DW 0000H
RBX3 DW 0000H
RCX3 DW 0000H
RDX3 DW 0000H
ISI3 DW 0000H
IDI3 DW 0000H
SSP3 DW 0000H
BBP3 DW 0000H

;GENERAL MEMORY1 
 MEMORY1 DB 16 DUP(0)    

;PLAYER 2  MEMORY1 
 MEMORY2 DB 16 DUP(0)  

;PLAYER 1 MEMORY
 MEMORY3 DB 16 DUP(0)  

 MEM_ROW DB 0
 MEM_COL DB 0

 MEM3_ROW DB 1
 MEM3_COL DB 17

 MEM2_ROW DB 1
 MEM2_COL DB 21
 

;ARRAY OF NUMBERS IN HEXA
NUM_HEXA DB '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'

;;NUMBER TO DISPLAY IN REGISTERS
NUMTODISP DB 4 DUP(?) 
NUM_TO_DISP DB 2 DUP(?)

;;;GENERAL FLAGS 
FLAGS DW 0

;;;PLAYER 2 FLAGS 
FLAGS2 DW 0

;;PLAYER 1 FLAGS
FLAGS3 DW 0


;PLAYER 1 COMMAND DATA 
COM1 DB 13,?,13 DUP('$')  
COMp1  DB 13 DUP('$')     ;command player 1
A_SIZE1 DB 0

;PALYER 2 COMMAND DATA
COM2 DB 13,?,13 DUP('$')  
COMp2  DB 13 DUP('$')     ;command player 2 
A_SIZE2 DB 0   
;Ganeral COMMAND DATA
COMp  DB 13 DUP('$')     ;command player 2 
ACT_SIZE DB 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;------------------------------------------------------------------------------------CODE----------------------------------------------------------------------------
.code 
INCLUDE MACROS.INC
INCLUDE COMPINST.INC
INCLUDE COUNTNUM.INC 
INCLUDE PRINTNUM.INC
INCLUDE COMINST2.INC
;INCLUDE MOVING.INC
;INCLUDE GAME_WINDOW.INC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;IMAGE
CleanTopLine proc FAR
    push ax
    push bx
    push cx
    push dx
    push si

    ; Clean the to top most line by set all the top line pixels to black
    mov ah,SET_PIXEL_COLOR      ;for pixel set
    mov al,0                    ;color 0 is black (to erase all pixels in the line)
    mov bx,0                    ;need to set bh=0h for page number always 0 for 320x200x256 CGA-256 
    mov dx,[TOP_Y]              ;usually set Y=9 from "ScreenMovement"  (or set Y to 0 form "show_bitmap") 
    mov di,0                    ;pixel X position counter 
LoopClsTop:
    mov cx,di                   ;set X
    int 10h                     ;draw(X, Y) CLS the pixel
    inc di                      ;add 1 to X 
    cmp di,MAX_X                ;stop when X==319 (since we start from 0 offset)
    je EndClean
    jmp LoopClsTop 

EndClean:
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret
CleanTopLine endp 



OpenFile proc FAR
    mov ah, 3Dh
    xor al, al
    mov dx, offset filename
    int 21h
    jc openerror
    mov [filehandle], ax 
    ret
    
openerror:
    mov dx, offset ErrorMsg
    mov ah, 9h
    int 21h
    ret
OpenFile endp 




ReadHeader proc FAR
    mov ah,3fh
    mov bx, [filehandle]
    mov cx,54
    mov dx,offset Header
    int 21h
    ret
ReadHeader endp 

ReadPalette proc  FAR
    mov ah,3fh
    mov cx,400h
    mov dx,offset Palette
    int 21h
    ret
ReadPalette endp  

 
CopyPal proc  FAR
    mov si,offset Palette
    mov cx,256
    mov dx,3C8h
    mov al,0
    ; Copy starting color to port 3C8h
    out dx,al
    ; Copy palette itself to port 3C9h
    inc dx
PalLoop:
    ; Note: Colors in a BMP file are saved as BGR values rather than RGB.
    mov al,[si+2] ; Get red value.
    shr al,1 ; Max. is 255, but video palette maximal
    shr al,1 ; Max. is 255, but video palette maximal
    ; value is 63. Therefore dividing by 4.
    out dx,al ; Send it.
    mov al,[si+1] ; Get green value.
 
    shr al,1
    shr al,1
    out dx,al ; Send it.
    mov al,[si] ; Get blue value.

    shr al,1
    shr al,1
    out dx,al ; Send it.
    add si,4 ; Point to next color.
    ; (There is a null chr. after every color.)

    loop PalLoop
    ret
CopyPal endp 


CopyBitmap proc FAR
    mov ax, 0A000h
    mov es, ax
    mov cx,200
PrintBMPLoop:
    push cx
    ; di = cx*320, point to the correct screen line
    mov di,cx
    
    ;shift left cx - 6 bits
    shl cx,1
    shl cx,1
    shl cx,1
    shl cx,1
    shl cx,1
    shl cx,1

    ;shift left di - 8 bits
    shl di,1
    shl di,1
    shl di,1
    shl di,1
    shl di,1
    shl di,1
    shl di,1
    shl di,1

    add di,cx
    ; Read one line
    mov ah,3fh
    mov cx,320
    mov dx,offset ScrLine
    int 21h
    ; Copy one line into video  MEMORY1
    cld ; Clear direction flag, for movsb
    mov cx,320
    mov si,offset ScrLine 

    rep movsb ; Copy line to the screen
    ;rep movsb is same as the following code:
    ;mov es:di, ds:si
    ;inc si
    ;inc di
    ;dec cx
    ;loop until cx=0
    pop cx
    loop PrintBMPLoop
    ret
CopyBitmap endp 


CloseFile proc FAR
    mov ah,3eh
    mov bx, [filehandle]
    int 21h
    ret
CloseFile endp  
show_bitmap proc FAR
    push ax
    push bx
    push cx
    push dx
    push si

    call OpenFile
    call ReadHeader
    call ReadPalette
    call CopyPal
    call CopyBitmap 
  
    call CloseFile

    ; Clean the to top most line by set all the top line pixels to black  
    mov ax,0
    mov [TOP_Y],ax
    call CleanTopLine    
    
    
    pop si
    pop dx
    pop cx
    pop bx
    pop ax  
    ret
show_bitmap endp  


PRINT_MEM3 PROC FAR

  LEA DI,NUM_TO_DISP
    MOV BL,MEMORY3
    SHR BL,4  
    MOV [DI],BL
    INC DI 
    MOV BL,MEMORY3
    SHL BL,4
    SHR BL,4 
    MOV [DI],BL
	
    MOV CX,2             ;NO OF LOOPS   
    LEA DI,NUM_TO_DISP   ;MOVE OFFSET OF NUM TO DISPLAY
    MOV DH,MEM_ROW
    MOV DL,MEM_COL
	
	MLOOP3:
    ;;PRINT NUMBER 
    MOV AL,[DI] 
    LEA BX,NUM_HEXA
    XLAT
    INC DI  
    PUSH CX
    
    ;;MOVE CURSOR POS 
    
    MOV AH,2
	  MOV BH,0
    INT 10H
    
    ;;display char
  	MOV BL,73H
	  MOV BH,BL
    MOV CL,4
    SHR BH,CL
    mov ah,9
    MOV CX,1
    int 10h 
    INC DL
    POP CX
    LOOP  MLOOP3

    RET
 PRINT_MEM3 ENDP

 PTS_TEXT PROC FAR
    PUSHA
    MOV BL,0FH
   	MOV BH,BL
    MOV CL,4
    SHR BH,CL
    mov ah,9
    MOV CX,1
    int 10h 
  	POPA
    RET
 PTS_TEXT ENDP

 ;BEFORE CALL PUT NUMBER TO DISPLAY IN AL AND DH=ROW AND DL=COL

 PRINT_SCORE PROC FAR

	  MOV AH,00
	  MOV BL,10
	  ;PRINT 1ST DIGIT
  	DIV BL
	  MOV TEMP_POINTS,AH ;BACKUP REMAINDER
	
  	ADD AL,30H
	;;MOVE CURSOR POS 
    MOV AH,2
  	MOV BH,0
    INT 10H
	  CALL PTS_TEXT
	  INC DL
   	MOV AL,TEMP_POINTS ;GET REMAINDER
  	ADD AL,30H
  	MOV AH,2
	  MOV BH,0
    INT 10H
	CALL PTS_TEXT 
  RET
 PRINT_SCORE ENDP

 ;MOVE CURSOR 
 PRINT_L PROC FAR
  mov ah,2
	mov dl,10
	int 21h
	mov ah,2
	mov dl,13
	int 21h
  RET
 PRINT_L ENDP

 DELAYING PROC FAR
          MOV DX,120
          DELAYFORSCREENP7:
          MOV CX,0EEEEH
          LABINP7:
          DEC CX
          JNZ LABINP7
          DEC DX
          JNZ DELAYFORSCREENP7

 RET
 DELAYING ENDP


 PRINT_COL_NUM PROC FAR 
    PUSHA
    MOV BL,73H
   	MOV BH,BL
    MOV CL,4
    SHR BH,CL
    mov ah,9
    MOV CX,1
    int 10h 
  	POPA
    RET
 PRINT_COL_NUM ENDP

 GET_ENTERKEYPROC PROC FAR
 AGAIN:
 MOV AH,0
 INT 16H
 cmp AH,1CH
 JZ FINISH 
 JMP AGAIN
 FINISH:
 RET
 GET_ENTERKEYPROC ENDP

CLEAR_SCREEN_PROC PROC FAR
    MOV AX,0600H ;06:scroll up,00:clear scr
    MOV BH,0FH   ;NORMAL COLORS(B/W)
    MOV CX,0     ;TOP LEFT CORNER
    MOV DL,79    ;BOTTOM RIGHT CORNER
    MOV DH,24
    INT 10H
    RET
 CLEAR_SCREEN_PROC ENDP


;-------------------COMP INST PROC-----------------------
COM_INST PROC FAR 
;FORBB CHECK
LEA DI,COMp
MOV DL,[SI]
MOV CH,0
MOV CL,ACT_SIZE
CHECK_FORB:
MOV AL,[DI]
CMP AL,DL
JE NOT_COMMAND
INC DI
LOOP CHECK_FORB
;CMP INST
LEA SI,COMp
MOV BH,0
MOV BL,[SI] 
MOV AX,0         ;;MOVE MESS TO AX
ADD AX,BX
INC SI                                                                  
MOV BH,0
MOV BL,[SI] 
ADD AX,BX
INC SI
MOV BH,0
MOV BL,[SI] 
ADD AX,BX    ;;GET SUM OF ASCII CODES OF 1ST 3 CHAR 


MOV BP,20
;;COMPARE 
;//1.MOVE
CMP AX,338D
MOV BP,1
JZ OPERATIONS1
;//2.ADD
CMP AX,297D
MOV BP,2
JZ OPERATIONS1
;//3.SUB
CMP AX,330D
MOV BP,3
JZ OPERATIONS1
;//4.AND
CMP AX,307D
MOV BP,6
JZ OPERATIONS1
;//5.ROR
CMP AX,339D
MOV BP,11
JZ OPERATIONS2
;//6.7.8.NOOP & SHR &ROL
CMP AX,333D
JZ EH

;//9.CLC
CMP AX,306D
JZ CLCF
;//10.XOR
CMP AX,345D
MOV BP,0
JZ OPERATIONS1
;//11.ADC
CMP AX,296D
MOV BP,5
JZ OPERATIONS1
;//12.SBB
CMP AX,311D
MOV BP,4
JZ OPERATIONS1
;//13.SHL & 16.RCR
CMP AX,327D
JZ EH2

;//14.SAR
CMP AX,326D
MOV BP,10
JZ OPERATIONS2

;//15.RCL
CMP AX,321
MOV BP,13
JZ OPERATIONS2
;INC
CMP AX,314D
MOV BP,30
JZ OPERATIONS3
;DEC
CMP AX,300D
MOV BP,31
JZ OPERATIONS3

;DIV
CMP AX,323D
MOV BP,40
JZ OPERATIONS3

;MUL
CMP AX,334D
MOV BP,32
JZ OPERATIONS3

;OR
CMP AX,257D
JZ ORING

JMP NOT_COMMAND

 
;//SHR,NOOP,ROL 
EH:
 LEA SI,COMp
 MOV DL,[SI]
 CMP DL,'n'
 JZ NOOP
 CMP DL,'s'
 MOV BP,9
 JZ OPERATIONS2
 CMP DL,'r'
 MOV BP,12
 JZ OPERATIONS2
 JMP NOT_COMMAND

;//SHL,RCR
EH2:
 LEA SI,COMp
 MOV DL,[SI]
 CMP DL,'s'
 MOV BP,8
 JMP OPERATIONS2

  CMP DL,'r'
  MOV BP,20
  JMP OPERATIONS2

  JMP NOT_COMMAND
RET

COM_INST ENDP





 ;REG P2 = GENERAL
 MIR_REG_P2 PROC FAR 
 ;PUT TEMP REG IN PLAYER 2 REG
       MIRROR RAX2,RAX1
       MIRROR RBX2,RBX1
       MIRROR RCX2,RCX1
       MIRROR RDX2,RDX1
       MIRROR ISI2,ISI1
       MIRROR IDI2,IDI1
       MIRROR BBP2,BBP1
       MIRROR SSP2,SSP1
       MIRROR FLAGS2,FLAGS
       ;MIRRORING MEMORY 1 FE MEMORY 2
       MOV CX,16
       BANAAL1:
       LEA SI,MEMORY1
       LEA DI,MEMORY2
       MOV DH,[SI]
       MOV [DI],DH
       INC SI
       INC DI
       LOOP BANAAL1

 RET
 MIR_REG_P2 ENDP

;REG P1 = GENERAL
 MIR_REG_P1 PROC FAR
    ;RETURN TEMP VALUE TO PLAYER 1 REG THAT IS SELECTED BY 2
         MIRROR RAX3,RAX1
         MIRROR RBX3,RBX1
         MIRROR RCX3,RCX1
         MIRROR RDX3,RDX1
         MIRROR ISI3,ISI1
         MIRROR IDI3,IDI1
         MIRROR BBP3,BBP1
         MIRROR SSP3,SSP1      
         MIRROR FLAGS3,FLAGS
         
         ;MIRRORING MEMORY 1 FE MEMORY 3
         MOV CX,16
         BANAAL3:
         LEA SI,MEMORY1
         LEA DI,MEMORY3
         MOV DH,[SI]
         MOV [DI],DH
         INC SI
         INC DI
         LOOP BANAAL3
 RET
 MIR_REG_P1 ENDP
;GENERAL = p1
 MIR_REG_GEN_p1 PROC FAR
    ;RETURN TEMP VALUE TO PLAYER 1 REG THAT IS SELECTED BY 2
    ;MIRROR MY OWN REG IN GENERAL
    ;PUT PLAYER 1 REG IN TEMP
       MIRROR RAX1,RAX3
       MIRROR RBX1,RBX3
       MIRROR RCX1,RCX3
       MIRROR RDX1,RDX3
       MIRROR ISI1,ISI3
       MIRROR IDI1,IDI3
       MIRROR BBP1,BBP3
       MIRROR SSP1,SSP3
       MIRROR FLAGS,FLAGS3
       ;MIRRORING MEMORY 3 FE MEMORY 1
      MOV CX,16
      BANAAL5:
      LEA SI,MEMORY3
      LEA DI,MEMORY1
      MOV DH,[SI]
      MOV [DI],DH
      INC SI
      INC DI
      LOOP BANAAL5
 RET
 MIR_REG_GEN_p1 ENDP

 ;GENERAL = p2
 MIR_REG_GEN_p2 PROC FAR
    ;RETURN TEMP VALUE TO PLAYER 1 REG THAT IS SELECTED BY 2
    ;MIRROR MY OWN REG IN GENERAL
    ;PUT PLAYER 1 REG IN TEMP
       MIRROR RAX1,RAX2
       MIRROR RBX1,RBX2
       MIRROR RCX1,RCX2
       MIRROR RDX1,RDX2
       MIRROR ISI1,ISI2
       MIRROR IDI1,IDI2
       MIRROR BBP1,BBP2
       MIRROR SSP1,SSP2
       MIRROR FLAGS,FLAGS2
       ;MIRRORING MEMORY 3 FE MEMORY 1
      MOV CX,16
      BANAAL9:
      LEA SI,MEMORY2
      LEA DI,MEMORY1
      MOV DH,[SI]
      MOV [DI],DH
      INC SI
      INC DI
      LOOP BANAAL9
 RET
 MIR_REG_GEN_p2 ENDP



;EXECUTE ON MY OWN PROCCESSOR
 OWN_PROCC PROC FAR
 CMP ELDOOR,2
 JZ DHP2
      ;MIRROR MY OWN REG IN GENERAL
      ;PUT PLAYER 1 REG IN TEMP
      CALL MIR_REG_GEN_p1
      ;NAFEZ 3ALA PROCC BETA3Y
      MOV PUP_INUSE,1
      CMP GAMELEVEL,2
      JZ SKIPO
      SUB P1_POINTS,5D
      SKIPO:
      JMP ON_MY_OWN_P1
 DHP2:
      ;MIRROR MY OWN REG IN GENERAL
      ;PUT PLAYER 2 REG IN TEMP
      CALL MIR_REG_GEN_p2
      MOV PUP_INUSE,1
       CMP GAMELEVEL,2
      JZ SKIPO1
      SUB P2_POINTS,5D
      SKIPO1:
      JMP ON_MY_OWN_P2
 MSH_P2:

 RET
 OWN_PROCC ENDP











;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;----------------------------------------------------------------MAIN PROC---------------------------------------------------------------------------------------------


MAIN PROC FAR  
    MOV AX,@DATA
    MOV DS,AX  
   GAME_GEDED:
    
    ;---------------------------------------------------WELCOME SCREEN----------------------------------------------------------------------------

    ;ENTER TEXT MODE
    MOV AH,0
    MOV AL,3
    INT 10H

    ;PLAYER 1  
    CALL PRINT_L                       
    DISPLAY_MESS INMESS1           ;display mess1
    CALL PRINT_L
    READ_USERNAME NAMEMESS         ;read username
    CALL PRINT_L
    DISPLAY_MESS INMESS2           ;display mess2
    CALL PRINT_L
    READ_POINTS PMESS,P1_POINTS ;read points
    CALL PRINT_L
    DISPLAY_MESS INMESS3           ;display mess3
    CALL GET_ENTERKEYPROC  
    CALL CLEAR_SCREEN_PROC                   ;CLEAR SCREEN

    ;PLAYER 2
    MOVE_CURSOR 0,0
    CALL PRINT_L                       
    DISPLAY_MESS INMESS1           ;display mess1
    CALL PRINT_L
    READ_USERNAME NAMEMESS2         ;read username
    CALL PRINT_L
    DISPLAY_MESS INMESS2           ;display mess2
    CALL PRINT_L
    READ_POINTS PMESS2,P2_POINTS ;read points
    CALL PRINT_L
    DISPLAY_MESS INMESS3           ;display mess3
    CALL GET_ENTERKEYPROC  
    CALL CLEAR_SCREEN_PROC                   ;CLEAR SCREEN

    ;INTIALIZE POINT OF GAME
    LEA SI,POINTS_GAME
    MOV BL,P1_POINTS
    MOV BH,P2_POINTS
    CMP BL,BH
    JA P1_HIGHER_POINTS  
    LEA DI, P2_POINTS          
    MOV [SI],BL                   ;P1 POINTS IS GAME POINTS
    MOV [DI],BL
    JMP P2_HIGHER_POINTS

    P1_HIGHER_POINTS:
    LEA DI,P1_POINTS
    MOV [SI],BH                   ;P2 POINTS IS GAME POINTS
    MOV [DI],BH

    P2_HIGHER_POINTS:


    
     ;---------------------------------------------------MAIN SCREEN----------------------------------------------------------------------------
    ;MAIN SCREEN FOR PLAYER 1
    ;MOVE_CURSOR 3,25
   ; DISPLAY_MESS PLAYER1MESS
   ; MOVE_CURSOR 6,25
    ;DISPLAY_MESS INMESS4           ;DISPLAY MESS4  
   ; MOVE_CURSOR 9,25
   ; DISPLAY_MESS INMESS5           ;DISPLAY MESS5
   ; MOVE_CURSOR 12,25
   ; DISPLAY_MESS INMESS6           ;DISPLAY MESS6 
   ; MOVE_CURSOR 18,0
    ;DISPLAY_MESS LINE              ;DRAW LINE 
   ; CALL PRINT_L
  ;////////////  FUNCTIONS_MAINSCREENP1 CHOICE_P1      ;FUNCTION WILL BE EXECUTED IN MAINSCREEN

    
    ;MAIN SCREEN FOR PLAYER 2
    ;PLAYER2_MODE_CHOICE:
   ; CALL CLEAR_SCREEN_PROC                   ;CLEAR SCREEN 
   ; MOVE_CURSOR 3,25
   ; DISPLAY_MESS PLAYER2MESS
   ; MOVE_CURSOR 6,25
   ; DISPLAY_MESS INMESS4           ;DISPLAY MESS4  
   ; MOVE_CURSOR 9,25
   ; DISPLAY_MESS INMESS5           ;DISPLAY MESS5
   ; MOVE_CURSOR 12,25
   ; DISPLAY_MESS INMESS6           ;DISPLAY MESS6 
   ; MOVE_CURSOR 18,0
   ; DISPLAY_MESS LINE              ;DRAW LINE 
   ; CALL PRINT_L
    ;LEA DI,CHOICE_P1                         
   ;///////////// FUNCTIONS_MAINSCREENP2 CHOICE_P2,CHOICE_P1        ;FUNCTION WILL BE EXECUTED IN MAINSCREEN 

    ;COMPARE CHOICES OF 2 PLAYERS
    ;CHECK_VALID_MODE:
   ; LEA SI,CHOICE_P1    ;PLAYER 1 CHOICE
   ; LEA DI,CHOICE_P2    ;PLAYER 2 CHOICE
   ; MOV DL,[SI]
   ; MOV DH,[DI]
   ; CMP DH,DL
   ; JZ EH_ELMODE
   ; JMP FINISHED

   ; EH_ELMODE:
   ; CMP DH,'C'
   ; JZ CHAT
   ; CMP DH,'G'
   ; JZ GAME
   ; CMP DH,'E'
   ; JZ FINISHED
    
;-----------------------------------------------------------------CHATTING MODE----------------------------------------------------------------------------------------
  ; CHAT:
  ; JMP FINISHED

;-----------------------------------------------------------------GAME MODE-------------------------------------------------------------------------------------------
   GAME:
    ;GAME SELECTION MODES AND LEVEL SCREEN
    CALL CLEAR_SCREEN_PROC                   ;CLEAR SCREEN 
    MOVE_CURSOR 6,25
    DISPLAY_MESS LEVELSEL           ;DISPLAY LEVEL SELECTION    
    GAME_WIND  GAMELEVEL                     ;FUNCTION TO SELECT LEVEL
    
    ;CHOOSING FORBIDDEN CHAR FOR EACH PLAYER AND PRESS F4 WHEN READY
    CALL CLEAR_SCREEN_PROC
       
      ;------------------PLAYER 1 SCREEN
      
      MOVE_CURSOR 4,35
      DISPLAY_MESS NAMEMESS+2   
      MOVE_CURSOR 12,17 
      DISPLAY_MESS FORB_MESS  
      FORBIDDEN_CHAR CHAR1 
      MOVE_CURSOR 16,17 
      DISPLAY_MESS MESS_F4
      ;CHECK FOR F4 TO START THE GAME
      CHECKO1:
       mov ah,0                             ;CONSUME BUFFER  
      int 16h 
      CHECK1:
	    MOV AH,1
	    INT 16H  
	    JZ CHECK1 
	                                        
      CMP AH,3EH
      JNZ CHECKO1 
      MOV AH,0                            ;CONSUME BUFFER
	    INT 16H 
      CALL CLEAR_SCREEN_PROC
      ;--------------------PLAYER 2 SCREEN
      MOVE_CURSOR 4,35
      DISPLAY_MESS NAMEMESS2+2
      MOVE_CURSOR 12,17 
      DISPLAY_MESS FORB_MESS 
      FORBIDDEN_CHAR CHAR2
      MOVE_CURSOR 16,17 
      DISPLAY_MESS MESS_F4
      ;CHECK FOR F4 TO START THE GAME
      CHECKO3:
      mov ah,0                             ;CONSUME BUFFER  
      int 16h   
      CHECK3:
	    MOV AH,1
	    INT 16H  
	    JZ CHECK3 
	                            
      CMP AH,3EH
      JNZ CHECKO3
      MOV AH,0                            ;CONSUME BUFFER
	    INT 16H
      CALL CLEAR_SCREEN_PROC

      ;SCREEN OF FOR BOTH CHARS
      MOV AL,GAMELEVEL
      CMP AL,2
      JZ NOT_TO_DSIPLAY
      MOVE_CURSOR 10,33
      DISPLAY_MESS MESs10
      DISPLAY_MESS CHAR1
      MOVE_CURSOR 15,33
      DISPLAY_MESS MESS11
      DISPLAY_MESS CHAR2

      CALL DELAYING

      NOT_TO_DSIPLAY:

     
      ;SCREEN OF AVALIABLE POWER UPS
      CALL CLEAR_SCREEN_PROC
      MOVE_CURSOR 4,22 
      DISPLAY_MESS  AVA_POWER 
      MOVE_CURSOR 7,26
      DISPLAY_MESS  POWERUP 
      CALL PRINT_L  
      CALL PRINT_L
      DISPLAY_MESS  PU_1
      CALL PRINT_L 
      CALL PRINT_L
      DISPLAY_MESS  PU_2
      CALL PRINT_L
      CALL PRINT_L
      DISPLAY_MESS  PU_3
      CALL PRINT_L
      CALL PRINT_L
      DISPLAY_MESS  PU_4 
      CMP GAMELEVEL,1
      JZ SKIP
      CALL PRINT_L
      CALL PRINT_L
      DISPLAY_MESS  PU_5
      SKIP:
      ;DELAY
         CALL DELAYING

          ;--------------------------------------------MAIN LOOP--------------------------------------------------------------------    


    MAIN_LOOP:

          ;------------------------------execute the game (IMAGE,COMMANDS,INTIALIZE THE REG)----------------------------------------
    ;IMAGE 

        mov ax,13h                     ; for 320x200x256 "CGA 256" colors (13h)
       mov bh,0h                      ;bh=0 page number always 0 for 320x200x256 CGA-256
       int 10h   
   
    CALL CLEAR_SCREEN_PROC                   ;CLEAR SCREEN

      ;---ENTER GRAPHICS MODE---------------------
   
    
                      
    
    call show_bitmap
    
    
    ;DISPLAY PLAYER NAMES
    ;PLAYER 1
    DISPLAY_GRAPH  NAMEMESS+2,NAMEMESS[1],12,2             ;DISPLAY PLAYER1 NAME
    ;MOVE_CURSOR 12,16
    PRINT_POINTS P1_POINTS,TEMP_POINTS,12,12               ;DISPLAY POINTS OF PLAYER 1

    ;PLAYER 2
    DISPLAY_GRAPH NAMEMESS2+2,NAMEMESS2[1],12,26          ;DISPLAY PLAYER2 NAME
    ;MOVE_CURSOR 12,35
    PRINT_POINTS P2_POINTS,TEMP_POINTS,12,36               ;DISPLAY POINTS OF PLAYER 2

    PRINT_NUMBER NUM_HEXA,TARG_VAL,NUMTODISP,15,28  


      ;------------------------------------------REGISTER--------------------------------------------------------------------
    ;;INTIALIZE REGISTERS PLAYER 1          THAT IS CHANGED IN COMMAND OF [P2] ETB3HA FE DOOR=2
     PRINT_NUMBER NUM_HEXA,RAX3,NUMTODISP,1,4     ;INTIALIZE AX
     PRINT_NUMBER NUM_HEXA,ISI3,NUMTODISP,1,11    ;INTIALIZE SI
     PRINT_NUMBER NUM_HEXA,RBX3,NUMTODISP,3,4     ;INTIALIZE BX
     PRINT_NUMBER NUM_HEXA,IDI3,NUMTODISP,3,11    ;INTIALIZE DI
     PRINT_NUMBER NUM_HEXA,RCX3,NUMTODISP,5,4     ;INTIALIZE cx
     PRINT_NUMBER NUM_HEXA,BBP3,NUMTODISP,5,11    ;INTIALIZE 
     PRINT_NUMBER NUM_HEXA,RDX3,NUMTODISP,8,4     ;INTIALIZE DX 
     PRINT_NUMBER NUM_HEXA,SSP3,NUMTODISP,8,11    ;INTIALIZE 
     MOV BL,MEM3_ROW
     MOV BH,MEM3_COL
     MOV MEM_ROW,BL
     MOV MEM_COL,BH
      ;MEMORY 3 DISPLAY
      MOV SI,16
      MEM3_PRIN:
      CALL PRINT_MEM3
      DEC SI
      MOV DH,MEM_ROW
      INC DH
      MOV MEM_ROW,DH
      CMP SI,0
      JNZ MEM3_PRIN
      CMP GAMELEVEL,2
      JZ MAT3RDSH_FB
       DISPLAY_GRAPH  CHAR2,ONE,15,15  
       MAT3RDSH_FB:

     ;;INTIALIZE REGISTERS PLAYER 2         THAT IS CHANGED IN COMMAND OF [P1]   ETB3HA FE DOOR=1
     PRINT_NUMBER NUM_HEXA,RAX2,NUMTODISP,1,28     ;INTIALIZE AX
     PRINT_NUMBER NUM_HEXA,ISI2,NUMTODISP,1,35     ;INTIALIZE SI
     PRINT_NUMBER NUM_HEXA,RBX2,NUMTODISP,3,28     ;INTIALIZE BX
     PRINT_NUMBER NUM_HEXA,IDI2,NUMTODISP,3,35     ;INTIALIZE DI
     PRINT_NUMBER NUM_HEXA,RCX2,NUMTODISP,5,28     ;INTIALIZE cx
     PRINT_NUMBER NUM_HEXA,BBP2,NUMTODISP,5,35    ;INTIALIZE 
     PRINT_NUMBER NUM_HEXA,RDX2,NUMTODISP,8,28    ;INTIALIZE DX
     PRINT_NUMBER NUM_HEXA,SSP2,NUMTODISP,8,35    ;INTIALIZE 
     ;MEMORY 2 
     MOV BL,MEM2_ROW
     MOV BH,MEM2_COL
     MOV MEM_ROW,BL
     MOV MEM_COL,BH
      ;MEMORY 3 DISPLAY
      MOV SI,16
      MEM2_PRIN:
      CALL PRINT_MEM3
      DEC SI
      MOV DH,MEM_ROW
      INC DH
      MOV MEM_ROW,DH
      CMP SI,0
      JNZ MEM2_PRIN
      CMP GAMELEVEL,2
      JZ MAT3RDSH_FB1
      DISPLAY_GRAPH  CHAR1,ONE,15,24  
      MAT3RDSH_FB1:

    ;----------------------------------------------------------READ COMMANDS--------------------------------------------------------- 

    RESET_COM COM1+2
    RESET_COM COMp1
    RESET_COM COM2+2
    RESET_COM COMp2
    RESET_COM COMp
    MOV AL,0
    MOV A_SIZE1,AL
    MOV COM1+1,AL
    MOV AL,0
    MOV A_SIZE2,AL
    MOV COM2+1,AL
    ;DECIDE LEVEL TO KNOW HOW TO EXECUTE
      MOV AL,GAMELEVEL
      CMP AL,2
      JZ LEVEL2
    ;---------------BASHOF DH DOR AMHY LA3EB--------------------

    EHELDOOR:
    
    CMP ELDOOR,2
    JZ DOOR_PLAYER2  
   
    DOOR_PLAYER1:

    ;player 1  
    
     ;--------PUT REG PLAYER 2 IN TEMP
      CALL MIR_REG_GEN_p2        ;GEN =P2

      ;READ COMMAND P1
      ON_MY_OWN_P1:
       READ_COMMAND COM1+2,A_SIZE1,1
       LEA DI,COM1+1
       MOV CL,A_SIZE1
       MOV [DI],CL
       UP_TO_LOW COM1+2,COMp1,A_SIZE1 

    ;--------- CHECK VALID INS (MOV,ADD,..)-------- 
    ;translate command
    ;PLAYER 1
    ;IF WE ARE LEVEL 1
      GENERAL_INST COMp,COMp1,A_SIZE1
      MOV AL,A_SIZE1
       MOV ACT_SIZE,AL
      LEA SI,CHAR2
      CALL COM_INST 

    ;---COMMAND PLAYER 1 ON REG PLAYER 2
    ;---player 2---
    ;--------- CHECK VALID INS (MOV,ADD,..)--------

    DOOR_PLAYER2: 
      MOV AL,2
      MOV ELDOOR,AL
      
      CMP PUP_INUSE,1
      JZ  SKIP_AS_PUP_IN_USE
       
      CALL MIR_REG_P2

       
       SKIP_AS_PUP_IN_USE:
       MOV PUP_INUSE,0

       ;PUT PLAYER 1 REG IN TEMP

       CALL MIR_REG_GEN_p1

      ON_MY_OWN_P2:
       ;..RESET COMMAND COMP
       RESET_COM COMp 
      ;READ COMMAND player 2
        READ_COMMAND COM2+2,A_SIZE2,21
        LEA DI,COM2+1
        MOV CL,A_SIZE2
        MOV [DI],CL
        UP_TO_LOW COM2+2,COMp2,A_SIZE2 

      ;TRANSLATE PLAYER 2
       GENERAL_INST COMp,COMp2,A_SIZE2
       MOV AL,A_SIZE2
       MOV ACT_SIZE,AL
       LEA SI,CHAR1
       CALL COM_INST 

     ;-------------------------------------------------AND-----------------------------------------------------------
OPERATIONS1:
      MOV AL,ELDOOR
      CMP AL,2
      JZ D2_AN

      D1_AN: 
        GENERAL_INST COMp,COMp1,A_SIZE1
       JMP START_AN

      D2_AN:
        GENERAL_INST COMp,COMp2,A_SIZE2

       START_AN:

       ;INST:
       
    MOV SI,OFFSET COMp 
    MOV AX,0
    MOV CX,0
    MOV AL,[SI+4] 
    ADD CL,[SI+5]
    ADD AX,CX   
    CMP AX,217
    JZ NTAAXDES_PLUS
    CMP AX,218                   ;BN3ML JUMP 3LA 7BT 7GAT W M7TNA4 MEMORY1 4OF B2AA
    JZ NTABXDES_PLUS
    CMP AX,219                    ;ANAM7TAGFUNCTIONT4OFLE 7WAR 2RKAM KDA KDA HOMA BLKTEER 4 2RKAM 3NDE M4AKL KBEERAA FE 2RKAM 5LE BALK ANA HA5OD 2RKAM MN 3LA 2L4MAAAL YAMEGZ 
    JZ NTACXDES_PLUS
    CMP AX,220
    JZ NTADXORSIDES_PLUS
    CMP AX,205
    JZ NTADIORALDES_PLUS
    CMP AX,201
    JZ NTAAHDIS_PLUS
    CMP AX,206
    JZ NTABLDIS_PLUS
    CMP AX,202
    JZ NTABHDIS_PLUS
    CMP AX,207
    JZ NTACLDIS_PLUS
    CMP AX,203
    JZ NTACHDIS_PLUS
    CMP AX,208
    JZ NTADLDIS_PLUS
    CMP AX,204
    JZ NTADHDIS_PLUS 
    JMP ERROR
NTAAXDES_PLUS:
  MOV BX,0
  MOV BL,[SI+8]
  CMP [SI+7],91D
  JZ HAKML3ADY_AX
  CMP BL,104
  JNAE NTAAXDESW_RKM_YA_MEMRKM_YAERROR_PLUS
  HAKML3ADY_AX:
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+7] 
  MOV CL,[SI+8]
  ADD AX,CX
  CMP AX,217
  JZ NTAAXDESWAXSOURCE_PLUS
  CMP AX,218 
  JZ NTAAXDESWBXSOURCE_PLUS
  CMP AX,219
  JZ NTAAXDESWCXSOURCE_PLUS
  CMP AX,220
 JZ NTAAXDESWDXORSISOURCE_PLUS
 CMP AX,205
 JZ NTAAXDESWDIORALLWALERROR_PLUS
 
 MOV CX,0
 MOV BX,0                            ;TAKECARE
 MOV BL,[SI+9]
 MOV CL,[SI+10]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ AXDES_WNTA_MEMBX_PLUS
 CMP AX,404              
 JZ NTA_AXDES_WMEMSI_PLUS
 CMP AX,389
 JZ NTA_AXDES_WMEMDI_PLUS
 JMP NTAAXDESW_RKM_YA_MEMRKM_YAERROR_PLUS 
 
 NTAAXDESWAXSOURCE_PLUS:  
 MOV BX,RAX1
 MOV AX,FLAGS
 PUSH AX
 
 AXDESWAXSOURCE_MOV:
 CMP BP,1
 JNZ AXDESWAXSOURCE_ADD
 MOV BX,BX
 JMP OUTT_AXDISAXSOURCE
 AXDESWAXSOURCE_ADD:
 CMP BP,2
 JNZ AXDESWAXSOURCE_SUB
 POPF 
 ADD BX,BX
 JMP OUTT_AXDISAXSOURCE
 AXDESWAXSOURCE_SUB:
 CMP BP,3
 JNZ AXDESWAXSOURCE_SBB
 POPF
 SUB BX,BX
 JMP OUTT_AXDISAXSOURCE
 AXDESWAXSOURCE_SBB:
 CMP BP,4
 JNZ AXDESWAXSOURCE_ADC
 POPF
 SBB BX,BX
 JMP OUTT_AXDISAXSOURCE
 AXDESWAXSOURCE_ADC:
 CMP BP,5
 JNZ AXDESWAXSOURCE_AND
 POPF
 ADC BX,BX
 JMP OUTT_AXDISAXSOURCE
 AXDESWAXSOURCE_AND:
 CMP BP,6
 JNZ AXDESWAXSOURCE_XOR 
 POPF
 AND BX,BX
 JMP OUTT_AXDISAXSOURCE
 AXDESWAXSOURCE_XOR:
 POPF
 XOR BX,BX
 
 OUTT_AXDISAXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,BX
 JMP NAG7
 
 NTAAXDESWBXSOURCE_PLUS: 
 MOV BX,RAX1
 MOV CX,RBX1
 MOV AX,FLAGS
 PUSH AX
 
 AXDESWBXSOURCE_MOV:
 CMP BP,1
 JNZ AXDESWBXSOURCE_ADD
 MOV BX,CX
 JMP OUTT_AXDISAXSOURCE
 AXDESWBXSOURCE_ADD:
 CMP BP,2
 JNZ AXDESWBXSOURCE_SUB
 POPF 
 ADD BX,CX
 JMP OUTT_AXDISBXSOURCE
 AXDESWBXSOURCE_SUB:
 CMP BP,3
 JNZ AXDESWBXSOURCE_SBB
 POPF
 SUB BX,CX
 JMP OUTT_AXDISBXSOURCE
 AXDESWBXSOURCE_SBB:
 CMP BP,4
 JNZ AXDESWBXSOURCE_ADC
 POPF
 SBB BX,CX
 JMP OUTT_AXDISBXSOURCE
 AXDESWBXSOURCE_ADC:
 CMP BP,5
 JNZ AXDESWBXSOURCE_AND
 POPF
 ADC BX,CX
 JMP OUTT_AXDISBXSOURCE
 AXDESWBXSOURCE_AND:
 CMP BP,6
 JNZ AXDESWBXSOURCE_XOR
 POPF
 AND BX,CX
 JMP OUTT_AXDISBXSOURCE
 AXDESWBXSOURCE_XOR:
 POPF
 XOR BX,CX
 
 OUTT_AXDISBXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,BX
 JMP NAG7
 
 NTAAXDESWCXSOURCE_PLUS:
 MOV BX,RAX1
 MOV CX,RCX1
 MOV AX,FLAGS
 PUSH AX
 
 AXDESWCXSOURCE_MOV:
 CMP BP,1
 JNZ AXDESWCXSOURCE_ADD
 MOV BX,CX
 JMP OUTT_AXDISCXSOURCE
 AXDESWCXSOURCE_ADD:
 CMP BP,2
 JNZ AXDESWCXSOURCE_SUB
 POPF 
 ADD BX,CX
 JMP OUTT_AXDISCXSOURCE
 AXDESWCXSOURCE_SUB:
 CMP BP,3
 JNZ AXDESWCXSOURCE_SBB
 POPF
 SUB BX,CX
 JMP OUTT_AXDISCXSOURCE
 AXDESWCXSOURCE_SBB:
 CMP BP,4
 JNZ AXDESWCXSOURCE_ADC
 POPF
 SBB BX,CX
 JMP OUTT_AXDISCXSOURCE
 AXDESWCXSOURCE_ADC:
 CMP BP,5
 JNZ AXDESWCXSOURCE_AND
 POPF
 ADC BX,CX
 JMP OUTT_AXDISCXSOURCE
 AXDESWCXSOURCE_AND:
 CMP BP,6
 JNZ AXDESWCXSOURCE_XOR
 POPF
 AND BX,CX
 JMP OUTT_AXDISCXSOURCE
 AXDESWCXSOURCE_XOR:
 POPF
 XOR BX,CX
 
 OUTT_AXDISCXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,BX
 JMP NAG7
 NTAAXDESWDXORSISOURCE_PLUS:
 MOV AL,[SI+7]
 CMP AL,100
 JZ NTAAXDESWDXSOURCE_PLUS
  
NTAAXDESWSISOURCE_PLUS:
 MOV BX,RAX1
 MOV CX,ISI1
 MOV AX,FLAGS
 PUSH AX
 
 AXDESWSISOURCE_MOV:
 CMP BP,1
 JNZ AXDESWSISOURCE_ADD
 MOV BX,CX
 JMP OUTT_AXDISSISOURCE
 AXDESWSISOURCE_ADD:
 CMP BP,2
 JNZ AXDESWSISOURCE_SUB
 ADD BX,CX
 JMP OUTT_AXDISSISOURCE
 AXDESWSISOURCE_SUB:
 CMP BP,3
 JNZ AXDESWSISOURCE_SBB 
 
 SUB BX,CX
 JMP OUTT_AXDISSISOURCE
 AXDESWSISOURCE_SBB:
 CMP BP,4
 JNZ AXDESWSISOURCE_ADC
 POPF
 SBB BX,CX
 JMP OUTT_AXDISSISOURCE
 AXDESWSISOURCE_ADC:
 CMP BP,5
 JNZ AXDESWSISOURCE_AND 
 POPF
 ADC BX,CX
 JMP OUTT_AXDISSISOURCE
 AXDESWSISOURCE_AND:
 CMP BP,6
 JNZ AXDESWSISOURCE_XOR
 AND BX,CX
 JMP OUTT_AXDISSISOURCE
 AXDESWSISOURCE_XOR:
 XOR BX,CX
 
 OUTT_AXDISSISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,BX
 JMP NAG7
NTAAXDESWDXSOURCE_PLUS:
 MOV BX,RAX1
 MOV CX,RDX1
 MOV AX,FLAGS
 PUSH AX

  AXDESWDXSOURCE_MOV:
 CMP BP,1
 JNZ AXDESWDXSOURCE_ADD
 MOV BX,CX
 JMP OUTT_AXDISDXSOURCE
 AXDESWDXSOURCE_ADD:
 CMP BP,2
 JNZ AXDESWDXSOURCE_SUB 
 ADD BX,CX
 JMP OUTT_AXDISDXSOURCE
 AXDESWDXSOURCE_SUB:
 CMP BP,3
 JNZ AXDESWDXSOURCE_SBB
 SUB BX,CX
 JMP OUTT_AXDISDXSOURCE
 AXDESWDXSOURCE_SBB:
 CMP BP,4
 JNZ AXDESWDXSOURCE_ADC 
 POPF
 SBB BX,CX
 JMP OUTT_AXDISDXSOURCE
 AXDESWDXSOURCE_ADC:
 CMP BP,5
 JNZ AXDESWDXSOURCE_AND
 POPF
 ADC BX,CX
 JMP OUTT_AXDISDXSOURCE
 AXDESWDXSOURCE_AND:
 CMP BP,6
 JNZ AXDESWDXSOURCE_XOR
 AND BX,CX
 JMP OUTT_AXDISDXSOURCE
 AXDESWDXSOURCE_XOR:
 XOR BX,CX
 
 OUTT_AXDISDXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,BX
 JMP NAG7
NTAAXDESWDIORALLWALERROR_PLUS: 
MOV AL,[SI+7]
CMP AL,100 
JZ NTAAXDESWDISOURCE_PLUS
JMP ERROR
NTAAXDESWDISOURCE_PLUS:
 MOV BX,RAX1
 MOV CX,IDI1
 MOV AX,FLAGS
 PUSH AX
 
 AXDESWDISOURCE_MOV:
 CMP BP,1
 JNZ AXDESWDISOURCE_ADD
 MOV BX,CX
 JMP OUTT_AXDISDISOURCE
 AXDESWDISOURCE_ADD:
 CMP BP,2
 JNZ AXDESWDISOURCE_SUB 
 ADD BX,CX
 JMP OUTT_AXDISDISOURCE
 AXDESWDISOURCE_SUB:
 CMP BP,3
 JNZ AXDESWDISOURCE_SBB
 SUB BX,CX
 JMP OUTT_AXDISDISOURCE
 AXDESWDISOURCE_SBB:
 CMP BP,4
 JNZ AXDESWDISOURCE_ADC 
 POPF
 SBB BX,CX
 JMP OUTT_AXDISDISOURCE
 AXDESWDISOURCE_ADC:
 CMP BP,5
 JNZ AXDESWDISOURCE_AND 
 POPF
 ADC BX,CX
 JMP OUTT_AXDISDISOURCE
 AXDESWDISOURCE_AND:
 CMP BP,6
 JNZ AXDESWDISOURCE_XOR
 AND BX,CX
 JMP OUTT_AXDISDISOURCE
 AXDESWDISOURCE_XOR:
 XOR BX,CX
 
 OUTT_AXDISDISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,BX
 JMP NAG7
AXDES_WNTA_MEMBX_PLUS:
MOV BX,RBX1
CMP BX,0FH
JA ERROR
MOV CL, MEMORY1[BX]
MOV CH, MEMORY1[BX+1]
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX
POPF
 AXDESWMEMBXSOURCE_MOV:
 CMP BP,1
 JNZ AXDESWMEMBXSOURCE_ADD
 MOV BX,CX
 JMP OUTT_AXDISMEMBXSOURCE
 AXDESWMEMBXSOURCE_ADD:
 CMP BP,2
 JNZ AXDESWMEMBXSOURCE_SUB 
 ADD BX,CX
 JMP OUTT_AXDISMEMBXSOURCE
 AXDESWMEMBXSOURCE_SUB:
 CMP BP,3
 JNZ AXDESWMEMBXSOURCE_SBB
 SUB BX,CX
 JMP OUTT_AXDISMEMBXSOURCE
 AXDESWMEMBXSOURCE_SBB:
 CMP BP,4
 JNZ AXDESWMEMBXSOURCE_ADC
 POPF
 SBB BX,CX
 JMP OUTT_AXDISMEMBXSOURCE
 AXDESWMEMBXSOURCE_ADC:
 CMP BP,5
 JNZ AXDESWMEMBXSOURCE_AND 
 POPF
 ADC BX,CX
 JMP OUTT_AXDISMEMBXSOURCE
 AXDESWMEMBXSOURCE_AND:
 CMP BP,6
 JNZ AXDESWMEMBXSOURCE_XOR
 AND BX,CX
 JMP OUTT_AXDISMEMBXSOURCE
 AXDESWMEMBXSOURCE_XOR:
 XOR BX,CX
 
 OUTT_AXDISMEMBXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,BX
 JMP NAG7
NTA_AXDES_WMEMSI_PLUS:
MOV BX,ISI1
CMP BX,0FH
JA ERROR
MOV CH, MEMORY1[BX+1]
MOV CL, MEMORY1[BX]
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX

 AXDESWMEMSISOURCE_MOV:
 CMP BP,1
 JNZ AXDESWMEMSISOURCE_ADD
 MOV BX,CX
 JMP OUTT_AXDISMEMSISOURCE
 AXDESWMEMSISOURCE_ADD:
 CMP BP,2
 JNZ AXDESWMEMSISOURCE_SUB 
 ADD BX,CX
 JMP OUTT_AXDISMEMSISOURCE
 AXDESWMEMSISOURCE_SUB:
 CMP BP,3
 JNZ AXDESWMEMSISOURCE_SBB
 SUB BX,CX
 JMP OUTT_AXDISMEMSISOURCE
 AXDESWMEMSISOURCE_SBB:
 CMP BP,4
 JNZ AXDESWMEMSISOURCE_ADC
 POPF
 SBB BX,CX
 JMP OUTT_AXDISMEMSISOURCE
 AXDESWMEMSISOURCE_ADC:
 CMP BP,5
 JNZ AXDESWMEMSISOURCE_AND 
 POPF
 ADC BX,CX
 JMP OUTT_AXDISMEMSISOURCE
 AXDESWMEMSISOURCE_AND:
 CMP BP,6
 JNZ AXDESWMEMSISOURCE_XOR
 AND BX,CX
 JMP OUTT_AXDISMEMSISOURCE
 AXDESWMEMSISOURCE_XOR:
 XOR BX,CX
 
 OUTT_AXDISMEMSISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,BX
 JMP NAG7
NTA_AXDES_WMEMDI_PLUS:
MOV BX,IDI1
CMP BX,0FH
JA ERROR
MOV CH, MEMORY1[BX+1]
MOV CL, MEMORY1[BX]
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX
POPF
 AXDESWMEMDISOURCE_MOV:
 CMP BP,1
 JNZ AXDESWMEMDISOURCE_ADD
 MOV BX,CX
 JMP OUTT_AXDISMEMDISOURCE
 AXDESWMEMDISOURCE_ADD:
 CMP BP,2
 JNZ AXDESWMEMDISOURCE_SUB 
 ADD BX,CX
 JMP OUTT_AXDISMEMDISOURCE
 AXDESWMEMDISOURCE_SUB:
 CMP BP,3
 JNZ AXDESWMEMDISOURCE_SBB
 SUB BX,CX
 JMP OUTT_AXDISMEMDISOURCE
 AXDESWMEMDISOURCE_SBB:
 CMP BP,4
 JNZ AXDESWMEMDISOURCE_ADC
 POPF
 SBB BX,CX
 JMP OUTT_AXDISMEMDISOURCE
 AXDESWMEMDISOURCE_ADC:
 CMP BP,5
 JNZ AXDESWMEMDISOURCE_AND
 POPF
 ADC BX,CX
 JMP OUTT_AXDISMEMDISOURCE
 AXDESWMEMDISOURCE_AND:
 CMP BP,6
 JNZ AXDESWMEMDISOURCE_XOR
 AND BX,CX
 JMP OUTT_AXDISMEMDISOURCE
 AXDESWMEMDISOURCE_XOR:
 XOR BX,CX
 
 OUTT_AXDISMEMDISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,BX
 JMP NAG7
NTAAXDESW_RKM_YA_MEMRKM_YAERROR_PLUS:
MOV AL,[SI+7]
CMP AL,91
JZ MEMRKM_ORERROR_AXDES_PLUS

RKM_ORERROR_AXDES_PLUS:
MOV AL,[SI+8]
CMP AL,108
JZ ERROR
CMP AL,104
JZ ERROR
MOV VAR,BP
RET_NUM_COUNT  COMp+7
MOV BP,VAR
CMP CX,4
JA ERROR
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX
 
 AXDESWRAKAMSOURCE_MOV:
 CMP BP,1
 JNZ AXDESWRAKAMSOURCE_ADD
 MOV BX,DX
 JMP OUTT_AXDISRAKAMSOURCE
 AXDESWRAKAMSOURCE_ADD:
 CMP BP,2
 JNZ AXDESWRAKAMSOURCE_SUB 
 ADD BX,DX
 JMP OUTT_AXDISRAKAMSOURCE
 AXDESWRAKAMSOURCE_SUB:
 CMP BP,3
 JNZ AXDESWRAKAMSOURCE_SBB
 SUB BX,DX
 JMP OUTT_AXDISRAKAMSOURCE
 AXDESWRAKAMSOURCE_SBB:
 CMP BP,4
 JNZ AXDESWRAKAMSOURCE_ADC
 POPF
 SBB BX,DX
 JMP OUTT_AXDISRAKAMSOURCE
 AXDESWRAKAMSOURCE_ADC:
 CMP BP,5
 JNZ AXDESWRAKAMSOURCE_AND
 POPF
 ADC BX,DX
 JMP OUTT_AXDISRAKAMSOURCE
 AXDESWRAKAMSOURCE_AND:
 CMP BP,6
 JNZ AXDESWRAKAMSOURCE_XOR
 AND BX,DX
 JMP OUTT_AXDISRAKAMSOURCE
 AXDESWRAKAMSOURCE_XOR:
 XOR BX,DX
 
 OUTT_AXDISRAKAMSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,BX
 JMP NAG7
MEMRKM_ORERROR_AXDES_PLUS:
MOV AL,[SI+9]
CMP AL,108
JZ ERROR
CMP AL,104
JZ ERROR
MOV VAR,BP
RET_NUM_COUNT  COMp+8
MOV BP,VAR
CMP CX,4
JA ERROR
CMP DX,0FH
JA ERROR
MOV BX,DX
MOV CH, MEMORY1[BX+1]
MOV CL, MEMORY1[BX]
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX
 
 AXDESWMEMRAKAMSOURCE_MOV:
 CMP BP,1
 JNZ AXDESWMEMRAKAMSOURCE_ADD
 MOV BX,CX
 JMP OUTT_AXDISMEMRAKAMSOURCE
 AXDESWMEMRAKAMSOURCE_ADD:
 CMP BP,2
 JNZ AXDESWMEMRAKAMSOURCE_SUB 
 ADD BX,CX
 JMP OUTT_AXDISMEMRAKAMSOURCE
 AXDESWMEMRAKAMSOURCE_SUB:
 CMP BP,3
 JNZ AXDESWMEMRAKAMSOURCE_SBB
 SUB BX,CX
 JMP OUTT_AXDISMEMRAKAMSOURCE
 AXDESWMEMRAKAMSOURCE_SBB:
 CMP BP,4
 JNZ AXDESWMEMRAKAMSOURCE_ADC 
 POPF
 SBB BX,CX 
 JMP OUTT_AXDISMEMRAKAMSOURCE
 AXDESWMEMRAKAMSOURCE_ADC:
 CMP BP,5
 JNZ AXDESWMEMRAKAMSOURCE_AND
 POPF
 ADC BX,CX
 JMP OUTT_AXDISMEMRAKAMSOURCE
 AXDESWMEMRAKAMSOURCE_AND:
 CMP BP,6
 JNZ AXDESWMEMRAKAMSOURCE_XOR
 AND BX,CX
 JMP OUTT_AXDISMEMRAKAMSOURCE
 AXDESWMEMRAKAMSOURCE_XOR:
 XOR BX,CX
 
 OUTT_AXDISMEMRAKAMSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,BX
 JMP NAG7
;/////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTABXDES_PLUS:
  MOV BX,0
  MOV BL,[SI+8]
  CMP BL,104
  JNAE NTABXDESW_RKM_YA_MEMRKM_YAERROR_PLUS
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+7] 
  MOV CL,[SI+8]
  ADD AX,CX
  CMP AX,217
  JZ NTABXDESWAXSOURCE_PLUS
  CMP AX,218 
  JZ NTABXDESWBXSOURCE_PLUS
  CMP AX,219
  JZ NTABXDESWCXSOURCE_PLUS
  CMP AX,220
 JZ NTABXDESWDXORSISOURCE_PLUS
 CMP AX,205
 JZ NTABXDESWDIORALLWALERROR_PLUS
 MOV CX,0
 MOV BX,0                            ;TAKECARE
 MOV BL,[SI+9]
 MOV CL,[SI+10]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ BXDES_WNTA_MEMBX_PLUS
 CMP AX,404              
 JZ NTA_BXDES_WMEMSI_PLUS
 CMP AX,389
 JZ NTA_BXDES_WMEMDI_PLUS
 JMP NTABXDESW_RKM_YA_MEMRKM_YAERROR_PLUS 
 
 NTABXDESWAXSOURCE_PLUS:  
 MOV BX,RBX1
 MOV CX,RAX1
 MOV AX,FLAGS
 PUSH AX
 POPF
 BXDESWAXSOURCE_MOV:
 CMP BP,1
 JNZ BXDESWAXSOURCE_ADD
 MOV BX,CX
 JMP OUTT_BXDISAXSOURCE
 BXDESWAXSOURCE_ADD:
 CMP BP,2
 JNZ BXDESWAXSOURCE_SUB 
 ADD BX,CX
 JMP OUTT_BXDISAXSOURCE
 BXDESWAXSOURCE_SUB:
 CMP BP,3
 JNZ BXDESWAXSOURCE_SBB
 SUB BX,CX
 JMP OUTT_BXDISAXSOURCE
 BXDESWAXSOURCE_SBB:
 CMP BP,4
 JNZ BXDESWAXSOURCE_ADC
 SBB BX,CX
 JMP OUTT_BXDISAXSOURCE
 BXDESWAXSOURCE_ADC:
 CMP BP,5
 JNZ BXDESWAXSOURCE_AND
 ADC BX,CX
 JMP OUTT_BXDISAXSOURCE
 BXDESWAXSOURCE_AND:
 CMP BP,6
 JNZ BXDESWAXSOURCE_XOR
 AND BX,CX
 JMP OUTT_BXDISAXSOURCE
 BXDESWAXSOURCE_XOR:
 XOR BX,CX
 
 OUTT_BXDISAXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,BX
 JMP NAG7
 
 NTABXDESWBXSOURCE_PLUS: 
 MOV BX,RBX1
 MOV AX,FLAGS
 PUSH AX
 POPF
 BXDESWBXSOURCE_MOV:
 CMP BP,1
 JNZ BXDESWBXSOURCE_ADD
 MOV BX,BX
 JMP OUTT_BXDISAXSOURCE
 BXDESWBXSOURCE_ADD:
 CMP BP,2
 JNZ BXDESWBXSOURCE_SUB 
 ADD BX,BX
 JMP OUTT_BXDISBXSOURCE
 BXDESWBXSOURCE_SUB:
 CMP BP,3
 JNZ BXDESWBXSOURCE_SBB
 SUB BX,BX
 JMP OUTT_BXDISBXSOURCE
 BXDESWBXSOURCE_SBB:
 CMP BP,4
 JNZ BXDESWBXSOURCE_ADC
 SBB BX,BX
 JMP OUTT_BXDISBXSOURCE
 BXDESWBXSOURCE_ADC:
 CMP BP,5
 JNZ BXDESWBXSOURCE_AND
 ADC BX,BX
 JMP OUTT_BXDISBXSOURCE
 BXDESWBXSOURCE_AND:
 CMP BP,6
 JNZ BXDESWBXSOURCE_XOR
 AND BX,BX
 JMP OUTT_BXDISBXSOURCE
 BXDESWBXSOURCE_XOR:
 XOR BX,BX
 
 OUTT_BXDISBXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,BX
 JMP NAG7
 
 NTABXDESWCXSOURCE_PLUS:
 MOV BX,RBX1
 MOV CX,RCX1
 MOV AX,FLAGS
 PUSH AX
 POPF
 BXDESWCXSOURCE_MOV:
 CMP BP,1
 JNZ BXDESWCXSOURCE_ADD
 MOV BX,CX
 JMP OUTT_BXDISCXSOURCE
 BXDESWCXSOURCE_ADD:
 CMP BP,2
 JNZ BXDESWCXSOURCE_SUB 
 ADD BX,CX
 JMP OUTT_BXDISCXSOURCE
 BXDESWCXSOURCE_SUB:
 CMP BP,3
 JNZ BXDESWCXSOURCE_SBB
 SUB BX,CX
 JMP OUTT_BXDISCXSOURCE
 BXDESWCXSOURCE_SBB:
 CMP BP,4
 JNZ BXDESWCXSOURCE_ADC
 SBB BX,CX
 JMP OUTT_BXDISCXSOURCE
 BXDESWCXSOURCE_ADC:
 CMP BP,5
 JNZ BXDESWCXSOURCE_AND
 ADC BX,CX
 JMP OUTT_BXDISCXSOURCE
 BXDESWCXSOURCE_AND:
 CMP BP,6
 JNZ BXDESWCXSOURCE_XOR
 AND BX,CX
 JMP OUTT_BXDISCXSOURCE
 BXDESWCXSOURCE_XOR:
 XOR BX,CX
 
 OUTT_BXDISCXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,BX
 JMP NAG7
 NTABXDESWDXORSISOURCE_PLUS:
 MOV AL,[SI+7]
 CMP AL,100
 JZ NTABXDESWDXSOURCE_PLUS
  
NTABXDESWSISOURCE_PLUS:
 MOV BX,RBX1
 MOV CX,ISI1
 MOV AX,FLAGS
 PUSH AX
 POPF
 BXDESWSISOURCE_MOV:
 CMP BP,1
 JNZ BXDESWSISOURCE_ADD
 MOV BX,CX
 JMP OUTT_BXDISSISOURCE
 BXDESWSISOURCE_ADD:
 CMP BP,2
 JNZ BXDESWSISOURCE_SUB 
 ADD BX,CX
 JMP OUTT_BXDISSISOURCE
 BXDESWSISOURCE_SUB:
 CMP BP,3
 JNZ BXDESWSISOURCE_SBB
 SUB BX,CX
 JMP OUTT_BXDISSISOURCE
 BXDESWSISOURCE_SBB:
 CMP BP,4
 JNZ BXDESWSISOURCE_ADC
 SBB BX,CX
 JMP OUTT_BXDISSISOURCE
 BXDESWSISOURCE_ADC:
 CMP BP,5
 JNZ BXDESWSISOURCE_AND
 ADC BX,CX
 JMP OUTT_BXDISSISOURCE
 BXDESWSISOURCE_AND:
 CMP BP,6
 JNZ BXDESWSISOURCE_XOR
 AND BX,CX
 JMP OUTT_BXDISSISOURCE
 BXDESWSISOURCE_XOR:
 XOR BX,CX
 
 OUTT_BXDISSISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,BX
 JMP NAG7
NTABXDESWDXSOURCE_PLUS:
 MOV BX,RBX1
 MOV CX,RDX1
 MOV AX,FLAGS
 PUSH AX
 POPF
  BXDESWDXSOURCE_MOV:
 CMP BP,1
 JNZ BXDESWDXSOURCE_ADD
 MOV BX,CX
 JMP OUTT_BXDISDXSOURCE
 BXDESWDXSOURCE_ADD:
 CMP BP,2
 JNZ BXDESWDXSOURCE_SUB 
 ADD BX,CX
 JMP OUTT_BXDISDXSOURCE
 BXDESWDXSOURCE_SUB:
 CMP BP,3
 JNZ BXDESWDXSOURCE_SBB
 SUB BX,CX
 JMP OUTT_BXDISDXSOURCE
 BXDESWDXSOURCE_SBB:
 CMP BP,4
 JNZ BXDESWDXSOURCE_ADC
 SBB BX,CX
 JMP OUTT_BXDISDXSOURCE
 BXDESWDXSOURCE_ADC:
 CMP BP,5
 JNZ BXDESWDXSOURCE_AND
 ADC BX,CX
 JMP OUTT_BXDISDXSOURCE
 BXDESWDXSOURCE_AND:
 CMP BP,6
 JNZ BXDESWDXSOURCE_XOR
 AND BX,CX
 JMP OUTT_BXDISDXSOURCE
 BXDESWDXSOURCE_XOR:
 XOR BX,CX
 
 OUTT_BXDISDXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,BX
 JMP NAG7
NTABXDESWDIORALLWALERROR_PLUS: 
MOV AL,[SI+7]
CMP AL,100 
JZ NTABXDESWDISOURCE_PLUS
JMP ERROR
NTABXDESWDISOURCE_PLUS:
 MOV BX,RBX1
 MOV CX,IDI1
 MOV AX,FLAGS
 PUSH AX
 POPF
 BXDESWDISOURCE_MOV:
 CMP BP,1
 JNZ BXDESWDISOURCE_ADD
 MOV BX,CX
 JMP OUTT_BXDISDISOURCE
 BXDESWDISOURCE_ADD:
 CMP BP,2
 JNZ BXDESWDISOURCE_SUB 
 ADD BX,CX
 JMP OUTT_BXDISDISOURCE
 BXDESWDISOURCE_SUB:
 CMP BP,3
 JNZ BXDESWDISOURCE_SBB
 SUB BX,CX
 JMP OUTT_BXDISDISOURCE
 BXDESWDISOURCE_SBB:
 CMP BP,4
 JNZ BXDESWDISOURCE_ADC
 SBB BX,CX
 JMP OUTT_BXDISDISOURCE
 BXDESWDISOURCE_ADC:
 CMP BP,5
 JNZ BXDESWDISOURCE_AND
 ADC BX,CX
 JMP OUTT_BXDISDISOURCE
 BXDESWDISOURCE_AND:
 CMP BP,6
 JNZ BXDESWDISOURCE_XOR
 AND BX,CX
 JMP OUTT_BXDISDISOURCE
 BXDESWDISOURCE_XOR:
 XOR BX,CX
 
 OUTT_BXDISDISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,BX
 JMP NAG7
BXDES_WNTA_MEMBX_PLUS:
MOV BX,RBX1
CMP BX,0FH
JA ERROR
MOV CL, MEMORY1[BX]
MOV CH, MEMORY1[BX+1]
MOV BX,RBX1
MOV AX,FLAGS
PUSH AX
POPF
 BXDESWMEMBXSOURCE_MOV:
 CMP BP,1
 JNZ BXDESWMEMBXSOURCE_ADD
 MOV BX,CX
 JMP OUTT_BXDISMEMBXSOURCE
 BXDESWMEMBXSOURCE_ADD:
 CMP BP,2
 JNZ BXDESWMEMBXSOURCE_SUB 
 ADD BX,CX
 JMP OUTT_BXDISMEMBXSOURCE
 BXDESWMEMBXSOURCE_SUB:
 CMP BP,3
 JNZ BXDESWMEMBXSOURCE_SBB
 SUB BX,CX
 JMP OUTT_BXDISMEMBXSOURCE
 BXDESWMEMBXSOURCE_SBB:
 CMP BP,4
 JNZ BXDESWMEMBXSOURCE_ADC
 SBB BX,CX
 JMP OUTT_BXDISMEMBXSOURCE
 BXDESWMEMBXSOURCE_ADC:
 CMP BP,5
 JNZ BXDESWMEMBXSOURCE_AND
 ADC BX,CX
 JMP OUTT_BXDISMEMBXSOURCE
 BXDESWMEMBXSOURCE_AND:
 CMP BP,6
 JNZ BXDESWMEMBXSOURCE_XOR
 AND BX,CX
 JMP OUTT_BXDISMEMBXSOURCE
 BXDESWMEMBXSOURCE_XOR:
 XOR BX,CX
 
 OUTT_BXDISMEMBXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,BX
 JMP NAG7
NTA_BXDES_WMEMSI_PLUS:
MOV BX,ISI1
CMP BX,0FH
JA ERROR
MOV CH, MEMORY1[BX+1]
MOV CL, MEMORY1[BX]
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX
POPF
 BXDESWMEMSISOURCE_MOV:
 CMP BP,1
 JNZ BXDESWMEMSISOURCE_ADD
 MOV BX,CX
 JMP OUTT_BXDISMEMSISOURCE
 BXDESWMEMSISOURCE_ADD:
 CMP BP,2
 JNZ BXDESWMEMSISOURCE_SUB 
 ADD BX,CX
 JMP OUTT_BXDISMEMSISOURCE
 BXDESWMEMSISOURCE_SUB:
 CMP BP,3
 JNZ BXDESWMEMSISOURCE_SBB
 SUB BX,CX
 JMP OUTT_BXDISMEMSISOURCE
 BXDESWMEMSISOURCE_SBB:
 CMP BP,4
 JNZ BXDESWMEMSISOURCE_ADC
 SBB BX,CX
 JMP OUTT_BXDISMEMSISOURCE
 BXDESWMEMSISOURCE_ADC:
 CMP BP,5
 JNZ BXDESWMEMSISOURCE_AND
 ADC BX,CX
 JMP OUTT_BXDISMEMSISOURCE
 BXDESWMEMSISOURCE_AND:
 CMP BP,6
 JNZ BXDESWMEMSISOURCE_XOR
 AND BX,CX
 JMP OUTT_BXDISMEMSISOURCE
 BXDESWMEMSISOURCE_XOR:
 XOR BX,CX
 
 OUTT_BXDISMEMSISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,BX
 JMP NAG7
NTA_BXDES_WMEMDI_PLUS:
MOV BX,IDI1
CMP BX,0FH
JA ERROR
MOV CH, MEMORY1[BX+1]
MOV CL, MEMORY1[BX]
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX
POPF
 BXDESWMEMDISOURCE_MOV:
 CMP BP,1
 JNZ BXDESWMEMDISOURCE_ADD
 MOV BX,CX
 JMP OUTT_BXDISMEMDISOURCE
 BXDESWMEMDISOURCE_ADD:
 CMP BP,2
 JNZ BXDESWMEMDISOURCE_SUB 
 ADD BX,CX
 JMP OUTT_BXDISMEMDISOURCE
 BXDESWMEMDISOURCE_SUB:
 CMP BP,3
 JNZ BXDESWMEMDISOURCE_SBB
 SUB BX,CX
 JMP OUTT_BXDISMEMDISOURCE
 BXDESWMEMDISOURCE_SBB:
 CMP BP,4
 JNZ BXDESWMEMDISOURCE_ADC
 SBB BX,CX
 JMP OUTT_BXDISMEMDISOURCE
 BXDESWMEMDISOURCE_ADC:
 CMP BP,5
 JNZ BXDESWMEMDISOURCE_AND
 ADC BX,CX
 JMP OUTT_BXDISMEMDISOURCE
 BXDESWMEMDISOURCE_AND:
 CMP BP,6
 JNZ BXDESWMEMDISOURCE_XOR
 AND BX,CX
 JMP OUTT_BXDISMEMDISOURCE
 BXDESWMEMDISOURCE_XOR:
 XOR BX,CX
 
 OUTT_BXDISMEMDISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,BX
 JMP NAG7
NTABXDESW_RKM_YA_MEMRKM_YAERROR_PLUS:
MOV AL,[SI+7]
CMP AL,91
JZ MEMRKM_ORERROR_BXDES_PLUS

RKM_ORERROR_BXDES_PLUS:
MOV AL,[SI+8]
CMP AL,108
JZ ERROR
CMP AL,104
JZ ERROR
MOV VAR,BP
RET_NUM_COUNT  COMp+7
MOV BP,VAR
CMP CX,4
JA ERROR
MOV BX,RBX1
MOV AX,FLAGS
PUSH AX
POPF 
 BXDESWRAKAMSOURCE_MOV:
 CMP BP,1
 JNZ BXDESWRAKAMSOURCE_ADD
 MOV BX,DX
 JMP OUTT_BXDISRAKAMSOURCE
 BXDESWRAKAMSOURCE_ADD:
 CMP BP,2
 JNZ BXDESWRAKAMSOURCE_SUB 
 ADD BX,DX
 JMP OUTT_BXDISRAKAMSOURCE
 BXDESWRAKAMSOURCE_SUB:
 CMP BP,3
 JNZ BXDESWRAKAMSOURCE_SBB
 SUB BX,DX
 JMP OUTT_BXDISRAKAMSOURCE
 BXDESWRAKAMSOURCE_SBB:
 CMP BP,4
 JNZ BXDESWRAKAMSOURCE_ADC
 SBB BX,DX
 JMP OUTT_BXDISRAKAMSOURCE
 BXDESWRAKAMSOURCE_ADC:
 CMP BP,5
 JNZ BXDESWRAKAMSOURCE_AND
 ADC BX,DX
 JMP OUTT_BXDISRAKAMSOURCE
 BXDESWRAKAMSOURCE_AND:
 CMP BP,6
 JNZ BXDESWRAKAMSOURCE_XOR
 AND BX,DX
 JMP OUTT_BXDISRAKAMSOURCE
 BXDESWRAKAMSOURCE_XOR:
 XOR BX,DX
 
 OUTT_BXDISRAKAMSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,BX
 JMP NAG7
 
MEMRKM_ORERROR_BXDES_PLUS:
MOV AL,[SI+9]
CMP AL,108
JZ ERROR
CMP AL,104
JZ ERROR
MOV VAR,BP
RET_NUM_COUNT  COMp+8
MOV BP,VAR
CMP CX,4
JA ERROR
CMP DX,0FH
JA ERROR
MOV BX,DX
MOV CH, MEMORY1[BX+1]
MOV CL, MEMORY1[BX]
MOV BX,RBX1
MOV AX,FLAGS
PUSH AX
POPF 
 BXDESWMEMRAKAMSOURCE_MOV:
 CMP BP,1
 JNZ BXDESWMEMRAKAMSOURCE_ADD
 MOV BX,CX
 JMP OUTT_BXDISMEMRAKAMSOURCE
 BXDESWMEMRAKAMSOURCE_ADD:
 CMP BP,2
 JNZ BXDESWMEMRAKAMSOURCE_SUB 
 ADD BX,CX
 JMP OUTT_BXDISMEMRAKAMSOURCE
 BXDESWMEMRAKAMSOURCE_SUB:
 CMP BP,3
 JNZ BXDESWMEMRAKAMSOURCE_SBB
 SUB BX,CX
 JMP OUTT_BXDISMEMRAKAMSOURCE
 BXDESWMEMRAKAMSOURCE_SBB:
 CMP BP,4
 JNZ BXDESWMEMRAKAMSOURCE_ADC
 SBB BX,CX
 JMP OUTT_BXDISMEMRAKAMSOURCE
 BXDESWMEMRAKAMSOURCE_ADC:
 CMP BP,5
 JNZ BXDESWMEMRAKAMSOURCE_AND
 ADC BX,CX
 JMP OUTT_BXDISMEMRAKAMSOURCE
 BXDESWMEMRAKAMSOURCE_AND:
 CMP BP,6
 JNZ BXDESWMEMRAKAMSOURCE_XOR
 AND BX,CX
 JMP OUTT_BXDISMEMRAKAMSOURCE
 BXDESWMEMRAKAMSOURCE_XOR:
 XOR BX,CX
 
 OUTT_BXDISMEMRAKAMSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,BX
 JMP NAG7
;/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTACXDES_PLUS:
  MOV BX,0
  MOV BL,[SI+8]
  CMP BL,104
  JNAE NTACXDESW_RKM_YA_MEMRKM_YAERROR_PLUS
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+7] 
  MOV CL,[SI+8]
  ADD AX,CX
  CMP AX,217
  JZ NTACXDESWAXSOURCE_PLUS
  CMP AX,218 
  JZ NTACXDESWBXSOURCE_PLUS
  CMP AX,219
  JZ NTACXDESWCXSOURCE_PLUS
  CMP AX,220
 JZ NTACXDESWDXORSISOURCE_PLUS
 CMP AX,205
 JZ NTACXDESWDIORALLWALERROR_PLUS
 MOV CX,0
 MOV BX,0                            ;TAKECARE
 MOV BL,[SI+9]
 MOV CL,[SI+10]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ CXDES_WNTA_MEMBX_PLUS
 CMP AX,404              
 JZ NTA_CXDES_WMEMSI_PLUS
 CMP AX,389
 JZ NTA_CXDES_WMEMDI_PLUS
 JMP NTACXDESW_RKM_YA_MEMRKM_YAERROR_PLUS 
 
 NTACXDESWAXSOURCE_PLUS:  
 MOV BX,RCX1
 MOV CX,RAX1
 MOV AX,FLAGS
 PUSH AX
 POPF
 CXDESWAXSOURCE_MOV:
 CMP BP,1
 JNZ CXDESWAXSOURCE_ADD
 MOV BX,CX
 JMP OUTT_CXDISAXSOURCE
 CXDESWAXSOURCE_ADD:
 CMP BP,2
 JNZ CXDESWAXSOURCE_SUB 
 ADD BX,CX
 JMP OUTT_CXDISAXSOURCE
 CXDESWAXSOURCE_SUB:
 CMP BP,3
 JNZ CXDESWAXSOURCE_SBB
 SUB BX,CX
 JMP OUTT_CXDISAXSOURCE
 CXDESWAXSOURCE_SBB:
 CMP BP,4
 JNZ CXDESWAXSOURCE_ADC
 SBB BX,CX
 JMP OUTT_CXDISAXSOURCE
 CXDESWAXSOURCE_ADC:
 CMP BP,5
 JNZ CXDESWAXSOURCE_AND
 ADC BX,CX
 JMP OUTT_CXDISAXSOURCE
 CXDESWAXSOURCE_AND:
 CMP BP,6
 JNZ CXDESWAXSOURCE_XOR
 AND BX,CX
 JMP OUTT_CXDISAXSOURCE
 CXDESWAXSOURCE_XOR:
 XOR BX,CX
 
 OUTT_CXDISAXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,BX
 JMP NAG7
 
 NTACXDESWBXSOURCE_PLUS: 
 MOV BX,RCX1
 MOV CX,RBX1
 MOV AX,FLAGS
 PUSH AX
 POPF
 CXDESWBXSOURCE_MOV:
 CMP BP,1
 JNZ CXDESWBXSOURCE_ADD
 MOV BX,CX
 JMP OUTT_CXDISAXSOURCE
 CXDESWBXSOURCE_ADD:
 CMP BP,2
 JNZ CXDESWBXSOURCE_SUB 
 ADD BX,CX
 JMP OUTT_CXDISBXSOURCE
 CXDESWBXSOURCE_SUB:
 CMP BP,3
 JNZ CXDESWBXSOURCE_SBB
 SUB BX,CX
 JMP OUTT_CXDISBXSOURCE
 CXDESWBXSOURCE_SBB:
 CMP BP,4
 JNZ CXDESWBXSOURCE_ADC
 SBB BX,CX
 JMP OUTT_CXDISBXSOURCE
 CXDESWBXSOURCE_ADC:
 CMP BP,5
 JNZ CXDESWBXSOURCE_AND
 ADC BX,CX
 JMP OUTT_CXDISBXSOURCE
 CXDESWBXSOURCE_AND:
 CMP BP,6
 JNZ CXDESWBXSOURCE_XOR
 AND BX,CX
 JMP OUTT_CXDISBXSOURCE
 CXDESWBXSOURCE_XOR:
 XOR BX,CX
 
 OUTT_CXDISBXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,BX
 JMP NAG7
 
 NTACXDESWCXSOURCE_PLUS:
 MOV BX,RCX1
 MOV AX,FLAGS
 PUSH AX
 POPF
 CXDESWCXSOURCE_MOV:
 CMP BP,1
 JNZ CXDESWCXSOURCE_ADD
 MOV BX,BX
 JMP OUTT_CXDISCXSOURCE
 CXDESWCXSOURCE_ADD:
 CMP BP,2
 JNZ CXDESWCXSOURCE_SUB 
 ADD BX,BX
 JMP OUTT_CXDISCXSOURCE
 CXDESWCXSOURCE_SUB:
 CMP BP,3
 JNZ CXDESWCXSOURCE_SBB
 SUB BX,BX
 JMP OUTT_CXDISCXSOURCE
 CXDESWCXSOURCE_SBB:
 CMP BP,4
 JNZ CXDESWCXSOURCE_ADC
 SBB BX,BX
 JMP OUTT_CXDISCXSOURCE
 CXDESWCXSOURCE_ADC:
 CMP BP,5
 JNZ CXDESWCXSOURCE_AND
 ADC BX,BX
 JMP OUTT_CXDISCXSOURCE
 CXDESWCXSOURCE_AND:
 CMP BP,6
 JNZ CXDESWCXSOURCE_XOR
 AND BX,BX
 JMP OUTT_CXDISCXSOURCE
 CXDESWCXSOURCE_XOR:
 XOR BX,BX
 
 OUTT_CXDISCXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,BX
 JMP NAG7
 NTACXDESWDXORSISOURCE_PLUS:
 MOV AL,[SI+7]
 CMP AL,100
 JZ NTACXDESWDXSOURCE_PLUS
  
NTACXDESWSISOURCE_PLUS:
 MOV BX,RCX1
 MOV CX,ISI1
 MOV AX,FLAGS
 PUSH AX
 POPF
 CXDESWSISOURCE_MOV:
 CMP BP,1
 JNZ CXDESWSISOURCE_ADD
 MOV BX,CX
 JMP OUTT_CXDISSISOURCE
 CXDESWSISOURCE_ADD:
 CMP BP,2
 JNZ CXDESWSISOURCE_SUB 
 ADD BX,CX
 JMP OUTT_CXDISSISOURCE
 CXDESWSISOURCE_SUB:
 CMP BP,3
 JNZ CXDESWSISOURCE_SBB
 SUB BX,CX
 JMP OUTT_CXDISSISOURCE
 CXDESWSISOURCE_SBB:
 CMP BP,4
 JNZ CXDESWSISOURCE_ADC
 SBB BX,CX
 JMP OUTT_CXDISSISOURCE
 CXDESWSISOURCE_ADC:
 CMP BP,5
 JNZ CXDESWSISOURCE_AND
 ADC BX,CX
 JMP OUTT_CXDISSISOURCE
 CXDESWSISOURCE_AND:
 CMP BP,6
 JNZ CXDESWSISOURCE_XOR
 AND BX,CX
 JMP OUTT_CXDISSISOURCE
 CXDESWSISOURCE_XOR:
 XOR BX,CX
 
 OUTT_CXDISSISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,BX
 JMP NAG7
NTACXDESWDXSOURCE_PLUS:
 MOV BX,RCX1
 MOV CX,RDX1
 MOV AX,FLAGS
 PUSH AX
 POPF
  CXDESWDXSOURCE_MOV:
 CMP BP,1
 JNZ CXDESWDXSOURCE_ADD
 MOV BX,CX
 JMP OUTT_CXDISDXSOURCE
 CXDESWDXSOURCE_ADD:
 CMP BP,2
 JNZ CXDESWDXSOURCE_SUB 
 ADD BX,CX
 JMP OUTT_CXDISDXSOURCE
 CXDESWDXSOURCE_SUB:
 CMP BP,3
 JNZ CXDESWDXSOURCE_SBB
 SUB BX,CX
 JMP OUTT_CXDISDXSOURCE
 CXDESWDXSOURCE_SBB:
 CMP BP,4
 JNZ CXDESWDXSOURCE_ADC
 SBB BX,CX
 JMP OUTT_CXDISDXSOURCE
 CXDESWDXSOURCE_ADC:
 CMP BP,5
 JNZ CXDESWDXSOURCE_AND
 ADC BX,CX
 JMP OUTT_CXDISDXSOURCE
 CXDESWDXSOURCE_AND:
 CMP BP,6
 JNZ CXDESWDXSOURCE_XOR
 AND BX,CX
 JMP OUTT_CXDISDXSOURCE
 CXDESWDXSOURCE_XOR:
 XOR BX,CX
 
 OUTT_CXDISDXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,BX
 JMP NAG7
NTACXDESWDIORALLWALERROR_PLUS: 
MOV AL,[SI+7]
CMP AL,100 
JZ NTACXDESWDISOURCE_PLUS
JMP ERROR
NTACXDESWDISOURCE_PLUS:
 MOV BX,RCX1
 MOV CX,IDI1
 MOV AX,FLAGS
 PUSH AX
 POPF
 CXDESWDISOURCE_MOV:
 CMP BP,1
 JNZ CXDESWDISOURCE_ADD
 MOV BX,CX
 JMP OUTT_CXDISDISOURCE
 CXDESWDISOURCE_ADD:
 CMP BP,2
 JNZ CXDESWDISOURCE_SUB 
 ADD BX,CX
 JMP OUTT_CXDISDISOURCE
 CXDESWDISOURCE_SUB:
 CMP BP,3
 JNZ CXDESWDISOURCE_SBB
 SUB BX,CX
 JMP OUTT_CXDISDISOURCE
 CXDESWDISOURCE_SBB:
 CMP BP,4
 JNZ CXDESWDISOURCE_ADC
 SBB BX,CX
 JMP OUTT_CXDISDISOURCE
 CXDESWDISOURCE_ADC:
 CMP BP,5
 JNZ CXDESWDISOURCE_AND
 ADC BX,CX
 JMP OUTT_CXDISDISOURCE
 CXDESWDISOURCE_AND:
 CMP BP,6
 JNZ CXDESWDISOURCE_XOR
 AND BX,CX
 JMP OUTT_CXDISDISOURCE
 CXDESWDISOURCE_XOR:
 XOR BX,CX
 
 OUTT_CXDISDISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,BX
 JMP NAG7
CXDES_WNTA_MEMBX_PLUS:
MOV BX,RBX1
CMP BX,0FH
JA ERROR
MOV CL, MEMORY1[BX]
MOV CH, MEMORY1[BX+1]
MOV BX,RCX1
MOV AX,FLAGS
PUSH AX
POPF
 CXDESWMEMBXSOURCE_MOV:
 CMP BP,1
 JNZ CXDESWMEMBXSOURCE_ADD
 MOV BX,CX
 JMP OUTT_CXDISMEMBXSOURCE
 CXDESWMEMBXSOURCE_ADD:
 CMP BP,2
 JNZ CXDESWMEMBXSOURCE_SUB 
 ADD BX,CX
 JMP OUTT_CXDISMEMBXSOURCE
 CXDESWMEMBXSOURCE_SUB:
 CMP BP,3
 JNZ CXDESWMEMBXSOURCE_SBB
 SUB BX,CX
 JMP OUTT_CXDISMEMBXSOURCE
 CXDESWMEMBXSOURCE_SBB:
 CMP BP,4
 JNZ CXDESWMEMBXSOURCE_ADC
 SBB BX,CX
 JMP OUTT_CXDISMEMBXSOURCE
 CXDESWMEMBXSOURCE_ADC:
 CMP BP,5
 JNZ CXDESWMEMBXSOURCE_AND
 ADC BX,CX
 JMP OUTT_CXDISMEMBXSOURCE
 CXDESWMEMBXSOURCE_AND:
 CMP BP,6
 JNZ CXDESWMEMBXSOURCE_XOR
 AND BX,CX
 JMP OUTT_CXDISMEMBXSOURCE
 CXDESWMEMBXSOURCE_XOR:
 XOR BX,CX
 
 OUTT_CXDISMEMBXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,BX
 JMP NAG7
NTA_CXDES_WMEMSI_PLUS:
MOV BX,ISI1
CMP BX,0FH
JA ERROR
MOV CH, MEMORY1[BX+1]
MOV CL, MEMORY1[BX]
MOV BX,RCX1
MOV AX,FLAGS
PUSH AX
POPF
 CXDESWMEMSISOURCE_MOV:
 CMP BP,1
 JNZ CXDESWMEMSISOURCE_ADD
 MOV BX,CX
 JMP OUTT_CXDISMEMSISOURCE
 CXDESWMEMSISOURCE_ADD:
 CMP BP,2
 JNZ CXDESWMEMSISOURCE_SUB 
 ADD BX,CX
 JMP OUTT_CXDISMEMSISOURCE
 CXDESWMEMSISOURCE_SUB:
 CMP BP,3
 JNZ CXDESWMEMSISOURCE_SBB
 SUB BX,CX
 JMP OUTT_CXDISMEMSISOURCE
 CXDESWMEMSISOURCE_SBB:
 CMP BP,4
 JNZ CXDESWMEMSISOURCE_ADC
 SBB BX,CX
 JMP OUTT_CXDISMEMSISOURCE
 CXDESWMEMSISOURCE_ADC:
 CMP BP,5
 JNZ CXDESWMEMSISOURCE_AND
 ADC BX,CX
 JMP OUTT_CXDISMEMSISOURCE
 CXDESWMEMSISOURCE_AND:
 CMP BP,6
 JNZ CXDESWMEMSISOURCE_XOR
 AND BX,CX
 JMP OUTT_CXDISMEMSISOURCE
 CXDESWMEMSISOURCE_XOR:
 XOR BX,CX
 
 OUTT_CXDISMEMSISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,BX
 JMP NAG7
NTA_CXDES_WMEMDI_PLUS:
MOV BX,IDI1
CMP BX,0FH
JA ERROR
MOV CH, MEMORY1[BX+1]
MOV CL, MEMORY1[BX]
MOV BX,RCX1
MOV AX,FLAGS
PUSH AX
POPF
 CXDESWMEMDISOURCE_MOV:
 CMP BP,1
 JNZ CXDESWMEMDISOURCE_ADD
 MOV BX,CX
 JMP OUTT_CXDISMEMDISOURCE
 CXDESWMEMDISOURCE_ADD:
 CMP BP,2
 JNZ CXDESWMEMDISOURCE_SUB 
 ADD BX,CX
 JMP OUTT_CXDISMEMDISOURCE
 CXDESWMEMDISOURCE_SUB:
 CMP BP,3
 JNZ CXDESWMEMDISOURCE_SBB
 SUB BX,CX
 JMP OUTT_CXDISMEMDISOURCE
 CXDESWMEMDISOURCE_SBB:
 CMP BP,4
 JNZ CXDESWMEMDISOURCE_ADC
 SBB BX,CX
 JMP OUTT_CXDISMEMDISOURCE
 CXDESWMEMDISOURCE_ADC:
 CMP BP,5
 JNZ CXDESWMEMDISOURCE_AND
 ADC BX,CX
 JMP OUTT_CXDISMEMDISOURCE
 CXDESWMEMDISOURCE_AND:
 CMP BP,6
 JNZ CXDESWMEMDISOURCE_XOR
 AND BX,CX
 JMP OUTT_CXDISMEMDISOURCE
 CXDESWMEMDISOURCE_XOR:
 XOR BX,CX
 
 OUTT_CXDISMEMDISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,BX
 JMP NAG7
NTACXDESW_RKM_YA_MEMRKM_YAERROR_PLUS:
MOV AL,[SI+7]
CMP AL,91
JZ MEMRKM_ORERROR_CXDES_PLUS

RKM_ORERROR_CXDES_PLUS:
MOV AL,[SI+8]
CMP AL,108
JZ ERROR
CMP AL,104
JZ ERROR
MOV VAR,BP
RET_NUM_COUNT  COMp+7
MOV BP,VAR
CMP CX,4
JA ERROR
MOV BX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
 CXDESWRAKAMSOURCE_MOV:
 CMP BP,1
 JNZ CXDESWRAKAMSOURCE_ADD
 MOV BX,DX
 JMP OUTT_CXDISRAKAMSOURCE
 CXDESWRAKAMSOURCE_ADD:
 CMP BP,2
 JNZ CXDESWRAKAMSOURCE_SUB 
 ADD BX,DX
 JMP OUTT_CXDISRAKAMSOURCE
 CXDESWRAKAMSOURCE_SUB:
 CMP BP,3
 JNZ CXDESWRAKAMSOURCE_SBB
 SUB BX,DX
 JMP OUTT_CXDISRAKAMSOURCE
 CXDESWRAKAMSOURCE_SBB:
 CMP BP,4
 JNZ CXDESWRAKAMSOURCE_ADC
 SBB BX,DX
 JMP OUTT_CXDISRAKAMSOURCE
 CXDESWRAKAMSOURCE_ADC:
 CMP BP,5
 JNZ CXDESWRAKAMSOURCE_AND
 ADC BX,DX
 JMP OUTT_CXDISRAKAMSOURCE
 CXDESWRAKAMSOURCE_AND:
 CMP BP,6
 JNZ CXDESWRAKAMSOURCE_XOR
 AND BX,DX
 JMP OUTT_CXDISRAKAMSOURCE
 CXDESWRAKAMSOURCE_XOR:
 XOR BX,DX
 
 OUTT_CXDISRAKAMSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,BX
 JMP NAG7
 
MEMRKM_ORERROR_CXDES_PLUS:
MOV AL,[SI+9]
CMP AL,108
JZ ERROR
CMP AL,104
JZ ERROR
MOV VAR,BP
RET_NUM_COUNT  COMp+8
MOV BP,VAR
CMP CX,4
JA ERROR
CMP DX,0FH
JA ERROR
MOV BX,DX
MOV CH, MEMORY1[BX+1]
MOV CL, MEMORY1[BX]
MOV BX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
 CXDESWMEMRAKAMSOURCE_MOV:
 CMP BP,1
 JNZ CXDESWMEMRAKAMSOURCE_ADD
 MOV BX,CX
 JMP OUTT_CXDISMEMRAKAMSOURCE
 CXDESWMEMRAKAMSOURCE_ADD:
 CMP BP,2
 JNZ CXDESWMEMRAKAMSOURCE_SUB 
 ADD BX,CX
 JMP OUTT_CXDISMEMRAKAMSOURCE
 CXDESWMEMRAKAMSOURCE_SUB:
 CMP BP,3
 JNZ CXDESWMEMRAKAMSOURCE_SBB
 SUB BX,CX
 JMP OUTT_CXDISMEMRAKAMSOURCE
 CXDESWMEMRAKAMSOURCE_SBB:
 CMP BP,4
 JNZ CXDESWMEMRAKAMSOURCE_ADC
 SBB BX,CX
 JMP OUTT_CXDISMEMRAKAMSOURCE
 CXDESWMEMRAKAMSOURCE_ADC:
 CMP BP,5
 JNZ CXDESWMEMRAKAMSOURCE_AND
 ADC BX,CX
 JMP OUTT_CXDISMEMRAKAMSOURCE
 CXDESWMEMRAKAMSOURCE_AND:
 CMP BP,6
 JNZ CXDESWMEMRAKAMSOURCE_XOR
 AND BX,CX
 JMP OUTT_CXDISMEMRAKAMSOURCE
 CXDESWMEMRAKAMSOURCE_XOR:
 XOR BX,CX
 
 OUTT_CXDISMEMRAKAMSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,BX
 JMP NAG7
;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTADXORSIDES_PLUS:
MOV AL,[SI+4]
CMP AL,115
JZ NTASIDES_PLUS
NTADXDES_PLUS:
  MOV BX,0
  MOV BL,[SI+8]
  CMP BL,104
  JNAE NTADXDESW_RKM_YA_MEMRKM_YAERROR_PLUS
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+7] 
  MOV CL,[SI+8]
  ADD AX,CX
  CMP AX,217
  JZ NTADXDESWAXSOURCE_PLUS
  CMP AX,218 
  JZ NTADXDESWBXSOURCE_PLUS
  CMP AX,219
  JZ NTADXDESWCXSOURCE_PLUS
  CMP AX,220
 JZ NTADXDESWDXORSISOURCE_PLUS
 CMP AX,205
 JZ NTADXDESWDIORALLWALERROR_PLUS
 MOV CX,0
 MOV BX,0                            ;TAKECARE
 MOV BL,[SI+9]
 MOV CL,[SI+10]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ DXDES_WNTA_MEMBX_PLUS
 CMP AX,404              
 JZ NTA_DXDES_WMEMSI_PLUS
 CMP AX,389
 JZ NTA_DXDES_WMEMDI_PLUS
 JMP NTADXDESW_RKM_YA_MEMRKM_YAERROR_PLUS 
 
 NTADXDESWAXSOURCE_PLUS:  
 MOV BX,RDX1
 MOV CX,RAX1
 MOV AX,FLAGS
 PUSH AX
 POPF
 DXDESWAXSOURCE_MOV:
 CMP BP,1
 JNZ DXDESWAXSOURCE_ADD
 MOV BX,CX
 JMP OUTT_DXDISAXSOURCE
 DXDESWAXSOURCE_ADD:
 CMP BP,2
 JNZ DXDESWAXSOURCE_SUB 
 ADD BX,CX
 JMP OUTT_DXDISAXSOURCE
 DXDESWAXSOURCE_SUB:
 CMP BP,3
 JNZ DXDESWAXSOURCE_SBB
 SUB BX,CX
 JMP OUTT_DXDISAXSOURCE
 DXDESWAXSOURCE_SBB:
 CMP BP,4
 JNZ DXDESWAXSOURCE_ADC
 SBB BX,CX
 JMP OUTT_DXDISAXSOURCE
 DXDESWAXSOURCE_ADC:
 CMP BP,5
 JNZ DXDESWAXSOURCE_AND
 ADC BX,CX
 JMP OUTT_DXDISAXSOURCE
 DXDESWAXSOURCE_AND:
 CMP BP,6
 JNZ DXDESWAXSOURCE_XOR
 AND BX,CX
 JMP OUTT_DXDISAXSOURCE
 DXDESWAXSOURCE_XOR:
 XOR BX,CX
 
 OUTT_DXDISAXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,BX
 JMP NAG7
 
 NTADXDESWBXSOURCE_PLUS: 
 MOV BX,RDX1
 MOV CX,RBX1
 MOV AX,FLAGS
 PUSH AX
 POPF
 DXDESWBXSOURCE_MOV:
 CMP BP,1
 JNZ DXDESWBXSOURCE_ADD
 MOV BX,CX
 JMP OUTT_DXDISAXSOURCE
 DXDESWBXSOURCE_ADD:
 CMP BP,2
 JNZ DXDESWBXSOURCE_SUB 
 ADD BX,CX
 JMP OUTT_DXDISBXSOURCE
 DXDESWBXSOURCE_SUB:
 CMP BP,3
 JNZ DXDESWBXSOURCE_SBB
 SUB BX,CX
 JMP OUTT_DXDISBXSOURCE
 DXDESWBXSOURCE_SBB:
 CMP BP,4
 JNZ DXDESWBXSOURCE_ADC
 SBB BX,CX
 JMP OUTT_DXDISBXSOURCE
 DXDESWBXSOURCE_ADC:
 CMP BP,5
 JNZ DXDESWBXSOURCE_AND
 ADC BX,CX
 JMP OUTT_DXDISBXSOURCE
 DXDESWBXSOURCE_AND:
 CMP BP,6
 JNZ DXDESWBXSOURCE_XOR
 AND BX,CX
 JMP OUTT_DXDISBXSOURCE
 DXDESWBXSOURCE_XOR:
 XOR BX,CX
 
 OUTT_DXDISBXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,BX
 JMP NAG7
 
 NTADXDESWCXSOURCE_PLUS:
 MOV BX,RDX1
 MOV CX,RCX1
 MOV AX,FLAGS
 PUSH AX
 POPF
 DXDESWCXSOURCE_MOV:
 CMP BP,1
 JNZ DXDESWCXSOURCE_ADD
 MOV BX,CX
 JMP OUTT_DXDISCXSOURCE
 DXDESWCXSOURCE_ADD:
 CMP BP,2
 JNZ DXDESWCXSOURCE_SUB 
 ADD BX,CX
 JMP OUTT_DXDISCXSOURCE
 DXDESWCXSOURCE_SUB:
 CMP BP,3
 JNZ DXDESWCXSOURCE_SBB
 SUB BX,CX
 JMP OUTT_DXDISCXSOURCE
 DXDESWCXSOURCE_SBB:
 CMP BP,4
 JNZ DXDESWCXSOURCE_ADC
 SBB BX,CX
 JMP OUTT_DXDISCXSOURCE
 DXDESWCXSOURCE_ADC:
 CMP BP,5
 JNZ DXDESWCXSOURCE_AND
 ADC BX,CX
 JMP OUTT_DXDISCXSOURCE
 DXDESWCXSOURCE_AND:
 CMP BP,6
 JNZ DXDESWCXSOURCE_XOR
 AND BX,CX
 JMP OUTT_DXDISCXSOURCE
 DXDESWCXSOURCE_XOR:
 XOR BX,CX
 
 OUTT_DXDISCXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,BX
 JMP NAG7
 NTADXDESWDXORSISOURCE_PLUS:
 MOV AL,[SI+7]
 CMP AL,100
 JZ NTADXDESWDXSOURCE_PLUS
  
NTADXDESWSISOURCE_PLUS:
 MOV BX,RDX1
 MOV CX,ISI1
 MOV AX,FLAGS
 PUSH AX
 POPF
 DXDESWSISOURCE_MOV:
 CMP BP,1
 JNZ DXDESWSISOURCE_ADD
 MOV BX,CX
 JMP OUTT_DXDISSISOURCE
 DXDESWSISOURCE_ADD:
 CMP BP,2
 JNZ DXDESWSISOURCE_SUB 
 ADD BX,CX
 JMP OUTT_DXDISSISOURCE
 DXDESWSISOURCE_SUB:
 CMP BP,3
 JNZ DXDESWSISOURCE_SBB
 SUB BX,CX
 JMP OUTT_DXDISSISOURCE
 DXDESWSISOURCE_SBB:
 CMP BP,4
 JNZ DXDESWSISOURCE_ADC
 SBB BX,CX
 JMP OUTT_DXDISSISOURCE
 DXDESWSISOURCE_ADC:
 CMP BP,5
 JNZ DXDESWSISOURCE_AND
 ADC BX,CX
 JMP OUTT_DXDISSISOURCE
 DXDESWSISOURCE_AND:
 CMP BP,6
 JNZ DXDESWSISOURCE_XOR
 AND BX,CX
 JMP OUTT_DXDISSISOURCE
 DXDESWSISOURCE_XOR:
 XOR BX,CX
 
 OUTT_DXDISSISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,BX
 JMP NAG7
NTADXDESWDXSOURCE_PLUS:
 MOV BX,RDX1
 MOV AX,FLAGS
 PUSH AX
 POPF
  DXDESWDXSOURCE_MOV:
 CMP BP,1
 JNZ DXDESWDXSOURCE_ADD
 MOV BX,BX
 JMP OUTT_DXDISDXSOURCE
 DXDESWDXSOURCE_ADD:
 CMP BP,2
 JNZ DXDESWDXSOURCE_SUB 
 ADD BX,BX
 JMP OUTT_DXDISDXSOURCE
 DXDESWDXSOURCE_SUB:
 CMP BP,3
 JNZ DXDESWDXSOURCE_SBB
 SUB BX,BX
 JMP OUTT_DXDISDXSOURCE
 DXDESWDXSOURCE_SBB:
 CMP BP,4
 JNZ DXDESWDXSOURCE_ADC
 SBB BX,BX
 JMP OUTT_DXDISDXSOURCE
 DXDESWDXSOURCE_ADC:
 CMP BP,5
 JNZ DXDESWDXSOURCE_AND
 ADC BX,BX
 JMP OUTT_DXDISDXSOURCE
 DXDESWDXSOURCE_AND:
 CMP BP,6
 JNZ DXDESWDXSOURCE_XOR
 AND BX,BX
 JMP OUTT_DXDISDXSOURCE
 DXDESWDXSOURCE_XOR:
 XOR BX,BX
 
 OUTT_DXDISDXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,BX
 JMP NAG7
NTADXDESWDIORALLWALERROR_PLUS: 
MOV AL,[SI+7]
CMP AL,100 
JZ NTADXDESWDISOURCE_PLUS
JMP ERROR
NTADXDESWDISOURCE_PLUS:
 MOV BX,RDX1
 MOV CX,IDI1
 MOV AX,FLAGS
 PUSH AX
 POPF
 DXDESWDISOURCE_MOV:
 CMP BP,1
 JNZ DXDESWDISOURCE_ADD
 MOV BX,CX
 JMP OUTT_DXDISDISOURCE
 DXDESWDISOURCE_ADD:
 CMP BP,2
 JNZ DXDESWDISOURCE_SUB 
 ADD BX,CX
 JMP OUTT_DXDISDISOURCE
 DXDESWDISOURCE_SUB:
 CMP BP,3
 JNZ DXDESWDISOURCE_SBB
 SUB BX,CX
 JMP OUTT_DXDISDISOURCE
 DXDESWDISOURCE_SBB:
 CMP BP,4
 JNZ DXDESWDISOURCE_ADC
 SBB BX,CX
 JMP OUTT_DXDISDISOURCE
 DXDESWDISOURCE_ADC:
 CMP BP,5
 JNZ DXDESWDISOURCE_AND
 ADC BX,CX
 JMP OUTT_DXDISDISOURCE
 DXDESWDISOURCE_AND:
 CMP BP,6
 JNZ DXDESWDISOURCE_XOR
 AND BX,CX
 JMP OUTT_DXDISDISOURCE
 DXDESWDISOURCE_XOR:
 XOR BX,CX
 
 OUTT_DXDISDISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,BX
 JMP NAG7
DXDES_WNTA_MEMBX_PLUS:
MOV BX,RBX1
CMP BX,0FH
JA ERROR
MOV CL, MEMORY1[BX]
MOV CH, MEMORY1[BX+1]
MOV BX,RDX1
MOV AX,FLAGS
PUSH AX
POPF
 DXDESWMEMBXSOURCE_MOV:
 CMP BP,1
 JNZ DXDESWMEMBXSOURCE_ADD
 MOV BX,CX
 JMP OUTT_DXDISMEMBXSOURCE
 DXDESWMEMBXSOURCE_ADD:
 CMP BP,2
 JNZ DXDESWMEMBXSOURCE_SUB 
 ADD BX,CX
 JMP OUTT_DXDISMEMBXSOURCE
 DXDESWMEMBXSOURCE_SUB:
 CMP BP,3
 JNZ DXDESWMEMBXSOURCE_SBB
 SUB BX,CX
 JMP OUTT_DXDISMEMBXSOURCE
 DXDESWMEMBXSOURCE_SBB:
 CMP BP,4
 JNZ DXDESWMEMBXSOURCE_ADC
 SBB BX,CX
 JMP OUTT_DXDISMEMBXSOURCE
 DXDESWMEMBXSOURCE_ADC:
 CMP BP,5
 JNZ DXDESWMEMBXSOURCE_AND
 ADC BX,CX
 JMP OUTT_DXDISMEMBXSOURCE
 DXDESWMEMBXSOURCE_AND:
 CMP BP,6
 JNZ DXDESWMEMBXSOURCE_XOR
 AND BX,CX
 JMP OUTT_DXDISMEMBXSOURCE
 DXDESWMEMBXSOURCE_XOR:
 XOR BX,CX
 
 OUTT_DXDISMEMBXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,BX
 JMP NAG7
NTA_DXDES_WMEMSI_PLUS:
MOV BX,ISI1
CMP BX,0FH
JA ERROR
MOV CH, MEMORY1[BX+1]
MOV CL, MEMORY1[BX]
MOV BX,RDX1
MOV AX,FLAGS
PUSH AX
POPF
 DXDESWMEMSISOURCE_MOV:
 CMP BP,1
 JNZ DXDESWMEMSISOURCE_ADD
 MOV BX,CX
 JMP OUTT_DXDISMEMSISOURCE
 DXDESWMEMSISOURCE_ADD:
 CMP BP,2
 JNZ DXDESWMEMSISOURCE_SUB 
 ADD BX,CX
 JMP OUTT_DXDISMEMSISOURCE
 DXDESWMEMSISOURCE_SUB:
 CMP BP,3
 JNZ DXDESWMEMSISOURCE_SBB
 SUB BX,CX
 JMP OUTT_DXDISMEMSISOURCE
 DXDESWMEMSISOURCE_SBB:
 CMP BP,4
 JNZ DXDESWMEMSISOURCE_ADC
 SBB BX,CX
 JMP OUTT_DXDISMEMSISOURCE
 DXDESWMEMSISOURCE_ADC:
 CMP BP,5
 JNZ DXDESWMEMSISOURCE_AND
 ADC BX,CX
 JMP OUTT_DXDISMEMSISOURCE
 DXDESWMEMSISOURCE_AND:
 CMP BP,6
 JNZ DXDESWMEMSISOURCE_XOR
 AND BX,CX
 JMP OUTT_DXDISMEMSISOURCE
 DXDESWMEMSISOURCE_XOR:
 XOR BX,CX
 
 OUTT_DXDISMEMSISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,BX
 JMP NAG7
NTA_DXDES_WMEMDI_PLUS:
MOV BX,IDI1
CMP BX,0FH
JA ERROR
MOV CH, MEMORY1[BX+1]
MOV CL, MEMORY1[BX]
MOV BX,RDX1
MOV AX,FLAGS
PUSH AX
POPF
 DXDESWMEMDISOURCE_MOV:
 CMP BP,1
 JNZ DXDESWMEMDISOURCE_ADD
 MOV BX,CX
 JMP OUTT_DXDISMEMDISOURCE
 DXDESWMEMDISOURCE_ADD:
 CMP BP,2
 JNZ DXDESWMEMDISOURCE_SUB 
 ADD BX,CX
 JMP OUTT_DXDISMEMDISOURCE
 DXDESWMEMDISOURCE_SUB:
 CMP BP,3
 JNZ DXDESWMEMDISOURCE_SBB
 SUB BX,CX
 JMP OUTT_DXDISMEMDISOURCE
 DXDESWMEMDISOURCE_SBB:
 CMP BP,4
 JNZ DXDESWMEMDISOURCE_ADC
 SBB BX,CX
 JMP OUTT_DXDISMEMDISOURCE
 DXDESWMEMDISOURCE_ADC:
 CMP BP,5
 JNZ DXDESWMEMDISOURCE_AND
 ADC BX,CX
 JMP OUTT_DXDISMEMDISOURCE
 DXDESWMEMDISOURCE_AND:
 CMP BP,6
 JNZ DXDESWMEMDISOURCE_XOR
 AND BX,CX
 JMP OUTT_DXDISMEMDISOURCE
 DXDESWMEMDISOURCE_XOR:
 XOR BX,CX
 
 OUTT_DXDISMEMDISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,BX
 JMP NAG7
NTADXDESW_RKM_YA_MEMRKM_YAERROR_PLUS:
MOV AL,[SI+7]
CMP AL,91
JZ MEMRKM_ORERROR_DXDES_PLUS

RKM_ORERROR_DXDES_PLUS:
MOV AL,[SI+8]
CMP AL,108
JZ ERROR
CMP AL,104
JZ ERROR
MOV VAR,BP
RET_NUM_COUNT  COMp+7
MOV BP,VAR
CMP CX,4
JA ERROR
MOV BX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
 DXDESWRAKAMSOURCE_MOV:
 CMP BP,1
 JNZ DXDESWRAKAMSOURCE_ADD
 MOV BX,DX
 JMP OUTT_DXDISRAKAMSOURCE
 DXDESWRAKAMSOURCE_ADD:
 CMP BP,2
 JNZ DXDESWRAKAMSOURCE_SUB 
 ADD BX,DX
 JMP OUTT_DXDISRAKAMSOURCE
 DXDESWRAKAMSOURCE_SUB:
 CMP BP,3
 JNZ DXDESWRAKAMSOURCE_SBB
 SUB BX,DX
 JMP OUTT_DXDISRAKAMSOURCE
 DXDESWRAKAMSOURCE_SBB:
 CMP BP,4
 JNZ DXDESWRAKAMSOURCE_ADC
 SBB BX,DX
 JMP OUTT_DXDISRAKAMSOURCE
 DXDESWRAKAMSOURCE_ADC:
 CMP BP,5
 JNZ DXDESWRAKAMSOURCE_AND
 ADC BX,DX
 JMP OUTT_DXDISRAKAMSOURCE
 DXDESWRAKAMSOURCE_AND:
 CMP BP,6
 JNZ DXDESWRAKAMSOURCE_XOR
 AND BX,DX
 JMP OUTT_DXDISRAKAMSOURCE
 DXDESWRAKAMSOURCE_XOR:
 XOR BX,DX
 
 OUTT_DXDISRAKAMSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,BX
 JMP NAG7
 
MEMRKM_ORERROR_DXDES_PLUS:
MOV AL,[SI+9]
CMP AL,108
JZ ERROR
CMP AL,104
JZ ERROR
MOV VAR,BP
RET_NUM_COUNT  COMp+8
MOV BP,VAR
CMP CX,4
JA ERROR
CMP DX,0FH
JA ERROR
MOV BX,DX
MOV CH, MEMORY1[BX+1]
MOV CL, MEMORY1[BX]
MOV BX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
 DXDESWMEMRAKAMSOURCE_MOV:
 CMP BP,1
 JNZ DXDESWMEMRAKAMSOURCE_ADD
 MOV BX,CX
 JMP OUTT_DXDISMEMRAKAMSOURCE
 DXDESWMEMRAKAMSOURCE_ADD:
 CMP BP,2
 JNZ DXDESWMEMRAKAMSOURCE_SUB 
 ADD BX,CX
 JMP OUTT_DXDISMEMRAKAMSOURCE
 DXDESWMEMRAKAMSOURCE_SUB:
 CMP BP,3
 JNZ DXDESWMEMRAKAMSOURCE_SBB
 SUB BX,CX
 JMP OUTT_DXDISMEMRAKAMSOURCE
 DXDESWMEMRAKAMSOURCE_SBB:
 CMP BP,4
 JNZ DXDESWMEMRAKAMSOURCE_ADC
 SBB BX,CX
 JMP OUTT_DXDISMEMRAKAMSOURCE
 DXDESWMEMRAKAMSOURCE_ADC:
 CMP BP,5
 JNZ DXDESWMEMRAKAMSOURCE_AND
 ADC BX,CX
 JMP OUTT_DXDISMEMRAKAMSOURCE
 DXDESWMEMRAKAMSOURCE_AND:
 CMP BP,6
 JNZ DXDESWMEMRAKAMSOURCE_XOR
 AND BX,CX
 JMP OUTT_DXDISMEMRAKAMSOURCE
 DXDESWMEMRAKAMSOURCE_XOR:
 XOR BX,CX
 
 OUTT_DXDISMEMRAKAMSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,BX
 JMP NAG7
;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTASIDES_PLUS:
  MOV BX,0
  MOV BL,[SI+8]
  CMP BL,104
  JNAE NTASIDESW_RKM_YA_MEMRKM_YAERROR_PLUS
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+7] 
  MOV CL,[SI+8]
  ADD AX,CX
  CMP AX,217
  JZ NTASIDESWAXSOURCE_PLUS
  CMP AX,218 
  JZ NTASIDESWBXSOURCE_PLUS
  CMP AX,219
  JZ NTASIDESWCXSOURCE_PLUS
  CMP AX,220
 JZ NTASIDESWDXORSISOURCE_PLUS
 CMP AX,205
 JZ NTASIDESWDIORALLWALERROR_PLUS
 MOV CX,0
 MOV BX,0                            ;TAKECARE
 MOV BL,[SI+9]
 MOV CL,[SI+10]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ SIDES_WNTA_MEMBX_PLUS
 CMP AX,404              
 JZ NTA_SIDES_WMEMSI_PLUS
 CMP AX,389
 JZ NTA_SIDES_WMEMDI_PLUS
 JMP NTASIDESW_RKM_YA_MEMRKM_YAERROR_PLUS 
 
 NTASIDESWAXSOURCE_PLUS:  
 MOV BX,ISI1
 MOV CX,RAX1
 MOV AX,FLAGS
 PUSH AX
 POPF
 SIDESWAXSOURCE_MOV:
 CMP BP,1
 JNZ SIDESWAXSOURCE_ADD
 MOV BX,CX
 JMP OUTT_SIDISAXSOURCE
 SIDESWAXSOURCE_ADD:
 CMP BP,2
 JNZ SIDESWAXSOURCE_SUB 
 ADD BX,CX
 JMP OUTT_SIDISAXSOURCE
 SIDESWAXSOURCE_SUB:
 CMP BP,3
 JNZ SIDESWAXSOURCE_SBB
 SUB BX,CX
 JMP OUTT_SIDISAXSOURCE
 SIDESWAXSOURCE_SBB:
 CMP BP,4
 JNZ SIDESWAXSOURCE_ADC
 SBB BX,CX
 JMP OUTT_SIDISAXSOURCE
 SIDESWAXSOURCE_ADC:
 CMP BP,5
 JNZ SIDESWAXSOURCE_AND
 ADC BX,CX
 JMP OUTT_SIDISAXSOURCE
 SIDESWAXSOURCE_AND:
 CMP BP,6
 JNZ SIDESWAXSOURCE_XOR
 AND BX,CX
 JMP OUTT_SIDISAXSOURCE
 SIDESWAXSOURCE_XOR:
 XOR BX,CX
 
 OUTT_SIDISAXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV ISI1,BX
 JMP NAG7
 
 NTASIDESWBXSOURCE_PLUS: 
 MOV BX,ISI1
 MOV CX,RBX1
 MOV AX,FLAGS
 PUSH AX
 POPF
 SIDESWBXSOURCE_MOV:
 CMP BP,1
 JNZ SIDESWBXSOURCE_ADD
 MOV BX,CX
 JMP OUTT_SIDISBXSOURCE
 SIDESWBXSOURCE_ADD:
 CMP BP,2
 JNZ SIDESWBXSOURCE_SUB 
 ADD BX,CX
 JMP OUTT_SIDISBXSOURCE
 SIDESWBXSOURCE_SUB:
 CMP BP,3
 JNZ SIDESWBXSOURCE_SBB
 SUB BX,CX
 JMP OUTT_SIDISBXSOURCE
 SIDESWBXSOURCE_SBB:
 CMP BP,4
 JNZ SIDESWBXSOURCE_ADC
 SBB BX,CX
 JMP OUTT_SIDISBXSOURCE
 SIDESWBXSOURCE_ADC:
 CMP BP,5
 JNZ SIDESWBXSOURCE_AND
 ADC BX,CX
 JMP OUTT_SIDISBXSOURCE
 SIDESWBXSOURCE_AND:
 CMP BP,6
 JNZ SIDESWBXSOURCE_XOR
 AND BX,CX
 JMP OUTT_SIDISBXSOURCE
 SIDESWBXSOURCE_XOR:
 XOR BX,CX
 
 OUTT_SIDISBXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV ISI1,BX
 JMP NAG7
 
 NTASIDESWCXSOURCE_PLUS:
 MOV BX,ISI1
 MOV CX,RCX1
 MOV AX,FLAGS
 PUSH AX
 POPF
SIDESWCXSOURCE_MOV:
 CMP BP,1
 JNZ SIDESWCXSOURCE_ADD
 MOV BX,CX
 JMP OUTT_SIDISCXSOURCE
 SIDESWCXSOURCE_ADD:
 CMP BP,2
 JNZ SIDESWCXSOURCE_SUB 
 ADD BX,CX
 JMP OUTT_SIDISCXSOURCE
 SIDESWCXSOURCE_SUB:
 CMP BP,3
 JNZ SIDESWCXSOURCE_SBB
 SUB BX,CX
 JMP OUTT_SIDISCXSOURCE
 SIDESWCXSOURCE_SBB:
 CMP BP,4
 JNZ SIDESWCXSOURCE_ADC
 SBB BX,CX
 JMP OUTT_SIDISCXSOURCE
 SIDESWCXSOURCE_ADC:
 CMP BP,5
 JNZ SIDESWCXSOURCE_AND
 ADC BX,CX
 JMP OUTT_SIDISCXSOURCE
 SIDESWCXSOURCE_AND:
 CMP BP,6
 JNZ SIDESWCXSOURCE_XOR
 AND BX,CX
 JMP OUTT_SIDISCXSOURCE
 SIDESWCXSOURCE_XOR:
 XOR BX,CX

 OUTT_SIDISCXSOURCE:
 
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV ISI1,BX
 JMP NAG7
 NTASIDESWDXORSISOURCE_PLUS:
 MOV AL,[SI+7]
 CMP AL,100
 JZ NTASIDESWDXSOURCE_PLUS
  
NTASIDESWSISOURCE_PLUS:
 MOV BX,ISI1
 MOV AX,FLAGS
 PUSH AX
 POPF
 SIDESWSISOURCE_MOV:
 CMP BP,1
 JNZ SIDESWSISOURCE_ADD
 MOV BX,BX
 JMP OUTT_SIDISSISOURCE
 SIDESWSISOURCE_ADD:
 CMP BP,2
 JNZ SIDESWSISOURCE_SUB 
 ADD BX,BX
 JMP OUTT_SIDISSISOURCE
 SIDESWSISOURCE_SUB:
 CMP BP,3
 JNZ SIDESWSISOURCE_SBB
 SUB BX,BX
 JMP OUTT_SIDISSISOURCE
 SIDESWSISOURCE_SBB:
 CMP BP,4
 JNZ SIDESWSISOURCE_ADC
 SBB BX,BX
 JMP OUTT_SIDISSISOURCE
 SIDESWSISOURCE_ADC:
 CMP BP,5
 JNZ SIDESWSISOURCE_AND
 ADC BX,BX
 JMP OUTT_SIDISSISOURCE
 SIDESWSISOURCE_AND:
 CMP BP,6
 JNZ SIDESWSISOURCE_XOR
 AND BX,BX
 JMP OUTT_SIDISSISOURCE
 SIDESWSISOURCE_XOR:
 XOR BX,BX
 
 OUTT_SIDISSISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV ISI1,BX
 JMP NAG7
NTASIDESWDXSOURCE_PLUS:
 MOV BX,ISI1
 MOV CX,RDX1
 MOV AX,FLAGS
 PUSH AX
 POPF
  SIDESWDXSOURCE_MOV:
 CMP BP,1
 JNZ SIDESWDXSOURCE_ADD
 MOV BX,CX
 JMP OUTT_SIDISDXSOURCE
 SIDESWDXSOURCE_ADD:
 CMP BP,2
 JNZ SIDESWDXSOURCE_SUB 
 ADD BX,CX
 JMP OUTT_SIDISDXSOURCE
 SIDESWDXSOURCE_SUB:
 CMP BP,3
 JNZ SIDESWDXSOURCE_SBB
 SUB BX,CX
 JMP OUTT_SIDISDXSOURCE
 SIDESWDXSOURCE_SBB:
 CMP BP,4
 JNZ SIDESWDXSOURCE_ADC
 SBB BX,CX
 JMP OUTT_SIDISDXSOURCE
 SIDESWDXSOURCE_ADC:
 CMP BP,5
 JNZ SIDESWDXSOURCE_AND
 ADC BX,CX
 JMP OUTT_SIDISDXSOURCE
 SIDESWDXSOURCE_AND:
 CMP BP,6
 JNZ SIDESWDXSOURCE_XOR
 AND BX,CX
 JMP OUTT_SIDISDXSOURCE
 SIDESWDXSOURCE_XOR:
 XOR BX,CX
 
 OUTT_SIDISDXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV ISI1,BX
 JMP NAG7
NTASIDESWDIORALLWALERROR_PLUS: 
MOV AL,[SI+7]
CMP AL,100 
JZ NTASIDESWDISOURCE_PLUS
JMP ERROR
NTASIDESWDISOURCE_PLUS:
 MOV BX,ISI1
 MOV CX,IDI1
 MOV AX,FLAGS
 PUSH AX
 POPF
 SIDESWDISOURCE_MOV:
 CMP BP,1
 JNZ SIDESWDISOURCE_ADD
 MOV BX,CX
 JMP OUTT_SIDISDISOURCE
 SIDESWDISOURCE_ADD:
 CMP BP,2
 JNZ SIDESWDISOURCE_SUB 
 ADD BX,CX
 JMP OUTT_SIDISDISOURCE
 SIDESWDISOURCE_SUB:
 CMP BP,3
 JNZ SIDESWDISOURCE_SBB
 SUB BX,CX
 JMP OUTT_SIDISDISOURCE
 SIDESWDISOURCE_SBB:
 CMP BP,4
 JNZ SIDESWDISOURCE_ADC
 SBB BX,CX
 JMP OUTT_SIDISDISOURCE
 SIDESWDISOURCE_ADC:
 CMP BP,5
 JNZ SIDESWDISOURCE_AND
 ADC BX,CX
 JMP OUTT_SIDISDISOURCE
 SIDESWDISOURCE_AND:
 CMP BP,6
 JNZ SIDESWDISOURCE_XOR
 AND BX,CX
 JMP OUTT_SIDISDISOURCE
 SIDESWDISOURCE_XOR:
 XOR BX,CX
 
 OUTT_SIDISDISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV ISI1,BX
 JMP NAG7
SIDES_WNTA_MEMBX_PLUS:
MOV BX,RBX1
CMP BX,0FH
JA ERROR
MOV CL, MEMORY1[BX]
MOV CH, MEMORY1[BX+1]
MOV BX,ISI1
MOV AX,FLAGS
PUSH AX
POPF
 SIDESWMEMBXSOURCE_MOV:
 CMP BP,1
 JNZ SIDESWMEMBXSOURCE_ADD
 MOV BX,CX
 JMP OUTT_SIDISMEMBXSOURCE
 SIDESWMEMBXSOURCE_ADD:
 CMP BP,2
 JNZ SIDESWMEMBXSOURCE_SUB 
 ADD BX,CX
 JMP OUTT_SIDISMEMBXSOURCE
 SIDESWMEMBXSOURCE_SUB:
 CMP BP,3
 JNZ SIDESWMEMBXSOURCE_SBB
 SUB BX,CX
 JMP OUTT_SIDISMEMBXSOURCE
 SIDESWMEMBXSOURCE_SBB:
 CMP BP,4
 JNZ SIDESWMEMBXSOURCE_ADC
 SBB BX,CX
 JMP OUTT_SIDISMEMBXSOURCE
 SIDESWMEMBXSOURCE_ADC:
 CMP BP,5
 JNZ SIDESWMEMBXSOURCE_AND
 ADC BX,CX
 JMP OUTT_SIDISMEMBXSOURCE
 SIDESWMEMBXSOURCE_AND:
 CMP BP,6
 JNZ SIDESWMEMBXSOURCE_XOR
 AND BX,CX
 JMP OUTT_SIDISMEMBXSOURCE
 SIDESWMEMBXSOURCE_XOR:
 XOR BX,CX
 
 OUTT_SIDISMEMBXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV ISI1,BX
 JMP NAG7
NTA_SIDES_WMEMSI_PLUS:
MOV BX,ISI1
CMP BX,0FH
JA ERROR
MOV CH, MEMORY1[BX+1]
MOV CL, MEMORY1[BX]
MOV BX,ISI1
MOV AX,FLAGS
PUSH AX
POPF
 SIDESWMEMSISOURCE_MOV:
 CMP BP,1
 JNZ SIDESWMEMSISOURCE_ADD
 MOV BX,CX
 JMP OUTT_SIDISMEMSISOURCE
 SIDESWMEMSISOURCE_ADD:
 CMP BP,2
 JNZ SIDESWMEMSISOURCE_SUB 
 ADD BX,CX
 JMP OUTT_SIDISMEMSISOURCE
 SIDESWMEMSISOURCE_SUB:
 CMP BP,3
 JNZ SIDESWMEMSISOURCE_SBB
 SUB BX,CX
 JMP OUTT_SIDISMEMSISOURCE
 SIDESWMEMSISOURCE_SBB:
 CMP BP,4
 JNZ SIDESWMEMSISOURCE_ADC
 SBB BX,CX
 JMP OUTT_SIDISMEMSISOURCE
 SIDESWMEMSISOURCE_ADC:
 CMP BP,5
 JNZ SIDESWMEMSISOURCE_AND
 ADC BX,CX
 JMP OUTT_SIDISMEMSISOURCE
 SIDESWMEMSISOURCE_AND:
 CMP BP,6
 JNZ SIDESWMEMSISOURCE_XOR
 AND BX,CX
 JMP OUTT_SIDISMEMSISOURCE
 SIDESWMEMSISOURCE_XOR:
 XOR BX,CX
 
 OUTT_SIDISMEMSISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV ISI1,BX
 JMP NAG7
NTA_SIDES_WMEMDI_PLUS:
MOV BX,IDI1
CMP BX,0FH
JA ERROR
MOV CH, MEMORY1[BX+1]
MOV CL, MEMORY1[BX]
MOV BX,ISI1
MOV AX,FLAGS
PUSH AX
POPF
 SIDESWMEMDISOURCE_MOV:
 CMP BP,1
 JNZ SIDESWMEMDISOURCE_ADD
 MOV BX,CX
 JMP OUTT_SIDISMEMDISOURCE
 SIDESWMEMDISOURCE_ADD:
 CMP BP,2
 JNZ SIDESWMEMDISOURCE_SUB 
 ADD BX,CX
 JMP OUTT_SIDISMEMDISOURCE
 SIDESWMEMDISOURCE_SUB:
 CMP BP,3
 JNZ SIDESWMEMDISOURCE_SBB
 SUB BX,CX
 JMP OUTT_SIDISMEMDISOURCE
 SIDESWMEMDISOURCE_SBB:
 CMP BP,4
 JNZ SIDESWMEMDISOURCE_ADC
 SBB BX,CX
 JMP OUTT_SIDISMEMDISOURCE
 SIDESWMEMDISOURCE_ADC:
 CMP BP,5
 JNZ SIDESWMEMDISOURCE_AND
 ADC BX,CX
 JMP OUTT_SIDISMEMDISOURCE
 SIDESWMEMDISOURCE_AND:
 CMP BP,6
 JNZ SIDESWMEMDISOURCE_XOR
 AND BX,CX
 JMP OUTT_SIDISMEMDISOURCE
 SIDESWMEMDISOURCE_XOR:
 XOR BX,CX
 
 OUTT_SIDISMEMDISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV ISI1,BX
 JMP NAG7
NTASIDESW_RKM_YA_MEMRKM_YAERROR_PLUS:
MOV AL,[SI+7]
CMP AL,91
JZ MEMRKM_ORERROR_SIDES_PLUS

RKM_ORERROR_SIDES_PLUS:
MOV AL,[SI+8]
CMP AL,108
JZ ERROR
CMP AL,104
JZ ERROR
MOV VAR,BP
RET_NUM_COUNT  COMp+7
MOV BP,VAR
CMP CX,4
JA ERROR
MOV BX,ISI1
MOV AX,FLAGS
PUSH AX
POPF 
 SIDESWRAKAMSOURCE_MOV:
 CMP BP,1
 JNZ SIDESWRAKAMSOURCE_ADD
 MOV BX,DX
 JMP OUTT_SIDISRAKAMSOURCE
 SIDESWRAKAMSOURCE_ADD:
 CMP BP,2
 JNZ SIDESWRAKAMSOURCE_SUB 
 ADD BX,DX
 JMP OUTT_SIDISRAKAMSOURCE
 SIDESWRAKAMSOURCE_SUB:
 CMP BP,3
 JNZ SIDESWRAKAMSOURCE_SBB
 SUB BX,DX
 JMP OUTT_SIDISRAKAMSOURCE
 SIDESWRAKAMSOURCE_SBB:
 CMP BP,4
 JNZ SIDESWRAKAMSOURCE_ADC
 SBB BX,DX
 JMP OUTT_SIDISRAKAMSOURCE
 SIDESWRAKAMSOURCE_ADC:
 CMP BP,5
 JNZ SIDESWRAKAMSOURCE_AND
 ADC BX,DX
 JMP OUTT_SIDISRAKAMSOURCE
 SIDESWRAKAMSOURCE_AND:
 CMP BP,6
 JNZ SIDESWRAKAMSOURCE_XOR
 AND BX,DX
 JMP OUTT_SIDISRAKAMSOURCE
 SIDESWRAKAMSOURCE_XOR:
 XOR BX,DX
 
 OUTT_SIDISRAKAMSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV ISI1,BX
 JMP NAG7
MEMRKM_ORERROR_SIDES_PLUS:
MOV AL,[SI+9]
CMP AL,108
JZ ERROR
CMP AL,104
JZ ERROR  
MOV VAR,BP
RET_NUM_COUNT  COMp+8
MOV BP,VAR
CMP CX,4
JA ERROR
CMP DX,0FH
JA ERROR
MOV BX,DX
MOV CH, MEMORY1[BX+1]
MOV CL, MEMORY1[BX]
MOV BX,ISI1
MOV AX,FLAGS
PUSH AX
POPF 
 SIDESWMEMRAKAMSOURCE_MOV:
 CMP BP,1
 JNZ SIDESWMEMRAKAMSOURCE_ADD
 MOV BX,CX
 JMP OUTT_SIDISMEMRAKAMSOURCE
 SIDESWMEMRAKAMSOURCE_ADD:
 CMP BP,2
 JNZ SIDESWMEMRAKAMSOURCE_SUB 
 ADD BX,CX
 JMP OUTT_SIDISMEMRAKAMSOURCE
 SIDESWMEMRAKAMSOURCE_SUB:
 CMP BP,3
 JNZ SIDESWMEMRAKAMSOURCE_SBB
 SUB BX,CX
 JMP OUTT_SIDISMEMRAKAMSOURCE
 SIDESWMEMRAKAMSOURCE_SBB:
 CMP BP,4
 JNZ SIDESWMEMRAKAMSOURCE_ADC
 SBB BX,CX
 JMP OUTT_SIDISMEMRAKAMSOURCE
 SIDESWMEMRAKAMSOURCE_ADC:
 CMP BP,5
 JNZ SIDESWMEMRAKAMSOURCE_AND
 ADC BX,CX
 JMP OUTT_SIDISMEMRAKAMSOURCE
 SIDESWMEMRAKAMSOURCE_AND:
 CMP BP,6
 JNZ SIDESWMEMRAKAMSOURCE_XOR
 AND BX,CX
 JMP OUTT_SIDISMEMRAKAMSOURCE
 SIDESWMEMRAKAMSOURCE_XOR:
 XOR BX,CX
 
 OUTT_SIDISMEMRAKAMSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV ISI1,BX
 JMP NAG7
;///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

NTADIORALDES_PLUS:
MOV AL,[SI+4]
CMP AL,97
JZ NTAALDES_PLUS
  NTADIDES_PLUS:
  MOV BX,0
  MOV BL,[SI+8]
  CMP BL,104
  JNAE NTADIDESW_RKM_YA_MEMRKM_YAERROR_PLUS
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+7] 
  MOV CL,[SI+8]
  ADD AX,CX
  CMP AX,217
  JZ NTADIDESWAXSOURCE_PLUS
  CMP AX,218 
  JZ NTADIDESWBXSOURCE_PLUS
  CMP AX,219
  JZ NTADIDESWCXSOURCE_PLUS
  CMP AX,220
 JZ NTADIDESWDXORSISOURCE_PLUS
 CMP AX,205
 JZ NTADIDESWDIORALLWALERROR_PLUS
 MOV CX,0
 MOV BX,0                            ;TAKECARE
 MOV BL,[SI+9]
 MOV CL,[SI+10]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ DIDES_WNTA_MEMBX_PLUS
 CMP AX,404              
 JZ NTA_DIDES_WMEMSI_PLUS
 CMP AX,389
 JZ NTA_DIDES_WMEMDI_PLUS
 JMP NTADIDESW_RKM_YA_MEMRKM_YAERROR_PLUS 
 
 NTADIDESWAXSOURCE_PLUS:  
 MOV BX,IDI1
 MOV CX,RAX1
 MOV AX,FLAGS
 PUSH AX
 POPF
 DIDESWAXSOURCE_MOV:
 CMP BP,1
 JNZ DIDESWAXSOURCE_ADD
 MOV BX,CX
 JMP OUTT_DIDISAXSOURCE
 DIDESWAXSOURCE_ADD:
 CMP BP,2
 JNZ DIDESWAXSOURCE_SUB 
 ADD BX,CX
 JMP OUTT_DIDISAXSOURCE
 DIDESWAXSOURCE_SUB:
 CMP BP,3
 JNZ DIDESWAXSOURCE_SBB
 SUB BX,CX
 JMP OUTT_DIDISAXSOURCE
 DIDESWAXSOURCE_SBB:
 CMP BP,4
 JNZ DIDESWAXSOURCE_ADC
 SBB BX,CX
 JMP OUTT_DIDISAXSOURCE
 DIDESWAXSOURCE_ADC:
 CMP BP,5
 JNZ DIDESWAXSOURCE_AND
 ADC BX,CX
 JMP OUTT_DIDISAXSOURCE
 DIDESWAXSOURCE_AND:
 CMP BP,6
 JNZ DIDESWAXSOURCE_XOR
 AND BX,CX
 JMP OUTT_DIDISAXSOURCE
 DIDESWAXSOURCE_XOR:
 XOR BX,CX
 
 OUTT_DIDISAXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV IDI1,BX
 JMP NAG7
 
 NTADIDESWBXSOURCE_PLUS: 
 MOV BX,IDI1
 MOV CX,RBX1
 MOV AX,FLAGS
 PUSH AX
 POPF
 DIDESWBXSOURCE_MOV:
 CMP BP,1
 JNZ DIDESWBXSOURCE_ADD
 MOV BX,CX
 JMP OUTT_DIDISBXSOURCE
 DIDESWBXSOURCE_ADD:
 CMP BP,2
 JNZ DIDESWBXSOURCE_SUB 
 ADD BX,CX
 JMP OUTT_DIDISBXSOURCE
 DIDESWBXSOURCE_SUB:
 CMP BP,3
 JNZ DIDESWBXSOURCE_SBB
 SUB BX,CX
 JMP OUTT_DIDISBXSOURCE
 DIDESWBXSOURCE_SBB:
 CMP BP,4
 JNZ DIDESWBXSOURCE_ADC
 SBB BX,CX
 JMP OUTT_DIDISBXSOURCE
 DIDESWBXSOURCE_ADC:
 CMP BP,5
 JNZ DIDESWBXSOURCE_AND
 ADC BX,CX
 JMP OUTT_DIDISBXSOURCE
 DIDESWBXSOURCE_AND:
 CMP BP,6
 JNZ DIDESWBXSOURCE_XOR
 AND BX,CX
 JMP OUTT_DIDISBXSOURCE
 DIDESWBXSOURCE_XOR:
 XOR BX,CX
 
 OUTT_DIDISBXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV IDI1,BX
 JMP NAG7
 
 NTADIDESWCXSOURCE_PLUS:
 MOV BX,IDI1
 MOV CX,RCX1
 MOV AX,FLAGS
 PUSH AX
 POPF
DIDESWCXSOURCE_MOV:
 CMP BP,1
 JNZ DIDESWCXSOURCE_ADD
 MOV BX,CX
 JMP OUTT_DIDISCXSOURCE
 DIDESWCXSOURCE_ADD:
 CMP BP,2
 JNZ DIDESWCXSOURCE_SUB 
 ADD BX,CX
 JMP OUTT_DIDISCXSOURCE
 DIDESWCXSOURCE_SUB:
 CMP BP,3
 JNZ DIDESWCXSOURCE_SBB
 SUB BX,CX
 JMP OUTT_DIDISCXSOURCE
 DIDESWCXSOURCE_SBB:
 CMP BP,4
 JNZ DIDESWCXSOURCE_ADC
 SBB BX,CX
 JMP OUTT_DIDISCXSOURCE
 DIDESWCXSOURCE_ADC:
 CMP BP,5
 JNZ DIDESWCXSOURCE_AND
 ADC BX,CX
 JMP OUTT_DIDISCXSOURCE
 DIDESWCXSOURCE_AND:
 CMP BP,6
 JNZ DIDESWCXSOURCE_XOR
 AND BX,CX
 JMP OUTT_DIDISCXSOURCE
 DIDESWCXSOURCE_XOR:
 XOR BX,CX

 OUTT_DIDISCXSOURCE:
 
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV IDI1,BX
 JMP NAG7
 NTADIDESWDXORSISOURCE_PLUS:
 MOV AL,[SI+7]
 CMP AL,100
 JZ NTADIDESWDXSOURCE_PLUS
  
NTADIDESWSISOURCE_PLUS:
 MOV BX,IDI1
 MOV CX,ISI1
 MOV AX,FLAGS
 PUSH AX
 POPF
 DIDESWSISOURCE_MOV:
 CMP BP,1
 JNZ DIDESWSISOURCE_ADD
 MOV BX,CX
 JMP OUTT_DIDISSISOURCE
 DIDESWSISOURCE_ADD:
 CMP BP,2
 JNZ DIDESWSISOURCE_SUB 
 ADD BX,CX
 JMP OUTT_DIDISSISOURCE
 DIDESWSISOURCE_SUB:
 CMP BP,3
 JNZ DIDESWSISOURCE_SBB
 SUB BX,CX
 JMP OUTT_DIDISSISOURCE
 DIDESWSISOURCE_SBB:
 CMP BP,4
 JNZ DIDESWSISOURCE_ADC
 SBB BX,CX
 JMP OUTT_DIDISSISOURCE
 DIDESWSISOURCE_ADC:
 CMP BP,5
 JNZ DIDESWSISOURCE_AND
 ADC BX,CX
 JMP OUTT_DIDISSISOURCE
 DIDESWSISOURCE_AND:
 CMP BP,6
 JNZ DIDESWSISOURCE_XOR
 AND BX,CX
 JMP OUTT_DIDISSISOURCE
 DIDESWSISOURCE_XOR:
 XOR BX,CX
 
 OUTT_DIDISSISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV IDI1,BX
 JMP NAG7
NTADIDESWDXSOURCE_PLUS:
 MOV BX,IDI1
 MOV CX,RDX1
 MOV AX,FLAGS
 PUSH AX
 POPF
  DIDESWDXSOURCE_MOV:
 CMP BP,1
 JNZ DIDESWDXSOURCE_ADD
 MOV BX,CX
 JMP OUTT_DIDISDXSOURCE
 DIDESWDXSOURCE_ADD:
 CMP BP,2
 JNZ DIDESWDXSOURCE_SUB 
 ADD BX,CX
 JMP OUTT_DIDISDXSOURCE
 DIDESWDXSOURCE_SUB:
 CMP BP,3
 JNZ DIDESWDXSOURCE_SBB
 SUB BX,CX
 JMP OUTT_DIDISDXSOURCE
 DIDESWDXSOURCE_SBB:
 CMP BP,4
 JNZ DIDESWDXSOURCE_ADC
 SBB BX,CX
 JMP OUTT_DIDISDXSOURCE
 DIDESWDXSOURCE_ADC:
 CMP BP,5
 JNZ DIDESWDXSOURCE_AND
 ADC BX,CX
 JMP OUTT_DIDISDXSOURCE
 DIDESWDXSOURCE_AND:
 CMP BP,6
 JNZ DIDESWDXSOURCE_XOR
 AND BX,CX
 JMP OUTT_DIDISDXSOURCE
 DIDESWDXSOURCE_XOR:
 XOR BX,CX
 
 OUTT_DIDISDXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV IDI1,BX
 JMP NAG7
NTADIDESWDIORALLWALERROR_PLUS: 
MOV AL,[SI+7]
CMP AL,100 
JZ NTADIDESWDISOURCE_PLUS
JMP ERROR
NTADIDESWDISOURCE_PLUS:
 MOV BX,IDI1
 MOV AX,FLAGS
 PUSH AX
 POPF
 DIDESWDISOURCE_MOV:
 CMP BP,1
 JNZ DIDESWDISOURCE_ADD
 MOV BX,BX
 JMP OUTT_DIDISDISOURCE
 DIDESWDISOURCE_ADD:
 CMP BP,2
 JNZ DIDESWDISOURCE_SUB 
 ADD BX,BX
 JMP OUTT_DIDISDISOURCE
 DIDESWDISOURCE_SUB:
 CMP BP,3
 JNZ DIDESWDISOURCE_SBB
 SUB BX,BX
 JMP OUTT_DIDISDISOURCE
 DIDESWDISOURCE_SBB:
 CMP BP,4
 JNZ DIDESWDISOURCE_ADC
 SBB BX,BX
 JMP OUTT_DIDISDISOURCE
 DIDESWDISOURCE_ADC:
 CMP BP,5
 JNZ DIDESWDISOURCE_AND
 ADC BX,BX
 JMP OUTT_DIDISDISOURCE
 DIDESWDISOURCE_AND:
 CMP BP,6
 JNZ DIDESWDISOURCE_XOR
 AND BX,BX
 JMP OUTT_DIDISDISOURCE
 DIDESWDISOURCE_XOR:
 XOR BX,BX
 
 OUTT_DIDISDISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV IDI1,BX
 JMP NAG7
DIDES_WNTA_MEMBX_PLUS:
MOV BX,RBX1
CMP BX,0FH
JA ERROR
MOV CL, MEMORY1[BX]
MOV CH, MEMORY1[BX+1]
MOV BX,IDI1
MOV AX,FLAGS
PUSH AX
POPF
 DIDESWMEMBXSOURCE_MOV:
 CMP BP,1
 JNZ DIDESWMEMBXSOURCE_ADD
 MOV BX,CX
 JMP OUTT_DIDISMEMBXSOURCE
 DIDESWMEMBXSOURCE_ADD:
 CMP BP,2
 JNZ DIDESWMEMBXSOURCE_SUB 
 ADD BX,CX
 JMP OUTT_DIDISMEMBXSOURCE
 DIDESWMEMBXSOURCE_SUB:
 CMP BP,3
 JNZ DIDESWMEMBXSOURCE_SBB
 SUB BX,CX
 JMP OUTT_DIDISMEMBXSOURCE
 DIDESWMEMBXSOURCE_SBB:
 CMP BP,4
 JNZ DIDESWMEMBXSOURCE_ADC
 SBB BX,CX
 JMP OUTT_DIDISMEMBXSOURCE
 DIDESWMEMBXSOURCE_ADC:
 CMP BP,5
 JNZ DIDESWMEMBXSOURCE_AND
 ADC BX,CX
 JMP OUTT_DIDISMEMBXSOURCE
 DIDESWMEMBXSOURCE_AND:
 CMP BP,6
 JNZ DIDESWMEMBXSOURCE_XOR
 AND BX,CX
 JMP OUTT_DIDISMEMBXSOURCE
 DIDESWMEMBXSOURCE_XOR:
 XOR BX,CX
 
 OUTT_DIDISMEMBXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV IDI1,BX
 JMP NAG7
NTA_DIDES_WMEMSI_PLUS:
MOV BX,ISI1
CMP BX,0FH
JA ERROR
MOV CH, MEMORY1[BX+1]
MOV CL, MEMORY1[BX]
MOV BX,IDI1
MOV AX,FLAGS
PUSH AX
POPF
 DIDESWMEMSISOURCE_MOV:
 CMP BP,1
 JNZ DIDESWMEMSISOURCE_ADD
 MOV BX,CX
 JMP OUTT_DIDISMEMSISOURCE
 DIDESWMEMSISOURCE_ADD:
 CMP BP,2
 JNZ DIDESWMEMSISOURCE_SUB 
 ADD BX,CX
 JMP OUTT_DIDISMEMSISOURCE
 DIDESWMEMSISOURCE_SUB:
 CMP BP,3
 JNZ DIDESWMEMSISOURCE_SBB
 SUB BX,CX
 JMP OUTT_DIDISMEMSISOURCE
 DIDESWMEMSISOURCE_SBB:
 CMP BP,4
 JNZ DIDESWMEMSISOURCE_ADC
 SBB BX,CX
 JMP OUTT_DIDISMEMSISOURCE
 DIDESWMEMSISOURCE_ADC:
 CMP BP,5
 JNZ DIDESWMEMSISOURCE_AND
 ADC BX,CX
 JMP OUTT_DIDISMEMSISOURCE
 DIDESWMEMSISOURCE_AND:
 CMP BP,6
 JNZ DIDESWMEMSISOURCE_XOR
 AND BX,CX
 JMP OUTT_DIDISMEMSISOURCE
 DIDESWMEMSISOURCE_XOR:
 XOR BX,CX
 
 OUTT_DIDISMEMSISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV IDI1,BX
 JMP NAG7
NTA_DIDES_WMEMDI_PLUS:
MOV BX,IDI1
CMP BX,0FH
JA ERROR
MOV CH, MEMORY1[BX+1]
MOV CL, MEMORY1[BX]
MOV BX,IDI1
MOV AX,FLAGS
PUSH AX
POPF
 DIDESWMEMDISOURCE_MOV:
 CMP BP,1
 JNZ DIDESWMEMDISOURCE_ADD
 MOV BX,CX
 JMP OUTT_DIDISMEMDISOURCE
 DIDESWMEMDISOURCE_ADD:
 CMP BP,2
 JNZ DIDESWMEMDISOURCE_SUB 
 ADD BX,CX
 JMP OUTT_DIDISMEMDISOURCE
 DIDESWMEMDISOURCE_SUB:
 CMP BP,3
 JNZ DIDESWMEMDISOURCE_SBB
 SUB BX,CX
 JMP OUTT_DIDISMEMDISOURCE
 DIDESWMEMDISOURCE_SBB:
 CMP BP,4
 JNZ DIDESWMEMDISOURCE_ADC
 SBB BX,CX
 JMP OUTT_DIDISMEMDISOURCE
 DIDESWMEMDISOURCE_ADC:
 CMP BP,5
 JNZ DIDESWMEMDISOURCE_AND
 ADC BX,CX
 JMP OUTT_DIDISMEMDISOURCE
 DIDESWMEMDISOURCE_AND:
 CMP BP,6
 JNZ DIDESWMEMDISOURCE_XOR
 AND BX,CX
 JMP OUTT_DIDISMEMDISOURCE
 DIDESWMEMDISOURCE_XOR:
 XOR BX,CX
 
 OUTT_DIDISMEMDISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV IDI1,BX
 JMP NAG7
NTADIDESW_RKM_YA_MEMRKM_YAERROR_PLUS:
MOV AL,[SI+7]
CMP AL,91
JZ MEMRKM_ORERROR_DIDES_PLUS

RKM_ORERROR_DIDES_PLUS:
MOV AL,[SI+8]
CMP AL,108
JZ ERROR
CMP AL,104
JZ ERROR
MOV VAR,BP
RET_NUM_COUNT  COMp+7
MOV BP,VAR
CMP CX,4
JA ERROR
MOV BX,IDI1
MOV AX,FLAGS
PUSH AX
POPF 
 DIDESWRAKAMSOURCE_MOV:
 CMP BP,1
 JNZ DIDESWRAKAMSOURCE_ADD
 MOV BX,DX
 JMP OUTT_DIDISRAKAMSOURCE
 DIDESWRAKAMSOURCE_ADD:
 CMP BP,2
 JNZ DIDESWRAKAMSOURCE_SUB 
 ADD BX,DX
 JMP OUTT_DIDISRAKAMSOURCE
 DIDESWRAKAMSOURCE_SUB:
 CMP BP,3
 JNZ DIDESWRAKAMSOURCE_SBB
 SUB BX,DX
 JMP OUTT_DIDISRAKAMSOURCE
 DIDESWRAKAMSOURCE_SBB:
 CMP BP,4
 JNZ DIDESWRAKAMSOURCE_ADC
 SBB BX,DX
 JMP OUTT_DIDISRAKAMSOURCE
 DIDESWRAKAMSOURCE_ADC:
 CMP BP,5
 JNZ DIDESWRAKAMSOURCE_AND
 ADC BX,DX
 JMP OUTT_DIDISRAKAMSOURCE
 DIDESWRAKAMSOURCE_AND:
 CMP BP,6
 JNZ DIDESWRAKAMSOURCE_XOR
 AND BX,DX
 JMP OUTT_DIDISRAKAMSOURCE
 DIDESWRAKAMSOURCE_XOR:
 XOR BX,DX
 
 OUTT_DIDISRAKAMSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV IDI1,BX
 JMP NAG7
MEMRKM_ORERROR_DIDES_PLUS:
MOV AL,[SI+9]
CMP AL,108
JZ ERROR
CMP AL,104
JZ ERROR  
MOV VAR,BP
RET_NUM_COUNT  COMp+8
MOV BP,VAR
CMP CX,4
JA ERROR
CMP DX,0FH
JA ERROR
MOV BX,DX
MOV CH, MEMORY1[BX+1]
MOV CL, MEMORY1[BX]
MOV BX,IDI1
MOV AX,FLAGS
PUSH AX
POPF 
 DIDESWMEMRAKAMSOURCE_MOV:
 CMP BP,1
 JNZ DIDESWMEMRAKAMSOURCE_ADD
 MOV BX,CX
 JMP OUTT_DIDISMEMRAKAMSOURCE
 DIDESWMEMRAKAMSOURCE_ADD:
 CMP BP,2
 JNZ DIDESWMEMRAKAMSOURCE_SUB 
 ADD BX,CX
 JMP OUTT_DIDISMEMRAKAMSOURCE
 DIDESWMEMRAKAMSOURCE_SUB:
 CMP BP,3
 JNZ DIDESWMEMRAKAMSOURCE_SBB
 SUB BX,CX
 JMP OUTT_DIDISMEMRAKAMSOURCE
 DIDESWMEMRAKAMSOURCE_SBB:
 CMP BP,4
 JNZ DIDESWMEMRAKAMSOURCE_ADC
 SBB BX,CX
 JMP OUTT_DIDISMEMRAKAMSOURCE
 DIDESWMEMRAKAMSOURCE_ADC:
 CMP BP,5
 JNZ DIDESWMEMRAKAMSOURCE_AND
 ADC BX,CX
 JMP OUTT_DIDISMEMRAKAMSOURCE
 DIDESWMEMRAKAMSOURCE_AND:
 CMP BP,6
 JNZ DIDESWMEMRAKAMSOURCE_XOR
 AND BX,CX
 JMP OUTT_DIDISMEMRAKAMSOURCE
 DIDESWMEMRAKAMSOURCE_XOR:
 XOR BX,CX
 
 OUTT_DIDISMEMRAKAMSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV IDI1,BX
 JMP NAG7
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTAALDES_PLUS:
  MOV BX,0
  MOV BL,[SI+8]
  CMP BL,104
  JNAE NTAALDESW_RKM_YA_MEMRKM_YAERROR_PLUS
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+7] 
  MOV CL,[SI+8]
  ADD AX,CX
  CMP AX,217
  JZ ERROR
  CMP AX,218 
  JZ ERROR
  CMP AX,219
  JZ ERROR
  CMP AX,220
 JZ ERROR
 CMP AX,205
 JZ NTAALDESWDIORALLWDIERROR_PLUS
 CMP AX,201
 JZ NTAALDESWAHSOURCE_PLUS
 CMP AX,206
 JZ NTAALDESWBLSOURCE_PLUS
 CMP AX,202
 JZ NTAALDESWBHSOURCE_PLUS 
 CMP AX,207
 JZ NTAALDESWCLSOURCE_PLUS
 CMP AX,203
 JZ NTAALDESWCHSOURCE_PLUS
 CMP AX,208
 JZ NTAALDESWDLSOURCE_PLUS
 CMP AX,204
 JZ NTAALDESWDHSOURCE_PLUS 
 MOV BX,0
 MOV CX,0                              ;TAKECARE
 MOV CL,[SI+9]
 MOV BL,[SI+10]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ ALDES_WNTA_MEMBX_PLUS
 CMP AX,404              
 JZ NTA_ALDES_WMEMSI_PLUS
 CMP AX,389
 JZ NTA_ALDES_WMEMDI_PLUS
 JMP NTAALDESW_RKM_YA_MEMRKM_YAERROR_PLUS  
 
 
  
NTAALDESWDIORALLWDIERROR_PLUS: 
MOV AL,[SI+7]
CMP AL,100 
JZ ERROR
NTAALDESWALSOURCE_PLUS:
MOV BL,BYTE PTR RAX1
MOV CX,RAX1
MOV AX,FLAGS
PUSH AX
 ALXDESWALSOURCE_MOV:
 CMP BP,1
 JNZ ALDESWALSOURCE_ADD
 POPF 
 MOV BL,CL
 JMP OUTT_ALDISALSOURCE
 ALDESWALSOURCE_ADD:
 CMP BP,2
 JNZ ALDESWALSOURCE_SUB
 POPF 
 ADD BL,CL
 JMP OUTT_ALDISALSOURCE
 ALDESWALSOURCE_SUB:
 CMP BP,3
 JNZ ALDESWALSOURCE_SBB
 POPF 
 SUB BL,CL
 JMP OUTT_ALDISALSOURCE
 ALDESWALSOURCE_SBB:
 CMP BP,4
 JNZ ALDESWALSOURCE_ADC
 POPF 
 SBB BL,CL
 JMP OUTT_ALDISALSOURCE
 ALDESWALSOURCE_ADC:
 CMP BP,5
 JNZ ALDESWALSOURCE_AND
 POPF 
 ADC BL,CL
 JMP OUTT_ALDISALSOURCE
 ALDESWALSOURCE_AND:
 CMP BP,6
 JNZ ALDESWALSOURCE_XOR
 POPF 
 AND BL,CL
 JMP OUTT_ALDISALSOURCE
 ALDESWALSOURCE_XOR:
 POPF 
 XOR BL,CL
 OUTT_ALDISALSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,BX
 JMP NAG7

ALDES_WNTA_MEMBX_PLUS:
MOV BX,RBX1
CMP BX,0FH
JA ERROR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX
 ALDESWMEMBXSOURCE_MOV:
 CMP BP,1
 JNZ ALDESWMEMBXSOURCE_ADD
 POPF 
 MOV BL,CL
 JMP OUTT_ALDISMEMBXSOURCE
 ALDESWMEMBXSOURCE_ADD:
 CMP BP,2
 JNZ ALDESWMEMBXSOURCE_SUB
 POPF 
 ADD BL,CL
 JMP OUTT_ALDISMEMBXSOURCE
 ALDESWMEMBXSOURCE_SUB:
 CMP BP,3
 JNZ ALDESWMEMBXSOURCE_SBB
 POPF 
 SUB BL,CL
 JMP OUTT_ALDISMEMBXSOURCE
 ALDESWMEMBXSOURCE_SBB:
 CMP BP,4
 JNZ ALDESWMEMBXSOURCE_ADC
 POPF 
 SBB BL,CL
 JMP OUTT_ALDISMEMBXSOURCE
 ALDESWMEMBXSOURCE_ADC:
 CMP BP,5
 JNZ ALDESWMEMBXSOURCE_AND
 POPF 
 ADC BL,CL
 JMP OUTT_ALDISMEMBXSOURCE
 ALDESWMEMBXSOURCE_AND:
 CMP BP,6
 JNZ ALDESWMEMBXSOURCE_XOR
 POPF 
 AND BL,CL
 JMP OUTT_ALDISMEMBXSOURCE
 ALDESWMEMBXSOURCE_XOR:
 POPF 
 XOR BL,CL
 OUTT_ALDISMEMBXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,BX
 JMP NAG7
NTA_ALDES_WMEMSI_PLUS:
MOV BX,ISI1
CMP BX,0FH
JA ERROR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX
 ALDESWMEMSISOURCE_MOV:
 CMP BP,1
 JNZ ALDESWMEMSISOURCE_ADD
 POPF 
 MOV BL,CL
 JMP OUTT_ALDISMEMSISOURCE
 ALDESWMEMSISOURCE_ADD:
 CMP BP,2
 JNZ ALDESWMEMSISOURCE_SUB
 POPF 
 ADD BL,CL
 JMP OUTT_ALDISMEMSISOURCE
 ALDESWMEMSISOURCE_SUB:
 CMP BP,3
 JNZ ALDESWMEMSISOURCE_SBB
 POPF 
 SUB BL,CL
 JMP OUTT_ALDISMEMSISOURCE
 ALDESWMEMSISOURCE_SBB:
 CMP BP,4
 JNZ ALDESWMEMSISOURCE_ADC
 POPF 
 SBB BL,CL
 JMP OUTT_ALDISMEMSISOURCE
 ALDESWMEMSISOURCE_ADC:
 CMP BP,5
 JNZ ALDESWMEMSISOURCE_AND
 POPF 
 ADC BL,CL
 JMP OUTT_ALDISMEMSISOURCE
 ALDESWMEMSISOURCE_AND:
 CMP BP,6
 JNZ ALDESWMEMSISOURCE_XOR
 POPF 
 AND BL,CL
 JMP OUTT_ALDISMEMSISOURCE
 ALDESWMEMSISOURCE_XOR:
 POPF 
 XOR BL,CL
 OUTT_ALDISMEMSISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,BX
 JMP NAG7
NTA_ALDES_WMEMDI_PLUS:
MOV BX,IDI1
CMP BX,0FH
JA ERROR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX
 ALDESWMEMDISOURCE_MOV:
 CMP BP,1
 JNZ ALDESWMEMDISOURCE_ADD
 POPF 
 MOV BL,CL
 JMP OUTT_ALDISMEMDISOURCE
 ALDESWMEMDISOURCE_ADD:
 CMP BP,2
 JNZ ALDESWMEMDISOURCE_SUB
 POPF 
 ADD BL,CL
 JMP OUTT_ALDISMEMDISOURCE
 ALDESWMEMDISOURCE_SUB:
 CMP BP,3
 JNZ ALDESWMEMDISOURCE_SBB
 POPF 
 SUB BL,CL
 JMP OUTT_ALDISMEMDISOURCE
 ALDESWMEMDISOURCE_SBB:
 CMP BP,4
 JNZ ALDESWMEMDISOURCE_ADC
 POPF 
 SBB BL,CL
 JMP OUTT_ALDISMEMDISOURCE
 ALDESWMEMDISOURCE_ADC:
 CMP BP,5
 JNZ ALDESWMEMDISOURCE_AND
 POPF 
 ADC BL,CL
 JMP OUTT_ALDISMEMDISOURCE
 ALDESWMEMDISOURCE_AND:
 CMP BP,6
 JNZ ALDESWMEMDISOURCE_XOR
 POPF 
 AND BL,CL
 JMP OUTT_ALDISMEMDISOURCE
 ALDESWMEMDISOURCE_XOR:
 POPF 
 XOR BL,CL
 
 OUTT_ALDISMEMDISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,BX
 JMP NAG7
NTAALDESWAHSOURCE_PLUS:
MOV BL,BYTE PTR RAX1+1
MOV DX,RAX1
MOV AX,FLAGS
PUSH AX
 ALXDESWAHSOURCE_MOV:
 CMP BP,1
 JNZ ALDESWAHSOURCE_ADD
 POPF 
 MOV DL,BL
 JMP OUTT_ALDISAHSOURCE
 ALDESWAHSOURCE_ADD:
 CMP BP,2
 JNZ ALDESWAHSOURCE_SUB
 POPF 
 ADD DL,BL
 JMP OUTT_ALDISAHSOURCE
 ALDESWAHSOURCE_SUB:
 CMP BP,3
 JNZ ALDESWAHSOURCE_SBB
 POPF 
 SUB DL,BL
 JMP OUTT_ALDISAHSOURCE
 ALDESWAHSOURCE_SBB:
 CMP BP,4
 JNZ ALDESWAHSOURCE_ADC
 POPF 
 SBB DL,BL
 JMP OUTT_ALDISAHSOURCE
 ALDESWAHSOURCE_ADC:
 CMP BP,5
 JNZ ALDESWAHSOURCE_AND
 POPF 
 ADC DL,BL
 JMP OUTT_ALDISAHSOURCE
 ALDESWAHSOURCE_AND:
 CMP BP,6
 JNZ ALDESWAHSOURCE_XOR
 POPF 
 AND DL,BL
 JMP OUTT_ALDISAHSOURCE
 ALDESWAHSOURCE_XOR:
 POPF 
 XOR DL,BL
 OUTT_ALDISAHSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,DX
 JMP NAG7
NTAALDESWBLSOURCE_PLUS:
MOV BL,BYTE PTR RBX1
MOV DX,RAX1
MOV AX,FLAGS
PUSH AX
 ALXDESWBLSOURCE_MOV:
 CMP BP,1
 JNZ ALDESWBLSOURCE_ADD
 MOV DL,BL
 JMP OUTT_ALDISBLSOURCE
 ALDESWBLSOURCE_ADD:
 CMP BP,2
 JNZ ALDESWBLSOURCE_SUB
 POPF 
 ADD DL,BL
 JMP OUTT_ALDISBLSOURCE
 ALDESWBLSOURCE_SUB:
 CMP BP,3
 JNZ ALDESWBLSOURCE_SBB
 POPF 
 SUB DL,BL
 JMP OUTT_ALDISBLSOURCE
 ALDESWBLSOURCE_SBB:
 CMP BP,4
 JNZ ALDESWBLSOURCE_ADC
 POPF 
 SBB DL,BL
 JMP OUTT_ALDISBLSOURCE
 ALDESWBLSOURCE_ADC:
 CMP BP,5
 JNZ ALDESWBLSOURCE_AND
 POPF 
 ADC DL,BL
 JMP OUTT_ALDISBLSOURCE
 ALDESWBLSOURCE_AND:
 CMP BP,6
 JNZ ALDESWBLSOURCE_XOR
 POPF 
 AND DL,BL
 JMP OUTT_ALDISBLSOURCE
 ALDESWBLSOURCE_XOR:
 POPF 
 XOR DL,BL
 OUTT_ALDISBLSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,DX
 JMP NAG7
NTAALDESWBHSOURCE_PLUS:
MOV BL,BYTE PTR RBX1+1
MOV DX,RAX1
MOV AX,FLAGS
PUSH AX
 ALXDESWBHSOURCE_MOV:
 CMP BP,1
 JNZ ALDESWBHSOURCE_ADD
 POPF 
 MOV DL,BL
 JMP OUTT_ALDISBHSOURCE
 ALDESWBHSOURCE_ADD:
 CMP BP,2
 JNZ ALDESWBHSOURCE_SUB
 POPF 
 ADD DL,BL
 JMP OUTT_ALDISBHSOURCE
 ALDESWBHSOURCE_SUB:
 CMP BP,3
 JNZ ALDESWBHSOURCE_SBB
 POPF 
 SUB DL,BL
 JMP OUTT_ALDISBHSOURCE
 ALDESWBHSOURCE_SBB:
 CMP BP,4
 JNZ ALDESWBHSOURCE_ADC
 POPF 
 SBB DL,BL
 JMP OUTT_ALDISBHSOURCE
 ALDESWBHSOURCE_ADC:
 CMP BP,5
 JNZ ALDESWBHSOURCE_AND
 POPF 
 ADC DL,BL
 JMP OUTT_ALDISBHSOURCE
 ALDESWBHSOURCE_AND:
 CMP BP,6
 JNZ ALDESWBHSOURCE_XOR
 POPF 
 AND DL,BL
 JMP OUTT_ALDISBHSOURCE
 ALDESWBHSOURCE_XOR:
 POPF 
 XOR DL,BL
 
 OUTT_ALDISBHSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,DX
 JMP NAG7
NTAALDESWCLSOURCE_PLUS:
MOV BL,BYTE PTR RCX1
MOV DX,RAX1
MOV AX,FLAGS
PUSH AX
 ALXDESWCLSOURCE_MOV:
 CMP BP,1
 JNZ ALDESWCLSOURCE_ADD
 POPF 
 MOV DL,BL
 JMP OUTT_ALDISCLSOURCE
 ALDESWCLSOURCE_ADD:
 CMP BP,2
 JNZ ALDESWCLSOURCE_SUB
 POPF 
 ADD DL,BL
 JMP OUTT_ALDISCLSOURCE
 ALDESWCLSOURCE_SUB:
 CMP BP,3
 JNZ ALDESWCLSOURCE_SBB
 POPF 
 SUB DL,BL
 JMP OUTT_ALDISCLSOURCE
 ALDESWCLSOURCE_SBB:
 CMP BP,4
 JNZ ALDESWCLSOURCE_ADC
 POPF 
 SBB DL,BL
 JMP OUTT_ALDISCLSOURCE
 ALDESWCLSOURCE_ADC:
 CMP BP,5
 JNZ ALDESWCLSOURCE_AND
 POPF 
 ADC DL,BL
 JMP OUTT_ALDISCLSOURCE
 ALDESWCLSOURCE_AND:
 CMP BP,6
 JNZ ALDESWCLSOURCE_XOR
 POPF 
 AND DL,BL
 JMP OUTT_ALDISCLSOURCE
 ALDESWCLSOURCE_XOR:
 POPF 
 XOR DL,BL
 
 OUTT_ALDISCLSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,DX
 JMP NAG7
NTAALDESWCHSOURCE_PLUS:
MOV BL,BYTE PTR RCX1+1
MOV DX,RAX1
MOV AX,FLAGS
PUSH AX
 ALXDESWCHSOURCE_MOV:
 CMP BP,1
 JNZ ALDESWCHSOURCE_ADD
 POPF 
 MOV DL,BL
 JMP OUTT_ALDISCHSOURCE
 ALDESWCHSOURCE_ADD:
 CMP BP,2
 JNZ ALDESWCHSOURCE_SUB
 POPF 
 ADD DL,BL
 JMP OUTT_ALDISCHSOURCE
 ALDESWCHSOURCE_SUB:
 CMP BP,3
 JNZ ALDESWCHSOURCE_SBB
 POPF 
 SUB DL,BL
 JMP OUTT_ALDISCHSOURCE
 ALDESWCHSOURCE_SBB:
 CMP BP,4
 JNZ ALDESWCHSOURCE_ADC
 POPF 
 SBB DL,BL
 JMP OUTT_ALDISCHSOURCE
 ALDESWCHSOURCE_ADC:
 CMP BP,5
 JNZ ALDESWCHSOURCE_AND
 POPF 
 ADC DL,BL
 JMP OUTT_ALDISCHSOURCE
 ALDESWCHSOURCE_AND:
 CMP BP,6
 JNZ ALDESWCHSOURCE_XOR
 POPF 
 AND DL,BL
 JMP OUTT_ALDISCHSOURCE
 ALDESWCHSOURCE_XOR:
 POPF 
 XOR DL,BL
 
 OUTT_ALDISCHSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,DX
 JMP NAG7
NTAALDESWDLSOURCE_PLUS:
MOV BL,BYTE PTR RDX1
MOV DX,RAX1
MOV AX,FLAGS
PUSH AX
 ALXDESWDLSOURCE_MOV:
 CMP BP,1
 JNZ ALDESWDLSOURCE_ADD
 POPF 
 MOV DL,BL
 JMP OUTT_ALDISDLSOURCE
 ALDESWDLSOURCE_ADD:
 CMP BP,2
 JNZ ALDESWDLSOURCE_SUB
 POPF 
 ADD DL,BL
 JMP OUTT_ALDISDLSOURCE
 ALDESWDLSOURCE_SUB:
 CMP BP,3
 JNZ ALDESWDLSOURCE_SBB
 POPF 
 SUB DL,BL
 JMP OUTT_ALDISDLSOURCE
 ALDESWDLSOURCE_SBB:
 CMP BP,4
 JNZ ALDESWDLSOURCE_ADC
 POPF 
 SBB DL,BL
 JMP OUTT_ALDISDLSOURCE
 ALDESWDLSOURCE_ADC:
 CMP BP,5
 JNZ ALDESWDLSOURCE_AND
 POPF 
 ADC DL,BL
 JMP OUTT_ALDISDLSOURCE
 ALDESWDLSOURCE_AND:
 CMP BP,6
 JNZ ALDESWDLSOURCE_XOR
 POPF 
 AND DL,BL
 JMP OUTT_ALDISDLSOURCE
 ALDESWDLSOURCE_XOR:
 POPF 
 XOR DL,BL
 
 OUTT_ALDISDLSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,DX
 JMP NAG7
NTAALDESWDHSOURCE_PLUS:
MOV BL,BYTE PTR RDX1+1
MOV DX,RAX1
MOV AX,FLAGS
PUSH AX
POPF 
 ALXDESWDHSOURCE_MOV:
 CMP BP,1
 JNZ ALDESWDHSOURCE_ADD
 MOV DL,BL
 JMP OUTT_ALDISDHSOURCE
 ALDESWDHSOURCE_ADD:
 CMP BP,2
 JNZ ALDESWDHSOURCE_SUB
 POPF 
 ADD DL,BL
 JMP OUTT_ALDISDHSOURCE
 ALDESWDHSOURCE_SUB:
 CMP BP,3
 JNZ ALDESWDHSOURCE_SBB
 POPF 
 SUB DL,BL
 JMP OUTT_ALDISDHSOURCE
 ALDESWDHSOURCE_SBB:
 CMP BP,4
 JNZ ALDESWDHSOURCE_ADC
 POPF 
 SBB DL,BL
 JMP OUTT_ALDISDHSOURCE
 ALDESWDHSOURCE_ADC:
 CMP BP,5
 JNZ ALDESWDHSOURCE_AND
 POPF 
 ADC DL,BL
 JMP OUTT_ALDISDHSOURCE
 ALDESWDHSOURCE_AND:
 CMP BP,6
 JNZ ALDESWDHSOURCE_XOR
 POPF 
 AND DL,BL
 JMP OUTT_ALDISDHSOURCE
 ALDESWDHSOURCE_XOR:
 POPF 
 XOR DL,BL
 
 OUTT_ALDISDHSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,DX
 JMP NAG7
NTAALDESW_RKM_YA_MEMRKM_YAERROR_PLUS:
MOV AL,[SI+7]
CMP AL,91
JZ MEMRKM_ORERROR_ALDES_PLUS

RKM_ORERROR_ALDES_PLUS:
MOV AL,[SI+8]
MOV VAR,BP
RET_NUM_COUNT  COMp+7
MOV BP,VAR
CMP CX,2
JA ERROR
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX 
 ALDESWRAKAMSOURCE_MOV:
 CMP BP,1
 JNZ ALDESWRAKAMSOURCE_ADD
POPF 
 MOV BL,DL
 JMP OUTT_ALDISRAKAMSOURCE
 ALDESWRAKAMSOURCE_ADD:
 CMP BP,2
 JNZ ALDESWRAKAMSOURCE_SUB 
POPF 
 ADD BL,DL
 JMP OUTT_ALDISRAKAMSOURCE
 ALDESWRAKAMSOURCE_SUB:
 CMP BP,3
 JNZ ALDESWRAKAMSOURCE_SBB
POPF 
 SUB BL,DL
 JMP OUTT_ALDISRAKAMSOURCE
 ALDESWRAKAMSOURCE_SBB:
 CMP BP,4
 JNZ ALDESWRAKAMSOURCE_ADC
POPF 
 SBB BL,DL
 JMP OUTT_ALDISRAKAMSOURCE
 ALDESWRAKAMSOURCE_ADC:
 CMP BP,5
 JNZ ALDESWRAKAMSOURCE_AND
POPF 
 ADC BL,DL
 JMP OUTT_ALDISRAKAMSOURCE
 ALDESWRAKAMSOURCE_AND:
 CMP BP,6
 JNZ ALDESWRAKAMSOURCE_XOR
 POPF 
 AND BL,DL
 JMP OUTT_ALDISRAKAMSOURCE
 ALDESWRAKAMSOURCE_XOR:
 POPF 
 XOR BL,DL
 
 OUTT_ALDISRAKAMSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,BX
 JMP NAG7
MEMRKM_ORERROR_ALDES_PLUS:
MOV AL,[SI+9]
CMP AL,108
JZ ERROR
CMP AL,104
JZ ERROR
MOV VAR,BP
RET_NUM_COUNT  COMp+8
MOV BP,VAR
CMP CX,4
JA ERROR
CMP DX,0FH
JA ERROR
MOV BX,DX
MOV CX,0
MOV CL,MEMORY1[BX]
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX 
 ALDESWMEMRAKAMSOURCE_MOV:
 CMP BP,1
 JNZ ALDESWMEMRAKAMSOURCE_ADD
 POPF 
 MOV BL,CL
 JMP OUTT_ALDISMEMRAKAMSOURCE
 ALDESWMEMRAKAMSOURCE_ADD:
 CMP BP,2
 JNZ ALDESWMEMRAKAMSOURCE_SUB 
 POPF 
 ADD BL,CL
 JMP OUTT_ALDISMEMRAKAMSOURCE
 ALDESWMEMRAKAMSOURCE_SUB:
 CMP BP,3
 JNZ ALDESWMEMRAKAMSOURCE_SBB
POPF 
 SUB BL,CL
 JMP OUTT_ALDISMEMRAKAMSOURCE
 ALDESWMEMRAKAMSOURCE_SBB:
 CMP BP,4
 JNZ ALDESWMEMRAKAMSOURCE_ADC
POPF 
 SBB BL,CL
 JMP OUTT_ALDISMEMRAKAMSOURCE
 ALDESWMEMRAKAMSOURCE_ADC:
 CMP BP,5
 JNZ ALDESWMEMRAKAMSOURCE_AND
POPF 
 ADC BL,CL
 JMP OUTT_ALDISMEMRAKAMSOURCE
 ALDESWMEMRAKAMSOURCE_AND:
 CMP BP,6
 JNZ ALDESWMEMRAKAMSOURCE_XOR
POPF 
 AND BL,CL
 JMP OUTT_ALDISMEMRAKAMSOURCE
 ALDESWMEMRAKAMSOURCE_XOR:
POPF 
 XOR BL,CL
 
 OUTT_ALDISMEMRAKAMSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,BX
 JMP NAG7
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTABLDIS_PLUS:
  MOV BX,0
  MOV BL,[SI+8]
  CMP BL,104
  JNAE NTABLDESW_RKM_YA_MEMRKM_YAERROR_PLUS
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+7] 
  MOV CL,[SI+8]
  ADD AX,CX
  CMP AX,217
  JZ ERROR
  CMP AX,218 
  JZ ERROR
  CMP AX,219
  JZ ERROR
  CMP AX,220
 JZ ERROR
 CMP AX,205
 JZ NTABLDESWDIORALLWDIERROR_PLUS
 CMP AX,201
 JZ NTABLDESWAHSOURCE_PLUS
 CMP AX,206
 JZ NTABLDESWBLSOURCE_PLUS
 CMP AX,202
 JZ NTABLDESWBHSOURCE_PLUS 
 CMP AX,207
 JZ NTABLDESWCLSOURCE_PLUS
 CMP AX,203
 JZ NTABLDESWCHSOURCE_PLUS
 CMP AX,208
 JZ NTABLDESWDLSOURCE_PLUS
 CMP AX,204
 JZ NTABLDESWDHSOURCE_PLUS 
 MOV BX,0
 MOV CX,0                              ;TAKECARE
 MOV CL,[SI+9]
 MOV BL,[SI+10]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ BLDES_WNTA_MEMBX_PLUS
 CMP AX,404              
 JZ NTA_BLDES_WMEMSI_PLUS
 CMP AX,389
 JZ NTA_BLDES_WMEMDI_PLUS
 JMP NTABLDESW_RKM_YA_MEMRKM_YAERROR_PLUS  
 
 
  
NTABLDESWDIORALLWDIERROR_PLUS: 
MOV AL,[SI+7]
CMP AL,100 
JZ ERROR
NTABLDESWALSOURCE_PLUS:
MOV BL,BYTE PTR RAX1
MOV CX,RBX1
MOV AX,FLAGS
PUSH AX 
 BLXDESWALSOURCE_MOV:
 CMP BP,1
 JNZ BLDESWALSOURCE_ADD
POPF 
 MOV CL,BL
 JMP OUTT_BLDISALSOURCE
 BLDESWALSOURCE_ADD:
 CMP BP,2
 JNZ BLDESWALSOURCE_SUB 
POPF 
 ADD CL,BL
 JMP OUTT_BLDISALSOURCE
 BLDESWALSOURCE_SUB:
 CMP BP,3
 JNZ BLDESWALSOURCE_SBB
POPF 
 SUB CL,BL
 JMP OUTT_BLDISALSOURCE
 BLDESWALSOURCE_SBB:
 CMP BP,4
 JNZ BLDESWALSOURCE_ADC
 POPF 
 SBB CL,BL
 JMP OUTT_BLDISALSOURCE
 BLDESWALSOURCE_ADC:
 CMP BP,5
 JNZ BLDESWALSOURCE_AND
 POPF 
 ADC CL,BL
 JMP OUTT_BLDISALSOURCE
 BLDESWALSOURCE_AND:
 CMP BP,6
 JNZ BLDESWALSOURCE_XOR
 POPF 
 AND CL,BL
 JMP OUTT_BLDISALSOURCE
 BLDESWALSOURCE_XOR:
POPF 
 XOR CL,BL
 
 OUTT_BLDISALSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,CX
 JMP NAG7

BLDES_WNTA_MEMBX_PLUS:
MOV BX,RBX1
CMP BX,0FH
JA ERROR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RBX1
MOV AX,FLAGS
PUSH AX
 BLDESWMEMBXSOURCE_MOV:
 CMP BP,1
 JNZ BLDESWMEMBXSOURCE_ADD
POPF 
 MOV BL,CL
 JMP OUTT_BLDISMEMBXSOURCE
 BLDESWMEMBXSOURCE_ADD:
 CMP BP,2
 JNZ BLDESWMEMBXSOURCE_SUB 
 POPF 
 ADD BL,CL
 JMP OUTT_BLDISMEMBXSOURCE
 BLDESWMEMBXSOURCE_SUB:
 CMP BP,3
 JNZ BLDESWMEMBXSOURCE_SBB
 POPF 
 SUB BL,CL
 JMP OUTT_BLDISMEMBXSOURCE
 BLDESWMEMBXSOURCE_SBB:
 CMP BP,4
 JNZ BLDESWMEMBXSOURCE_ADC
 POPF 
 SBB BL,CL
 JMP OUTT_BLDISMEMBXSOURCE
 BLDESWMEMBXSOURCE_ADC:
 CMP BP,5
 JNZ BLDESWMEMBXSOURCE_AND
 POPF 
 ADC BL,CL
 JMP OUTT_BLDISMEMBXSOURCE
 BLDESWMEMBXSOURCE_AND:
 CMP BP,6
 JNZ BLDESWMEMBXSOURCE_XOR
 POPF 
 AND BL,CL
 JMP OUTT_BLDISMEMBXSOURCE
 BLDESWMEMBXSOURCE_XOR:
 POPF 
 XOR BL,CL
 OUTT_BLDISMEMBXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,BX
 JMP NAG7
NTA_BLDES_WMEMSI_PLUS:
MOV BX,ISI1
CMP BX,0FH
JA ERROR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RBX1
MOV AX,FLAGS
PUSH AX
 BLDESWMEMSISOURCE_MOV:
 CMP BP,1
 JNZ BLDESWMEMSISOURCE_ADD
POPF 
 MOV BL,CL
 JMP OUTT_BLDISMEMSISOURCE
 BLDESWMEMSISOURCE_ADD:
 CMP BP,2
 JNZ BLDESWMEMSISOURCE_SUB 
 POPF 
 ADD BL,CL
 JMP OUTT_BLDISMEMSISOURCE
 BLDESWMEMSISOURCE_SUB:
 CMP BP,3
 JNZ BLDESWMEMSISOURCE_SBB
 POPF 
 SUB BL,CL
 JMP OUTT_BLDISMEMSISOURCE
 BLDESWMEMSISOURCE_SBB:
 CMP BP,4
 JNZ BLDESWMEMSISOURCE_ADC
 POPF 
 SBB BL,CL
 JMP OUTT_BLDISMEMSISOURCE
 BLDESWMEMSISOURCE_ADC:
 CMP BP,5
 JNZ BLDESWMEMSISOURCE_AND
 POPF 
 ADC BL,CL
 JMP OUTT_BLDISMEMSISOURCE
 BLDESWMEMSISOURCE_AND:
 CMP BP,6
 JNZ BLDESWMEMSISOURCE_XOR
POPF 
 AND BL,CL
 JMP OUTT_BLDISMEMSISOURCE
 BLDESWMEMSISOURCE_XOR:
POPF 
 XOR BL,CL
 
 OUTT_BLDISMEMSISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,BX
 JMP NAG7
NTA_BLDES_WMEMDI_PLUS:
MOV BX,IDI1
CMP BX,0FH
JA ERROR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RBX1
MOV AX,FLAGS
PUSH AX
 BLDESWMEMDISOURCE_MOV:
 CMP BP,1
 JNZ BLDESWMEMDISOURCE_ADD
 POPF 
 MOV BL,CL
 JMP OUTT_BLDISMEMDISOURCE
 BLDESWMEMDISOURCE_ADD:
 CMP BP,2
 JNZ BLDESWMEMDISOURCE_SUB 
 POPF 
 ADD BL,CL
 JMP OUTT_BLDISMEMDISOURCE
 BLDESWMEMDISOURCE_SUB:
 CMP BP,3
 JNZ BLDESWMEMDISOURCE_SBB
 POPF 
 SUB BL,CL
 JMP OUTT_BLDISMEMDISOURCE
 BLDESWMEMDISOURCE_SBB:
 CMP BP,4
 JNZ BLDESWMEMDISOURCE_ADC
POPF 
 SBB BL,CL
 JMP OUTT_BLDISMEMDISOURCE
 BLDESWMEMDISOURCE_ADC:
 CMP BP,5
 JNZ BLDESWMEMDISOURCE_AND
POPF 
 ADC BL,CL
 JMP OUTT_BLDISMEMDISOURCE
 BLDESWMEMDISOURCE_AND:
 CMP BP,6
 JNZ BLDESWMEMDISOURCE_XOR
POPF 
 AND BL,CL
 JMP OUTT_BLDISMEMDISOURCE
 BLDESWMEMDISOURCE_XOR:
 POPF 
 XOR BL,CL
 
 OUTT_BLDISMEMDISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,BX
 JMP NAG7
NTABLDESWAHSOURCE_PLUS:
MOV BL,BYTE PTR RAX1+1
MOV DX,RBX1
MOV AX,FLAGS
PUSH AX 
 BLDESWAHSOURCE_MOV:
 CMP BP,1
 JNZ BLDESWAHSOURCE_ADD
 POPF 
 MOV DL,BL
 JMP OUTT_BLDISAHSOURCE
 BLDESWAHSOURCE_ADD:
 CMP BP,2
 JNZ BLDESWAHSOURCE_SUB 
 POPF 
 ADD DL,BL
 JMP OUTT_BLDISAHSOURCE
 BLDESWAHSOURCE_SUB:
 CMP BP,3
 JNZ BLDESWAHSOURCE_SBB
 POPF 
 SUB DL,BL
 JMP OUTT_BLDISAHSOURCE
 BLDESWAHSOURCE_SBB:
 CMP BP,4
 JNZ BLDESWAHSOURCE_ADC
 POPF 
 SBB DL,BL
 JMP OUTT_BLDISAHSOURCE
 BLDESWAHSOURCE_ADC:
 CMP BP,5
 JNZ BLDESWAHSOURCE_AND
 POPF 
 ADC DL,BL
 JMP OUTT_BLDISAHSOURCE
 BLDESWAHSOURCE_AND:
 CMP BP,6
 JNZ BLDESWAHSOURCE_XOR
 POPF 
 AND DL,BL
 JMP OUTT_BLDISAHSOURCE
 BLDESWAHSOURCE_XOR:
 POPF 
 XOR DL,BL
 
 OUTT_BLDISAHSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,DX
 JMP NAG7
NTABLDESWBLSOURCE_PLUS:
MOV BL,BYTE PTR RBX1
MOV DX,RBX1
MOV AX,FLAGS
PUSH AX 
 BLXDESWBLSOURCE_MOV:
 CMP BP,1
 JNZ BLDESWBLSOURCE_ADD
POPF 
 MOV DL,BL
 JMP OUTT_BLDISBLSOURCE
 BLDESWBLSOURCE_ADD:
 CMP BP,2
 JNZ BLDESWBLSOURCE_SUB 
POPF 
 ADD DL,BL
 JMP OUTT_BLDISBLSOURCE
 BLDESWBLSOURCE_SUB:
 CMP BP,3
 JNZ BLDESWBLSOURCE_SBB
POPF 
 SUB DL,BL
 JMP OUTT_BLDISBLSOURCE
 BLDESWBLSOURCE_SBB:
 CMP BP,4
 JNZ BLDESWBLSOURCE_ADC
POPF 
 SBB DL,BL
 JMP OUTT_BLDISBLSOURCE
 BLDESWBLSOURCE_ADC:
 CMP BP,5
 JNZ BLDESWBLSOURCE_AND
POPF 
 ADC DL,BL
 JMP OUTT_BLDISBLSOURCE
 BLDESWBLSOURCE_AND:
 CMP BP,6
 JNZ BLDESWBLSOURCE_XOR
POPF 
 AND DL,BL
 JMP OUTT_BLDISBLSOURCE
 BLDESWBLSOURCE_XOR:
POPF 
 XOR DL,BL
 
 OUTT_BLDISBLSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,DX
 JMP NAG7
NTABLDESWBHSOURCE_PLUS:
MOV BL,BYTE PTR RBX1+1
MOV DX,RBX1
MOV AX,FLAGS
PUSH AX 
 BLXDESWBHSOURCE_MOV:
 CMP BP,1
 JNZ BLDESWBHSOURCE_ADD
 POPF 
 MOV DL,BL
 JMP OUTT_BLDISBHSOURCE
 BLDESWBHSOURCE_ADD:
 CMP BP,2
 JNZ BLDESWBHSOURCE_SUB 
 POPF 
 ADD DL,BL
 JMP OUTT_BLDISBHSOURCE
 BLDESWBHSOURCE_SUB:
 CMP BP,3
 JNZ BLDESWBHSOURCE_SBB
 POPF 
 SUB DL,BL
 JMP OUTT_BLDISBHSOURCE
 BLDESWBHSOURCE_SBB:
 CMP BP,4
 JNZ BLDESWBHSOURCE_ADC
 POPF 
 SBB DL,BL
 JMP OUTT_BLDISBHSOURCE
 BLDESWBHSOURCE_ADC:
 CMP BP,5
 JNZ BLDESWBHSOURCE_AND
 POPF 
 ADC DL,BL
 JMP OUTT_BLDISBHSOURCE
 BLDESWBHSOURCE_AND:
 CMP BP,6
 JNZ BLDESWBHSOURCE_XOR
 POPF 
 AND DL,BL
 JMP OUTT_BLDISBHSOURCE
 BLDESWBHSOURCE_XOR:
 POPF 
 XOR DL,BL
 
 OUTT_BLDISBHSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,DX
 JMP NAG7
NTABLDESWCLSOURCE_PLUS:
MOV BL,BYTE PTR RCX1
MOV DX,RBX1
MOV AX,FLAGS
PUSH AX 
 BLXDESWCLSOURCE_MOV:
 CMP BP,1
 JNZ BLDESWCLSOURCE_ADD
 POPF 
 MOV DL,BL
 JMP OUTT_BLDISCLSOURCE
 BLDESWCLSOURCE_ADD:
 CMP BP,2
 JNZ BLDESWCLSOURCE_SUB 
 POPF 
 ADD DL,BL
 JMP OUTT_BLDISCLSOURCE
 BLDESWCLSOURCE_SUB:
 CMP BP,3
 JNZ BLDESWCLSOURCE_SBB
 POPF 
 SUB DL,BL
 JMP OUTT_BLDISCLSOURCE
 BLDESWCLSOURCE_SBB:
 CMP BP,4
 JNZ BLDESWCLSOURCE_ADC
 POPF 
 SBB DL,BL
 JMP OUTT_BLDISCLSOURCE
 BLDESWCLSOURCE_ADC:
 CMP BP,5
 JNZ BLDESWCLSOURCE_AND
 POPF 
 ADC DL,BL
 JMP OUTT_BLDISCLSOURCE
 BLDESWCLSOURCE_AND:
 CMP BP,6
 JNZ BLDESWCLSOURCE_XOR
 POPF 
 AND DL,BL
 JMP OUTT_BLDISCLSOURCE
 BLDESWCLSOURCE_XOR:
 POPF 
 XOR DL,BL
 
 OUTT_BLDISCLSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,DX
 JMP NAG7
NTABLDESWCHSOURCE_PLUS:
MOV BL,BYTE PTR RCX1+1
MOV DX,RBX1
MOV AX,FLAGS
PUSH AX 
 BLXDESWCHSOURCE_MOV:
 CMP BP,1
 JNZ BLDESWCHSOURCE_ADD
 POPF 
 MOV DL,BL
 JMP OUTT_BLDISCHSOURCE
 BLDESWCHSOURCE_ADD:
 CMP BP,2
 JNZ BLDESWCHSOURCE_SUB 
 POPF 
 ADD DL,BL
 JMP OUTT_BLDISCHSOURCE
 BLDESWCHSOURCE_SUB:
 CMP BP,3
 JNZ BLDESWCHSOURCE_SBB
 POPF 
 SUB DL,BL
 JMP OUTT_BLDISCHSOURCE
 BLDESWCHSOURCE_SBB:
 CMP BP,4
 JNZ BLDESWCHSOURCE_ADC
 POPF 
 SBB DL,BL
 JMP OUTT_BLDISCHSOURCE
 BLDESWCHSOURCE_ADC:
 CMP BP,5
 JNZ BLDESWCHSOURCE_AND
 POPF 
 ADC DL,BL
 JMP OUTT_BLDISCHSOURCE
 BLDESWCHSOURCE_AND:
 CMP BP,6
 JNZ BLDESWCHSOURCE_XOR
 POPF 
 AND DL,BL
 JMP OUTT_BLDISCHSOURCE
 BLDESWCHSOURCE_XOR:
 POPF 
 XOR DL,BL
 
 OUTT_BLDISCHSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,DX
 JMP NAG7
NTABLDESWDLSOURCE_PLUS:
MOV BL,BYTE PTR RDX1
MOV DX,RBX1
MOV AX,FLAGS
PUSH AX 
 BLXDESWDLSOURCE_MOV:
 CMP BP,1
 JNZ BLDESWDLSOURCE_ADD
 POPF 
 MOV DL,BL
 JMP OUTT_BLDISDLSOURCE
 BLDESWDLSOURCE_ADD:
 CMP BP,2
 JNZ BLDESWDLSOURCE_SUB 
 POPF 
 ADD DL,BL
 JMP OUTT_BLDISDLSOURCE
 BLDESWDLSOURCE_SUB:
 CMP BP,3
 JNZ BLDESWDLSOURCE_SBB
 POPF 
 SUB DL,BL
 JMP OUTT_BLDISDLSOURCE
 BLDESWDLSOURCE_SBB:
 CMP BP,4
 JNZ BLDESWDLSOURCE_ADC
 POPF 
 SBB DL,BL
 JMP OUTT_BLDISDLSOURCE
 BLDESWDLSOURCE_ADC:
 CMP BP,5
 JNZ BLDESWDLSOURCE_AND
 POPF 
 ADC DL,BL
 JMP OUTT_BLDISDLSOURCE
 BLDESWDLSOURCE_AND:
 CMP BP,6
 JNZ BLDESWDLSOURCE_XOR
 POPF 
 AND DL,BL
 JMP OUTT_BLDISDLSOURCE
 BLDESWDLSOURCE_XOR:
 POPF 
 XOR DL,BL
 
 OUTT_BLDISDLSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,DX
 JMP NAG7
NTABLDESWDHSOURCE_PLUS:
MOV BL,BYTE PTR RDX1+1
MOV DX,RBX1
MOV AX,FLAGS
PUSH AX 
 BLXDESWDHSOURCE_MOV:
 CMP BP,1
 JNZ BLDESWDHSOURCE_ADD
POPF 
 MOV DL,BL
 JMP OUTT_BLDISDHSOURCE
 BLDESWDHSOURCE_ADD:
 CMP BP,2
 JNZ BLDESWDHSOURCE_SUB 
 POPF 
 ADD DL,BL
 JMP OUTT_BLDISDHSOURCE
 BLDESWDHSOURCE_SUB:
 CMP BP,3
 JNZ BLDESWDHSOURCE_SBB
POPF 
 SUB DL,BL
 JMP OUTT_BLDISDHSOURCE
 BLDESWDHSOURCE_SBB:
 CMP BP,4
 JNZ BLDESWDHSOURCE_ADC
 POPF 
 SBB DL,BL
 JMP OUTT_BLDISDHSOURCE
 BLDESWDHSOURCE_ADC:
 CMP BP,5
 JNZ BLDESWDHSOURCE_AND
 POPF 
 ADC DL,BL
 JMP OUTT_BLDISDHSOURCE
 BLDESWDHSOURCE_AND:
 CMP BP,6
 JNZ BLDESWDHSOURCE_XOR
 POPF 
 AND DL,BL
 JMP OUTT_BLDISDHSOURCE
 BLDESWDHSOURCE_XOR:
 POPF 
 XOR DL,BL
 
 OUTT_BLDISDHSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,DX
 JMP NAG7
NTABLDESW_RKM_YA_MEMRKM_YAERROR_PLUS:
MOV AL,[SI+7]
CMP AL,91
JZ MEMRKM_ORERROR_BLDES_PLUS

RKM_ORERROR_BLDES_PLUS:
MOV AL,[SI+8]
MOV VAR,BP
RET_NUM_COUNT  COMp+7
MOV BP,VAR
CMP CX,2
JA ERROR
MOV BX,RBX1
MOV AX,FLAGS
PUSH AX 
 BLDESWRAKAMSOURCE_MOV:
 CMP BP,1
 JNZ BLDESWRAKAMSOURCE_ADD
 POPF 
 MOV BL,DL
 JMP OUTT_BLDISRAKAMSOURCE
 BLDESWRAKAMSOURCE_ADD:
 CMP BP,2
 JNZ BLDESWRAKAMSOURCE_SUB 
 POPF 
 ADD BL,DL
 JMP OUTT_BLDISRAKAMSOURCE
 BLDESWRAKAMSOURCE_SUB:
 CMP BP,3
 JNZ BLDESWRAKAMSOURCE_SBB
 POPF 
 SUB BL,DL
 JMP OUTT_BLDISRAKAMSOURCE
 BLDESWRAKAMSOURCE_SBB:
 CMP BP,4
 JNZ BLDESWRAKAMSOURCE_ADC
 POPF 
 SBB BL,DL
 JMP OUTT_BLDISRAKAMSOURCE
 BLDESWRAKAMSOURCE_ADC:
 CMP BP,5
 JNZ BLDESWRAKAMSOURCE_AND
POPF 
 ADC BL,DL
 JMP OUTT_BLDISRAKAMSOURCE
 BLDESWRAKAMSOURCE_AND:
 CMP BP,6
 JNZ BLDESWRAKAMSOURCE_XOR
 POPF 
 AND BL,DL
 JMP OUTT_BLDISRAKAMSOURCE
 BLDESWRAKAMSOURCE_XOR:
 POPF 
 XOR BL,DL
 
 OUTT_BLDISRAKAMSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,BX
 JMP NAG7
MEMRKM_ORERROR_BLDES_PLUS:
MOV AL,[SI+9]
CMP AL,108
JZ ERROR
CMP AL,104
JZ ERROR
MOV VAR,BP
RET_NUM_COUNT  COMp+8
MOV BP,VAR
CMP CX,4
JA ERROR
CMP DX,0FH
JA ERROR
MOV BX,DX
MOV CX,0
MOV CL,MEMORY1[BX]
MOV BX,RBX1
MOV AX,FLAGS
PUSH AX 
 BLDESWMEMRAKAMSOURCE_MOV:
 CMP BP,1
 JNZ BLDESWMEMRAKAMSOURCE_ADD
 POPF 
 MOV BL,CL
 JMP OUTT_BLDISMEMRAKAMSOURCE
 BLDESWMEMRAKAMSOURCE_ADD:
 CMP BP,2
 JNZ BLDESWMEMRAKAMSOURCE_SUB 
POPF 
 ADD BL,CL
 JMP OUTT_BLDISMEMRAKAMSOURCE
 BLDESWMEMRAKAMSOURCE_SUB:
 CMP BP,3
 JNZ BLDESWMEMRAKAMSOURCE_SBB
POPF 
 SUB BL,CL
 JMP OUTT_BLDISMEMRAKAMSOURCE
 BLDESWMEMRAKAMSOURCE_SBB:
 CMP BP,4
 JNZ BLDESWMEMRAKAMSOURCE_ADC
POPF 
 SBB BL,CL
 JMP OUTT_BLDISMEMRAKAMSOURCE
 BLDESWMEMRAKAMSOURCE_ADC:
 CMP BP,5
 JNZ BLDESWMEMRAKAMSOURCE_AND
POPF 
 ADC BL,CL
 JMP OUTT_BLDISMEMRAKAMSOURCE
 BLDESWMEMRAKAMSOURCE_AND:
 CMP BP,6
 JNZ BLDESWMEMRAKAMSOURCE_XOR
POPF 
 AND BL,CL
 JMP OUTT_BLDISMEMRAKAMSOURCE
 BLDESWMEMRAKAMSOURCE_XOR:
POPF 
 XOR BL,CL
 
 OUTT_BLDISMEMRAKAMSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,BX
 JMP NAG7
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTACLDIS_PLUS:
MOV BX,0
  MOV BL,[SI+8]
  CMP BL,104
  JNAE NTACLDESW_RKM_YA_MEMRKM_YAERROR_PLUS
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+7] 
  MOV CL,[SI+8]
  ADD AX,CX
  CMP AX,217
  JZ ERROR
  CMP AX,218 
  JZ ERROR
  CMP AX,219
  JZ ERROR
  CMP AX,220
 JZ ERROR
 CMP AX,205
 JZ NTACLDESWDIORALLWDIERROR_PLUS
 CMP AX,201
 JZ NTACLDESWAHSOURCE_PLUS
 CMP AX,206
 JZ NTACLDESWBLSOURCE_PLUS
 CMP AX,202
 JZ NTACLDESWBHSOURCE_PLUS 
 CMP AX,207
 JZ NTACLDESWCLSOURCE_PLUS
 CMP AX,203
 JZ NTACLDESWCHSOURCE_PLUS
 CMP AX,208
 JZ NTACLDESWDLSOURCE_PLUS
 CMP AX,204
 JZ NTACLDESWDHSOURCE_PLUS 
 MOV BX,0
 MOV CX,0                              ;TAKECARE
 MOV CL,[SI+9]
 MOV BL,[SI+10]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ CLDES_WNTA_MEMBX_PLUS
 CMP AX,404              
 JZ NTA_CLDES_WMEMSI_PLUS
 CMP AX,389
 JZ NTA_CLDES_WMEMDI_PLUS
 JMP NTACLDESW_RKM_YA_MEMRKM_YAERROR_PLUS  
 
 
  
NTACLDESWDIORALLWDIERROR_PLUS: 
MOV AL,[SI+7]
CMP AL,100 
JZ ERROR
NTACLDESWALSOURCE_PLUS:
MOV BL,BYTE PTR RAX1
MOV CX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
 CLXDESWALSOURCE_MOV:
 CMP BP,1
 JNZ CLDESWALSOURCE_ADD
 MOV CL,BL
 JMP OUTT_CLDISALSOURCE
 CLDESWALSOURCE_ADD:
 CMP BP,2
 JNZ CLDESWALSOURCE_SUB 
 ADD CL,BL
 JMP OUTT_CLDISALSOURCE
 CLDESWALSOURCE_SUB:
 CMP BP,3
 JNZ CLDESWALSOURCE_SBB
 SUB CL,BL
 JMP OUTT_CLDISALSOURCE
 CLDESWALSOURCE_SBB:
 CMP BP,4
 JNZ CLDESWALSOURCE_ADC
 SBB CL,BL
 JMP OUTT_CLDISALSOURCE
 CLDESWALSOURCE_ADC:
 CMP BP,5
 JNZ CLDESWALSOURCE_AND
 ADC CL,BL
 JMP OUTT_CLDISALSOURCE
 CLDESWALSOURCE_AND:
 CMP BP,6
 JNZ CLDESWALSOURCE_XOR
 AND CL,BL
 JMP OUTT_CLDISALSOURCE
 CLDESWALSOURCE_XOR:
 XOR CL,BL
 
 OUTT_CLDISALSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,CX
 JMP NAG7

CLDES_WNTA_MEMBX_PLUS:
MOV BX,RBX1
CMP BX,0FH
JA ERROR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RCX1
MOV AX,FLAGS
PUSH AX
POPF
 CLDESWMEMBXSOURCE_MOV:
 CMP BP,1
 JNZ CLDESWMEMBXSOURCE_ADD
 MOV BL,CL
 JMP OUTT_CLDISMEMBXSOURCE
 CLDESWMEMBXSOURCE_ADD:
 CMP BP,2
 JNZ CLDESWMEMBXSOURCE_SUB 
 ADD BL,CL
 JMP OUTT_CLDISMEMBXSOURCE
 CLDESWMEMBXSOURCE_SUB:
 CMP BP,3
 JNZ CLDESWMEMBXSOURCE_SBB
 SUB BL,CL
 JMP OUTT_CLDISMEMBXSOURCE
 CLDESWMEMBXSOURCE_SBB:
 CMP BP,4
 JNZ CLDESWMEMBXSOURCE_ADC
 SBB BL,CL
 JMP OUTT_CLDISMEMBXSOURCE
 CLDESWMEMBXSOURCE_ADC:
 CMP BP,5
 JNZ CLDESWMEMBXSOURCE_AND
 ADC BL,CL
 JMP OUTT_CLDISMEMBXSOURCE
 CLDESWMEMBXSOURCE_AND:
 CMP BP,6
 JNZ CLDESWMEMBXSOURCE_XOR
 AND BL,CL
 JMP OUTT_CLDISMEMBXSOURCE
 CLDESWMEMBXSOURCE_XOR:
 XOR BL,CL
 
 OUTT_CLDISMEMBXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,BX
 JMP NAG7
NTA_CLDES_WMEMSI_PLUS:
MOV BX,ISI1
CMP BX,0FH
JA ERROR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RCX1
MOV AX,FLAGS
PUSH AX
POPF
 CLDESWMEMSISOURCE_MOV:
 CMP BP,1
 JNZ CLDESWMEMSISOURCE_ADD
 MOV BL,CL
 JMP OUTT_CLDISMEMSISOURCE
 CLDESWMEMSISOURCE_ADD:
 CMP BP,2
 JNZ CLDESWMEMSISOURCE_SUB 
 ADD BL,CL
 JMP OUTT_CLDISMEMSISOURCE
 CLDESWMEMSISOURCE_SUB:
 CMP BP,3
 JNZ CLDESWMEMSISOURCE_SBB
 SUB BL,CL
 JMP OUTT_CLDISMEMSISOURCE
 CLDESWMEMSISOURCE_SBB:
 CMP BP,4
 JNZ CLDESWMEMSISOURCE_ADC
 SBB BL,CL
 JMP OUTT_CLDISMEMSISOURCE
 CLDESWMEMSISOURCE_ADC:
 CMP BP,5
 JNZ CLDESWMEMSISOURCE_AND
 ADC BL,CL
 JMP OUTT_CLDISMEMSISOURCE
 CLDESWMEMSISOURCE_AND:
 CMP BP,6
 JNZ CLDESWMEMSISOURCE_XOR
 AND BL,CL
 JMP OUTT_CLDISMEMSISOURCE
 CLDESWMEMSISOURCE_XOR:
 XOR BL,CL
 
 OUTT_CLDISMEMSISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,BX
 JMP NAG7
NTA_CLDES_WMEMDI_PLUS:
MOV BX,IDI1
CMP BX,0FH
JA ERROR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RCX1
MOV AX,FLAGS
PUSH AX
POPF
 CLDESWMEMDISOURCE_MOV:
 CMP BP,1
 JNZ CLDESWMEMDISOURCE_ADD
 MOV BL,CL
 JMP OUTT_CLDISMEMDISOURCE
 CLDESWMEMDISOURCE_ADD:
 CMP BP,2
 JNZ CLDESWMEMDISOURCE_SUB 
 ADD BL,CL
 JMP OUTT_CLDISMEMDISOURCE
 CLDESWMEMDISOURCE_SUB:
 CMP BP,3
 JNZ CLDESWMEMDISOURCE_SBB
 SUB BL,CL
 JMP OUTT_CLDISMEMDISOURCE
 CLDESWMEMDISOURCE_SBB:
 CMP BP,4
 JNZ CLDESWMEMDISOURCE_ADC
 SBB BL,CL
 JMP OUTT_CLDISMEMDISOURCE
 CLDESWMEMDISOURCE_ADC:
 CMP BP,5
 JNZ CLDESWMEMDISOURCE_AND
 ADC BL,CL
 JMP OUTT_CLDISMEMDISOURCE
 CLDESWMEMDISOURCE_AND:
 CMP BP,6
 JNZ CLDESWMEMDISOURCE_XOR
 AND BL,CL
 JMP OUTT_CLDISMEMDISOURCE
 CLDESWMEMDISOURCE_XOR:
 XOR BL,CL
 
 OUTT_CLDISMEMDISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,BX
 JMP NAG7
NTACLDESWAHSOURCE_PLUS:
MOV BL,BYTE PTR RAX1+1
MOV DX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
 CLDESWAHSOURCE_MOV:
 CMP BP,1
 JNZ CLDESWAHSOURCE_ADD
 MOV DL,BL
 JMP OUTT_CLDISAHSOURCE
 CLDESWAHSOURCE_ADD:
 CMP BP,2
 JNZ CLDESWAHSOURCE_SUB 
 ADD DL,BL
 JMP OUTT_CLDISAHSOURCE
 CLDESWAHSOURCE_SUB:
 CMP BP,3
 JNZ CLDESWAHSOURCE_SBB
 SUB DL,BL
 JMP OUTT_CLDISAHSOURCE
 CLDESWAHSOURCE_SBB:
 CMP BP,4
 JNZ CLDESWAHSOURCE_ADC
 SBB DL,BL
 JMP OUTT_CLDISAHSOURCE
 CLDESWAHSOURCE_ADC:
 CMP BP,5
 JNZ CLDESWAHSOURCE_AND
 ADC DL,BL
 JMP OUTT_CLDISAHSOURCE
 CLDESWAHSOURCE_AND:
 CMP BP,6
 JNZ CLDESWAHSOURCE_XOR
 AND DL,BL
 JMP OUTT_CLDISAHSOURCE
 CLDESWAHSOURCE_XOR:
 XOR DL,BL
 
 OUTT_CLDISAHSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,DX
 JMP NAG7
NTACLDESWBLSOURCE_PLUS:
MOV BL,BYTE PTR RBX1
MOV DX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
 CLXDESWBLSOURCE_MOV:
 CMP BP,1
 JNZ CLDESWBLSOURCE_ADD
 MOV DL,BL
 JMP OUTT_CLDISBLSOURCE
 CLDESWBLSOURCE_ADD:
 CMP BP,2
 JNZ CLDESWBLSOURCE_SUB 
 ADD DL,BL
 JMP OUTT_CLDISBLSOURCE
 CLDESWBLSOURCE_SUB:
 CMP BP,3
 JNZ CLDESWBLSOURCE_SBB
 SUB DL,BL
 JMP OUTT_CLDISBLSOURCE
 CLDESWBLSOURCE_SBB:
 CMP BP,4
 JNZ CLDESWBLSOURCE_ADC
 SBB DL,BL
 JMP OUTT_CLDISBLSOURCE
 CLDESWBLSOURCE_ADC:
 CMP BP,5
 JNZ CLDESWBLSOURCE_AND
 ADC DL,BL
 JMP OUTT_CLDISBLSOURCE
 CLDESWBLSOURCE_AND:
 CMP BP,6
 JNZ CLDESWBLSOURCE_XOR
 AND DL,BL
 JMP OUTT_CLDISBLSOURCE
 CLDESWBLSOURCE_XOR:
 XOR DL,BL
 
 OUTT_CLDISBLSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,DX
 JMP NAG7
NTACLDESWBHSOURCE_PLUS:
MOV BL,BYTE PTR RBX1+1
MOV DX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
 CLXDESWBHSOURCE_MOV:
 CMP BP,1
 JNZ CLDESWBHSOURCE_ADD
 MOV DL,BL
 JMP OUTT_CLDISBHSOURCE
 CLDESWBHSOURCE_ADD:
 CMP BP,2
 JNZ CLDESWBHSOURCE_SUB 
 ADD DL,BL
 JMP OUTT_CLDISBHSOURCE
 CLDESWBHSOURCE_SUB:
 CMP BP,3
 JNZ CLDESWBHSOURCE_SBB
 SUB DL,BL
 JMP OUTT_CLDISBHSOURCE
 CLDESWBHSOURCE_SBB:
 CMP BP,4
 JNZ CLDESWBHSOURCE_ADC
 SBB DL,BL
 JMP OUTT_CLDISBHSOURCE
 CLDESWBHSOURCE_ADC:
 CMP BP,5
 JNZ CLDESWBHSOURCE_AND
 ADC DL,BL
 JMP OUTT_CLDISBHSOURCE
 CLDESWBHSOURCE_AND:
 CMP BP,6
 JNZ CLDESWBHSOURCE_XOR
 AND DL,BL
 JMP OUTT_CLDISBHSOURCE
 CLDESWBHSOURCE_XOR:
 XOR DL,BL
 
 OUTT_CLDISBHSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,DX
 JMP NAG7
NTACLDESWCLSOURCE_PLUS:
MOV BL,BYTE PTR RCX1
MOV DX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
 CLXDESWCLSOURCE_MOV:
 CMP BP,1
 JNZ CLDESWCLSOURCE_ADD
 MOV DL,BL
 JMP OUTT_CLDISCLSOURCE
 CLDESWCLSOURCE_ADD:
 CMP BP,2
 JNZ CLDESWCLSOURCE_SUB 
 ADD DL,BL
 JMP OUTT_CLDISCLSOURCE
 CLDESWCLSOURCE_SUB:
 CMP BP,3
 JNZ CLDESWCLSOURCE_SBB
 SUB DL,BL
 JMP OUTT_CLDISCLSOURCE
 CLDESWCLSOURCE_SBB:
 CMP BP,4
 JNZ CLDESWCLSOURCE_ADC
 SBB DL,BL
 JMP OUTT_CLDISCLSOURCE
 CLDESWCLSOURCE_ADC:
 CMP BP,5
 JNZ CLDESWCLSOURCE_AND
 ADC DL,BL
 JMP OUTT_CLDISCLSOURCE
 CLDESWCLSOURCE_AND:
 CMP BP,6
 JNZ CLDESWCLSOURCE_XOR
 AND DL,BL
 JMP OUTT_CLDISCLSOURCE
 CLDESWCLSOURCE_XOR:
 XOR DL,BL
 
 OUTT_CLDISCLSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,DX
 JMP NAG7
NTACLDESWCHSOURCE_PLUS:
MOV BL,BYTE PTR RCX1+1
MOV DX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
 CLXDESWCHSOURCE_MOV:
 CMP BP,1
 JNZ CLDESWCHSOURCE_ADD
 MOV DL,BL
 JMP OUTT_CLDISCHSOURCE
 CLDESWCHSOURCE_ADD:
 CMP BP,2
 JNZ CLDESWCHSOURCE_SUB 
 ADD DL,BL
 JMP OUTT_CLDISCHSOURCE
 CLDESWCHSOURCE_SUB:
 CMP BP,3
 JNZ CLDESWCHSOURCE_SBB
 SUB DL,BL
 JMP OUTT_CLDISCHSOURCE
 CLDESWCHSOURCE_SBB:
 CMP BP,4
 JNZ CLDESWCHSOURCE_ADC
 SBB DL,BL
 JMP OUTT_CLDISCHSOURCE
 CLDESWCHSOURCE_ADC:
 CMP BP,5
 JNZ CLDESWCHSOURCE_AND
 ADC DL,BL
 JMP OUTT_CLDISCHSOURCE
 CLDESWCHSOURCE_AND:
 CMP BP,6
 JNZ CLDESWCHSOURCE_XOR
 AND DL,BL
 JMP OUTT_CLDISCHSOURCE
 CLDESWCHSOURCE_XOR:
 XOR DL,BL
 
 OUTT_CLDISCHSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,DX
 JMP NAG7
NTACLDESWDLSOURCE_PLUS:
MOV BL,BYTE PTR RDX1
MOV DX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
 CLXDESWDLSOURCE_MOV:
 CMP BP,1
 JNZ CLDESWDLSOURCE_ADD
 MOV DL,BL
 JMP OUTT_CLDISDLSOURCE
 CLDESWDLSOURCE_ADD:
 CMP BP,2
 JNZ CLDESWDLSOURCE_SUB 
 ADD DL,BL
 JMP OUTT_CLDISDLSOURCE
 CLDESWDLSOURCE_SUB:
 CMP BP,3
 JNZ CLDESWDLSOURCE_SBB
 SUB DL,BL
 JMP OUTT_CLDISDLSOURCE
 CLDESWDLSOURCE_SBB:
 CMP BP,4
 JNZ CLDESWDLSOURCE_ADC
 SBB DL,BL
 JMP OUTT_CLDISDLSOURCE
 CLDESWDLSOURCE_ADC:
 CMP BP,5
 JNZ CLDESWDLSOURCE_AND
 ADC DL,BL
 JMP OUTT_CLDISDLSOURCE
 CLDESWDLSOURCE_AND:
 CMP BP,6
 JNZ CLDESWDLSOURCE_XOR
 AND DL,BL
 JMP OUTT_CLDISDLSOURCE
 CLDESWDLSOURCE_XOR:
 XOR DL,BL
 
 OUTT_CLDISDLSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,DX
 JMP NAG7
NTACLDESWDHSOURCE_PLUS:
MOV BL,BYTE PTR RDX1+1
MOV DX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
 CLXDESWDHSOURCE_MOV:
 CMP BP,1
 JNZ CLDESWDHSOURCE_ADD
 MOV DL,BL
 JMP OUTT_CLDISDHSOURCE
 CLDESWDHSOURCE_ADD:
 CMP BP,2
 JNZ CLDESWDHSOURCE_SUB 
 ADD DL,BL
 JMP OUTT_CLDISDHSOURCE
 CLDESWDHSOURCE_SUB:
 CMP BP,3
 JNZ CLDESWDHSOURCE_SBB
 SUB DL,BL
 JMP OUTT_CLDISDHSOURCE
 CLDESWDHSOURCE_SBB:
 CMP BP,4
 JNZ CLDESWDHSOURCE_ADC
 SBB DL,BL
 JMP OUTT_CLDISDHSOURCE
 CLDESWDHSOURCE_ADC:
 CMP BP,5
 JNZ CLDESWDHSOURCE_AND
 ADC DL,BL
 JMP OUTT_CLDISDHSOURCE
 CLDESWDHSOURCE_AND:
 CMP BP,6
 JNZ CLDESWDHSOURCE_XOR
 AND DL,BL
 JMP OUTT_CLDISDHSOURCE
 CLDESWDHSOURCE_XOR:
 XOR DL,BL
 
 OUTT_CLDISDHSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,DX
 JMP NAG7
NTACLDESW_RKM_YA_MEMRKM_YAERROR_PLUS:
MOV AL,[SI+7]
CMP AL,91
JZ MEMRKM_ORERROR_CLDES_PLUS

RKM_ORERROR_CLDES_PLUS:
MOV AL,[SI+8]
MOV VAR,BP
RET_NUM_COUNT  COMp+7
MOV BP,VAR
CMP CX,2
JA ERROR
MOV BX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
 CLDESWRAKAMSOURCE_MOV:
 CMP BP,1
 JNZ CLDESWRAKAMSOURCE_ADD
 MOV BL,DL
 JMP OUTT_CLDISRAKAMSOURCE
 CLDESWRAKAMSOURCE_ADD:
 CMP BP,2
 JNZ CLDESWRAKAMSOURCE_SUB 
 ADD BL,DL
 JMP OUTT_CLDISRAKAMSOURCE
 CLDESWRAKAMSOURCE_SUB:
 CMP BP,3
 JNZ CLDESWRAKAMSOURCE_SBB
 SUB BL,DL
 JMP OUTT_CLDISRAKAMSOURCE
 CLDESWRAKAMSOURCE_SBB:
 CMP BP,4
 JNZ CLDESWRAKAMSOURCE_ADC
 SBB BL,DL
 JMP OUTT_CLDISRAKAMSOURCE
 CLDESWRAKAMSOURCE_ADC:
 CMP BP,5
 JNZ CLDESWRAKAMSOURCE_AND
 ADC BL,DL
 JMP OUTT_CLDISRAKAMSOURCE
 CLDESWRAKAMSOURCE_AND:
 CMP BP,6
 JNZ CLDESWRAKAMSOURCE_XOR
 AND BL,DL
 JMP OUTT_CLDISRAKAMSOURCE
 CLDESWRAKAMSOURCE_XOR:
 XOR BL,DL
 
 OUTT_CLDISRAKAMSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,BX
 JMP NAG7
MEMRKM_ORERROR_CLDES_PLUS:
MOV AL,[SI+9]
CMP AL,108
JZ ERROR
CMP AL,104
JZ ERROR
MOV VAR,BP
RET_NUM_COUNT  COMp+8
MOV BP,VAR
CMP CX,4
JA ERROR
CMP DX,0FH
JA ERROR
MOV BX,DX
MOV CX,0
MOV CL,MEMORY1[BX]
MOV BX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
 CLDESWMEMRAKAMSOURCE_MOV:
 CMP BP,1
 JNZ CLDESWMEMRAKAMSOURCE_ADD
 MOV BL,CL
 JMP OUTT_CLDISMEMRAKAMSOURCE
 CLDESWMEMRAKAMSOURCE_ADD:
 CMP BP,2
 JNZ CLDESWMEMRAKAMSOURCE_SUB 
 ADD BL,CL
 JMP OUTT_CLDISMEMRAKAMSOURCE
 CLDESWMEMRAKAMSOURCE_SUB:
 CMP BP,3
 JNZ CLDESWMEMRAKAMSOURCE_SBB
 SUB BL,CL
 JMP OUTT_CLDISMEMRAKAMSOURCE
 CLDESWMEMRAKAMSOURCE_SBB:
 CMP BP,4
 JNZ CLDESWMEMRAKAMSOURCE_ADC
 SBB BL,CL
 JMP OUTT_CLDISMEMRAKAMSOURCE
 CLDESWMEMRAKAMSOURCE_ADC:
 CMP BP,5
 JNZ CLDESWMEMRAKAMSOURCE_AND
 ADC BL,CL
 JMP OUTT_CLDISMEMRAKAMSOURCE
 CLDESWMEMRAKAMSOURCE_AND:
 CMP BP,6
 JNZ CLDESWMEMRAKAMSOURCE_XOR
 AND BL,CL
 JMP OUTT_CLDISMEMRAKAMSOURCE
 CLDESWMEMRAKAMSOURCE_XOR:
 XOR BL,CL
 
 OUTT_CLDISMEMRAKAMSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,BX
 JMP NAG7       
       
NTADLDIS_PLUS:
 MOV BX,0
  MOV BL,[SI+8]
  CMP BL,104
  JNAE NTADLDESW_RKM_YA_MEMRKM_YAERROR_PLUS
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+7] 
  MOV CL,[SI+8]
  ADD AX,CX
  CMP AX,217
  JZ ERROR
  CMP AX,218 
  JZ ERROR
  CMP AX,219
  JZ ERROR
  CMP AX,220
 JZ ERROR
 CMP AX,205
 JZ NTADLDESWDIORALLWDIERROR_PLUS
 CMP AX,201
 JZ NTADLDESWAHSOURCE_PLUS
 CMP AX,206
 JZ NTADLDESWBLSOURCE_PLUS
 CMP AX,202
 JZ NTADLDESWBHSOURCE_PLUS 
 CMP AX,207
 JZ NTADLDESWCLSOURCE_PLUS
 CMP AX,203
 JZ NTADLDESWCHSOURCE_PLUS
 CMP AX,208
 JZ NTADLDESWDLSOURCE_PLUS
 CMP AX,204
 JZ NTADLDESWDHSOURCE_PLUS 
 MOV BX,0
 MOV CX,0                              ;TAKECARE
 MOV CL,[SI+9]
 MOV BL,[SI+10]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ DLDES_WNTA_MEMBX_PLUS
 CMP AX,404              
 JZ NTA_DLDES_WMEMSI_PLUS
 CMP AX,389
 JZ NTA_DLDES_WMEMDI_PLUS
 JMP NTADLDESW_RKM_YA_MEMRKM_YAERROR_PLUS  
 
 
  
NTADLDESWDIORALLWDIERROR_PLUS: 
MOV AL,[SI+7]
CMP AL,100 
JZ ERROR
NTADLDESWALSOURCE_PLUS:
MOV BL,BYTE PTR RAX1
MOV CX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
 DLXDESWALSOURCE_MOV:
 CMP BP,1
 JNZ DLDESWALSOURCE_ADD
 MOV CL,BL
 JMP OUTT_DLDISALSOURCE
 DLDESWALSOURCE_ADD:
 CMP BP,2
 JNZ DLDESWALSOURCE_SUB 
 ADD CL,BL
 JMP OUTT_DLDISALSOURCE
 DLDESWALSOURCE_SUB:
 CMP BP,3
 JNZ DLDESWALSOURCE_SBB
 SUB CL,BL
 JMP OUTT_DLDISALSOURCE
 DLDESWALSOURCE_SBB:
 CMP BP,4
 JNZ DLDESWALSOURCE_ADC
 SBB CL,BL
 JMP OUTT_DLDISALSOURCE
 DLDESWALSOURCE_ADC:
 CMP BP,5
 JNZ DLDESWALSOURCE_AND
 ADC CL,BL
 JMP OUTT_DLDISALSOURCE
 DLDESWALSOURCE_AND:
 CMP BP,6
 JNZ DLDESWALSOURCE_XOR
 AND CL,BL
 JMP OUTT_DLDISALSOURCE
 DLDESWALSOURCE_XOR:
 XOR CL,BL
 
 OUTT_DLDISALSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,CX
 JMP NAG7

DLDES_WNTA_MEMBX_PLUS:
MOV BX,RBX1
CMP BX,0FH
JA ERROR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RDX1
MOV AX,FLAGS
PUSH AX
POPF
 DLDESWMEMBXSOURCE_MOV:
 CMP BP,1
 JNZ DLDESWMEMBXSOURCE_ADD
 MOV BL,CL
 JMP OUTT_DLDISMEMBXSOURCE
 DLDESWMEMBXSOURCE_ADD:
 CMP BP,2
 JNZ DLDESWMEMBXSOURCE_SUB 
 ADD BL,CL
 JMP OUTT_DLDISMEMBXSOURCE
 DLDESWMEMBXSOURCE_SUB:
 CMP BP,3
 JNZ DLDESWMEMBXSOURCE_SBB
 SUB BL,CL
 JMP OUTT_DLDISMEMBXSOURCE
 DLDESWMEMBXSOURCE_SBB:
 CMP BP,4
 JNZ DLDESWMEMBXSOURCE_ADC
 SBB BL,CL
 JMP OUTT_DLDISMEMBXSOURCE
 DLDESWMEMBXSOURCE_ADC:
 CMP BP,5
 JNZ DLDESWMEMBXSOURCE_AND
 ADC BL,CL
 JMP OUTT_DLDISMEMBXSOURCE
 DLDESWMEMBXSOURCE_AND:
 CMP BP,6
 JNZ DLDESWMEMBXSOURCE_XOR
 AND BL,CL
 JMP OUTT_DLDISMEMBXSOURCE
 DLDESWMEMBXSOURCE_XOR:
 XOR BL,CL
 
 OUTT_DLDISMEMBXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,BX
 JMP NAG7
NTA_DLDES_WMEMSI_PLUS:
MOV BX,ISI1
CMP BX,0FH
JA ERROR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RDX1
MOV AX,FLAGS
PUSH AX
POPF
 DLDESWMEMSISOURCE_MOV:
 CMP BP,1
 JNZ DLDESWMEMSISOURCE_ADD
 MOV BL,CL
 JMP OUTT_DLDISMEMSISOURCE
 DLDESWMEMSISOURCE_ADD:
 CMP BP,2
 JNZ DLDESWMEMSISOURCE_SUB 
 ADD BL,CL
 JMP OUTT_DLDISMEMSISOURCE
 DLDESWMEMSISOURCE_SUB:
 CMP BP,3
 JNZ DLDESWMEMSISOURCE_SBB
 SUB BL,CL
 JMP OUTT_DLDISMEMSISOURCE
 DLDESWMEMSISOURCE_SBB:
 CMP BP,4
 JNZ DLDESWMEMSISOURCE_ADC
 SBB BL,CL
 JMP OUTT_DLDISMEMSISOURCE
 DLDESWMEMSISOURCE_ADC:
 CMP BP,5
 JNZ DLDESWMEMSISOURCE_AND
 ADC BL,CL
 JMP OUTT_DLDISMEMSISOURCE
 DLDESWMEMSISOURCE_AND:
 CMP BP,6
 JNZ DLDESWMEMSISOURCE_XOR
 AND BL,CL
 JMP OUTT_DLDISMEMSISOURCE
 DLDESWMEMSISOURCE_XOR:
 XOR BL,CL
 
 OUTT_DLDISMEMSISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,BX
 JMP NAG7
NTA_DLDES_WMEMDI_PLUS:
MOV BX,IDI1
CMP BX,0FH
JA ERROR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RDX1
MOV AX,FLAGS
PUSH AX
POPF
 DLDESWMEMDISOURCE_MOV:
 CMP BP,1
 JNZ DLDESWMEMDISOURCE_ADD
 MOV BL,CL
 JMP OUTT_DLDISMEMDISOURCE
 DLDESWMEMDISOURCE_ADD:
 CMP BP,2
 JNZ DLDESWMEMDISOURCE_SUB 
 ADD BL,CL
 JMP OUTT_DLDISMEMDISOURCE
 DLDESWMEMDISOURCE_SUB:
 CMP BP,3
 JNZ DLDESWMEMDISOURCE_SBB
 SUB BL,CL
 JMP OUTT_DLDISMEMDISOURCE
 DLDESWMEMDISOURCE_SBB:
 CMP BP,4
 JNZ DLDESWMEMDISOURCE_ADC
 SBB BL,CL
 JMP OUTT_DLDISMEMDISOURCE
 DLDESWMEMDISOURCE_ADC:
 CMP BP,5
 JNZ DLDESWMEMDISOURCE_AND
 ADC BL,CL
 JMP OUTT_DLDISMEMDISOURCE
 DLDESWMEMDISOURCE_AND:
 CMP BP,6
 JNZ DLDESWMEMDISOURCE_XOR
 AND BL,CL
 JMP OUTT_DLDISMEMDISOURCE
 DLDESWMEMDISOURCE_XOR:
 XOR BL,CL
 
 OUTT_DLDISMEMDISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,BX
 JMP NAG7
NTADLDESWAHSOURCE_PLUS:
MOV BL,BYTE PTR RAX1+1
MOV DX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
 DLDESWAHSOURCE_MOV:
 CMP BP,1
 JNZ DLDESWAHSOURCE_ADD
 MOV DL,BL
 JMP OUTT_DLDISAHSOURCE
 DLDESWAHSOURCE_ADD:
 CMP BP,2
 JNZ DLDESWAHSOURCE_SUB 
 ADD DL,BL
 JMP OUTT_DLDISAHSOURCE
 DLDESWAHSOURCE_SUB:
 CMP BP,3
 JNZ DLDESWAHSOURCE_SBB
 SUB DL,BL
 JMP OUTT_DLDISAHSOURCE
 DLDESWAHSOURCE_SBB:
 CMP BP,4
 JNZ DLDESWAHSOURCE_ADC
 SBB DL,BL
 JMP OUTT_DLDISAHSOURCE
 DLDESWAHSOURCE_ADC:
 CMP BP,5
 JNZ DLDESWAHSOURCE_AND
 ADC DL,BL
 JMP OUTT_DLDISAHSOURCE
 DLDESWAHSOURCE_AND:
 CMP BP,6
 JNZ DLDESWAHSOURCE_XOR
 AND DL,BL
 JMP OUTT_DLDISAHSOURCE
 DLDESWAHSOURCE_XOR:
 XOR DL,BL
 
 OUTT_DLDISAHSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,DX
 JMP NAG7
NTADLDESWBLSOURCE_PLUS:
MOV BL,BYTE PTR RBX1
MOV DX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
 DLXDESWBLSOURCE_MOV:
 CMP BP,1
 JNZ DLDESWBLSOURCE_ADD
 MOV DL,BL
 JMP OUTT_DLDISBLSOURCE
 DLDESWBLSOURCE_ADD:
 CMP BP,2
 JNZ DLDESWBLSOURCE_SUB 
 ADD DL,BL
 JMP OUTT_DLDISBLSOURCE
 DLDESWBLSOURCE_SUB:
 CMP BP,3
 JNZ DLDESWBLSOURCE_SBB
 SUB DL,BL
 JMP OUTT_DLDISBLSOURCE
 DLDESWBLSOURCE_SBB:
 CMP BP,4
 JNZ DLDESWBLSOURCE_ADC
 SBB DL,BL
 JMP OUTT_DLDISBLSOURCE
 DLDESWBLSOURCE_ADC:
 CMP BP,5
 JNZ DLDESWBLSOURCE_AND
 ADC DL,BL
 JMP OUTT_DLDISBLSOURCE
 DLDESWBLSOURCE_AND:
 CMP BP,6
 JNZ DLDESWBLSOURCE_XOR
 AND DL,BL
 JMP OUTT_DLDISBLSOURCE
 DLDESWBLSOURCE_XOR:
 XOR DL,BL
 
 OUTT_DLDISBLSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,DX
 JMP NAG7
NTADLDESWBHSOURCE_PLUS:
MOV BL,BYTE PTR RBX1+1
MOV DX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
 DLXDESWBHSOURCE_MOV:
 CMP BP,1
 JNZ DLDESWBHSOURCE_ADD
 MOV DL,BL
 JMP OUTT_DLDISBHSOURCE
 DLDESWBHSOURCE_ADD:
 CMP BP,2
 JNZ DLDESWBHSOURCE_SUB 
 ADD DL,BL
 JMP OUTT_DLDISBHSOURCE
 DLDESWBHSOURCE_SUB:
 CMP BP,3
 JNZ DLDESWBHSOURCE_SBB
 SUB DL,BL
 JMP OUTT_DLDISBHSOURCE
 DLDESWBHSOURCE_SBB:
 CMP BP,4
 JNZ DLDESWBHSOURCE_ADC
 SBB DL,BL
 JMP OUTT_DLDISBHSOURCE
 DLDESWBHSOURCE_ADC:
 CMP BP,5
 JNZ DLDESWBHSOURCE_AND
 ADC DL,BL
 JMP OUTT_DLDISBHSOURCE
 DLDESWBHSOURCE_AND:
 CMP BP,6
 JNZ DLDESWBHSOURCE_XOR
 AND DL,BL
 JMP OUTT_DLDISBHSOURCE
 DLDESWBHSOURCE_XOR:
 XOR DL,BL
 
 OUTT_DLDISBHSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,DX
 JMP NAG7
NTADLDESWCLSOURCE_PLUS:
MOV BL,BYTE PTR RCX1
MOV DX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
 DLXDESWCLSOURCE_MOV:
 CMP BP,1
 JNZ DLDESWCLSOURCE_ADD
 MOV DL,BL
 JMP OUTT_DLDISCLSOURCE
 DLDESWCLSOURCE_ADD:
 CMP BP,2
 JNZ DLDESWCLSOURCE_SUB 
 ADD DL,BL
 JMP OUTT_DLDISCLSOURCE
 DLDESWCLSOURCE_SUB:
 CMP BP,3
 JNZ DLDESWCLSOURCE_SBB
 SUB DL,BL
 JMP OUTT_DLDISCLSOURCE
 DLDESWCLSOURCE_SBB:
 CMP BP,4
 JNZ DLDESWCLSOURCE_ADC
 SBB DL,BL
 JMP OUTT_DLDISCLSOURCE
 DLDESWCLSOURCE_ADC:
 CMP BP,5
 JNZ DLDESWCLSOURCE_AND
 ADC DL,BL
 JMP OUTT_DLDISCLSOURCE
 DLDESWCLSOURCE_AND:
 CMP BP,6
 JNZ DLDESWCLSOURCE_XOR
 AND DL,BL
 JMP OUTT_DLDISCLSOURCE
 DLDESWCLSOURCE_XOR:
 XOR DL,BL
 
 OUTT_DLDISCLSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,DX
 JMP NAG7
NTADLDESWCHSOURCE_PLUS:
MOV BL,BYTE PTR RCX1+1
MOV DX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
 DLXDESWCHSOURCE_MOV:
 CMP BP,1
 JNZ DLDESWCHSOURCE_ADD
 MOV DL,BL
 JMP OUTT_DLDISCHSOURCE
 DLDESWCHSOURCE_ADD:
 CMP BP,2
 JNZ DLDESWCHSOURCE_SUB 
 ADD DL,BL
 JMP OUTT_DLDISCHSOURCE
 DLDESWCHSOURCE_SUB:
 CMP BP,3
 JNZ DLDESWCHSOURCE_SBB
 SUB DL,BL
 JMP OUTT_DLDISCHSOURCE
 DLDESWCHSOURCE_SBB:
 CMP BP,4
 JNZ DLDESWCHSOURCE_ADC
 SBB DL,BL
 JMP OUTT_DLDISCHSOURCE
 DLDESWCHSOURCE_ADC:
 CMP BP,5
 JNZ DLDESWCHSOURCE_AND
 ADC DL,BL
 JMP OUTT_DLDISCHSOURCE
 DLDESWCHSOURCE_AND:
 CMP BP,6
 JNZ DLDESWCHSOURCE_XOR
 AND DL,BL
 JMP OUTT_DLDISCHSOURCE
 DLDESWCHSOURCE_XOR:
 XOR DL,BL
 
 OUTT_DLDISCHSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,DX
 JMP NAG7
NTADLDESWDLSOURCE_PLUS:
MOV BL,BYTE PTR RDX1
MOV DX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
 DLXDESWDLSOURCE_MOV:
 CMP BP,1
 JNZ DLDESWDLSOURCE_ADD
 MOV DL,BL
 JMP OUTT_DLDISDLSOURCE
 DLDESWDLSOURCE_ADD:
 CMP BP,2
 JNZ DLDESWDLSOURCE_SUB 
 ADD DL,BL
 JMP OUTT_DLDISDLSOURCE
 DLDESWDLSOURCE_SUB:
 CMP BP,3
 JNZ DLDESWDLSOURCE_SBB
 SUB DL,BL
 JMP OUTT_DLDISDLSOURCE
 DLDESWDLSOURCE_SBB:
 CMP BP,4
 JNZ DLDESWDLSOURCE_ADC
 SBB DL,BL
 JMP OUTT_DLDISDLSOURCE
 DLDESWDLSOURCE_ADC:
 CMP BP,5
 JNZ DLDESWDLSOURCE_AND
 ADC DL,BL
 JMP OUTT_DLDISDLSOURCE
 DLDESWDLSOURCE_AND:
 CMP BP,6
 JNZ DLDESWDLSOURCE_XOR
 AND DL,BL
 JMP OUTT_DLDISDLSOURCE
 DLDESWDLSOURCE_XOR:
 XOR DL,BL
 
 OUTT_DLDISDLSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,DX
 JMP NAG7
NTADLDESWDHSOURCE_PLUS:
MOV BL,BYTE PTR RDX1+1
MOV DX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
 DLXDESWDHSOURCE_MOV:
 CMP BP,1
 JNZ DLDESWDHSOURCE_ADD
 MOV DL,BL
 JMP OUTT_DLDISDHSOURCE
 DLDESWDHSOURCE_ADD:
 CMP BP,2
 JNZ DLDESWDHSOURCE_SUB 
 ADD DL,BL
 JMP OUTT_DLDISDHSOURCE
 DLDESWDHSOURCE_SUB:
 CMP BP,3
 JNZ DLDESWDHSOURCE_SBB
 SUB DL,BL
 JMP OUTT_DLDISDHSOURCE
 DLDESWDHSOURCE_SBB:
 CMP BP,4
 JNZ DLDESWDHSOURCE_ADC
 SBB DL,BL
 JMP OUTT_DLDISDHSOURCE
 DLDESWDHSOURCE_ADC:
 CMP BP,5
 JNZ DLDESWDHSOURCE_AND
 ADC DL,BL
 JMP OUTT_DLDISDHSOURCE
 DLDESWDHSOURCE_AND:
 CMP BP,6
 JNZ DLDESWDHSOURCE_XOR
 AND DL,BL
 JMP OUTT_DLDISDHSOURCE
 DLDESWDHSOURCE_XOR:
 XOR DL,BL
 
 OUTT_DLDISDHSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,DX
 JMP NAG7
NTADLDESW_RKM_YA_MEMRKM_YAERROR_PLUS:
MOV AL,[SI+7]
CMP AL,91
JZ MEMRKM_ORERROR_DLDES_PLUS

RKM_ORERROR_DLDES_PLUS:
MOV AL,[SI+8]
MOV VAR,BP
RET_NUM_COUNT  COMp+7
MOV BP,VAR
CMP CX,2
JA ERROR
MOV BX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
 DLDESWRAKAMSOURCE_MOV:
 CMP BP,1
 JNZ DLDESWRAKAMSOURCE_ADD
 MOV BL,DL
 JMP OUTT_DLDISRAKAMSOURCE
 DLDESWRAKAMSOURCE_ADD:
 CMP BP,2
 JNZ DLDESWRAKAMSOURCE_SUB 
 ADD BL,DL
 JMP OUTT_DLDISRAKAMSOURCE
 DLDESWRAKAMSOURCE_SUB:
 CMP BP,3
 JNZ DLDESWRAKAMSOURCE_SBB
 SUB BL,DL
 JMP OUTT_DLDISRAKAMSOURCE
 DLDESWRAKAMSOURCE_SBB:
 CMP BP,4
 JNZ DLDESWRAKAMSOURCE_ADC
 SBB BL,DL
 JMP OUTT_DLDISRAKAMSOURCE
 DLDESWRAKAMSOURCE_ADC:
 CMP BP,5
 JNZ DLDESWRAKAMSOURCE_AND
 ADC BL,DL
 JMP OUTT_DLDISRAKAMSOURCE
 DLDESWRAKAMSOURCE_AND:
 CMP BP,6
 JNZ DLDESWRAKAMSOURCE_XOR
 AND BL,DL
 JMP OUTT_DLDISRAKAMSOURCE
 DLDESWRAKAMSOURCE_XOR:
 XOR BL,DL
 
 OUTT_DLDISRAKAMSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,BX
 JMP NAG7
MEMRKM_ORERROR_DLDES_PLUS:
MOV AL,[SI+9]
CMP AL,108
JZ ERROR
CMP AL,104
JZ ERROR
MOV VAR,BP
RET_NUM_COUNT  COMp+8
MOV BP,VAR
CMP CX,4
JA ERROR
CMP DX,0FH
JA ERROR
MOV BX,DX
MOV CX,0
MOV CL,MEMORY1[BX]
MOV BX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
 DLDESWMEMRAKAMSOURCE_MOV:
 CMP BP,1
 JNZ DLDESWMEMRAKAMSOURCE_ADD
 MOV BL,CL
 JMP OUTT_DLDISMEMRAKAMSOURCE
 DLDESWMEMRAKAMSOURCE_ADD:
 CMP BP,2
 JNZ DLDESWMEMRAKAMSOURCE_SUB 
 ADD BL,CL
 JMP OUTT_DLDISMEMRAKAMSOURCE
 DLDESWMEMRAKAMSOURCE_SUB:
 CMP BP,3
 JNZ DLDESWMEMRAKAMSOURCE_SBB
 SUB BL,CL
 JMP OUTT_DLDISMEMRAKAMSOURCE
 DLDESWMEMRAKAMSOURCE_SBB:
 CMP BP,4
 JNZ DLDESWMEMRAKAMSOURCE_ADC
 SBB BL,CL
 JMP OUTT_DLDISMEMRAKAMSOURCE
 DLDESWMEMRAKAMSOURCE_ADC:
 CMP BP,5
 JNZ DLDESWMEMRAKAMSOURCE_AND
 ADC BL,CL
 JMP OUTT_DLDISMEMRAKAMSOURCE
 DLDESWMEMRAKAMSOURCE_AND:
 CMP BP,6
 JNZ DLDESWMEMRAKAMSOURCE_XOR
 AND BL,CL
 JMP OUTT_DLDISMEMRAKAMSOURCE
 DLDESWMEMRAKAMSOURCE_XOR:
 XOR BL,CL
 
 OUTT_DLDISMEMRAKAMSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,BX
 JMP NAG7   



;/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTAAHDIS_PLUS:
  MOV BX,0
  MOV BL,[SI+8]
  CMP BL,104
  JNAE NTAAHDESW_RKM_YA_MEMRKM_YAERROR_PLUS
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+7] 
  MOV CL,[SI+8]
  ADD AX,CX
  CMP AX,217
  JZ ERROR
  CMP AX,218 
  JZ ERROR
  CMP AX,219
  JZ ERROR
  CMP AX,220
 JZ ERROR
 CMP AX,205
 JZ NTAAHDESWDIORALLWDIERROR_PLUS
 CMP AX,201
 JZ NTAAHDESWAHSOURCE_PLUS
 CMP AX,206
 JZ NTAAHDESWBLSOURCE_PLUS
 CMP AX,202
 JZ NTAAHDESWBHSOURCE_PLUS 
 CMP AX,207
 JZ NTAAHDESWCLSOURCE_PLUS
 CMP AX,203
 JZ NTAAHDESWCHSOURCE_PLUS
 CMP AX,208
 JZ NTAAHDESWDLSOURCE_PLUS
 CMP AX,204
 JZ NTAAHDESWDHSOURCE_PLUS   
 MOV BX,0
 MOV CX,0                              ;TAKECARE
 MOV CL,[SI+9]
 MOV BL,[SI+10]
 ADD CX,BX
 ADD AX,CX

 CMP AX,402
 JZ AHDES_WNTA_MEMBX_PLUS
 CMP AX,404              
 JZ NTA_AHDES_WMEMSI_PLUS
 CMP AX,389
 JZ NTA_AHDES_WMEMDI_PLUS
  
 JMP NTAAHDESW_RKM_YA_MEMRKM_YAERROR_PLUS  
 
 
  
NTAAHDESWDIORALLWDIERROR_PLUS: 
MOV AL,[SI+7]
CMP AL,100 
JZ ERROR
NTAAHDESWALSOURCE_PLUS:
MOV BL,BYTE PTR RAX1
MOV CX,RAX1
MOV AX,FLAGS
PUSH AX
POPF 
 AHDESWALSOURCE_MOV:
 CMP BP,1
 JNZ AHDESWALSOURCE_ADD
 MOV CH,BL
 JMP OUTT_AHDISALSOURCE
 AHDESWALSOURCE_ADD:
 CMP BP,2
 JNZ AHDESWALSOURCE_SUB 
 ADD CH,BL
 JMP OUTT_AHDISALSOURCE
 AHDESWALSOURCE_SUB:
 CMP BP,3
 JNZ AHDESWALSOURCE_SBB
 SUB CH,BL
 JMP OUTT_AHDISALSOURCE
 AHDESWALSOURCE_SBB:
 CMP BP,4
 JNZ AHDESWALSOURCE_ADC
 SBB CH,BL
 JMP OUTT_AHDISALSOURCE
 AHDESWALSOURCE_ADC:
 CMP BP,5
 JNZ AHDESWALSOURCE_AND
 ADC CH,BL
 JMP OUTT_AHDISALSOURCE
 AHDESWALSOURCE_AND:
 CMP BP,6
 JNZ AHDESWALSOURCE_XOR
 AND CH,BL
 JMP OUTT_AHDISALSOURCE
 AHDESWALSOURCE_XOR:
 XOR CH,BL
 
 OUTT_AHDISALSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,CX
 JMP NAG7

AHDES_WNTA_MEMBX_PLUS:
MOV BX,RBX1
CMP BX,0FH
JA ERROR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX
POPF
 AHDESWMEMBXSOURCE_MOV:
 CMP BP,1
 JNZ AHDESWMEMBXSOURCE_ADD
 MOV BH,CL
 JMP OUTT_AHDISMEMBXSOURCE
 AHDESWMEMBXSOURCE_ADD:
 CMP BP,2
 JNZ AHDESWMEMBXSOURCE_SUB 
 ADD BH,CL
 JMP OUTT_AHDISMEMBXSOURCE
 AHDESWMEMBXSOURCE_SUB:
 CMP BP,3
 JNZ AHDESWMEMBXSOURCE_SBB
 SUB BH,CL
 JMP OUTT_AHDISMEMBXSOURCE
 AHDESWMEMBXSOURCE_SBB:
 CMP BP,4
 JNZ AHDESWMEMBXSOURCE_ADC
 SBB BH,CL
 JMP OUTT_AHDISMEMBXSOURCE
 AHDESWMEMBXSOURCE_ADC:
 CMP BP,5
 JNZ AHDESWMEMBXSOURCE_AND
 ADC BH,CL
 JMP OUTT_AHDISMEMBXSOURCE
 AHDESWMEMBXSOURCE_AND:
 CMP BP,6
 JNZ AHDESWMEMBXSOURCE_XOR
 AND BH,CL
 JMP OUTT_AHDISMEMBXSOURCE
 AHDESWMEMBXSOURCE_XOR:
 XOR BH,CL
 
 OUTT_AHDISMEMBXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,BX
 JMP NAG7
NTA_AHDES_WMEMSI_PLUS:
MOV BX,ISI1
CMP BX,0FH
JA ERROR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX
POPF
 AHDESWMEMSISOURCE_MOV:
 CMP BP,1
 JNZ AHDESWMEMSISOURCE_ADD
 MOV BH,CL
 JMP OUTT_AHDISMEMSISOURCE
 AHDESWMEMSISOURCE_ADD:
 CMP BP,2
 JNZ AHDESWMEMSISOURCE_SUB 
 ADD BH,CL
 JMP OUTT_AHDISMEMSISOURCE
 AHDESWMEMSISOURCE_SUB:
 CMP BP,3
 JNZ AHDESWMEMSISOURCE_SBB
 SUB BH,CL
 JMP OUTT_AHDISMEMSISOURCE
 AHDESWMEMSISOURCE_SBB:
 CMP BP,4
 JNZ AHDESWMEMSISOURCE_ADC
 SBB BH,CL
 JMP OUTT_AHDISMEMSISOURCE
 AHDESWMEMSISOURCE_ADC:
 CMP BP,5
 JNZ AHDESWMEMSISOURCE_AND
 ADC BH,CL
 JMP OUTT_AHDISMEMSISOURCE
 AHDESWMEMSISOURCE_AND:
 CMP BP,6
 JNZ AHDESWMEMSISOURCE_XOR
 AND BH,CL
 JMP OUTT_AHDISMEMSISOURCE
 AHDESWMEMSISOURCE_XOR:
 XOR BH,CL
 
 OUTT_AHDISMEMSISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,BX
 JMP NAG7
NTA_AHDES_WMEMDI_PLUS:
MOV BX,IDI1
CMP BX,0FH
JA ERROR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX
POPF
 AHDESWMEMDISOURCE_MOV:
 CMP BP,1
 JNZ AHDESWMEMDISOURCE_ADD
 MOV BH,CL
 JMP OUTT_AHDISMEMDISOURCE
 AHDESWMEMDISOURCE_ADD:
 CMP BP,2
 JNZ AHDESWMEMDISOURCE_SUB 
 ADD BH,CL
 JMP OUTT_AHDISMEMDISOURCE
 AHDESWMEMDISOURCE_SUB:
 CMP BP,3
 JNZ AHDESWMEMDISOURCE_SBB
 SUB BH,CL
 JMP OUTT_AHDISMEMDISOURCE
 AHDESWMEMDISOURCE_SBB:
 CMP BP,4
 JNZ AHDESWMEMDISOURCE_ADC
 SBB BH,CL
 JMP OUTT_AHDISMEMDISOURCE
 AHDESWMEMDISOURCE_ADC:
 CMP BP,5
 JNZ AHDESWMEMDISOURCE_AND
 ADC BH,CL
 JMP OUTT_AHDISMEMDISOURCE
 AHDESWMEMDISOURCE_AND:
 CMP BP,6
 JNZ AHDESWMEMDISOURCE_XOR
 AND BH,CL
 JMP OUTT_AHDISMEMDISOURCE
 AHDESWMEMDISOURCE_XOR:
 XOR BH,CL
 
 OUTT_AHDISMEMDISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,BX
 JMP NAG7
NTAAHDESWAHSOURCE_PLUS:
MOV BL,BYTE PTR RAX1+1
MOV DX,RAX1
MOV AX,FLAGS
PUSH AX
POPF 
 AHXDESWAHSOURCE_MOV:
 CMP BP,1
 JNZ AHDESWAHSOURCE_ADD
 MOV DH,BL
 JMP OUTT_AHDISAHSOURCE
 AHDESWAHSOURCE_ADD:
 CMP BP,2
 JNZ AHDESWAHSOURCE_SUB 
 ADD DH,BL
 JMP OUTT_AHDISAHSOURCE
 AHDESWAHSOURCE_SUB:
 CMP BP,3
 JNZ AHDESWAHSOURCE_SBB
 SUB DH,BL
 JMP OUTT_AHDISAHSOURCE
 AHDESWAHSOURCE_SBB:
 CMP BP,4
 JNZ AHDESWAHSOURCE_ADC
 SBB DH,BL
 JMP OUTT_AHDISAHSOURCE
 AHDESWAHSOURCE_ADC:
 CMP BP,5
 JNZ AHDESWAHSOURCE_AND
 ADC DH,BL
 JMP OUTT_AHDISAHSOURCE
 AHDESWAHSOURCE_AND:
 CMP BP,6
 JNZ AHDESWAHSOURCE_XOR
 AND DH,BL
 JMP OUTT_AHDISAHSOURCE
 AHDESWAHSOURCE_XOR:
 XOR DH,BL
 
 OUTT_AHDISAHSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,DX
 JMP NAG7
NTAAHDESWBLSOURCE_PLUS:
MOV BL,BYTE PTR RBX1
MOV DX,RAX1
MOV AX,FLAGS
PUSH AX
POPF 
 AHXDESWBLSOURCE_MOV:
 CMP BP,1
 JNZ AHDESWBLSOURCE_ADD
 MOV DH,BL
 JMP OUTT_AHDISBLSOURCE
 AHDESWBLSOURCE_ADD:
 CMP BP,2
 JNZ AHDESWBLSOURCE_SUB 
 ADD DH,BL
 JMP OUTT_AHDISBLSOURCE
 AHDESWBLSOURCE_SUB:
 CMP BP,3
 JNZ AHDESWBLSOURCE_SBB
 SUB DH,BL
 JMP OUTT_AHDISBLSOURCE
 AHDESWBLSOURCE_SBB:
 CMP BP,4
 JNZ AHDESWBLSOURCE_ADC
 SBB DH,BL
 JMP OUTT_AHDISBLSOURCE
 AHDESWBLSOURCE_ADC:
 CMP BP,5
 JNZ AHDESWBLSOURCE_AND
 ADC DH,BL
 JMP OUTT_AHDISBLSOURCE
 AHDESWBLSOURCE_AND:
 CMP BP,6
 JNZ AHDESWBLSOURCE_XOR
 AND DH,BL
 JMP OUTT_AHDISBLSOURCE
 AHDESWBLSOURCE_XOR:
 XOR DH,BL
 
 OUTT_AHDISBLSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,DX
 JMP NAG7
NTAAHDESWBHSOURCE_PLUS:
MOV BL,BYTE PTR RBX1+1
MOV DX,RAX1
MOV AX,FLAGS
PUSH AX
POPF 
 AHXDESWBHSOURCE_MOV:
 CMP BP,1
 JNZ AHDESWBHSOURCE_ADD
 MOV DH,BL
 JMP OUTT_AHDISBHSOURCE
 AHDESWBHSOURCE_ADD:
 CMP BP,2
 JNZ AHDESWBHSOURCE_SUB 
 ADD DH,BL
 JMP OUTT_AHDISBHSOURCE
 AHDESWBHSOURCE_SUB:
 CMP BP,3
 JNZ AHDESWBHSOURCE_SBB
 SUB DH,BL
 JMP OUTT_AHDISBHSOURCE
 AHDESWBHSOURCE_SBB:
 CMP BP,4
 JNZ AHDESWBHSOURCE_ADC
 SBB DH,BL
 JMP OUTT_AHDISBHSOURCE
 AHDESWBHSOURCE_ADC:
 CMP BP,5
 JNZ AHDESWBHSOURCE_AND
 ADC DH,BL
 JMP OUTT_AHDISBHSOURCE
 AHDESWBHSOURCE_AND:
 CMP BP,6
 JNZ AHDESWBHSOURCE_XOR
 AND DH,BL
 JMP OUTT_AHDISBHSOURCE
 AHDESWBHSOURCE_XOR:
 XOR DH,BL
 
 OUTT_AHDISBHSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,DX
 JMP NAG7
NTAAHDESWCLSOURCE_PLUS:
MOV BL,BYTE PTR RCX1
MOV DX,RAX1
MOV AX,FLAGS
PUSH AX
POPF 
 AHXDESWCLSOURCE_MOV:
 CMP BP,1
 JNZ AHDESWCLSOURCE_ADD
 MOV DH,BL
 JMP OUTT_AHDISCLSOURCE
 AHDESWCLSOURCE_ADD:
 CMP BP,2
 JNZ AHDESWCLSOURCE_SUB 
 ADD DH,BL
 JMP OUTT_AHDISCLSOURCE
 AHDESWCLSOURCE_SUB:
 CMP BP,3
 JNZ AHDESWCLSOURCE_SBB
 SUB DH,BL
 JMP OUTT_AHDISCLSOURCE
 AHDESWCLSOURCE_SBB:
 CMP BP,4
 JNZ AHDESWCLSOURCE_ADC
 SBB DH,BL
 JMP OUTT_AHDISCLSOURCE
 AHDESWCLSOURCE_ADC:
 CMP BP,5
 JNZ AHDESWCLSOURCE_AND
 ADC DH,BL
 JMP OUTT_AHDISCLSOURCE
 AHDESWCLSOURCE_AND:
 CMP BP,6
 JNZ AHDESWCLSOURCE_XOR
 AND DH,BL
 JMP OUTT_AHDISCLSOURCE
 AHDESWCLSOURCE_XOR:
 XOR DH,BL
 
 OUTT_AHDISCLSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,DX
 JMP NAG7
NTAAHDESWCHSOURCE_PLUS:
MOV BL,BYTE PTR RCX1+1
MOV DX,RAX1
MOV AX,FLAGS
PUSH AX
POPF 
 AHXDESWCHSOURCE_MOV:
 CMP BP,1
 JNZ AHDESWCHSOURCE_ADD
 MOV DH,BL
 JMP OUTT_AHDISCHSOURCE
 AHDESWCHSOURCE_ADD:
 CMP BP,2
 JNZ AHDESWCHSOURCE_SUB 
 ADD DH,BL
 JMP OUTT_AHDISCHSOURCE
 AHDESWCHSOURCE_SUB:
 CMP BP,3
 JNZ AHDESWCHSOURCE_SBB
 SUB DH,BL
 JMP OUTT_AHDISCHSOURCE
 AHDESWCHSOURCE_SBB:
 CMP BP,4
 JNZ AHDESWCHSOURCE_ADC
 SBB DH,BL
 JMP OUTT_AHDISCHSOURCE
 AHDESWCHSOURCE_ADC:
 CMP BP,5
 JNZ AHDESWCHSOURCE_AND
 ADC DH,BL
 JMP OUTT_AHDISCHSOURCE
 AHDESWCHSOURCE_AND:
 CMP BP,6
 JNZ AHDESWCHSOURCE_XOR
 AND DH,BL
 JMP OUTT_AHDISCHSOURCE
 AHDESWCHSOURCE_XOR:
 XOR DH,BL
 
 OUTT_AHDISCHSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,DX
 JMP NAG7
NTAAHDESWDLSOURCE_PLUS:
MOV BL,BYTE PTR RDX1
MOV DX,RAX1
MOV AX,FLAGS
PUSH AX
POPF 
 AHXDESWDLSOURCE_MOV:
 CMP BP,1
 JNZ AHDESWDLSOURCE_ADD
 MOV DH,BL
 JMP OUTT_AHDISDLSOURCE
 AHDESWDLSOURCE_ADD:
 CMP BP,2
 JNZ AHDESWDLSOURCE_SUB 
 ADD DH,BL
 JMP OUTT_AHDISDLSOURCE
 AHDESWDLSOURCE_SUB:
 CMP BP,3
 JNZ AHDESWDLSOURCE_SBB
 SUB DH,BL
 JMP OUTT_AHDISDLSOURCE
 AHDESWDLSOURCE_SBB:
 CMP BP,4
 JNZ AHDESWDLSOURCE_ADC
 SBB DH,BL
 JMP OUTT_AHDISDLSOURCE
 AHDESWDLSOURCE_ADC:
 CMP BP,5
 JNZ AHDESWDLSOURCE_AND
 ADC DH,BL
 JMP OUTT_AHDISDLSOURCE
 AHDESWDLSOURCE_AND:
 CMP BP,6
 JNZ AHDESWDLSOURCE_XOR
 AND DH,BL
 JMP OUTT_AHDISDLSOURCE
 AHDESWDLSOURCE_XOR:
 XOR DH,BL
 
 OUTT_AHDISDLSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,DX
 JMP NAG7
NTAAHDESWDHSOURCE_PLUS:
MOV BL,BYTE PTR RDX1+1
MOV DX,RAX1
MOV AX,FLAGS
PUSH AX
POPF 
 AHXDESWDHSOURCE_MOV:
 CMP BP,1
 JNZ AHDESWDHSOURCE_ADD
 MOV DH,BL
 JMP OUTT_AHDISDHSOURCE
 AHDESWDHSOURCE_ADD:
 CMP BP,2
 JNZ AHDESWDHSOURCE_SUB 
 ADD DH,BL
 JMP OUTT_AHDISDHSOURCE
 AHDESWDHSOURCE_SUB:
 CMP BP,3
 JNZ AHDESWDHSOURCE_SBB
 SUB DH,BL
 JMP OUTT_AHDISDHSOURCE
 AHDESWDHSOURCE_SBB:
 CMP BP,4
 JNZ AHDESWDHSOURCE_ADC
 SBB DH,BL
 JMP OUTT_AHDISDHSOURCE
 AHDESWDHSOURCE_ADC:
 CMP BP,5
 JNZ AHDESWDHSOURCE_AND
 ADC DH,BL
 JMP OUTT_AHDISDHSOURCE
 AHDESWDHSOURCE_AND:
 CMP BP,6
 JNZ AHDESWDHSOURCE_XOR
 AND DH,BL
 JMP OUTT_AHDISDHSOURCE
 AHDESWDHSOURCE_XOR:
 XOR DH,BL
 
 OUTT_AHDISDHSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,DX
 JMP NAG7
NTAAHDESW_RKM_YA_MEMRKM_YAERROR_PLUS:
MOV AL,[SI+7]
CMP AL,91
JZ MEMRKM_ORERROR_AHDES_PLUS

RKM_ORERROR_AHDES_PLUS:
MOV AL,[SI+8]
MOV VAR,BP
RET_NUM_COUNT  COMp+7
MOV BP,VAR
CMP CX,2
JA ERROR
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX
POPF 
 AHDESWRAKAMSOURCE_MOV:
 CMP BP,1
 JNZ AHDESWRAKAMSOURCE_ADD
 MOV BH,DL
 JMP OUTT_AHDISRAKAMSOURCE
 AHDESWRAKAMSOURCE_ADD:
 CMP BP,2
 JNZ AHDESWRAKAMSOURCE_SUB 
 ADD BH,DL
 JMP OUTT_AHDISRAKAMSOURCE
 AHDESWRAKAMSOURCE_SUB:
 CMP BP,3
 JNZ AHDESWRAKAMSOURCE_SBB
 SUB BH,DL
 JMP OUTT_AHDISRAKAMSOURCE
 AHDESWRAKAMSOURCE_SBB:
 CMP BP,4
 JNZ AHDESWRAKAMSOURCE_ADC
 SBB BH,DL
 JMP OUTT_AHDISRAKAMSOURCE
 AHDESWRAKAMSOURCE_ADC:
 CMP BP,5
 JNZ AHDESWRAKAMSOURCE_AND
 ADC BH,DL
 JMP OUTT_AHDISRAKAMSOURCE
 AHDESWRAKAMSOURCE_AND:
 CMP BP,6
 JNZ AHDESWRAKAMSOURCE_XOR
 AND BH,DL
 JMP OUTT_AHDISRAKAMSOURCE
 AHDESWRAKAMSOURCE_XOR:
 XOR BH,DL
 
 OUTT_AHDISRAKAMSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,BX
 JMP NAG7
MEMRKM_ORERROR_AHDES_PLUS:
MOV AL,[SI+9]
CMP AL,108
JZ ERROR
CMP AL,104
JZ ERROR
MOV VAR,BP
RET_NUM_COUNT  COMp+8
MOV BP,VAR
CMP CX,4
JA ERROR
CMP DX,0FH
JA ERROR
MOV BX,DX
MOV CX,0
MOV CL,MEMORY1[BX]
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX
POPF 
 AHDESWMEMRAKAMSOURCE_MOV:
 CMP BP,1
 JNZ AHDESWMEMRAKAMSOURCE_ADD
 MOV BH,CL
 JMP OUTT_AHDISMEMRAKAMSOURCE
 AHDESWMEMRAKAMSOURCE_ADD:
 CMP BP,2
 JNZ AHDESWMEMRAKAMSOURCE_SUB 
 ADD BH,CL
 JMP OUTT_AHDISMEMRAKAMSOURCE
 AHDESWMEMRAKAMSOURCE_SUB:
 CMP BP,3
 JNZ AHDESWMEMRAKAMSOURCE_SBB
 SUB BH,CL
 JMP OUTT_AHDISMEMRAKAMSOURCE
 AHDESWMEMRAKAMSOURCE_SBB:
 CMP BP,4
 JNZ AHDESWMEMRAKAMSOURCE_ADC
 SBB BH,CL
 JMP OUTT_AHDISMEMRAKAMSOURCE
 AHDESWMEMRAKAMSOURCE_ADC:
 CMP BP,5
 JNZ AHDESWMEMRAKAMSOURCE_AND
 ADC BH,CL
 JMP OUTT_AHDISMEMRAKAMSOURCE
 AHDESWMEMRAKAMSOURCE_AND:
 CMP BP,6
 JNZ AHDESWMEMRAKAMSOURCE_XOR
 AND BH,CL
 JMP OUTT_AHDISMEMRAKAMSOURCE
 AHDESWMEMRAKAMSOURCE_XOR:
 XOR BH,CL
 
 OUTT_AHDISMEMRAKAMSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,BX
 JMP NAG7
 ;///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTABHDIS_PLUS:
MOV BX,0
  MOV BL,[SI+8]
  CMP BL,104
  JNAE NTABHDESW_RKM_YA_MEMRKM_YAERROR_PLUS
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+7] 
  MOV CL,[SI+8]
  ADD AX,CX
  CMP AX,217
  JZ ERROR
  CMP AX,218 
  JZ ERROR
  CMP AX,219
  JZ ERROR
  CMP AX,220
 JZ ERROR
 CMP AX,205
 JZ NTABHDESWDIORALLWDIERROR_PLUS
 CMP AX,201
 JZ NTABHDESWAHSOURCE_PLUS
 CMP AX,206
 JZ NTABHDESWBLSOURCE_PLUS
 CMP AX,202
 JZ NTABHDESWBHSOURCE_PLUS 
 CMP AX,207
 JZ NTABHDESWCLSOURCE_PLUS
 CMP AX,203
 JZ NTABHDESWCHSOURCE_PLUS
 CMP AX,208
 JZ NTABHDESWDLSOURCE_PLUS
 CMP AX,204
 JZ NTABHDESWDHSOURCE_PLUS   
 MOV BX,0
 MOV CX,0                              ;TAKECARE
 MOV CL,[SI+9]
 MOV BL,[SI+10]
 ADD CX,BX
 ADD AX,CX

 CMP AX,402
 JZ BHDES_WNTA_MEMBX_PLUS
 CMP AX,404              
 JZ NTA_BHDES_WMEMSI_PLUS
 CMP AX,389
 JZ NTA_BHDES_WMEMDI_PLUS
  
 JMP NTABHDESW_RKM_YA_MEMRKM_YAERROR_PLUS  
 
 
  
NTABHDESWDIORALLWDIERROR_PLUS: 
MOV AL,[SI+7]
CMP AL,100 
JZ ERROR
NTABHDESWALSOURCE_PLUS:
MOV BL,BYTE PTR RAX1
MOV CX,RBX1
MOV AX,FLAGS
PUSH AX
POPF 
 BHDESWALSOURCE_MOV:
 CMP BP,1
 JNZ BHDESWALSOURCE_ADD
 MOV CH,BL
 JMP OUTT_BHDISALSOURCE
 BHDESWALSOURCE_ADD:
 CMP BP,2
 JNZ BHDESWALSOURCE_SUB 
 ADD CH,BL
 JMP OUTT_BHDISALSOURCE
 BHDESWALSOURCE_SUB:
 CMP BP,3
 JNZ BHDESWALSOURCE_SBB
 SUB CH,BL
 JMP OUTT_BHDISALSOURCE
 BHDESWALSOURCE_SBB:
 CMP BP,4
 JNZ BHDESWALSOURCE_ADC
 SBB CH,BL
 JMP OUTT_BHDISALSOURCE
 BHDESWALSOURCE_ADC:
 CMP BP,5
 JNZ BHDESWALSOURCE_AND
 ADC CH,BL
 JMP OUTT_BHDISALSOURCE
 BHDESWALSOURCE_AND:
 CMP BP,6
 JNZ BHDESWALSOURCE_XOR
 AND CH,BL
 JMP OUTT_BHDISALSOURCE
 BHDESWALSOURCE_XOR:
 XOR CH,BL
 
 OUTT_BHDISALSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,CX
 JMP NAG7

BHDES_WNTA_MEMBX_PLUS:
MOV BX,RBX1
CMP BX,0FH
JA ERROR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RBX1
MOV AX,FLAGS
PUSH AX
POPF
 BHDESWMEMBXSOURCE_MOV:
 CMP BP,1
 JNZ BHDESWMEMBXSOURCE_ADD
 MOV BH,CL
 JMP OUTT_BHDISMEMBXSOURCE
 BHDESWMEMBXSOURCE_ADD:
 CMP BP,2
 JNZ BHDESWMEMBXSOURCE_SUB 
 ADD BH,CL
 JMP OUTT_BHDISMEMBXSOURCE
 BHDESWMEMBXSOURCE_SUB:
 CMP BP,3
 JNZ BHDESWMEMBXSOURCE_SBB
 SUB BH,CL
 JMP OUTT_BHDISMEMBXSOURCE
 BHDESWMEMBXSOURCE_SBB:
 CMP BP,4
 JNZ BHDESWMEMBXSOURCE_ADC
 SBB BH,CL
 JMP OUTT_BHDISMEMBXSOURCE
 BHDESWMEMBXSOURCE_ADC:
 CMP BP,5
 JNZ BHDESWMEMBXSOURCE_AND
 ADC BH,CL
 JMP OUTT_BHDISMEMBXSOURCE
 BHDESWMEMBXSOURCE_AND:
 CMP BP,6
 JNZ BHDESWMEMBXSOURCE_XOR
 AND BH,CL
 JMP OUTT_BHDISMEMBXSOURCE
 BHDESWMEMBXSOURCE_XOR:
 XOR BH,CL
 
 OUTT_BHDISMEMBXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,BX
 JMP NAG7
NTA_BHDES_WMEMSI_PLUS:
MOV BX,ISI1
CMP BX,0FH
JA ERROR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RBX1
MOV AX,FLAGS
PUSH AX
POPF
 BHDESWMEMSISOURCE_MOV:
 CMP BP,1
 JNZ BHDESWMEMSISOURCE_ADD
 MOV BH,CL
 JMP OUTT_BHDISMEMSISOURCE
 BHDESWMEMSISOURCE_ADD:
 CMP BP,2
 JNZ BHDESWMEMSISOURCE_SUB 
 ADD BH,CL
 JMP OUTT_BHDISMEMSISOURCE
 BHDESWMEMSISOURCE_SUB:
 CMP BP,3
 JNZ BHDESWMEMSISOURCE_SBB
 SUB BH,CL
 JMP OUTT_BHDISMEMSISOURCE
 BHDESWMEMSISOURCE_SBB:
 CMP BP,4
 JNZ BHDESWMEMSISOURCE_ADC
 SBB BH,CL
 JMP OUTT_BHDISMEMSISOURCE
 BHDESWMEMSISOURCE_ADC:
 CMP BP,5
 JNZ BHDESWMEMSISOURCE_AND
 ADC BH,CL
 JMP OUTT_BHDISMEMSISOURCE
 BHDESWMEMSISOURCE_AND:
 CMP BP,6
 JNZ BHDESWMEMSISOURCE_XOR
 AND BH,CL
 JMP OUTT_BHDISMEMSISOURCE
 BHDESWMEMSISOURCE_XOR:
 XOR BH,CL
 
 OUTT_BHDISMEMSISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,BX
 JMP NAG7
NTA_BHDES_WMEMDI_PLUS:
MOV BX,IDI1
CMP BX,0FH
JA ERROR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RBX1
MOV AX,FLAGS
PUSH AX
POPF
 BHDESWMEMDISOURCE_MOV:
 CMP BP,1
 JNZ BHDESWMEMDISOURCE_ADD
 MOV BH,CL
 JMP OUTT_BHDISMEMDISOURCE
 BHDESWMEMDISOURCE_ADD:
 CMP BP,2
 JNZ BHDESWMEMDISOURCE_SUB 
 ADD BH,CL
 JMP OUTT_BHDISMEMDISOURCE
 BHDESWMEMDISOURCE_SUB:
 CMP BP,3
 JNZ BHDESWMEMDISOURCE_SBB
 SUB BH,CL
 JMP OUTT_BHDISMEMDISOURCE
 BHDESWMEMDISOURCE_SBB:
 CMP BP,4
 JNZ BHDESWMEMDISOURCE_ADC
 SBB BH,CL
 JMP OUTT_BHDISMEMDISOURCE
 BHDESWMEMDISOURCE_ADC:
 CMP BP,5
 JNZ BHDESWMEMDISOURCE_AND
 ADC BH,CL
 JMP OUTT_BHDISMEMDISOURCE
 BHDESWMEMDISOURCE_AND:
 CMP BP,6
 JNZ BHDESWMEMDISOURCE_XOR
 AND BH,CL
 JMP OUTT_BHDISMEMDISOURCE
 BHDESWMEMDISOURCE_XOR:
 XOR BH,CL
 
 OUTT_BHDISMEMDISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,BX
 JMP NAG7
NTABHDESWAHSOURCE_PLUS:
MOV BL,BYTE PTR RAX1+1
MOV DX,RBX1
MOV AX,FLAGS
PUSH AX
POPF 
 BHXDESWAHSOURCE_MOV:
 CMP BP,1
 JNZ BHDESWAHSOURCE_ADD
 MOV DH,BL
 JMP OUTT_BHDISAHSOURCE
 BHDESWAHSOURCE_ADD:
 CMP BP,2
 JNZ BHDESWAHSOURCE_SUB 
 ADD DH,BL
 JMP OUTT_BHDISAHSOURCE
 BHDESWAHSOURCE_SUB:
 CMP BP,3
 JNZ BHDESWAHSOURCE_SBB
 SUB DH,BL
 JMP OUTT_BHDISAHSOURCE
 BHDESWAHSOURCE_SBB:
 CMP BP,4
 JNZ BHDESWAHSOURCE_ADC
 SBB DH,BL
 JMP OUTT_BHDISAHSOURCE
 BHDESWAHSOURCE_ADC:
 CMP BP,5
 JNZ BHDESWAHSOURCE_AND
 ADC DH,BL
 JMP OUTT_BHDISAHSOURCE
 BHDESWAHSOURCE_AND:
 CMP BP,6
 JNZ BHDESWAHSOURCE_XOR
 AND DH,BL
 JMP OUTT_BHDISAHSOURCE
 BHDESWAHSOURCE_XOR:
 XOR DH,BL
 
 OUTT_BHDISAHSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,DX
 JMP NAG7
NTABHDESWBLSOURCE_PLUS:
MOV BL,BYTE PTR RBX1
MOV DX,RBX1
MOV AX,FLAGS
PUSH AX
POPF 
 BHXDESWBLSOURCE_MOV:
 CMP BP,1
 JNZ BHDESWBLSOURCE_ADD
 MOV DH,BL
 JMP OUTT_BHDISBLSOURCE
 BHDESWBLSOURCE_ADD:
 CMP BP,2
 JNZ BHDESWBLSOURCE_SUB 
 ADD DH,BL
 JMP OUTT_BHDISBLSOURCE
 BHDESWBLSOURCE_SUB:
 CMP BP,3
 JNZ BHDESWBLSOURCE_SBB
 SUB DH,BL
 JMP OUTT_BHDISBLSOURCE
 BHDESWBLSOURCE_SBB:
 CMP BP,4
 JNZ BHDESWBLSOURCE_ADC
 SBB DH,BL
 JMP OUTT_BHDISBLSOURCE
 BHDESWBLSOURCE_ADC:
 CMP BP,5
 JNZ BHDESWBLSOURCE_AND
 ADC DH,BL
 JMP OUTT_BHDISBLSOURCE
 BHDESWBLSOURCE_AND:
 CMP BP,6
 JNZ BHDESWBLSOURCE_XOR
 AND DH,BL
 JMP OUTT_BHDISBLSOURCE
 BHDESWBLSOURCE_XOR:
 XOR DH,BL
 
 OUTT_BHDISBLSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,DX
 JMP NAG7
NTABHDESWBHSOURCE_PLUS:
MOV BL,BYTE PTR RBX1+1
MOV DX,RBX1
MOV AX,FLAGS
PUSH AX
POPF 
 BHXDESWBHSOURCE_MOV:
 CMP BP,1
 JNZ BHDESWBHSOURCE_ADD
 MOV DH,BL
 JMP OUTT_BHDISBHSOURCE
 BHDESWBHSOURCE_ADD:
 CMP BP,2
 JNZ BHDESWBHSOURCE_SUB 
 ADD DH,BL
 JMP OUTT_BHDISBHSOURCE
 BHDESWBHSOURCE_SUB:
 CMP BP,3
 JNZ BHDESWBHSOURCE_SBB
 SUB DH,BL
 JMP OUTT_BHDISBHSOURCE
 BHDESWBHSOURCE_SBB:
 CMP BP,4
 JNZ BHDESWBHSOURCE_ADC
 SBB DH,BL
 JMP OUTT_BHDISBHSOURCE
 BHDESWBHSOURCE_ADC:
 CMP BP,5
 JNZ BHDESWBHSOURCE_AND
 ADC DH,BL
 JMP OUTT_BHDISBHSOURCE
 BHDESWBHSOURCE_AND:
 CMP BP,6
 JNZ BHDESWBHSOURCE_XOR
 AND DH,BL
 JMP OUTT_BHDISBHSOURCE
 BHDESWBHSOURCE_XOR:
 XOR DH,BL
 
 OUTT_BHDISBHSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,DX
 JMP NAG7
NTABHDESWCLSOURCE_PLUS:
MOV BL,BYTE PTR RCX1
MOV DX,RBX1
MOV AX,FLAGS
PUSH AX
POPF 
 BHXDESWCLSOURCE_MOV:
 CMP BP,1
 JNZ BHDESWCLSOURCE_ADD
 MOV DH,BL
 JMP OUTT_BHDISCLSOURCE
 BHDESWCLSOURCE_ADD:
 CMP BP,2
 JNZ BHDESWCLSOURCE_SUB 
 ADD DH,BL
 JMP OUTT_BHDISCLSOURCE
 BHDESWCLSOURCE_SUB:
 CMP BP,3
 JNZ BHDESWCLSOURCE_SBB
 SUB DH,BL
 JMP OUTT_BHDISCLSOURCE
 BHDESWCLSOURCE_SBB:
 CMP BP,4
 JNZ BHDESWCLSOURCE_ADC
 SBB DH,BL
 JMP OUTT_BHDISCLSOURCE
 BHDESWCLSOURCE_ADC:
 CMP BP,5
 JNZ BHDESWCLSOURCE_AND
 ADC DH,BL
 JMP OUTT_BHDISCLSOURCE
 BHDESWCLSOURCE_AND:
 CMP BP,6
 JNZ BHDESWCLSOURCE_XOR
 AND DH,BL
 JMP OUTT_BHDISCLSOURCE
 BHDESWCLSOURCE_XOR:
 XOR DH,BL
 
 OUTT_BHDISCLSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,DX
 JMP NAG7
NTABHDESWCHSOURCE_PLUS:
MOV BL,BYTE PTR RCX1+1
MOV DX,RBX1
MOV AX,FLAGS
PUSH AX
POPF 
 BHXDESWCHSOURCE_MOV:
 CMP BP,1
 JNZ BHDESWCHSOURCE_ADD
 MOV DH,BL
 JMP OUTT_BHDISCHSOURCE
 BHDESWCHSOURCE_ADD:
 CMP BP,2
 JNZ BHDESWCHSOURCE_SUB 
 ADD DH,BL
 JMP OUTT_BHDISCHSOURCE
 BHDESWCHSOURCE_SUB:
 CMP BP,3
 JNZ BHDESWCHSOURCE_SBB
 SUB DH,BL
 JMP OUTT_BHDISCHSOURCE
 BHDESWCHSOURCE_SBB:
 CMP BP,4
 JNZ BHDESWCHSOURCE_ADC
 SBB DH,BL
 JMP OUTT_BHDISCHSOURCE
 BHDESWCHSOURCE_ADC:
 CMP BP,5
 JNZ BHDESWCHSOURCE_AND
 ADC DH,BL
 JMP OUTT_BHDISCHSOURCE
 BHDESWCHSOURCE_AND:
 CMP BP,6
 JNZ BHDESWCHSOURCE_XOR
 AND DH,BL
 JMP OUTT_BHDISCHSOURCE
 BHDESWCHSOURCE_XOR:
 XOR DH,BL
 
 OUTT_BHDISCHSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,DX
 JMP NAG7
NTABHDESWDLSOURCE_PLUS:
MOV BL,BYTE PTR RDX1
MOV DX,RBX1
MOV AX,FLAGS
PUSH AX
POPF 
 BHXDESWDLSOURCE_MOV:
 CMP BP,1
 JNZ BHDESWDLSOURCE_ADD
 MOV DH,BL
 JMP OUTT_BHDISDLSOURCE
 BHDESWDLSOURCE_ADD:
 CMP BP,2
 JNZ BHDESWDLSOURCE_SUB 
 ADD DH,BL
 JMP OUTT_BHDISDLSOURCE
 BHDESWDLSOURCE_SUB:
 CMP BP,3
 JNZ BHDESWDLSOURCE_SBB
 SUB DH,BL
 JMP OUTT_BHDISDLSOURCE
 BHDESWDLSOURCE_SBB:
 CMP BP,4
 JNZ BHDESWDLSOURCE_ADC
 SBB DH,BL
 JMP OUTT_BHDISDLSOURCE
 BHDESWDLSOURCE_ADC:
 CMP BP,5
 JNZ BHDESWDLSOURCE_AND
 ADC DH,BL
 JMP OUTT_BHDISDLSOURCE
 BHDESWDLSOURCE_AND:
 CMP BP,6
 JNZ BHDESWDLSOURCE_XOR
 AND DH,BL
 JMP OUTT_BHDISDLSOURCE
 BHDESWDLSOURCE_XOR:
 XOR DH,BL
 
 OUTT_BHDISDLSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,DX
 JMP NAG7
NTABHDESWDHSOURCE_PLUS:
MOV BL,BYTE PTR RDX1+1
MOV DX,RBX1
MOV AX,FLAGS
PUSH AX
POPF 
 BHXDESWDHSOURCE_MOV:
 CMP BP,1
 JNZ BHDESWDHSOURCE_ADD
 MOV DH,BL
 JMP OUTT_BHDISDHSOURCE
 BHDESWDHSOURCE_ADD:
 CMP BP,2
 JNZ BHDESWDHSOURCE_SUB 
 ADD DH,BL
 JMP OUTT_BHDISDHSOURCE
 BHDESWDHSOURCE_SUB:
 CMP BP,3
 JNZ BHDESWDHSOURCE_SBB
 SUB DH,BL
 JMP OUTT_BHDISDHSOURCE
 BHDESWDHSOURCE_SBB:
 CMP BP,4
 JNZ BHDESWDHSOURCE_ADC
 SBB DH,BL
 JMP OUTT_BHDISDHSOURCE
 BHDESWDHSOURCE_ADC:
 CMP BP,5
 JNZ BHDESWDHSOURCE_AND
 ADC DH,BL
 JMP OUTT_BHDISDHSOURCE
 BHDESWDHSOURCE_AND:
 CMP BP,6
 JNZ BHDESWDHSOURCE_XOR
 AND DH,BL
 JMP OUTT_BHDISDHSOURCE
 BHDESWDHSOURCE_XOR:
 XOR DH,BL
 
 OUTT_BHDISDHSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,DX
 JMP NAG7
NTABHDESW_RKM_YA_MEMRKM_YAERROR_PLUS:
MOV AL,[SI+7]
CMP AL,91
JZ MEMRKM_ORERROR_BHDES_PLUS

RKM_ORERROR_BHDES_PLUS:
MOV AL,[SI+8]
MOV VAR,BP
RET_NUM_COUNT  COMp+7
MOV BP,VAR
CMP CX,2
JA ERROR
MOV BX,RBX1
MOV AX,FLAGS
PUSH AX
POPF 
 BHDESWRAKAMSOURCE_MOV:
 CMP BP,1
 JNZ BHDESWRAKAMSOURCE_ADD
 MOV BH,DL
 JMP OUTT_BHDISRAKAMSOURCE
 BHDESWRAKAMSOURCE_ADD:
 CMP BP,2
 JNZ BHDESWRAKAMSOURCE_SUB 
 ADD BH,DL
 JMP OUTT_BHDISRAKAMSOURCE
 BHDESWRAKAMSOURCE_SUB:
 CMP BP,3
 JNZ BHDESWRAKAMSOURCE_SBB
 SUB BH,DL
 JMP OUTT_BHDISRAKAMSOURCE
 BHDESWRAKAMSOURCE_SBB:
 CMP BP,4
 JNZ BHDESWRAKAMSOURCE_ADC
 SBB BH,DL
 JMP OUTT_BHDISRAKAMSOURCE
 BHDESWRAKAMSOURCE_ADC:
 CMP BP,5
 JNZ BHDESWRAKAMSOURCE_AND
 ADC BH,DL
 JMP OUTT_BHDISRAKAMSOURCE
 BHDESWRAKAMSOURCE_AND:
 CMP BP,6
 JNZ BHDESWRAKAMSOURCE_XOR
 AND BH,DL
 JMP OUTT_BHDISRAKAMSOURCE
 BHDESWRAKAMSOURCE_XOR:
 XOR BH,DL
 
 OUTT_BHDISRAKAMSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,BX
 JMP NAG7
MEMRKM_ORERROR_BHDES_PLUS:
MOV AL,[SI+9]
CMP AL,108
JZ ERROR
CMP AL,104
JZ ERROR
MOV VAR,BP
RET_NUM_COUNT  COMp+8
MOV BP,VAR
CMP CX,4
JA ERROR
CMP DX,0FH
JA ERROR
MOV BX,DX
MOV CX,0
MOV CL,MEMORY1[BX]
MOV BX,RBX1
MOV AX,FLAGS
PUSH AX
POPF 
 BHDESWMEMRAKAMSOURCE_MOV:
 CMP BP,1
 JNZ BHDESWMEMRAKAMSOURCE_ADD
 MOV BH,CL
 JMP OUTT_BHDISMEMRAKAMSOURCE
 BHDESWMEMRAKAMSOURCE_ADD:
 CMP BP,2
 JNZ BHDESWMEMRAKAMSOURCE_SUB 
 ADD BH,CL
 JMP OUTT_BHDISMEMRAKAMSOURCE
 BHDESWMEMRAKAMSOURCE_SUB:
 CMP BP,3
 JNZ BHDESWMEMRAKAMSOURCE_SBB
 SUB BH,CL
 JMP OUTT_BHDISMEMRAKAMSOURCE
 BHDESWMEMRAKAMSOURCE_SBB:
 CMP BP,4
 JNZ BHDESWMEMRAKAMSOURCE_ADC
 SBB BH,CL
 JMP OUTT_BHDISMEMRAKAMSOURCE
 BHDESWMEMRAKAMSOURCE_ADC:
 CMP BP,5
 JNZ BHDESWMEMRAKAMSOURCE_AND
 ADC BH,CL
 JMP OUTT_BHDISMEMRAKAMSOURCE
 BHDESWMEMRAKAMSOURCE_AND:
 CMP BP,6
 JNZ BHDESWMEMRAKAMSOURCE_XOR
 AND BH,CL
 JMP OUTT_BHDISMEMRAKAMSOURCE
 BHDESWMEMRAKAMSOURCE_XOR:
 XOR BH,CL
 
 OUTT_BHDISMEMRAKAMSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,BX
 JMP NAG7  
;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTACHDIS_PLUS:
      MOV BX,0
  MOV BL,[SI+8]
  CMP BL,104
  JNAE NTACHDESW_RKM_YA_MEMRKM_YAERROR_PLUS
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+7] 
  MOV CL,[SI+8]
  ADD AX,CX
  CMP AX,217
  JZ ERROR
  CMP AX,218 
  JZ ERROR
  CMP AX,219
  JZ ERROR
  CMP AX,220
 JZ ERROR
 CMP AX,205
 JZ NTACHDESWDIORALLWDIERROR_PLUS
 CMP AX,201
 JZ NTACHDESWAHSOURCE_PLUS
 CMP AX,206
 JZ NTACHDESWBLSOURCE_PLUS
 CMP AX,202
 JZ NTACHDESWBHSOURCE_PLUS 
 CMP AX,207
 JZ NTACHDESWCLSOURCE_PLUS
 CMP AX,203
 JZ NTACHDESWCHSOURCE_PLUS
 CMP AX,208
 JZ NTACHDESWDLSOURCE_PLUS
 CMP AX,204
 JZ NTACHDESWDHSOURCE_PLUS   
 MOV BX,0
 MOV CX,0                              ;TAKECARE
 MOV CL,[SI+9]
 MOV BL,[SI+10]
 ADD CX,BX
 ADD AX,CX

 CMP AX,402
 JZ CHDES_WNTA_MEMBX_PLUS
 CMP AX,404              
 JZ NTA_CHDES_WMEMSI_PLUS
 CMP AX,389
 JZ NTA_CHDES_WMEMDI_PLUS
  
 JMP NTACHDESW_RKM_YA_MEMRKM_YAERROR_PLUS  
 
 
  
NTACHDESWDIORALLWDIERROR_PLUS: 
MOV AL,[SI+7]
CMP AL,100 
JZ ERROR
NTACHDESWALSOURCE_PLUS:
MOV BL,BYTE PTR RAX1
MOV CX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
 CHDESWALSOURCE_MOV:
 CMP BP,1
 JNZ CHDESWALSOURCE_ADD
 MOV CH,BL
 JMP OUTT_CHDISALSOURCE
 CHDESWALSOURCE_ADD:
 CMP BP,2
 JNZ CHDESWALSOURCE_SUB 
 ADD CH,BL
 JMP OUTT_CHDISALSOURCE
 CHDESWALSOURCE_SUB:
 CMP BP,3
 JNZ CHDESWALSOURCE_SBB
 SUB CH,BL
 JMP OUTT_CHDISALSOURCE
 CHDESWALSOURCE_SBB:
 CMP BP,4
 JNZ CHDESWALSOURCE_ADC
 SBB CH,BL
 JMP OUTT_CHDISALSOURCE
 CHDESWALSOURCE_ADC:
 CMP BP,5
 JNZ CHDESWALSOURCE_AND
 ADC CH,BL
 JMP OUTT_CHDISALSOURCE
 CHDESWALSOURCE_AND:
 CMP BP,6
 JNZ CHDESWALSOURCE_XOR
 AND CH,BL
 JMP OUTT_CHDISALSOURCE
 CHDESWALSOURCE_XOR:
 XOR CH,BL
 
 OUTT_CHDISALSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,CX
 JMP NAG7

CHDES_WNTA_MEMBX_PLUS:
MOV BX,RCX1
CMP BX,0FH
JA ERROR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RCX1
MOV AX,FLAGS
PUSH AX
POPF
 CHDESWMEMBXSOURCE_MOV:
 CMP BP,1
 JNZ CHDESWMEMBXSOURCE_ADD
 MOV BH,CL
 JMP OUTT_CHDISMEMBXSOURCE
 CHDESWMEMBXSOURCE_ADD:
 CMP BP,2
 JNZ CHDESWMEMBXSOURCE_SUB 
 ADD BH,CL
 JMP OUTT_CHDISMEMBXSOURCE
 CHDESWMEMBXSOURCE_SUB:
 CMP BP,3
 JNZ CHDESWMEMBXSOURCE_SBB
 SUB BH,CL
 JMP OUTT_CHDISMEMBXSOURCE
 CHDESWMEMBXSOURCE_SBB:
 CMP BP,4
 JNZ CHDESWMEMBXSOURCE_ADC
 SBB BH,CL
 JMP OUTT_CHDISMEMBXSOURCE
 CHDESWMEMBXSOURCE_ADC:
 CMP BP,5
 JNZ CHDESWMEMBXSOURCE_AND
 ADC BH,CL
 JMP OUTT_CHDISMEMBXSOURCE
 CHDESWMEMBXSOURCE_AND:
 CMP BP,6
 JNZ CHDESWMEMBXSOURCE_XOR
 AND BH,CL
 JMP OUTT_CHDISMEMBXSOURCE
 CHDESWMEMBXSOURCE_XOR:
 XOR BH,CL
 
 OUTT_CHDISMEMBXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,BX
 JMP NAG7
NTA_CHDES_WMEMSI_PLUS:
MOV BX,ISI1
CMP BX,0FH
JA ERROR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RCX1
MOV AX,FLAGS
PUSH AX
POPF
 CHDESWMEMSISOURCE_MOV:
 CMP BP,1
 JNZ CHDESWMEMSISOURCE_ADD
 MOV BH,CL
 JMP OUTT_CHDISMEMSISOURCE
 CHDESWMEMSISOURCE_ADD:
 CMP BP,2
 JNZ CHDESWMEMSISOURCE_SUB 
 ADD BH,CL
 JMP OUTT_CHDISMEMSISOURCE
 CHDESWMEMSISOURCE_SUB:
 CMP BP,3
 JNZ CHDESWMEMSISOURCE_SBB
 SUB BH,CL
 JMP OUTT_CHDISMEMSISOURCE
 CHDESWMEMSISOURCE_SBB:
 CMP BP,4
 JNZ CHDESWMEMSISOURCE_ADC
 SBB BH,CL
 JMP OUTT_CHDISMEMSISOURCE
 CHDESWMEMSISOURCE_ADC:
 CMP BP,5
 JNZ CHDESWMEMSISOURCE_AND
 ADC BH,CL
 JMP OUTT_CHDISMEMSISOURCE
 CHDESWMEMSISOURCE_AND:
 CMP BP,6
 JNZ CHDESWMEMSISOURCE_XOR
 AND BH,CL
 JMP OUTT_CHDISMEMSISOURCE
 CHDESWMEMSISOURCE_XOR:
 XOR BH,CL
 
 OUTT_CHDISMEMSISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,BX
 JMP NAG7
NTA_CHDES_WMEMDI_PLUS:
MOV BX,IDI1
CMP BX,0FH
JA ERROR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RCX1
MOV AX,FLAGS
PUSH AX
POPF
 CHDESWMEMDISOURCE_MOV:
 CMP BP,1
 JNZ CHDESWMEMDISOURCE_ADD
 MOV BH,CL
 JMP OUTT_CHDISMEMDISOURCE
 CHDESWMEMDISOURCE_ADD:
 CMP BP,2
 JNZ CHDESWMEMDISOURCE_SUB 
 ADD BH,CL
 JMP OUTT_CHDISMEMDISOURCE
 CHDESWMEMDISOURCE_SUB:
 CMP BP,3
 JNZ CHDESWMEMDISOURCE_SBB
 SUB BH,CL
 JMP OUTT_CHDISMEMDISOURCE
 CHDESWMEMDISOURCE_SBB:
 CMP BP,4
 JNZ CHDESWMEMDISOURCE_ADC
 SBB BH,CL
 JMP OUTT_CHDISMEMDISOURCE
 CHDESWMEMDISOURCE_ADC:
 CMP BP,5
 JNZ CHDESWMEMDISOURCE_AND
 ADC BH,CL
 JMP OUTT_CHDISMEMDISOURCE
 CHDESWMEMDISOURCE_AND:
 CMP BP,6
 JNZ CHDESWMEMDISOURCE_XOR
 AND BH,CL
 JMP OUTT_CHDISMEMDISOURCE
 CHDESWMEMDISOURCE_XOR:
 XOR BH,CL
 
 OUTT_CHDISMEMDISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,BX
 JMP NAG7
NTACHDESWAHSOURCE_PLUS:
MOV BL,BYTE PTR RAX1+1
MOV DX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
 CHXDESWAHSOURCE_MOV:
 CMP BP,1
 JNZ CHDESWAHSOURCE_ADD
 MOV DH,BL
 JMP OUTT_CHDISAHSOURCE
 CHDESWAHSOURCE_ADD:
 CMP BP,2
 JNZ CHDESWAHSOURCE_SUB 
 ADD DH,BL
 JMP OUTT_CHDISAHSOURCE
 CHDESWAHSOURCE_SUB:
 CMP BP,3
 JNZ CHDESWAHSOURCE_SBB
 SUB DH,BL
 JMP OUTT_CHDISAHSOURCE
 CHDESWAHSOURCE_SBB:
 CMP BP,4
 JNZ CHDESWAHSOURCE_ADC
 SBB DH,BL
 JMP OUTT_CHDISAHSOURCE
 CHDESWAHSOURCE_ADC:
 CMP BP,5
 JNZ CHDESWAHSOURCE_AND
 ADC DH,BL
 JMP OUTT_CHDISAHSOURCE
 CHDESWAHSOURCE_AND:
 CMP BP,6
 JNZ CHDESWAHSOURCE_XOR
 AND DH,BL
 JMP OUTT_CHDISAHSOURCE
 CHDESWAHSOURCE_XOR:
 XOR DH,BL
 
 OUTT_CHDISAHSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,DX
 JMP NAG7
NTACHDESWBLSOURCE_PLUS:
MOV BL,BYTE PTR RBX1
MOV DX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
 CHXDESWBLSOURCE_MOV:
 CMP BP,1
 JNZ CHDESWBLSOURCE_ADD
 MOV DH,BL
 JMP OUTT_CHDISBLSOURCE
 CHDESWBLSOURCE_ADD:
 CMP BP,2
 JNZ CHDESWBLSOURCE_SUB 
 ADD DH,BL
 JMP OUTT_CHDISBLSOURCE
 CHDESWBLSOURCE_SUB:
 CMP BP,3
 JNZ CHDESWBLSOURCE_SBB
 SUB DH,BL
 JMP OUTT_CHDISBLSOURCE
 CHDESWBLSOURCE_SBB:
 CMP BP,4
 JNZ CHDESWBLSOURCE_ADC
 SBB DH,BL
 JMP OUTT_CHDISBLSOURCE
 CHDESWBLSOURCE_ADC:
 CMP BP,5
 JNZ CHDESWBLSOURCE_AND
 ADC DH,BL
 JMP OUTT_CHDISBLSOURCE
 CHDESWBLSOURCE_AND:
 CMP BP,6
 JNZ CHDESWBLSOURCE_XOR
 AND DH,BL
 JMP OUTT_CHDISBLSOURCE
 CHDESWBLSOURCE_XOR:
 XOR DH,BL
 
 OUTT_CHDISBLSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,DX
 JMP NAG7
NTACHDESWBHSOURCE_PLUS:
MOV BL,BYTE PTR RBX1+1
MOV DX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
 CHXDESWBHSOURCE_MOV:
 CMP BP,1
 JNZ CHDESWBHSOURCE_ADD
 MOV DH,BL
 JMP OUTT_CHDISBHSOURCE
 CHDESWBHSOURCE_ADD:
 CMP BP,2
 JNZ CHDESWBHSOURCE_SUB 
 ADD DH,BL
 JMP OUTT_CHDISBHSOURCE
 CHDESWBHSOURCE_SUB:
 CMP BP,3
 JNZ CHDESWBHSOURCE_SBB
 SUB DH,BL
 JMP OUTT_CHDISBHSOURCE
 CHDESWBHSOURCE_SBB:
 CMP BP,4
 JNZ CHDESWBHSOURCE_ADC
 SBB DH,BL
 JMP OUTT_CHDISBHSOURCE
 CHDESWBHSOURCE_ADC:
 CMP BP,5
 JNZ CHDESWBHSOURCE_AND
 ADC DH,BL
 JMP OUTT_CHDISBHSOURCE
 CHDESWBHSOURCE_AND:
 CMP BP,6
 JNZ CHDESWBHSOURCE_XOR
 AND DH,BL
 JMP OUTT_CHDISBHSOURCE
 CHDESWBHSOURCE_XOR:
 XOR DH,BL
 
 OUTT_CHDISBHSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,DX
 JMP NAG7
NTACHDESWCLSOURCE_PLUS:
MOV BL,BYTE PTR RCX1
MOV DX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
 CHXDESWCLSOURCE_MOV:
 CMP BP,1
 JNZ CHDESWCLSOURCE_ADD
 MOV DH,BL
 JMP OUTT_CHDISCLSOURCE
 CHDESWCLSOURCE_ADD:
 CMP BP,2
 JNZ CHDESWCLSOURCE_SUB 
 ADD DH,BL
 JMP OUTT_CHDISCLSOURCE
 CHDESWCLSOURCE_SUB:
 CMP BP,3
 JNZ CHDESWCLSOURCE_SBB
 SUB DH,BL
 JMP OUTT_CHDISCLSOURCE
 CHDESWCLSOURCE_SBB:
 CMP BP,4
 JNZ CHDESWCLSOURCE_ADC
 SBB DH,BL
 JMP OUTT_CHDISCLSOURCE
 CHDESWCLSOURCE_ADC:
 CMP BP,5
 JNZ CHDESWCLSOURCE_AND
 ADC DH,BL
 JMP OUTT_CHDISCLSOURCE
 CHDESWCLSOURCE_AND:
 CMP BP,6
 JNZ CHDESWCLSOURCE_XOR
 AND DH,BL
 JMP OUTT_CHDISCLSOURCE
 CHDESWCLSOURCE_XOR:
 XOR DH,BL
 
 OUTT_CHDISCLSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,DX
 JMP NAG7
NTACHDESWCHSOURCE_PLUS:
MOV BL,BYTE PTR RCX1+1
MOV DX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
 CHXDESWCHSOURCE_MOV:
 CMP BP,1
 JNZ CHDESWCHSOURCE_ADD
 MOV DH,BL
 JMP OUTT_CHDISCHSOURCE
 CHDESWCHSOURCE_ADD:
 CMP BP,2
 JNZ CHDESWCHSOURCE_SUB 
 ADD DH,BL
 JMP OUTT_CHDISCHSOURCE
 CHDESWCHSOURCE_SUB:
 CMP BP,3
 JNZ CHDESWCHSOURCE_SBB
 SUB DH,BL
 JMP OUTT_CHDISCHSOURCE
 CHDESWCHSOURCE_SBB:
 CMP BP,4
 JNZ CHDESWCHSOURCE_ADC
 SBB DH,BL
 JMP OUTT_CHDISCHSOURCE
 CHDESWCHSOURCE_ADC:
 CMP BP,5
 JNZ CHDESWCHSOURCE_AND
 ADC DH,BL
 JMP OUTT_CHDISCHSOURCE
 CHDESWCHSOURCE_AND:
 CMP BP,6
 JNZ CHDESWCHSOURCE_XOR
 AND DH,BL
 JMP OUTT_CHDISCHSOURCE
 CHDESWCHSOURCE_XOR:
 XOR DH,BL
 
 OUTT_CHDISCHSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,DX
 JMP NAG7
NTACHDESWDLSOURCE_PLUS:
MOV BL,BYTE PTR RDX1
MOV DX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
 CHXDESWDLSOURCE_MOV:
 CMP BP,1
 JNZ CHDESWDLSOURCE_ADD
 MOV DH,BL
 JMP OUTT_CHDISDLSOURCE
 CHDESWDLSOURCE_ADD:
 CMP BP,2
 JNZ CHDESWDLSOURCE_SUB 
 ADD DH,BL
 JMP OUTT_CHDISDLSOURCE
 CHDESWDLSOURCE_SUB:
 CMP BP,3
 JNZ CHDESWDLSOURCE_SBB
 SUB DH,BL
 JMP OUTT_CHDISDLSOURCE
 CHDESWDLSOURCE_SBB:
 CMP BP,4
 JNZ CHDESWDLSOURCE_ADC
 SBB DH,BL
 JMP OUTT_CHDISDLSOURCE
 CHDESWDLSOURCE_ADC:
 CMP BP,5
 JNZ CHDESWDLSOURCE_AND
 ADC DH,BL
 JMP OUTT_CHDISDLSOURCE
 CHDESWDLSOURCE_AND:
 CMP BP,6
 JNZ CHDESWDLSOURCE_XOR
 AND DH,BL
 JMP OUTT_CHDISDLSOURCE
 CHDESWDLSOURCE_XOR:
 XOR DH,BL
 
 OUTT_CHDISDLSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,DX
 JMP NAG7
NTACHDESWDHSOURCE_PLUS:
MOV BL,BYTE PTR RDX1+1
MOV DX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
 CHXDESWDHSOURCE_MOV:
 CMP BP,1
 JNZ CHDESWDHSOURCE_ADD
 MOV DH,BL
 JMP OUTT_CHDISDHSOURCE
 CHDESWDHSOURCE_ADD:
 CMP BP,2
 JNZ CHDESWDHSOURCE_SUB 
 ADD DH,BL
 JMP OUTT_CHDISDHSOURCE
 CHDESWDHSOURCE_SUB:
 CMP BP,3
 JNZ CHDESWDHSOURCE_SBB
 SUB DH,BL
 JMP OUTT_CHDISDHSOURCE
 CHDESWDHSOURCE_SBB:
 CMP BP,4
 JNZ CHDESWDHSOURCE_ADC
 SBB DH,BL
 JMP OUTT_CHDISDHSOURCE
 CHDESWDHSOURCE_ADC:
 CMP BP,5
 JNZ CHDESWDHSOURCE_AND
 ADC DH,BL
 JMP OUTT_CHDISDHSOURCE
 CHDESWDHSOURCE_AND:
 CMP BP,6
 JNZ CHDESWDHSOURCE_XOR
 AND DH,BL
 JMP OUTT_CHDISDHSOURCE
 CHDESWDHSOURCE_XOR:
 XOR DH,BL
 
 OUTT_CHDISDHSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,DX
 JMP NAG7
NTACHDESW_RKM_YA_MEMRKM_YAERROR_PLUS:
MOV AL,[SI+7]
CMP AL,91
JZ MEMRKM_ORERROR_CHDES_PLUS

RKM_ORERROR_CHDES_PLUS:
MOV AL,[SI+8]
MOV VAR,BP
RET_NUM_COUNT  COMp+7
MOV BP,VAR
CMP CX,2
JA ERROR
MOV BX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
 CHDESWRAKAMSOURCE_MOV:
 CMP BP,1
 JNZ CHDESWRAKAMSOURCE_ADD
 MOV BH,DL
 JMP OUTT_CHDISRAKAMSOURCE
 CHDESWRAKAMSOURCE_ADD:
 CMP BP,2
 JNZ CHDESWRAKAMSOURCE_SUB 
 ADD BH,DL
 JMP OUTT_CHDISRAKAMSOURCE
 CHDESWRAKAMSOURCE_SUB:
 CMP BP,3
 JNZ CHDESWRAKAMSOURCE_SBB
 SUB BH,DL
 JMP OUTT_CHDISRAKAMSOURCE
 CHDESWRAKAMSOURCE_SBB:
 CMP BP,4
 JNZ CHDESWRAKAMSOURCE_ADC
 SBB BH,DL
 JMP OUTT_CHDISRAKAMSOURCE
 CHDESWRAKAMSOURCE_ADC:
 CMP BP,5
 JNZ CHDESWRAKAMSOURCE_AND
 ADC BH,DL
 JMP OUTT_CHDISRAKAMSOURCE
 CHDESWRAKAMSOURCE_AND:
 CMP BP,6
 JNZ CHDESWRAKAMSOURCE_XOR
 AND BH,DL
 JMP OUTT_CHDISRAKAMSOURCE
 CHDESWRAKAMSOURCE_XOR:
 XOR BH,DL
 
 OUTT_CHDISRAKAMSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,BX
 JMP NAG7
MEMRKM_ORERROR_CHDES_PLUS:
MOV AL,[SI+9]
CMP AL,108
JZ ERROR
CMP AL,104
JZ ERROR
MOV VAR,BP
RET_NUM_COUNT  COMp+8
MOV BP,VAR
CMP CX,4
JA ERROR
CMP DX,0FH
JA ERROR
MOV BX,DX
MOV CX,0
MOV CL,MEMORY1[BX]
MOV BX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
 CHDESWMEMRAKAMSOURCE_MOV:
 CMP BP,1
 JNZ CHDESWMEMRAKAMSOURCE_ADD
 MOV BH,CL
 JMP OUTT_CHDISMEMRAKAMSOURCE
 CHDESWMEMRAKAMSOURCE_ADD:
 CMP BP,2
 JNZ CHDESWMEMRAKAMSOURCE_SUB 
 ADD BH,CL
 JMP OUTT_CHDISMEMRAKAMSOURCE
 CHDESWMEMRAKAMSOURCE_SUB:
 CMP BP,3
 JNZ CHDESWMEMRAKAMSOURCE_SBB
 SUB BH,CL
 JMP OUTT_CHDISMEMRAKAMSOURCE
 CHDESWMEMRAKAMSOURCE_SBB:
 CMP BP,4
 JNZ CHDESWMEMRAKAMSOURCE_ADC
 SBB BH,CL
 JMP OUTT_CHDISMEMRAKAMSOURCE
 CHDESWMEMRAKAMSOURCE_ADC:
 CMP BP,5
 JNZ CHDESWMEMRAKAMSOURCE_AND
 ADC BH,CL
 JMP OUTT_CHDISMEMRAKAMSOURCE
 CHDESWMEMRAKAMSOURCE_AND:
 CMP BP,6
 JNZ CHDESWMEMRAKAMSOURCE_XOR
 AND BH,CL
 JMP OUTT_CHDISMEMRAKAMSOURCE
 CHDESWMEMRAKAMSOURCE_XOR:
 XOR BH,CL
 
 OUTT_CHDISMEMRAKAMSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,BX
 JMP NAG7  
;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTADHDIS_PLUS:
MOV BX,0
  MOV BL,[SI+8]
  CMP BL,104
  JNAE NTADHDESW_RKM_YA_MEMRKM_YAERROR_PLUS
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+7] 
  MOV CL,[SI+8]
  ADD AX,CX
  CMP AX,217
  JZ ERROR
  CMP AX,218 
  JZ ERROR
  CMP AX,219
  JZ ERROR
  CMP AX,220
 JZ ERROR
 CMP AX,205
 JZ NTADHDESWDIORALLWDIERROR_PLUS
 CMP AX,201
 JZ NTADHDESWAHSOURCE_PLUS
 CMP AX,206
 JZ NTADHDESWBLSOURCE_PLUS
 CMP AX,202
 JZ NTADHDESWBHSOURCE_PLUS 
 CMP AX,207
 JZ NTADHDESWCLSOURCE_PLUS
 CMP AX,203
 JZ NTADHDESWCHSOURCE_PLUS
 CMP AX,208
 JZ NTADHDESWDLSOURCE_PLUS
 CMP AX,204
 JZ NTADHDESWDHSOURCE_PLUS   
 MOV BX,0
 MOV CX,0                              ;TAKECARE
 MOV CL,[SI+9]
 MOV BL,[SI+10]
 ADD CX,BX
 ADD AX,CX

 CMP AX,402
 JZ DHDES_WNTA_MEMBX_PLUS
 CMP AX,404              
 JZ NTA_DHDES_WMEMSI_PLUS
 CMP AX,389
 JZ NTA_DHDES_WMEMDI_PLUS
  
 JMP NTADHDESW_RKM_YA_MEMRKM_YAERROR_PLUS  
 
 
  
NTADHDESWDIORALLWDIERROR_PLUS: 
MOV AL,[SI+7]
CMP AL,100 
JZ ERROR
NTADHDESWALSOURCE_PLUS:
MOV BL,BYTE PTR RAX1
MOV CX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
 DHDESWALSOURCE_MOV:
 CMP BP,1
 JNZ DHDESWALSOURCE_ADD
 MOV CH,BL
 JMP OUTT_DHDISALSOURCE
 DHDESWALSOURCE_ADD:
 CMP BP,2
 JNZ DHDESWALSOURCE_SUB 
 ADD CH,BL
 JMP OUTT_DHDISALSOURCE
 DHDESWALSOURCE_SUB:
 CMP BP,3
 JNZ DHDESWALSOURCE_SBB
 SUB CH,BL
 JMP OUTT_DHDISALSOURCE
 DHDESWALSOURCE_SBB:
 CMP BP,4
 JNZ DHDESWALSOURCE_ADC
 SBB CH,BL
 JMP OUTT_DHDISALSOURCE
 DHDESWALSOURCE_ADC:
 CMP BP,5
 JNZ DHDESWALSOURCE_AND
 ADC CH,BL
 JMP OUTT_DHDISALSOURCE
 DHDESWALSOURCE_AND:
 CMP BP,6
 JNZ DHDESWALSOURCE_XOR
 AND CH,BL
 JMP OUTT_DHDISALSOURCE
 DHDESWALSOURCE_XOR:
 XOR CH,BL
 
 OUTT_DHDISALSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,CX
 JMP NAG7

DHDES_WNTA_MEMBX_PLUS:
MOV BX,RBX1
CMP BX,0FH
JA ERROR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RDX1
MOV AX,FLAGS
PUSH AX
POPF
 DHDESWMEMBXSOURCE_MOV:
 CMP BP,1
 JNZ DHDESWMEMBXSOURCE_ADD
 MOV BH,CL
 JMP OUTT_DHDISMEMBXSOURCE
 DHDESWMEMBXSOURCE_ADD:
 CMP BP,2
 JNZ DHDESWMEMBXSOURCE_SUB 
 ADD BH,CL
 JMP OUTT_DHDISMEMBXSOURCE
 DHDESWMEMBXSOURCE_SUB:
 CMP BP,3
 JNZ DHDESWMEMBXSOURCE_SBB
 SUB BH,CL
 JMP OUTT_DHDISMEMBXSOURCE
 DHDESWMEMBXSOURCE_SBB:
 CMP BP,4
 JNZ DHDESWMEMBXSOURCE_ADC
 SBB BH,CL
 JMP OUTT_DHDISMEMBXSOURCE
 DHDESWMEMBXSOURCE_ADC:
 CMP BP,5
 JNZ DHDESWMEMBXSOURCE_AND
 ADC BH,CL
 JMP OUTT_DHDISMEMBXSOURCE
 DHDESWMEMBXSOURCE_AND:
 CMP BP,6
 JNZ DHDESWMEMBXSOURCE_XOR
 AND BH,CL
 JMP OUTT_DHDISMEMBXSOURCE
 DHDESWMEMBXSOURCE_XOR:
 XOR BH,CL
 
 OUTT_DHDISMEMBXSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,BX
 JMP NAG7
NTA_DHDES_WMEMSI_PLUS:
MOV BX,ISI1
CMP BX,0FH
JA ERROR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RDX1
MOV AX,FLAGS
PUSH AX
POPF
 DHDESWMEMSISOURCE_MOV:
 CMP BP,1
 JNZ DHDESWMEMSISOURCE_ADD
 MOV BH,CL
 JMP OUTT_DHDISMEMSISOURCE
 DHDESWMEMSISOURCE_ADD:
 CMP BP,2
 JNZ DHDESWMEMSISOURCE_SUB 
 ADD BH,CL
 JMP OUTT_DHDISMEMSISOURCE
 DHDESWMEMSISOURCE_SUB:
 CMP BP,3
 JNZ DHDESWMEMSISOURCE_SBB
 SUB BH,CL
 JMP OUTT_DHDISMEMSISOURCE
 DHDESWMEMSISOURCE_SBB:
 CMP BP,4
 JNZ DHDESWMEMSISOURCE_ADC
 SBB BH,CL
 JMP OUTT_DHDISMEMSISOURCE
 DHDESWMEMSISOURCE_ADC:
 CMP BP,5
 JNZ DHDESWMEMSISOURCE_AND
 ADC BH,CL
 JMP OUTT_DHDISMEMSISOURCE
 DHDESWMEMSISOURCE_AND:
 CMP BP,6
 JNZ DHDESWMEMSISOURCE_XOR
 AND BH,CL
 JMP OUTT_DHDISMEMSISOURCE
 DHDESWMEMSISOURCE_XOR:
 XOR BH,CL
 
 OUTT_DHDISMEMSISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,BX
 JMP NAG7
NTA_DHDES_WMEMDI_PLUS:
MOV BX,IDI1
CMP BX,0FH
JA ERROR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RDX1
MOV AX,FLAGS
PUSH AX
POPF
 DHDESWMEMDISOURCE_MOV:
 CMP BP,1
 JNZ DHDESWMEMDISOURCE_ADD
 MOV BH,CL
 JMP OUTT_DHDISMEMDISOURCE
 DHDESWMEMDISOURCE_ADD:
 CMP BP,2
 JNZ DHDESWMEMDISOURCE_SUB 
 ADD BH,CL
 JMP OUTT_DHDISMEMDISOURCE
 DHDESWMEMDISOURCE_SUB:
 CMP BP,3
 JNZ DHDESWMEMDISOURCE_SBB
 SUB BH,CL
 JMP OUTT_DHDISMEMDISOURCE
 DHDESWMEMDISOURCE_SBB:
 CMP BP,4
 JNZ DHDESWMEMDISOURCE_ADC
 SBB BH,CL
 JMP OUTT_DHDISMEMDISOURCE
 DHDESWMEMDISOURCE_ADC:
 CMP BP,5
 JNZ DHDESWMEMDISOURCE_AND
 ADC BH,CL
 JMP OUTT_DHDISMEMDISOURCE
 DHDESWMEMDISOURCE_AND:
 CMP BP,6
 JNZ DHDESWMEMDISOURCE_XOR
 AND BH,CL
 JMP OUTT_DHDISMEMDISOURCE
 DHDESWMEMDISOURCE_XOR:
 XOR BH,CL
 
 OUTT_DHDISMEMDISOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,BX
 JMP NAG7
NTADHDESWAHSOURCE_PLUS:
MOV BL,BYTE PTR RAX1+1
MOV DX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
 DHXDESWAHSOURCE_MOV:
 CMP BP,1
 JNZ DHDESWAHSOURCE_ADD
 MOV DH,BL
 JMP OUTT_DHDISAHSOURCE
 DHDESWAHSOURCE_ADD:
 CMP BP,2
 JNZ DHDESWAHSOURCE_SUB 
 ADD DH,BL
 JMP OUTT_DHDISAHSOURCE
 DHDESWAHSOURCE_SUB:
 CMP BP,3
 JNZ DHDESWAHSOURCE_SBB
 SUB DH,BL
 JMP OUTT_DHDISAHSOURCE
 DHDESWAHSOURCE_SBB:
 CMP BP,4
 JNZ DHDESWAHSOURCE_ADC
 SBB DH,BL
 JMP OUTT_DHDISAHSOURCE
 DHDESWAHSOURCE_ADC:
 CMP BP,5
 JNZ DHDESWAHSOURCE_AND
 ADC DH,BL
 JMP OUTT_DHDISAHSOURCE
 DHDESWAHSOURCE_AND:
 CMP BP,6
 JNZ DHDESWAHSOURCE_XOR
 AND DH,BL
 JMP OUTT_DHDISAHSOURCE
 DHDESWAHSOURCE_XOR:
 XOR DH,BL
 
 OUTT_DHDISAHSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,DX
 JMP NAG7
NTADHDESWBLSOURCE_PLUS:
MOV BL,BYTE PTR RBX1
MOV DX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
 DHXDESWBLSOURCE_MOV:
 CMP BP,1
 JNZ DHDESWBLSOURCE_ADD
 MOV DH,BL
 JMP OUTT_DHDISBLSOURCE
 DHDESWBLSOURCE_ADD:
 CMP BP,2
 JNZ DHDESWBLSOURCE_SUB 
 ADD DH,BL
 JMP OUTT_DHDISBLSOURCE
 DHDESWBLSOURCE_SUB:
 CMP BP,3
 JNZ DHDESWBLSOURCE_SBB
 SUB DH,BL
 JMP OUTT_DHDISBLSOURCE
 DHDESWBLSOURCE_SBB:
 CMP BP,4
 JNZ DHDESWBLSOURCE_ADC
 SBB DH,BL
 JMP OUTT_DHDISBLSOURCE
 DHDESWBLSOURCE_ADC:
 CMP BP,5
 JNZ DHDESWBLSOURCE_AND
 ADC DH,BL
 JMP OUTT_DHDISBLSOURCE
 DHDESWBLSOURCE_AND:
 CMP BP,6
 JNZ DHDESWBLSOURCE_XOR
 AND DH,BL
 JMP OUTT_DHDISBLSOURCE
 DHDESWBLSOURCE_XOR:
 XOR DH,BL
 
 OUTT_DHDISBLSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,DX
 JMP NAG7
NTADHDESWBHSOURCE_PLUS:
MOV BL,BYTE PTR RBX1+1
MOV DX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
 DHXDESWBHSOURCE_MOV:
 CMP BP,1
 JNZ DHDESWBHSOURCE_ADD
 MOV DH,BL
 JMP OUTT_DHDISBHSOURCE
 DHDESWBHSOURCE_ADD:
 CMP BP,2
 JNZ DHDESWBHSOURCE_SUB 
 ADD DH,BL
 JMP OUTT_DHDISBHSOURCE
 DHDESWBHSOURCE_SUB:
 CMP BP,3
 JNZ DHDESWBHSOURCE_SBB
 SUB DH,BL
 JMP OUTT_DHDISBHSOURCE
 DHDESWBHSOURCE_SBB:
 CMP BP,4
 JNZ DHDESWBHSOURCE_ADC
 SBB DH,BL
 JMP OUTT_DHDISBHSOURCE
 DHDESWBHSOURCE_ADC:
 CMP BP,5
 JNZ DHDESWBHSOURCE_AND
 ADC DH,BL
 JMP OUTT_DHDISBHSOURCE
 DHDESWBHSOURCE_AND:
 CMP BP,6
 JNZ DHDESWBHSOURCE_XOR
 AND DH,BL
 JMP OUTT_DHDISBHSOURCE
 DHDESWBHSOURCE_XOR:
 XOR DH,BL
 
 OUTT_DHDISBHSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,DX
 JMP NAG7
NTADHDESWCLSOURCE_PLUS:
MOV BL,BYTE PTR RCX1
MOV DX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
 DHXDESWCLSOURCE_MOV:
 CMP BP,1
 JNZ DHDESWCLSOURCE_ADD
 MOV DH,BL
 JMP OUTT_DHDISCLSOURCE
 DHDESWCLSOURCE_ADD:
 CMP BP,2
 JNZ DHDESWCLSOURCE_SUB 
 ADD DH,BL
 JMP OUTT_DHDISCLSOURCE
 DHDESWCLSOURCE_SUB:
 CMP BP,3
 JNZ DHDESWCLSOURCE_SBB
 SUB DH,BL
 JMP OUTT_DHDISCLSOURCE
 DHDESWCLSOURCE_SBB:
 CMP BP,4
 JNZ DHDESWCLSOURCE_ADC
 SBB DH,BL
 JMP OUTT_DHDISCLSOURCE
 DHDESWCLSOURCE_ADC:
 CMP BP,5
 JNZ DHDESWCLSOURCE_AND
 ADC DH,BL
 JMP OUTT_DHDISCLSOURCE
 DHDESWCLSOURCE_AND:
 CMP BP,6
 JNZ DHDESWCLSOURCE_XOR
 AND DH,BL
 JMP OUTT_DHDISCLSOURCE
 DHDESWCLSOURCE_XOR:
 XOR DH,BL
 
 OUTT_DHDISCLSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,DX
 JMP NAG7
NTADHDESWCHSOURCE_PLUS:
MOV BL,BYTE PTR RCX1+1
MOV DX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
 DHXDESWCHSOURCE_MOV:
 CMP BP,1
 JNZ DHDESWCHSOURCE_ADD
 MOV DH,BL
 JMP OUTT_DHDISCHSOURCE
 DHDESWCHSOURCE_ADD:
 CMP BP,2
 JNZ DHDESWCHSOURCE_SUB 
 ADD DH,BL
 JMP OUTT_DHDISCHSOURCE
 DHDESWCHSOURCE_SUB:
 CMP BP,3
 JNZ DHDESWCHSOURCE_SBB
 SUB DH,BL
 JMP OUTT_DHDISCHSOURCE
 DHDESWCHSOURCE_SBB:
 CMP BP,4
 JNZ DHDESWCHSOURCE_ADC
 SBB DH,BL
 JMP OUTT_DHDISCHSOURCE
 DHDESWCHSOURCE_ADC:
 CMP BP,5
 JNZ DHDESWCHSOURCE_AND
 ADC DH,BL
 JMP OUTT_DHDISCHSOURCE
 DHDESWCHSOURCE_AND:
 CMP BP,6
 JNZ DHDESWCHSOURCE_XOR
 AND DH,BL
 JMP OUTT_DHDISCHSOURCE
 DHDESWCHSOURCE_XOR:
 XOR DH,BL
 
 OUTT_DHDISCHSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,DX
 JMP NAG7
NTADHDESWDLSOURCE_PLUS:
MOV BL,BYTE PTR RDX1
MOV DX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
 DHXDESWDLSOURCE_MOV:
 CMP BP,1
 JNZ DHDESWDLSOURCE_ADD
 MOV DH,BL
 JMP OUTT_DHDISDLSOURCE
 DHDESWDLSOURCE_ADD:
 CMP BP,2
 JNZ DHDESWDLSOURCE_SUB 
 ADD DH,BL
 JMP OUTT_DHDISDLSOURCE
 DHDESWDLSOURCE_SUB:
 CMP BP,3
 JNZ DHDESWDLSOURCE_SBB
 SUB DH,BL
 JMP OUTT_DHDISDLSOURCE
 DHDESWDLSOURCE_SBB:
 CMP BP,4
 JNZ DHDESWDLSOURCE_ADC
 SBB DH,BL
 JMP OUTT_DHDISDLSOURCE
 DHDESWDLSOURCE_ADC:
 CMP BP,5
 JNZ DHDESWDLSOURCE_AND
 ADC DH,BL
 JMP OUTT_DHDISDLSOURCE
 DHDESWDLSOURCE_AND:
 CMP BP,6
 JNZ DHDESWDLSOURCE_XOR
 AND DH,BL
 JMP OUTT_DHDISDLSOURCE
 DHDESWDLSOURCE_XOR:
 XOR DH,BL
 
 OUTT_DHDISDLSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,DX
 JMP NAG7
NTADHDESWDHSOURCE_PLUS:
MOV BL,BYTE PTR RDX1+1
MOV DX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
 DHXDESWDHSOURCE_MOV:
 CMP BP,1
 JNZ DHDESWDHSOURCE_ADD
 MOV DH,BL
 JMP OUTT_DHDISDHSOURCE
 DHDESWDHSOURCE_ADD:
 CMP BP,2
 JNZ DHDESWDHSOURCE_SUB 
 ADD DH,BL
 JMP OUTT_DHDISDHSOURCE
 DHDESWDHSOURCE_SUB:
 CMP BP,3
 JNZ DHDESWDHSOURCE_SBB
 SUB DH,BL
 JMP OUTT_DHDISDHSOURCE
 DHDESWDHSOURCE_SBB:
 CMP BP,4
 JNZ DHDESWDHSOURCE_ADC
 SBB DH,BL
 JMP OUTT_DHDISDHSOURCE
 DHDESWDHSOURCE_ADC:
 CMP BP,5
 JNZ DHDESWDHSOURCE_AND
 ADC DH,BL
 JMP OUTT_DHDISDHSOURCE
 DHDESWDHSOURCE_AND:
 CMP BP,6
 JNZ DHDESWDHSOURCE_XOR
 AND DH,BL
 JMP OUTT_DHDISDHSOURCE
 DHDESWDHSOURCE_XOR:
 XOR DH,BL
 
 OUTT_DHDISDHSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,DX
 JMP NAG7
NTADHDESW_RKM_YA_MEMRKM_YAERROR_PLUS:
MOV AL,[SI+7]
CMP AL,91
JZ MEMRKM_ORERROR_DHDES_PLUS

RKM_ORERROR_DHDES_PLUS:
MOV AL,[SI+8]
MOV VAR,BP
RET_NUM_COUNT  COMp+7
MOV BP,VAR
CMP CX,2
JA ERROR
MOV BX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
 DHDESWRAKAMSOURCE_MOV:
 CMP BP,1
 JNZ DHDESWRAKAMSOURCE_ADD
 MOV BH,DL
 JMP OUTT_DHDISRAKAMSOURCE
 DHDESWRAKAMSOURCE_ADD:
 CMP BP,2
 JNZ DHDESWRAKAMSOURCE_SUB 
 ADD BH,DL
 JMP OUTT_DHDISRAKAMSOURCE
 DHDESWRAKAMSOURCE_SUB:
 CMP BP,3
 JNZ DHDESWRAKAMSOURCE_SBB
 SUB BH,DL
 JMP OUTT_DHDISRAKAMSOURCE
 DHDESWRAKAMSOURCE_SBB:
 CMP BP,4
 JNZ DHDESWRAKAMSOURCE_ADC
 SBB BH,DL
 JMP OUTT_DHDISRAKAMSOURCE
 DHDESWRAKAMSOURCE_ADC:
 CMP BP,5
 JNZ DHDESWRAKAMSOURCE_AND
 ADC BH,DL
 JMP OUTT_DHDISRAKAMSOURCE
 DHDESWRAKAMSOURCE_AND:
 CMP BP,6
 JNZ DHDESWRAKAMSOURCE_XOR
 AND BH,DL
 JMP OUTT_DHDISRAKAMSOURCE
 DHDESWRAKAMSOURCE_XOR:
 XOR BH,DL
 
 OUTT_DHDISRAKAMSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,BX
 JMP NAG7
MEMRKM_ORERROR_DHDES_PLUS:
MOV AL,[SI+9]
CMP AL,108
JZ ERROR
CMP AL,104
JZ ERROR
MOV VAR,BP
RET_NUM_COUNT  COMp+8
MOV BP,VAR
CMP CX,4
JA ERROR
CMP DX,0FH
JA ERROR
MOV BX,DX
MOV CX,0
MOV CL,MEMORY1[BX]
MOV BX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
 DHDESWMEMRAKAMSOURCE_MOV:
 CMP BP,1
 JNZ DHDESWMEMRAKAMSOURCE_ADD
 MOV BH,CL
 JMP OUTT_DHDISMEMRAKAMSOURCE
 DHDESWMEMRAKAMSOURCE_ADD:
 CMP BP,2
 JNZ DHDESWMEMRAKAMSOURCE_SUB 
 ADD BH,CL
 JMP OUTT_DHDISMEMRAKAMSOURCE
 DHDESWMEMRAKAMSOURCE_SUB:
 CMP BP,3
 JNZ DHDESWMEMRAKAMSOURCE_SBB
 SUB BH,CL
 JMP OUTT_DHDISMEMRAKAMSOURCE
 DHDESWMEMRAKAMSOURCE_SBB:
 CMP BP,4
 JNZ DHDESWMEMRAKAMSOURCE_ADC
 SBB BH,CL
 JMP OUTT_DHDISMEMRAKAMSOURCE
 DHDESWMEMRAKAMSOURCE_ADC:
 CMP BP,5
 JNZ DHDESWMEMRAKAMSOURCE_AND
 ADC BH,CL
 JMP OUTT_DHDISMEMRAKAMSOURCE
 DHDESWMEMRAKAMSOURCE_AND:
 CMP BP,6
 JNZ DHDESWMEMRAKAMSOURCE_XOR
 AND BH,CL
 JMP OUTT_DHDISMEMRAKAMSOURCE
 DHDESWMEMRAKAMSOURCE_XOR:
 XOR BH,CL
 
 OUTT_DHDISMEMRAKAMSOURCE:
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,BX
 JMP NAG7  
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
ERROR:

JMP OUTT
NAG7:
    
OUTT:
         ;AFTER INST:
       JMP NOT_COMMAND
     ;-------------------------------------------------------ROR----------------------------------------------------------

OPERATIONS2: 
      MOV AL,ELDOOR
      CMP AL,2
      JZ D2_RR

      D1_RR: 
        GENERAL_INST COMp,COMp1,A_SIZE1
       JMP START_RR

      D2_RR:
        GENERAL_INST COMp,COMp2,A_SIZE2

       START_RR:

       ;INST:
        
    MOV SI,OFFSET COMP 
    MOV AX,0
    MOV CX,0
    MOV AL,[SI+4] 
    ADD CL,[SI+5]
    ADD AX,CX   
    CMP AX,217
    JZ NTAAXDES_SHL
    CMP AX,218                   ;BN3ML JUMP 3LA 7BT 7GAT W M7TNA4 MEMORY1 4OF B2AA
    JZ NTABXDES_SHL
    CMP AX,219                    ;ANAM7TAGFUNCTIONT4OFLE 7WAR 2RKAM KDA KDA HOMA BLKTEER 4 2RKAM 3NDE M4AKL KBEERAA FE 2RKAM 5LE BALK ANA HA5OD 2RKAM MN 3LA 2L4MAAAL YAMEGZ 
    JZ NTACXDES_SHL
    CMP AX,220
    JZ NTADXORSIDES_SHL
    CMP AX,205
    JZ NTADIORALDES_SHL
    CMP AX,201
    JZ NTAAHDIS_SHL
    CMP AX,206
    JZ NTABLDIS_SHL
    CMP AX,202
    JZ NTABHDIS_SHL
    CMP AX,207
    JZ NTACLDIS_SHL
    CMP AX,203
    JZ NTACHDIS_SHL
    CMP AX,208
    JZ NTADLDIS_SHL
    CMP AX,204
    JZ NTADHDIS_SHL 
    JMP ERROR1
NTAAXDES_SHL:
  MOV BX,0
  MOV BL,[SI+8]
  CMP BL,104
  JNAE NTAAXDESW_RKM_YA_MEMRKM_YAERROR1_SHL
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+7] 
  MOV CL,[SI+8]
  ADD AX,CX
  CMP AX,217
  JZ ERROR1
  CMP AX,218 
  JZ ERROR1
  CMP AX,219
  JZ ERROR1
  CMP AX,220
  JZ ERROR1
  CMP AX,205
  JZ ERROR1
  CMP AX,207
  JZ AXNTACLSHIFTL 
  MOV CX,0
  MOV BX,0                            ;TAKECARE
  MOV BL,[SI+9]
  MOV CL,[SI+10]
  ADD CX,BX
  ADD AX,CX
  CMP AX,402
  JZ ERROR1
  CMP AX,404              
  JZ ERROR1
  CMP AX,389
  JZ ERROR1

  JMP NTAAXDESW_RKM_YA_MEMRKM_YAERROR1_SHL  
 
AXNTACLSHIFTL:

MOV CX,RCX1
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX
AXDESWCLSOURCE_SHL:
CMP BP,8
JNZ AXDESWCLSOURCE_SHR
POPF
SHL BX,CL
JMP OUTAXDESWCLSOURCE
AXDESWCLSOURCE_SHR:
CMP BP,9
JNZ AXDESWCLSOURCE_SAR
POPF
SHR BX,CL
JMP OUTAXDESWCLSOURCE  
AXDESWCLSOURCE_SAR:
CMP BP,10
JNZ AXDESWCLSOURCE_ROR
POPF
SAR BX,CL
JMP OUTAXDESWCLSOURCE 
AXDESWCLSOURCE_ROR: 
CMP BP,11
JNZ AXDESWCLSOURCE_ROL
POPF
ROR BX,CL
JMP OUTAXDESWCLSOURCE
AXDESWCLSOURCE_ROL: 
CMP BP,12
JNZ AXDESWCLSOURCE_RCL
POPF
ROL BX,CL
JMP OUTAXDESWCLSOURCE 
AXDESWCLSOURCE_RCL: 
CMP BP,13
JNZ AXDESWCLSOURCE_RCR
POPF
RCL BX,CL
JMP OUTAXDESWCLSOURCE 
AXDESWCLSOURCE_RCR:
POPF
RCR BX,CL
OUTAXDESWCLSOURCE:
MOV RAX1,BX
PUSHF
POP AX
MOV FLAGS,AX
JMP NAG7_SHL
NTAAXDESW_RKM_YA_MEMRKM_YAERROR1_SHL:
MOV AL,[SI+7]
CMP AL,91
JZ ERROR1

RKM_ORERROR1_AXDES_SHL:
MOV AL,[SI+8]
CMP AL,108
JZ ERROR1
CMP AL,104
JZ ERROR1

MOV VAR,BP
RET_NUM_COUNT  COMP+7
MOV BP,VAR
CMP CX,2
JA ERROR1
MOV BX,RAX1 
MOV CL,DL
MOV AX,FLAGS
PUSH AX
AXDESWRKMSOURCE_SHL:
CMP BP,8
JNZ AXDESWRKMSOURCE_SHR
POPF
SHL BX,CL
JMP OUTAXDESWRKMSOURCE
AXDESWRKMSOURCE_SHR:
CMP BP,9
JNZ AXDESWRKMSOURCE_SAR
POPF
SHR BX,CL
JMP OUTAXDESWRKMSOURCE  
AXDESWRKMSOURCE_SAR:
CMP BP,10
JNZ AXDESWRKMSOURCE_ROR
POPF
SAR BX,CL
JMP OUTAXDESWRKMSOURCE 
AXDESWRKMSOURCE_ROR: 
CMP BP,11
JNZ AXDESWRKMSOURCE_ROL
POPF
ROR BX,CL
JMP OUTAXDESWRKMSOURCE
AXDESWRKMSOURCE_ROL: 
CMP BP,12
JNZ AXDESWRKMSOURCE_RCL
POPF
ROL BX,CL
JMP OUTAXDESWRKMSOURCE 
AXDESWRKMSOURCE_RCL: 
CMP BP,13
JNZ AXDESWRKMSOURCE_RCR
POPF
RCL BX,CL
JMP OUTAXDESWRKMSOURCE 
AXDESWRKMSOURCE_RCR:
POPF
RCR BX,CL
OUTAXDESWRKMSOURCE:
PUSHF
POP AX
MOV FLAGS,AX
MOV RAX1,BX
JMP NAG7_SHL
;/////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTABXDES_SHL:
  MOV BX,0
  MOV BL,[SI+8]
  CMP BL,104
  JNAE NTABXDESW_RKM_YA_MEMRKM_YAERROR1_SHL
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+7] 
  MOV CL,[SI+8]
  ADD AX,CX
  CMP AX,217
  JZ ERROR1
  CMP AX,218 
  JZ ERROR1
  CMP AX,219
  JZ ERROR1
  CMP AX,220
 JZ ERROR1
 CMP AX,205
 JZ ERROR1
 CMP AX,207
 JZ BXNTACLSHIFTL
 MOV CX,0
 MOV BX,0                            ;TAKECARE
 MOV BL,[SI+9]
 MOV CL,[SI+10]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ ERROR1
 CMP AX,404              
 JZ ERROR1
 CMP AX,389
 JZ ERROR1
 
 JMP NTABXDESW_RKM_YA_MEMRKM_YAERROR1_SHL  
 
BXNTACLSHIFTL:

MOV CX,RCX1
MOV BX,RBX1
MOV AX,FLAGS
PUSH AX
BXDESWCLSOURCE_SHL:
CMP BP,8
JNZ BXDESWCLSOURCE_SHR
POPF
SHL BX,CL
JMP OUTBXDESWCLSOURCE
BXDESWCLSOURCE_SHR:
CMP BP,9
JNZ BXDESWCLSOURCE_SAR
POPF
SHR BX,CL
JMP OUTBXDESWCLSOURCE  
BXDESWCLSOURCE_SAR:
CMP BP,10
JNZ BXDESWCLSOURCE_ROR
POPF
SAR BX,CL
JMP OUTBXDESWCLSOURCE 
BXDESWCLSOURCE_ROR: 
CMP BP,11
JNZ BXDESWCLSOURCE_ROL
POPF
ROR BX,CL
JMP OUTBXDESWCLSOURCE
BXDESWCLSOURCE_ROL: 
CMP BP,12
JNZ BXDESWCLSOURCE_RCL
POPF
ROL BX,CL
JMP OUTBXDESWCLSOURCE 
BXDESWCLSOURCE_RCL: 
CMP BP,13
JNZ BXDESWCLSOURCE_RCR
POPF
RCL BX,CL
JMP OUTBXDESWCLSOURCE 
BXDESWCLSOURCE_RCR:
POPF
RCR BX,CL 
OUTBXDESWCLSOURCE:
MOV RBX1,BX
PUSHF
POP AX 
MOV FLAGS,AX
JMP NAG7_SHL

 
NTABXDESW_RKM_YA_MEMRKM_YAERROR1_SHL:
MOV AL,[SI+7]
CMP AL,91
JZ ERROR1

RKM_ORERROR1_BXDES_SHL:
MOV AL,[SI+8]
CMP AL,108
JZ ERROR1
CMP AL,104
JZ ERROR1


MOV VAR,BP
RET_NUM_COUNT  COMP+7
MOV BP,VAR

CMP CX,2
JA ERROR1
MOV BX,RBX1 
MOV CL,DL
MOV AX,FLAGS
PUSH AX
BXDESWRKMSOURCE_SHL:
CMP BP,8
JNZ BXDESWRKMSOURCE_SHR
POPF
SHL BX,CL
JMP OUTBXDESWRKMSOURCE
BXDESWRKMSOURCE_SHR:
CMP BP,9
JNZ BXDESWRKMSOURCE_SAR
POPF
SHR BX,CL
JMP OUTBXDESWRKMSOURCE  
BXDESWRKMSOURCE_SAR:
CMP BP,10
JNZ BXDESWRKMSOURCE_ROR
POPF
SAR BX,CL
JMP OUTBXDESWRKMSOURCE 
BXDESWRKMSOURCE_ROR: 
CMP BP,11
JNZ BXDESWRKMSOURCE_ROL
POPF
ROR BX,CL
JMP OUTBXDESWRKMSOURCE
BXDESWRKMSOURCE_ROL: 
CMP BP,12
JNZ BXDESWRKMSOURCE_RCL
POPF
ROL BX,CL
JMP OUTBXDESWRKMSOURCE 
BXDESWRKMSOURCE_RCL: 
CMP BP,13
JNZ BXDESWRKMSOURCE_RCR
POPF
RCL BX,CL
JMP OUTBXDESWRKMSOURCE 
BXDESWRKMSOURCE_RCR:
POPF
RCR BX,CL
OUTBXDESWRKMSOURCE:
PUSHF
POP AX
MOV FLAGS,AX
MOV RBX1,BX
JMP NAG7_SHL
;/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTACXDES_SHL:
  MOV BX,0
  MOV BL,[SI+8]
  CMP BL,104
  JNAE NTACXDESW_RKM_YA_MEMRKM_YAERROR1_SHL
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+7] 
  MOV CL,[SI+8]
  ADD AX,CX
  CMP AX,217
  JZ ERROR1
  CMP AX,218 
  JZ ERROR1
  CMP AX,219
  JZ ERROR1
  CMP AX,220
 JZ ERROR1
 CMP AX,205
 JZ ERROR1
 CMP AX,207
 JZ CXNTACLSHIFTL
 MOV CX,0
 MOV BX,0                            ;TAKECARE
 MOV BL,[SI+9]
 MOV CL,[SI+10]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ ERROR1
 CMP AX,404              
 JZ ERROR1
 CMP AX,389
 JZ ERROR1
 
 JMP NTACXDESW_RKM_YA_MEMRKM_YAERROR1_SHL  
 
CXNTACLSHIFTL:

MOV CX,RCX1
MOV BX,RCX1
MOV AX,FLAGS
PUSH AX
CXDESWCLSOURCE_SHL:
CMP BP,8
JNZ CXDESWCLSOURCE_SHR
POPF
SHL BX,CL
JMP OUTCXDESWCLSOURCE
CXDESWCLSOURCE_SHR:
CMP BP,9
JNZ CXDESWCLSOURCE_SAR
POPF
SHR BX,CL
JMP OUTCXDESWCLSOURCE  
CXDESWCLSOURCE_SAR:
CMP BP,10
JNZ CXDESWCLSOURCE_ROR
POPF
SAR BX,CL
JMP OUTCXDESWCLSOURCE 
CXDESWCLSOURCE_ROR: 
CMP BP,11
JNZ CXDESWCLSOURCE_ROL
POPF
ROR BX,CL
JMP OUTCXDESWCLSOURCE
CXDESWCLSOURCE_ROL: 
CMP BP,12
JNZ CXDESWCLSOURCE_RCL
POPF
ROL BX,CL
JMP OUTCXDESWCLSOURCE 
CXDESWCLSOURCE_RCL: 
CMP BP,13
JNZ CXDESWCLSOURCE_RCR
POPF
RCL BX,CL
JMP OUTCXDESWCLSOURCE 
CXDESWCLSOURCE_RCR:
POPF
RCR BX,CL
OUTCXDESWCLSOURCE:
MOV RCX1,BX
PUSHF
POP AX
MOV FLAGS,AX
JMP NAG7_SHL

 
NTACXDESW_RKM_YA_MEMRKM_YAERROR1_SHL:
MOV AL,[SI+7]
CMP AL,91
JZ ERROR1

RKM_ORERROR1_CXDES_SHL:
MOV AL,[SI+8]
CMP AL,108
JZ ERROR1
CMP AL,104
JZ ERROR1

MOV VAR,BP
RET_NUM_COUNT  COMP+7
MOV BP,VAR
CMP CX,2
JA ERROR1
MOV BX,RCX1 
MOV CL,DL
MOV AX,FLAGS
PUSH AX
CXDESWRKMSOURCE_SHL:
CMP BP,8
JNZ CXDESWRKMSOURCE_SHR
POPF
SHL BX,CL
JMP OUTCXDESWRKMSOURCE
CXDESWRKMSOURCE_SHR:
CMP BP,9
JNZ CXDESWRKMSOURCE_SAR
POPF
SHR BX,CL
JMP OUTCXDESWRKMSOURCE  
CXDESWRKMSOURCE_SAR:
CMP BP,10
JNZ CXDESWRKMSOURCE_ROR
POPF
SAR BX,CL
JMP OUTCXDESWRKMSOURCE 
CXDESWRKMSOURCE_ROR: 
CMP BP,11
JNZ CXDESWRKMSOURCE_ROL
POPF
ROR BX,CL
JMP OUTCXDESWRKMSOURCE
CXDESWRKMSOURCE_ROL: 
CMP BP,12
JNZ CXDESWRKMSOURCE_RCL
POPF
ROL BX,CL
JMP OUTCXDESWRKMSOURCE 
CXDESWRKMSOURCE_RCL: 
CMP BP,13
JNZ CXDESWRKMSOURCE_RCR
POPF
RCL BX,CL
JMP OUTCXDESWRKMSOURCE 
CXDESWRKMSOURCE_RCR:
POPF
RCR BX,CL
OUTCXDESWRKMSOURCE:
PUSHF
POP AX
MOV FLAGS,AX
MOV RCX1,BX
JMP NAG7_SHL
;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTADXORSIDES_SHL:
MOV AL,[SI+4]
CMP AL,115
JZ NTASIDES_SHL
NTADXDES_SHL:
  MOV BX,0
  MOV BL,[SI+8]
  CMP BL,104
  JNAE NTADXDESW_RKM_YA_MEMRKM_YAERROR1_SHL
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+7] 
  MOV CL,[SI+8]
  ADD AX,CX
  CMP AX,217
  JZ ERROR1
  CMP AX,218 
  JZ ERROR1
  CMP AX,219
  JZ ERROR1
  CMP AX,220
 JZ ERROR1
 CMP AX,205
 JZ ERROR1
 CMP AX,207
 JZ DXNTACLSHIFTL
 MOV CX,0
 MOV BX,0                            ;TAKECARE
 MOV BL,[SI+9]
 MOV CL,[SI+10]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ ERROR1
 CMP AX,404              
 JZ ERROR1
 CMP AX,389
 JZ ERROR1
 
 JMP NTADXDESW_RKM_YA_MEMRKM_YAERROR1_SHL  
 
DXNTACLSHIFTL:

MOV CX,RCX1
MOV BX,RDX1
MOV AX,FLAGS
PUSH AX
DXDESWCLSOURCE_SHL:
CMP BP,8
JNZ DXDESWCLSOURCE_SHR
POPF
SHL BX,CL
JMP OUTDXDESWCLSOURCE
DXDESWCLSOURCE_SHR:
CMP BP,9
JNZ DXDESWCLSOURCE_SAR
POPF
SHR BX,CL
JMP OUTDXDESWCLSOURCE  
DXDESWCLSOURCE_SAR:
CMP BP,10
JNZ DXDESWCLSOURCE_ROR
POPF
SAR BX,CL
JMP OUTDXDESWCLSOURCE 
DXDESWCLSOURCE_ROR: 
CMP BP,11
JNZ DXDESWCLSOURCE_ROL
POPF
ROR BX,CL
JMP OUTDXDESWCLSOURCE
DXDESWCLSOURCE_ROL: 
CMP BP,12
JNZ DXDESWCLSOURCE_RCL
POPF
ROL BX,CL
JMP OUTDXDESWCLSOURCE 
DXDESWCLSOURCE_RCL: 
CMP BP,13
JNZ DXDESWCLSOURCE_RCR
POPF
RCL BX,CL
JMP OUTDXDESWCLSOURCE 
DXDESWCLSOURCE_RCR:
POPF
RCR BX,CL
OUTDXDESWCLSOURCE:
MOV RDX1,BX
PUSHF
POP AX 
MOV FLAGS,AX
JMP NAG7_SHL

 
NTADXDESW_RKM_YA_MEMRKM_YAERROR1_SHL:
MOV AL,[SI+7]
CMP AL,91
JZ ERROR1

RKM_ORERROR1_DXDES_SHL:
MOV AL,[SI+8]
CMP AL,108
JZ ERROR1
CMP AL,104
JZ ERROR1

MOV VAR,BP
RET_NUM_COUNT  COMP+7
MOV BP,VAR
CMP CX,2
JA ERROR1
MOV BX,RDX1 
MOV CL,DL
MOV AX,FLAGS
PUSH AX
DXDESWRKMSOURCE_SHL:
CMP BP,8
JNZ DXDESWRKMSOURCE_SHR
POPF
SHL BX,CL
JMP OUTDXDESWRKMSOURCE
DXDESWRKMSOURCE_SHR:
CMP BP,9
JNZ DXDESWRKMSOURCE_SAR
POPF
SHR BX,CL
JMP OUTDXDESWRKMSOURCE  
DXDESWRKMSOURCE_SAR:
CMP BP,10
JNZ DXDESWRKMSOURCE_ROR
POPF
SAR BX,CL
JMP OUTDXDESWRKMSOURCE 
DXDESWRKMSOURCE_ROR: 
CMP BP,11
JNZ DXDESWRKMSOURCE_ROL
POPF
ROR BX,CL
JMP OUTDXDESWRKMSOURCE
DXDESWRKMSOURCE_ROL: 
CMP BP,12
JNZ DXDESWRKMSOURCE_RCL
POPF
ROL BX,CL
JMP OUTDXDESWRKMSOURCE 
DXDESWRKMSOURCE_RCL: 
CMP BP,13
JNZ DXDESWRKMSOURCE_RCR
POPF
RCL BX,CL
JMP OUTDXDESWRKMSOURCE 
DXDESWRKMSOURCE_RCR:
POPF
RCR BX,CL
OUTDXDESWRKMSOURCE:
PUSHF
POP AX
MOV FLAGS,AX
MOV RDX1,BX
JMP NAG7_SHL

;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTASIDES_SHL:
  MOV BX,0
  MOV BL,[SI+8]
  CMP BL,104
  JNAE NTASIDESW_RKM_YA_MEMRKM_YAERROR1_SHL
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+7] 
  MOV CL,[SI+8]
  ADD AX,CX
  CMP AX,217
  JZ ERROR1
  CMP AX,218 
  JZ ERROR1
  CMP AX,219
  JZ ERROR1
  CMP AX,220
 JZ ERROR1
 CMP AX,205
 JZ ERROR1
 CMP AX,207
 JZ SINTACLSHIFTL
 MOV CX,0
 MOV BX,0                            ;TAKECARE
 MOV BL,[SI+9]
 MOV CL,[SI+10]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ ERROR1
 CMP AX,404              
 JZ ERROR1
 CMP AX,389
 JZ ERROR1
 
 JMP NTASIDESW_RKM_YA_MEMRKM_YAERROR1_SHL  
 
SINTACLSHIFTL:

MOV CX,RCX1
MOV BX,ISI1
MOV AX,FLAGS
PUSH AX
SIDESWCLSOURCE_SHL:
CMP BP,8
JNZ SIDESWCLSOURCE_SHR
POPF
SHL BX,CL
JMP OUTSIDESWCLSOURCE
SIDESWCLSOURCE_SHR:
CMP BP,9
JNZ SIDESWCLSOURCE_SAR
POPF
SHR BX,CL
JMP OUTSIDESWCLSOURCE  
SIDESWCLSOURCE_SAR:
CMP BP,10
JNZ SIDESWCLSOURCE_ROR
POPF
SAR BX,CL
JMP OUTSIDESWCLSOURCE 
SIDESWCLSOURCE_ROR: 
CMP BP,11
JNZ SIDESWCLSOURCE_ROL
POPF
ROR BX,CL
JMP OUTSIDESWCLSOURCE
SIDESWCLSOURCE_ROL: 
CMP BP,12
JNZ SIDESWCLSOURCE_RCL
POPF
ROL BX,CL
JMP OUTSIDESWCLSOURCE 
SIDESWCLSOURCE_RCL: 
CMP BP,13
JNZ SIDESWCLSOURCE_RCR
POPF
RCL BX,CL
JMP OUTSIDESWCLSOURCE 
SIDESWCLSOURCE_RCR:
POPF
RCR BX,CL
OUTSIDESWCLSOURCE:
MOV ISI1,BX
PUSHF
POP AX
MOV FLAGS,AX
JMP NAG7_SHL

 
NTASIDESW_RKM_YA_MEMRKM_YAERROR1_SHL:
MOV AL,[SI+7]
CMP AL,91
JZ ERROR1

RKM_ORERROR1_SIDES_SHL:
MOV AL,[SI+8]
CMP AL,108
JZ ERROR1
CMP AL,104
JZ ERROR1

MOV VAR,BP
RET_NUM_COUNT  COMP+7
MOV BP,VAR
CMP CX,2
JA ERROR1
MOV BX,ISI1 
MOV CL,DL
MOV AX,FLAGS
PUSH AX
SIDESWRKMSOURCE_SHL:
CMP BP,8
JNZ SIDESWRKMSOURCE_SHR
POPF
SHL BX,CL
JMP OUTSIDESWRKMSOURCE
SIDESWRKMSOURCE_SHR:
CMP BP,9
JNZ SIDESWRKMSOURCE_SAR
POPF
SHR BX,CL
JMP OUTSIDESWRKMSOURCE  
SIDESWRKMSOURCE_SAR:
CMP BP,10
JNZ SIDESWRKMSOURCE_ROR
POPF
SAR BX,CL
JMP OUTSIDESWRKMSOURCE 
SIDESWRKMSOURCE_ROR: 
CMP BP,11
JNZ SIDESWRKMSOURCE_ROL
POPF
ROR BX,CL
JMP OUTSIDESWRKMSOURCE
SIDESWRKMSOURCE_ROL: 
CMP BP,12
JNZ SIDESWRKMSOURCE_RCL
POPF
ROL BX,CL
JMP OUTSIDESWRKMSOURCE 
SIDESWRKMSOURCE_RCL: 
CMP BP,13
JNZ SIDESWRKMSOURCE_RCR
POPF
RCL BX,CL
JMP OUTSIDESWRKMSOURCE 
SIDESWRKMSOURCE_RCR:
POPF
RCR BX,CL
OUTSIDESWRKMSOURCE:
PUSHF
POP AX
MOV FLAGS,AX
MOV ISI1,BX
JMP NAG7_SHL

;///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

NTADIORALDES_SHL:
MOV AL,[SI+4]
CMP AL,97
JZ NTAALDES_SHL
NTADIDES_SHL:
  MOV BX,0
  MOV BL,[SI+8]
  CMP BL,104
  JNAE NTADIDESW_RKM_YA_MEMRKM_YAERROR1_SHL
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+7] 
  MOV CL,[SI+8]
  ADD AX,CX
  CMP AX,217
  JZ ERROR1
  CMP AX,218 
  JZ ERROR1
  CMP AX,219
  JZ ERROR1
  CMP AX,220
 JZ ERROR1
 CMP AX,205
 JZ ERROR1
 CMP AX,207
 JZ DINTACLSHIFTL
 MOV CX,0
 MOV BX,0                            ;TAKECARE
 MOV BL,[SI+9]
 MOV CL,[SI+10]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ ERROR1
 CMP AX,404              
 JZ ERROR1
 CMP AX,389
 JZ ERROR1
 
 JMP NTADIDESW_RKM_YA_MEMRKM_YAERROR1_SHL  
 
DINTACLSHIFTL:

MOV CX,RCX1
MOV BX,IDI1
MOV AX,FLAGS
PUSH AX
DIDESWCLSOURCE_SHL:
CMP BP,8
JNZ DIDESWCLSOURCE_SHR
POPF
SHL BX,CL
JMP OUTDIDESWCLSOURCE
DIDESWCLSOURCE_SHR:
CMP BP,9
JNZ DIDESWCLSOURCE_SAR
POPF
SHR BX,CL
JMP OUTDIDESWCLSOURCE  
DIDESWCLSOURCE_SAR:
CMP BP,10
JNZ DIDESWCLSOURCE_ROR
POPF
SAR BX,CL
JMP OUTDIDESWCLSOURCE 
DIDESWCLSOURCE_ROR: 
CMP BP,11
JNZ DIDESWCLSOURCE_ROL
POPF
ROR BX,CL
JMP OUTDIDESWCLSOURCE
DIDESWCLSOURCE_ROL: 
CMP BP,12
JNZ DIDESWCLSOURCE_RCL
POPF
ROL BX,CL
JMP OUTDIDESWCLSOURCE 
DIDESWCLSOURCE_RCL: 
CMP BP,13
JNZ DIDESWCLSOURCE_RCR
POPF
RCL BX,CL
JMP OUTDIDESWCLSOURCE 
DIDESWCLSOURCE_RCR:
POPF
RCR BX,CL
OUTDIDESWCLSOURCE:
MOV IDI1,BX
PUSHF
POP AX 
MOV FLAGS,AX
JMP NAG7_SHL
NTADIDESW_RKM_YA_MEMRKM_YAERROR1_SHL:
MOV AL,[SI+7]
CMP AL,91
JZ ERROR1
RKM_ORERROR1_DIDES_SHL:
MOV AL,[SI+8]
CMP AL,108
JZ ERROR1
CMP AL,104
JZ ERROR1
MOV VAR,BP
RET_NUM_COUNT  COMP+7
MOV BP,VAR
CMP CX,2
JA ERROR1
MOV BX,IDI1 
MOV CL,DL
MOV AX,FLAGS
PUSH AX
DIDESWRKMSOURCE_SHL:
CMP BP,8
JNZ DIDESWRKMSOURCE_SHR
POPF
SHL BX,CL
JMP OUTDIDESWRKMSOURCE
DIDESWRKMSOURCE_SHR:
CMP BP,9
JNZ DIDESWRKMSOURCE_SAR
POPF
SHR BX,CL
JMP OUTDIDESWRKMSOURCE  
DIDESWRKMSOURCE_SAR:
CMP BP,10
JNZ DIDESWRKMSOURCE_ROR
POPF
SAR BX,CL
JMP OUTDIDESWRKMSOURCE 
DIDESWRKMSOURCE_ROR: 
CMP BP,11
JNZ DIDESWRKMSOURCE_ROL
POPF
ROR BX,CL
JMP OUTDIDESWRKMSOURCE
DIDESWRKMSOURCE_ROL: 
CMP BP,12
JNZ DIDESWRKMSOURCE_RCL
POPF
ROL BX,CL
JMP OUTDIDESWRKMSOURCE 
DIDESWRKMSOURCE_RCL: 
CMP BP,13
JNZ DIDESWRKMSOURCE_RCR
POPF
RCL BX,CL
JMP OUTDIDESWRKMSOURCE 
DIDESWRKMSOURCE_RCR:
POPF
RCR BX,CL
OUTDIDESWRKMSOURCE:
PUSHF
POP AX
MOV FLAGS,AX
MOV IDI1,BX
JMP NAG7_SHL
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTAALDES_SHL:
  MOV BX,0
  MOV BL,[SI+8]
  CMP BL,104
  JNAE NTAALDESW_RKM_YA_MEMRKM_YAERROR1_SHL
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+7] 
  MOV CL,[SI+8]
  ADD AX,CX
  CMP AX,217
  JZ ERROR1
  CMP AX,218 
  JZ ERROR1
  CMP AX,219
  JZ ERROR1
  CMP AX,220
 JZ ERROR1
 CMP AX,205
 JZ ERROR1
 CMP AX,207
 JZ ALNTACLSHIFTL
 MOV CX,0
 MOV BX,0                            ;TAKECARE
 MOV BL,[SI+9]
 MOV CL,[SI+10]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ ERROR1
 CMP AX,404              
 JZ ERROR1
 CMP AX,389
 JZ ERROR1
 
 JMP NTAALDESW_RKM_YA_MEMRKM_YAERROR1_SHL  
 
ALNTACLSHIFTL:

MOV CX,RCX1
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX
ALDESWCLSOURCE_SHL:
CMP BP,8
JNZ ALDESWCLSOURCE_SHR
POPF
SHL BL,CL
JMP OUTALDESWCLSOURCE
ALDESWCLSOURCE_SHR:
CMP BP,9
JNZ ALDESWCLSOURCE_SAR
POPF
SHR BL,CL
JMP OUTALDESWCLSOURCE  
ALDESWCLSOURCE_SAR:
CMP BP,10
JNZ ALDESWCLSOURCE_ROR
POPF
SAR BL,CL
JMP OUTALDESWCLSOURCE 
ALDESWCLSOURCE_ROR: 
CMP BP,11
JNZ ALDESWCLSOURCE_ROL
POPF
ROR BL,CL
JMP OUTALDESWCLSOURCE
ALDESWCLSOURCE_ROL: 
CMP BP,12
JNZ ALDESWCLSOURCE_RCL
POPF
ROL BL,CL
JMP OUTALDESWCLSOURCE 
ALDESWCLSOURCE_RCL: 
CMP BP,13
JNZ ALDESWCLSOURCE_RCR
POPF
RCL BL,CL
JMP OUTALDESWCLSOURCE 
ALDESWCLSOURCE_RCR:
POPF
RCR BL,CL
OUTALDESWCLSOURCE:
MOV RAX1,BX
PUSHF
POP AX 
MOV FLAGS,AX
JMP NAG7_SHL

 
NTAALDESW_RKM_YA_MEMRKM_YAERROR1_SHL:
MOV AL,[SI+7]
CMP AL,91
JZ ERROR1

RKM_ORERROR1_ALDES_SHL:
MOV AL,[SI+8]
CMP AL,108
JZ ERROR1
CMP AL,104
JZ ERROR1

MOV VAR,BP
RET_NUM_COUNT  COMP+7
MOV BP,VAR
CMP CX,2
JA ERROR1
MOV BX,RAX1 
MOV CL,DL
MOV AX,FLAGS
PUSH AX
ALDESWRKMSOURCE_SHL:
CMP BP,8
JNZ ALDESWRKMSOURCE_SHR
POPF
SHL BL,CL
JMP OUTALDESWRKMSOURCE
ALDESWRKMSOURCE_SHR:
CMP BP,9
JNZ ALDESWRKMSOURCE_SAR
POPF
SHR BL,CL
JMP OUTALDESWRKMSOURCE  
ALDESWRKMSOURCE_SAR:
CMP BP,10
JNZ ALDESWRKMSOURCE_ROR
POPF
SAR BL,CL
JMP OUTALDESWRKMSOURCE 
ALDESWRKMSOURCE_ROR: 
CMP BP,11
JNZ ALDESWRKMSOURCE_ROL
POPF
ROR BL,CL
JMP OUTALDESWRKMSOURCE
ALDESWRKMSOURCE_ROL: 
CMP BP,12
JNZ ALDESWRKMSOURCE_RCL
POPF
ROL BL,CL
JMP OUTALDESWRKMSOURCE 
ALDESWRKMSOURCE_RCL: 
CMP BP,13
JNZ ALDESWRKMSOURCE_RCR
POPF
RCL BL,CL
JMP OUTALDESWRKMSOURCE 
ALDESWRKMSOURCE_RCR:
POPF
RCR BL,CL
OUTALDESWRKMSOURCE:
PUSHF
POP AX
MOV FLAGS,AX
MOV RAX1,BX
JMP NAG7_SHL
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTABLDIS_SHL:
  MOV BX,0
  MOV BL,[SI+8]
  CMP BL,104
  JNAE NTABLDESW_RKM_YA_MEMRKM_YAERROR1_SHL
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+7] 
  MOV CL,[SI+8]
  ADD AX,CX
  CMP AX,217
  JZ ERROR1
  CMP AX,218 
  JZ ERROR1
  CMP AX,219
  JZ ERROR1
  CMP AX,220
 JZ ERROR1
 CMP AX,205
 JZ ERROR1
 CMP AX,207
 JZ BLNTACLSHIFTL
 MOV CX,0
 MOV BX,0                            ;TAKECARE
 MOV BL,[SI+9]
 MOV CL,[SI+10]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ ERROR1
 CMP AX,404              
 JZ ERROR1
 CMP AX,389
 JZ ERROR1
 
 JMP NTABLDESW_RKM_YA_MEMRKM_YAERROR1_SHL  
 
BLNTACLSHIFTL:

MOV CX,RCX1
MOV BX,RBX1
MOV AX,FLAGS
PUSH AX
BLDESWCLSOURCE_SHL:
CMP BP,8
JNZ BLDESWCLSOURCE_SHR
POPF
SHL BL,CL
JMP OUTBLDESWCLSOURCE
BLDESWCLSOURCE_SHR:
CMP BP,9
JNZ BLDESWCLSOURCE_SAR
POPF
SHR BL,CL
JMP OUTBLDESWCLSOURCE  
BLDESWCLSOURCE_SAR:
CMP BP,10
JNZ BLDESWCLSOURCE_ROR
POPF
SAR BL,CL
JMP OUTBLDESWCLSOURCE 
BLDESWCLSOURCE_ROR: 
CMP BP,11
JNZ BLDESWCLSOURCE_ROL
POPF
ROR BL,CL
JMP OUTBLDESWCLSOURCE
BLDESWCLSOURCE_ROL: 
CMP BP,12
JNZ BLDESWCLSOURCE_RCL
POPF
ROL BL,CL
JMP OUTBLDESWCLSOURCE 
BLDESWCLSOURCE_RCL: 
CMP BP,13
JNZ BLDESWCLSOURCE_RCR
POPF
RCL BL,CL
JMP OUTBLDESWCLSOURCE 
BLDESWCLSOURCE_RCR:
POPF
RCR BL,CL
OUTBLDESWCLSOURCE:
MOV RBX1,BX
PUSHF
POP AX 
MOV FLAGS,AX
JMP NAG7_SHL

 
NTABLDESW_RKM_YA_MEMRKM_YAERROR1_SHL:
MOV AL,[SI+7]
CMP AL,91
JZ ERROR1

RKM_ORERROR1_BLDES_SHL:
MOV AL,[SI+8]
CMP AL,108
JZ ERROR1
CMP AL,104
JZ ERROR1

MOV VAR,BP
RET_NUM_COUNT  COMP+7
MOV BP,VAR
CMP CX,2
JA ERROR1
MOV BX,RBX1 
MOV CL,DL
MOV AX,FLAGS
PUSH AX
BLDESWRKMSOURCE_SHL:
CMP BP,8
JNZ BLDESWRKMSOURCE_SHR
POPF
SHL BL,CL
JMP OUTBLDESWRKMSOURCE
BLDESWRKMSOURCE_SHR:
CMP BP,9
JNZ BLDESWRKMSOURCE_SAR
POPF
SHR BL,CL
JMP OUTBLDESWRKMSOURCE  
BLDESWRKMSOURCE_SAR:
CMP BP,10
JNZ BLDESWRKMSOURCE_ROR
POPF
SAR BL,CL
JMP OUTBLDESWRKMSOURCE 
BLDESWRKMSOURCE_ROR: 
CMP BP,11
JNZ BLDESWRKMSOURCE_ROL
POPF
ROR BL,CL
JMP OUTBLDESWRKMSOURCE
BLDESWRKMSOURCE_ROL: 
CMP BP,12
JNZ BLDESWRKMSOURCE_RCL
POPF
ROL BL,CL
JMP OUTBLDESWRKMSOURCE 
BLDESWRKMSOURCE_RCL: 
CMP BP,13
JNZ BLDESWRKMSOURCE_RCR
POPF
RCL BL,CL
JMP OUTBLDESWRKMSOURCE 
BLDESWRKMSOURCE_RCR:
POPF
RCR BL,CL
OUTBLDESWRKMSOURCE:
PUSHF
POP AX
MOV FLAGS,AX
MOV RBX1,BX
JMP NAG7_SHL
;/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTACLDIS_SHL:
  MOV BX,0
  MOV BL,[SI+8]
  CMP BL,104
  JNAE NTACLDESW_RKM_YA_MEMRKM_YAERROR1_SHL
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+7] 
  MOV CL,[SI+8]
  ADD AX,CX
  CMP AX,217
  JZ ERROR1
  CMP AX,218 
  JZ ERROR1
  CMP AX,219
  JZ ERROR1
  CMP AX,220
 JZ ERROR1
 CMP AX,205
 JZ ERROR1
 CMP AX,207
 JZ CLNTACLSHIFTL
 MOV CX,0
 MOV BX,0                            ;TAKECARE
 MOV BL,[SI+9]
 MOV CL,[SI+10]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ ERROR1
 CMP AX,404              
 JZ ERROR1
 CMP AX,389
 JZ ERROR1
 
 JMP NTACLDESW_RKM_YA_MEMRKM_YAERROR1_SHL  
 
CLNTACLSHIFTL:

MOV CX,RCX1
MOV BX,RCX1
MOV AX,FLAGS
PUSH AX
CLDESWCLSOURCE_SHL:
CMP BP,8
JNZ CLDESWCLSOURCE_SHR
POPF
SHL BL,CL
JMP OUTCLDESWCLSOURCE
CLDESWCLSOURCE_SHR:
CMP BP,9
JNZ CLDESWCLSOURCE_SAR
POPF
SHR BL,CL
JMP OUTCLDESWCLSOURCE  
CLDESWCLSOURCE_SAR:
CMP BP,10
JNZ CLDESWCLSOURCE_ROR
POPF
SAR BL,CL
JMP OUTCLDESWCLSOURCE 
CLDESWCLSOURCE_ROR: 
CMP BP,11
JNZ CLDESWCLSOURCE_ROL
POPF
ROR BL,CL
JMP OUTCLDESWCLSOURCE
CLDESWCLSOURCE_ROL: 
CMP BP,12
JNZ CLDESWCLSOURCE_RCL
POPF
ROL BL,CL
JMP OUTCLDESWCLSOURCE 
CLDESWCLSOURCE_RCL: 
CMP BP,13
JNZ CLDESWCLSOURCE_RCR
POPF
RCL BL,CL
JMP OUTCLDESWCLSOURCE 
CLDESWCLSOURCE_RCR:
POPF
RCR BL,CL
OUTCLDESWCLSOURCE:
MOV RCX1,BX
PUSHF
POP AX
MOV FLAGS,AX
JMP NAG7_SHL

 
NTACLDESW_RKM_YA_MEMRKM_YAERROR1_SHL:
MOV AL,[SI+7]
CMP AL,91
JZ ERROR1

RKM_ORERROR1_CLDES_SHL:
MOV AL,[SI+8]
CMP AL,108
JZ ERROR1
CMP AL,104
JZ ERROR1

MOV VAR,BP
RET_NUM_COUNT  COMP+7
MOV BP,VAR
CMP CX,2
JA ERROR1
MOV BX,RCX1 
MOV CL,DL
MOV AX,FLAGS
PUSH AX
CLDESWRKMSOURCE_SHL:
CMP BP,8
JNZ ALDESWCLSOURCE_SHR
POPF
SHL BL,CL
JMP OUTCLDESWRKMSOURCE
CLDESWRKMSOURCE_SHR:
CMP BP,9
JNZ CLDESWRKMSOURCE_SAR
POPF
SHR BL,CL
JMP OUTCLDESWRKMSOURCE  
CLDESWRKMSOURCE_SAR:
CMP BP,10
JNZ CLDESWRKMSOURCE_ROR
POPF
SAR BL,CL
JMP OUTCLDESWRKMSOURCE 
CLDESWRKMSOURCE_ROR: 
CMP BP,11
JNZ CLDESWRKMSOURCE_ROL
POPF
ROR BL,CL
JMP OUTCLDESWRKMSOURCE
CLDESWRKMSOURCE_ROL: 
CMP BP,12
JNZ CLDESWRKMSOURCE_RCL
POPF
ROL BL,CL
JMP OUTCLDESWRKMSOURCE 
CLDESWRKMSOURCE_RCL: 
CMP BP,13
JNZ CLDESWRKMSOURCE_RCR
POPF
RCL BL,CL
JMP OUTCLDESWRKMSOURCE 
CLDESWRKMSOURCE_RCR:
POPF
RCR BL,CL
OUTCLDESWRKMSOURCE:
PUSHF
POP AX
MOV FLAGS,AX
MOV RCX1,BX
JMP NAG7_SHL
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTADLDIS_SHL:
  MOV BX,0
  MOV BL,[SI+8]
  CMP BL,104
  JNAE NTADLDESW_RKM_YA_MEMRKM_YAERROR1_SHL
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+7] 
  MOV CL,[SI+8]
  ADD AX,CX
  CMP AX,217
  JZ ERROR1
  CMP AX,218 
  JZ ERROR1
  CMP AX,219
  JZ ERROR1
  CMP AX,220
 JZ ERROR1
 CMP AX,205
 JZ ERROR1
 CMP AX,207
 JZ DLNTACLSHIFTL
 MOV CX,0
 MOV BX,0                            ;TAKECARE
 MOV BL,[SI+9]
 MOV CL,[SI+10]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ ERROR1
 CMP AX,404              
 JZ ERROR1
 CMP AX,389
 JZ ERROR1
 
 JMP NTADLDESW_RKM_YA_MEMRKM_YAERROR1_SHL  
 
DLNTACLSHIFTL:

MOV CX,RCX1
MOV BX,RDX1
MOV AX,FLAGS
PUSH AX
DLDESWCLSOURCE_SHL:
CMP BP,8
JNZ DLDESWCLSOURCE_SHR
POPF
SHL BL,CL
JMP OUTDLDESWCLSOURCE
DLDESWCLSOURCE_SHR:
CMP BP,9
JNZ DLDESWCLSOURCE_SAR
POPF
SHR BL,CL
JMP OUTDLDESWCLSOURCE  
DLDESWCLSOURCE_SAR:
CMP BP,10
JNZ DLDESWCLSOURCE_ROR
POPF
SAR BL,CL
JMP OUTDLDESWCLSOURCE 
DLDESWCLSOURCE_ROR: 
CMP BP,11
JNZ DLDESWCLSOURCE_ROL
POPF
ROR BL,CL
JMP OUTDLDESWCLSOURCE
DLDESWCLSOURCE_ROL: 
CMP BP,12
JNZ DLDESWCLSOURCE_RCL
POPF
ROL BL,CL
JMP OUTDLDESWCLSOURCE 
DLDESWCLSOURCE_RCL: 
CMP BP,13
JNZ DLDESWCLSOURCE_RCR
POPF
RCL BL,CL
JMP OUTDLDESWCLSOURCE 
DLDESWCLSOURCE_RCR:
POPF
RCR BL,CL
OUTDLDESWCLSOURCE:
MOV RDX1,BX
PUSHF
POP AX
MOV FLAGS,AX
JMP NAG7_SHL

 
NTADLDESW_RKM_YA_MEMRKM_YAERROR1_SHL:
MOV AL,[SI+7]
CMP AL,91
JZ ERROR1

RKM_ORERROR1_DLDES_SHL:
MOV AL,[SI+8]
CMP AL,108
JZ ERROR1
CMP AL,104
JZ ERROR1

MOV VAR,BP
RET_NUM_COUNT  COMP+7
MOV BP,VAR
CMP CX,2
JA ERROR1
MOV BX,RDX1 
MOV CL,DL
MOV AX,FLAGS
PUSH AX
DLDESWRKMSOURCE_SHL:
CMP BP,8
JNZ DLDESWRKMSOURCE_SHR
POPF
SHL BL,CL
JMP OUTDLDESWRKMSOURCE
DLDESWRKMSOURCE_SHR:
CMP BP,9
JNZ DLDESWRKMSOURCE_SAR
POPF
SHR BL,CL
JMP OUTDLDESWRKMSOURCE  
DLDESWRKMSOURCE_SAR:
CMP BP,10
JNZ DLDESWRKMSOURCE_ROR
POPF
SAR BL,CL
JMP OUTDLDESWRKMSOURCE 
DLDESWRKMSOURCE_ROR: 
CMP BP,11
JNZ DLDESWRKMSOURCE_ROL
POPF
ROR BL,CL
JMP OUTDLDESWRKMSOURCE
DLDESWRKMSOURCE_ROL: 
CMP BP,12
JNZ DLDESWRKMSOURCE_RCL
POPF
ROL BL,CL
JMP OUTDLDESWRKMSOURCE 
DLDESWRKMSOURCE_RCL: 
CMP BP,13
JNZ DLDESWRKMSOURCE_RCR
POPF
RCL BL,CL
JMP OUTDLDESWRKMSOURCE 
DLDESWRKMSOURCE_RCR:
POPF
RCR BL,CL
OUTDLDESWRKMSOURCE:
PUSHF
POP AX
MOV FLAGS,AX
MOV RDX1,BX
JMP NAG7_SHL
;/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTAAHDIS_SHL:
  MOV BX,0
  MOV BL,[SI+8]
  CMP BL,104
  JNAE NTAAHDESW_RKM_YA_MEMRKM_YAERROR1_SHL
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+7] 
  MOV CL,[SI+8]
  ADD AX,CX
  CMP AX,217
  JZ ERROR1
  CMP AX,218 
  JZ ERROR1
  CMP AX,219
  JZ ERROR1
  CMP AX,220
 JZ ERROR1
 CMP AX,205
 JZ ERROR1
 CMP AX,207
 JZ AHNTACLSHIFTL
 MOV CX,0
 MOV BX,0                            ;TAKECARE
 MOV BL,[SI+9]
 MOV CL,[SI+10]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ ERROR1
 CMP AX,404              
 JZ ERROR1
 CMP AX,389
 JZ ERROR1
 
 JMP NTAAHDESW_RKM_YA_MEMRKM_YAERROR1_SHL  
 
AHNTACLSHIFTL:

MOV CX,RCX1
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX
AHDESWCLSOURCE_SHL:
CMP BP,8
JNZ AHDESWCLSOURCE_SHR
POPF
SHL BH,CL
JMP OUTAHDESWCLSOURCE
AHDESWCLSOURCE_SHR:
CMP BP,9
JNZ AHDESWCLSOURCE_SAR
POPF
SHR BH,CL
JMP OUTAHDESWCLSOURCE  
AHDESWCLSOURCE_SAR:
CMP BP,10
JNZ AHDESWCLSOURCE_ROR
POPF
SAR BH,CL
JMP OUTAHDESWCLSOURCE 
AHDESWCLSOURCE_ROR: 
CMP BP,11
JNZ AHDESWCLSOURCE_ROL
POPF
ROR BH,CL
JMP OUTAHDESWCLSOURCE
AHDESWCLSOURCE_ROL: 
CMP BP,12
JNZ AHDESWCLSOURCE_RCL
POPF
ROL BH,CL
JMP OUTAHDESWCLSOURCE 
AHDESWCLSOURCE_RCL: 
CMP BP,13
JNZ AHDESWCLSOURCE_RCR
POPF
RCL BH,CL
JMP OUTAHDESWCLSOURCE 
AHDESWCLSOURCE_RCR:
POPF
RCR BH,CL
OUTAHDESWCLSOURCE:
MOV RAX1,BX
PUSHF
POP AX
MOV FLAGS,AX
JMP NAG7_SHL

 
NTAAHDESW_RKM_YA_MEMRKM_YAERROR1_SHL:
MOV AL,[SI+7]
CMP AL,91
JZ ERROR1

RKM_ORERROR1_AHDES_SHL:
MOV AL,[SI+8]
CMP AL,108
JZ ERROR1
CMP AL,104
JZ ERROR1

MOV VAR,BP
RET_NUM_COUNT  COMP+7
MOV BP,VAR
CMP CX,2
JA ERROR1
MOV BX,RAX1 
MOV CL,DL
MOV AX,FLAGS
PUSH AX
AHDESWRKMSOURCE_SHL:
CMP BP,8
JNZ AHDESWRKMSOURCE_SHR
POPF
SHL BH,CL
JMP OUTAHDESWRKMSOURCE
AHDESWRKMSOURCE_SHR:
CMP BP,9
JNZ AHDESWRKMSOURCE_SAR
POPF
SHR BH,CL
JMP OUTAHDESWRKMSOURCE  
AHDESWRKMSOURCE_SAR:
CMP BP,10
JNZ AHDESWRKMSOURCE_ROR
POPF
SAR BH,CL
JMP OUTAHDESWRKMSOURCE 
AHDESWRKMSOURCE_ROR: 
CMP BP,11
JNZ AHDESWRKMSOURCE_ROL
POPF
ROR BH,CL
JMP OUTAHDESWRKMSOURCE
AHDESWRKMSOURCE_ROL: 
CMP BP,12
JNZ AHDESWRKMSOURCE_RCL
POPF
ROL BH,CL
JMP OUTAHDESWRKMSOURCE 
AHDESWRKMSOURCE_RCL: 
CMP BP,13
JNZ AHDESWRKMSOURCE_RCR
POPF
RCL BH,CL
JMP OUTAHDESWRKMSOURCE 
AHDESWRKMSOURCE_RCR:
POPF
RCR BH,CL
OUTAHDESWRKMSOURCE:
PUSHF
POP AX
MOV FLAGS,AX
MOV RAX1,BX
JMP NAG7_SHL
 ;///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTABHDIS_SHL:
  MOV BX,0
  MOV BL,[SI+8]
  CMP BL,104
  JNAE NTABHDESW_RKM_YA_MEMRKM_YAERROR1_SHL
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+7] 
  MOV CL,[SI+8]
  ADD AX,CX
  CMP AX,217
  JZ ERROR1
  CMP AX,218 
  JZ ERROR1
  CMP AX,219
  JZ ERROR1
  CMP AX,220
 JZ ERROR1
 CMP AX,205
 JZ ERROR1
 CMP AX,207
 JZ BHNTACLSHIFTL
 MOV CX,0
 MOV BX,0                            ;TAKECARE
 MOV BL,[SI+9]
 MOV CL,[SI+10]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ ERROR1
 CMP AX,404              
 JZ ERROR1
 CMP AX,389
 JZ ERROR1
 
 JMP NTABHDESW_RKM_YA_MEMRKM_YAERROR1_SHL  
 
BHNTACLSHIFTL:

MOV CX,RCX1
MOV BX,RBX1
MOV AX,FLAGS
PUSH AX
BHDESWCLSOURCE_SHL:
CMP BP,8
JNZ BHDESWCLSOURCE_SHR
POPF
SHL BH,CL
JMP OUTBHDESWCLSOURCE
BHDESWCLSOURCE_SHR:
CMP BP,9
JNZ BHDESWCLSOURCE_SAR
POPF
SHR BH,CL
JMP OUTBHDESWCLSOURCE  
BHDESWCLSOURCE_SAR:
CMP BP,10
JNZ BHDESWCLSOURCE_ROR
POPF
SAR BH,CL
JMP OUTBHDESWCLSOURCE 
BHDESWCLSOURCE_ROR: 
CMP BP,11
JNZ BHDESWCLSOURCE_ROL
POPF
ROR BH,CL
JMP OUTBHDESWCLSOURCE
BHDESWCLSOURCE_ROL: 
CMP BP,12
JNZ BHDESWCLSOURCE_RCL
POPF
ROL BH,CL
JMP OUTBHDESWCLSOURCE 
BHDESWCLSOURCE_RCL: 
CMP BP,13
JNZ BHDESWCLSOURCE_RCR
POPF
RCL BH,CL
JMP OUTBHDESWCLSOURCE 
BHDESWCLSOURCE_RCR:
POPF
RCR BH,CL
OUTBHDESWCLSOURCE:
MOV RBX1,BX
PUSHF
POP AX
MOV FLAGS,AX
JMP NAG7_SHL

 
NTABHDESW_RKM_YA_MEMRKM_YAERROR1_SHL:
MOV AL,[SI+7]
CMP AL,91
JZ ERROR1

RKM_ORERROR1_BHDES_SHL:
MOV AL,[SI+8]
CMP AL,108
JZ ERROR1
CMP AL,104
JZ ERROR1

MOV VAR,BP
RET_NUM_COUNT  COMP+7
MOV BP,VAR
CMP CX,2
JA ERROR1
MOV BX,RBX1 
MOV CL,DL
MOV AX,FLAGS
PUSH AX
BHDESWRKMSOURCE_SHL:
CMP BP,8
JNZ BHDESWRKMSOURCE_SHR
POPF
SHL BH,CL
JMP OUTBHDESWRKMSOURCE
BHDESWRKMSOURCE_SHR:
CMP BP,9
JNZ BHDESWRKMSOURCE_SAR
POPF
SHR BH,CL
JMP OUTBHDESWRKMSOURCE  
BHDESWRKMSOURCE_SAR:
CMP BP,10
JNZ BHDESWRKMSOURCE_ROR
POPF
SAR BH,CL
JMP OUTBHDESWRKMSOURCE 
BHDESWRKMSOURCE_ROR: 
CMP BP,11
JNZ BHDESWRKMSOURCE_ROL
POPF
ROR BH,CL
JMP OUTBHDESWRKMSOURCE
BHDESWRKMSOURCE_ROL: 
CMP BP,12
JNZ BHDESWRKMSOURCE_RCL
POPF
ROL BH,CL
JMP OUTBHDESWRKMSOURCE 
BHDESWRKMSOURCE_RCL: 
CMP BP,13
JNZ BHDESWRKMSOURCE_RCR
POPF
RCL BH,CL
JMP OUTBHDESWRKMSOURCE 
BHDESWRKMSOURCE_RCR:
POPF
RCR BH,CL
OUTBHDESWRKMSOURCE:
PUSHF
POP AX
MOV FLAGS,AX
MOV RBX1,BX
JMP NAG7_SHL
;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTACHDIS_SHL:
  MOV BX,0
  MOV BL,[SI+8]
  CMP BL,104
  JNAE NTACHDESW_RKM_YA_MEMRKM_YAERROR1_SHL
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+7] 
  MOV CL,[SI+8]
  ADD AX,CX
  CMP AX,217
  JZ ERROR1
  CMP AX,218 
  JZ ERROR1
  CMP AX,219
  JZ ERROR1
  CMP AX,220
 JZ ERROR1
 CMP AX,205
 JZ ERROR1
 CMP AX,207
 JZ CHNTACLSHIFTL
 MOV CX,0
 MOV BX,0                            ;TAKECARE
 MOV BL,[SI+9]
 MOV CL,[SI+10]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ ERROR1
 CMP AX,404              
 JZ ERROR1
 CMP AX,389
 JZ ERROR1
 
 JMP NTACHDESW_RKM_YA_MEMRKM_YAERROR1_SHL  
 
CHNTACLSHIFTL:

MOV CX,RCX1
MOV BX,RCX1
MOV AX,FLAGS
PUSH AX
CHDESWCLSOURCE_SHL:
CMP BP,8
JNZ CHDESWCLSOURCE_SHR
POPF
SHL BH,CL
JMP OUTCHDESWCLSOURCE
CHDESWCLSOURCE_SHR:
CMP BP,9
JNZ CHDESWCLSOURCE_SAR
POPF
SHR BH,CL
JMP OUTCHDESWCLSOURCE  
CHDESWCLSOURCE_SAR:
CMP BP,10
JNZ CHDESWCLSOURCE_ROR
POPF
SAR BH,CL
JMP OUTCHDESWCLSOURCE 
CHDESWCLSOURCE_ROR: 
CMP BP,11
JNZ CHDESWCLSOURCE_ROL
POPF
ROR BH,CL
JMP OUTCHDESWCLSOURCE
CHDESWCLSOURCE_ROL: 
CMP BP,12
JNZ CHDESWCLSOURCE_RCL
POPF
ROL BH,CL
JMP OUTCHDESWCLSOURCE 
CHDESWCLSOURCE_RCL: 
CMP BP,13
JNZ CHDESWCLSOURCE_RCR
POPF
RCL BH,CL
JMP OUTCHDESWCLSOURCE 
CHDESWCLSOURCE_RCR:
POPF
RCR BH,CL
OUTCHDESWCLSOURCE:
MOV RCX1,BX
PUSHF
POP AX
MOV FLAGS,AX
JMP NAG7_SHL

 
NTACHDESW_RKM_YA_MEMRKM_YAERROR1_SHL:
MOV AL,[SI+7]
CMP AL,91
JZ ERROR1

RKM_ORERROR1_CHDES_SHL:
MOV AL,[SI+8]
CMP AL,108
JZ ERROR1
CMP AL,104
JZ ERROR1

MOV VAR,BP
RET_NUM_COUNT  COMP+7
MOV BP,VAR
CMP CX,2
JA ERROR1
MOV BX,RCX1 
MOV CL,DL
MOV AX,FLAGS
PUSH AX
CHDESWRKMSOURCE_SHL:
CMP BP,8
JNZ CHDESWRKMSOURCE_SHR
POPF
SHL BH,CL
JMP OUTCHDESWRKMSOURCE
CHDESWRKMSOURCE_SHR:
CMP BP,9
JNZ CHDESWRKMSOURCE_SAR
POPF
SHR BH,CL
JMP OUTCHDESWRKMSOURCE  
CHDESWRKMSOURCE_SAR:
CMP BP,10
JNZ CHDESWRKMSOURCE_ROR
POPF
SAR BH,CL
JMP OUTCHDESWRKMSOURCE 
CHDESWRKMSOURCE_ROR: 
CMP BP,11
JNZ CHDESWRKMSOURCE_ROL
POPF
ROR BH,CL
JMP OUTCHDESWRKMSOURCE
CHDESWRKMSOURCE_ROL: 
CMP BP,12
JNZ CHDESWRKMSOURCE_RCL
POPF
ROL BH,CL
JMP OUTCHDESWRKMSOURCE 
CHDESWRKMSOURCE_RCL: 
CMP BP,13
JNZ CHDESWRKMSOURCE_RCR
POPF
RCL BH,CL
JMP OUTCHDESWRKMSOURCE 
CHDESWRKMSOURCE_RCR:
POPF
RCR BH,CL
OUTCHDESWRKMSOURCE:
PUSHF
POP AX
MOV FLAGS,AX
MOV RCX1,BX
JMP NAG7_SHL
;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTADHDIS_SHL:
  MOV BX,0
  MOV BL,[SI+8]
  CMP BL,104
  JNAE NTADHDESW_RKM_YA_MEMRKM_YAERROR1_SHL
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+7] 
  MOV CL,[SI+8]
  ADD AX,CX
  CMP AX,217
  JZ ERROR1
  CMP AX,218 
  JZ ERROR1
  CMP AX,219
  JZ ERROR1
  CMP AX,220
 JZ ERROR1
 CMP AX,205
 JZ ERROR1
 CMP AX,207
 JZ DHNTACLSHIFTL
 MOV CX,0
 MOV BX,0                            ;TAKECARE
 MOV BL,[SI+9]
 MOV CL,[SI+10]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ ERROR1
 CMP AX,404              
 JZ ERROR1
 CMP AX,389
 JZ ERROR1
 
 JMP NTADHDESW_RKM_YA_MEMRKM_YAERROR1_SHL  
 
DHNTACLSHIFTL:

MOV CX,RCX1
MOV BX,RDX1
MOV AX,FLAGS
PUSH AX
DHDESWCLSOURCE_SHL:
CMP BP,8
JNZ DHDESWCLSOURCE_SHR
POPF
SHL BH,CL
JMP OUTDHDESWCLSOURCE
DHDESWCLSOURCE_SHR:
CMP BP,9
JNZ DHDESWCLSOURCE_SAR
POPF
SHR BH,CL
JMP OUTDHDESWCLSOURCE  
DHDESWCLSOURCE_SAR:
CMP BP,10
JNZ DHDESWCLSOURCE_ROR
POPF
SAR BH,CL
JMP OUTDHDESWCLSOURCE 
DHDESWCLSOURCE_ROR: 
CMP BP,11
JNZ DHDESWCLSOURCE_ROL
POPF
ROR BH,CL
JMP OUTDHDESWCLSOURCE
DHDESWCLSOURCE_ROL: 
CMP BP,12
JNZ DHDESWCLSOURCE_RCL
POPF
ROL BH,CL
JMP OUTDHDESWCLSOURCE 
DHDESWCLSOURCE_RCL: 
CMP BP,13
JNZ DHDESWCLSOURCE_RCR
POPF
RCL BH,CL
JMP OUTDHDESWCLSOURCE 
DHDESWCLSOURCE_RCR:
POPF
RCR BH,CL
OUTDHDESWCLSOURCE:
MOV RDX1,BX
PUSHF
POP AX
MOV FLAGS,AX 
JMP NAG7_SHL

 
NTADHDESW_RKM_YA_MEMRKM_YAERROR1_SHL:
MOV AL,[SI+7]
CMP AL,91
JZ ERROR1

RKM_ORERROR1_DHDES_SHL:
MOV AL,[SI+8]
CMP AL,108
JZ ERROR1
CMP AL,104
JZ ERROR1

MOV VAR,BP
RET_NUM_COUNT  COMP+7
MOV BP,VAR
CMP CX,2
JA ERROR1
MOV BX,RDX1 
MOV CL,DL
MOV AX,FLAGS
PUSH AX
DHDESWRKMSOURCE_SHL:
CMP BP,8
JNZ DHDESWRKMSOURCE_SHR
POPF
SHL BH,CL
JMP OUTDHDESWRKMSOURCE
DHDESWRKMSOURCE_SHR:
CMP BP,9
JNZ DHDESWRKMSOURCE_SAR
POPF
SHR BH,CL
JMP OUTDHDESWRKMSOURCE  
DHDESWRKMSOURCE_SAR:
CMP BP,10
JNZ DHDESWRKMSOURCE_ROR
POPF
SAR BH,CL
JMP OUTDHDESWRKMSOURCE 
DHDESWRKMSOURCE_ROR: 
CMP BP,11
JNZ DHDESWRKMSOURCE_ROL
POPF
ROR BH,CL
JMP OUTDHDESWRKMSOURCE
DHDESWRKMSOURCE_ROL: 
CMP BP,12
JNZ DHDESWRKMSOURCE_RCL
POPF
ROL BH,CL
JMP OUTDHDESWRKMSOURCE 
DHDESWRKMSOURCE_RCL: 
CMP BP,13
JNZ DHDESWRKMSOURCE_RCR
POPF
RCL BH,CL
JMP OUTDHDESWRKMSOURCE 
DHDESWRKMSOURCE_RCR:
POPF
RCR BH,CL
OUTDHDESWRKMSOURCE:
PUSHF
POP AX
MOV FLAGS,AX
MOV RDX1,BX
JMP NAG7_SHL
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
ERROR1:

JMP OUTT_44
NAG7_SHL:
  
OUTT_44:       
       ;AFTER INST:
      JMP NOT_COMMAND

      ;-------------------------------OPERATIONS3----------------------------------

      OPERATIONS3:
      MOV AL,ELDOOR
      CMP AL,2
      JZ D2_NO1
      D1_NO1: 
       GENERAL_INST COMp,COMp1,A_SIZE1
       JMP START_NO1
      D2_NO1:
        GENERAL_INST COMp,COMp2,A_SIZE2
       START_NO1:
       ;INST:
       
    MOV SI,OFFSET COMp 
    MOV AX,0
    MOV CX,0
    MOV AL,[SI+4] 
    ADD CL,[SI+5]
    ADD AX,CX   
    CMP AX,217
    JZ NTAAXDES_GROUP
    CMP AX,218                   ;BN3ML JUMP 3LA 7BT 7GAT W M7TNA4 MEMORY1 4OF B2AA
    JZ NTABXDES_GROUP
    CMP AX,219                    ;ANAM7TAGFUNCTIONT4OFLE 7WAR 2RKAM KDA KDA HOMA BLKTEER 4 2RKAM 3NDE M4AKL KBEERAA FE 2RKAM 5LE BALK ANA HA5OD 2RKAM MN 3LA 2L4MAAAL YAMEGZ 
    JZ NTACXDES_GROUP
    CMP AX,220
    JZ NTADXORSIDES_GROUP
    CMP AX,205
    JZ NTADIORALDES_GROUP
    CMP AX,201
    JZ NTAAHDIS_GROUP
    CMP AX,206
    JZ NTABLDIS_GROUP
    CMP AX,202
    JZ NTABHDIS_GROUP
    CMP AX,207
    JZ NTACLDIS_GROUP
    CMP AX,203
    JZ NTACHDIS_GROUP
    CMP AX,208
    JZ NTADLDIS_GROUP
    CMP AX,204
    JZ NTADHDIS_GROUP 
    JMP ERROR_IN
    
    
NTAAXDES_GROUP:
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX
AXDES_INC:
 CMP BP,30
 JNZ AXDES_DEC
 POPF
 INC BX
 MOV RAX1,BX
 JMP OUTT_BARA_AXDIS
 AXDES_DEC:
 CMP BP,31
 JNZ AXDES_MUL
 POPF 
 DEC BX
 MOV RAX1,BX
 JMP OUTT_BARA_AXDIS
 AXDES_MUL:
 MOV AX,RAX1
 CMP BP,32
 JNZ AXDES_DIV 
 POPF
 MUL BX
 MOV RAX1,AX
 MOV RDX1,DX
 JMP OUTT_BARA_AXDIS
 AXDES_DIV:
 MOV AX,RAX1
 MOV DX,RDX1
 POPF
 DIV BX
 MOV RAX1,AX
 MOV RDX1,DX
 JMP OUTT_BARA_AXDIS
 
 OUTT_BARA_AXDIS:
 PUSHF
 POP AX
 MOV FLAGS,AX
 JMP NAG7_AWY
 ;//////////////////////////////////////////////////////////////////////////////////////////////////////////////
 NTABXDES_GROUP:
MOV BX,RBX1
MOV AX,FLAGS
PUSH AX
BXDES_INC:
 CMP BP,30
 JNZ BXDES_DEC
 POPF
 INC BX
 MOV RBX1,BX
 JMP OUTT_BARA_BXDIS
 BXDES_DEC:
 CMP BP,31
 JNZ BXDES_MUL
 POPF 
 DEC BX
 MOV RBX1,BX
 JMP OUTT_BARA_BXDIS
 BXDES_MUL: 
 MOV AX,RAX1
 CMP BP,32
 JNZ BXDES_DIV
 POPF
 MUL BX
 MOV RAX1,AX
 MOV RDX1,DX
 JMP OUTT_BARA_BXDIS
 BXDES_DIV:
 MOV AX,RAX1
 MOV DX,RDX1
 POPF
 DIV BX
 MOV RAX1,AX
 MOV RDX1,DX
 JMP OUTT_BARA_BXDIS
 
 OUTT_BARA_BXDIS:
 PUSHF
 POP AX
 MOV FLAGS,AX
 JMP NAG7_AWY
 
 ;////////////////////////////////////////////////////////////////////////////////////////////////////////////// 
NTACXDES_GROUP: 
MOV BX,RCX1
MOV AX,FLAGS
PUSH AX
CXDES_INC:
 CMP BP,30
 JNZ CXDES_DEC
 POPF
 INC BX
 MOV RCX1,BX
 JMP OUTT_BARA_CXDIS
 CXDES_DEC:
 CMP BP,31
 JNZ CXDES_MUL
 POPF 
 DEC BX
 MOV RCX1,BX
 JMP OUTT_BARA_CXDIS
 CXDES_MUL:
 MOV AX,RAX1
 CMP BP,32
 JNZ CXDES_DIV
 POPF
 MUL BX
 MOV RAX1,AX
 MOV RDX1,DX
 JMP OUTT_BARA_CXDIS
 CXDES_DIV:  
 MOV AX,RAX1
 MOV DX,RDX1
 POPF
 DIV BX
 MOV RAX1,AX
 MOV RDX1,DX
 JMP OUTT_BARA_CXDIS
 
 OUTT_BARA_CXDIS:
 PUSHF
 POP AX
 MOV FLAGS,AX
 JMP NAG7_AWY
 ;///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
 NTADXORSIDES_GROUP:
MOV AL,[SI+4]
CMP AL,115
JZ NTASIDES_GROUP
NTADXDES_GROUP:
MOV BX,RDX1
MOV AX,FLAGS
PUSH AX
DXDES_INC:
 CMP BP,30
 JNZ DXDES_DEC
 POPF
 INC BX
 MOV RDX1,BX
 JMP OUTT_BARA_DXDIS
 DXDES_DEC:
 CMP BP,31
 JNZ DXDES_MUL
 POPF 
 DEC BX
 MOV RDX1,BX
 JMP OUTT_BARA_DXDIS
 DXDES_MUL:
 MOV AX,RAX1
 CMP BP,32
 JNZ DXDES_DIV
 POPF
 MUL BX
 MOV RAX1,AX
 MOV RDX1,DX
 JMP OUTT_BARA_DXDIS
 DXDES_DIV: 
 MOV AX,RAX1
 MOV DX,RDX1
 POPF
 DIV BX
 MOV RAX1,AX
 MOV RDX1,DX
 JMP OUTT_BARA_DXDIS
 
 OUTT_BARA_DXDIS:
 PUSHF
 POP AX
 MOV FLAGS,AX
 JMP NAG7_AWY
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTASIDES_GROUP:
MOV BX,ISI1
MOV AX,FLAGS
PUSH AX
SIDES_INC:
 CMP BP,30
 JNZ SIDES_DEC
 POPF
 INC BX
 MOV ISI1,BX
 JMP OUTT_BARA_SIDIS
 SIDES_DEC:
 CMP BP,31
 JNZ SIDES_MUL
 POPF 
 DEC BX
 MOV ISI1,BX
 JMP OUTT_BARA_SIDIS
 SIDES_MUL:
 MOV AX,RAX1
 CMP BP,32
 JNZ SIDES_DIV
 POPF
 MUL BX
 MOV RAX1,AX
 MOV RDX1,DX
 JMP OUTT_BARA_SIDIS
 SIDES_DIV: 
 MOV AX,RAX1
 MOV DX,RDX1
 POPF
 DIV BX
 MOV RAX1,AX
 MOV RDX1,DX
 JMP OUTT_BARA_SIDIS
 
 OUTT_BARA_SIDIS:
 PUSHF
 POP AX
 MOV FLAGS,AX
 JMP NAG7_AWY 
 ;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
 NTADIORALDES_GROUP:
 MOV AL,[SI+4]
 CMP AL,97
 JZ NTAALDES_GROUP
 NTADIDES_GROUP:
 MOV BX,IDI1
MOV AX,FLAGS
PUSH AX
DIDES_INC:
 CMP BP,30
 JNZ DIDES_DEC
 POPF
 INC BX
 MOV IDI1,BX
 JMP OUTT_BARA_DIDIS
 DIDES_DEC:
 CMP BP,31
 JNZ DIDES_MUL
 POPF 
 DEC BX
 MOV IDI1,BX
 JMP OUTT_BARA_DIDIS
 DIDES_MUL: 
 MOV AX,RAX1
 CMP BP,32
 JNZ DIDES_DIV
 POPF
 MUL BX
 MOV RAX1,AX
 MOV RDX1,DX
 JMP OUTT_BARA_DIDIS
 DIDES_DIV:
 MOV AX,RAX1
 MOV DX,RDX1
 POPF
 DIV BX
 MOV RAX1,AX
 MOV RDX1,DX
 JMP OUTT_BARA_DIDIS
 
 OUTT_BARA_DIDIS:
 PUSHF
 POP AX
 MOV FLAGS,AX
 JMP NAG7_AWY
 ;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTAALDES_GROUP:
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX
ALDES_INC:
 CMP BP,30
 JNZ ALDES_DEC
 POPF
 INC BL
 MOV RAX1,BX
 JMP OUTT_BARA_ALDIS
 ALDES_DEC:
 CMP BP,31
 JNZ ALDES_MUL
 POPF 
 DEC BL
 MOV RAX1,BX
 JMP OUTT_BARA_ALDIS
 ALDES_MUL:
 MOV AX,RAX1
 CMP BP,32
 JNZ ALDES_DIV
 POPF
 MUL BL
 MOV RAX1,AX
 JMP OUTT_BARA_ALDIS
 ALDES_DIV: 
 MOV AX,RAX1
 POPF
 DIV BL
 MOV RAX1,AX
 JMP OUTT_BARA_ALDIS
 
 OUTT_BARA_ALDIS:
 PUSHF
 POP AX
 MOV FLAGS,AX
 JMP NAG7_AWY
 ;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTABLDIS_GROUP:
 MOV BX,RBX1
MOV AX,FLAGS
PUSH AX
BLDES_INC:
 CMP BP,30
 JNZ BLDES_DEC
 POPF
 INC BL
 MOV RBX1,BX
 JMP OUTT_BARA_BLDIS
 BLDES_DEC:
 CMP BP,31
 JNZ BLDES_MUL
 POPF 
 DEC BL
 MOV RBX1,BX
 JMP OUTT_BARA_BLDIS
 BLDES_MUL:
 MOV AX,RAX1
 CMP BP,32
 JNZ BLDES_DIV
 POPF
 MUL BL
 MOV RAX1,AX
 JMP OUTT_BARA_BLDIS
 BLDES_DIV:
 MOV AX,RAX1
 POPF
 DIV BL
 MOV RAX1,AX
 JMP OUTT_BARA_BLDIS
 
 OUTT_BARA_BLDIS:
 PUSHF
 POP AX
 MOV FLAGS,AX
 JMP NAG7_AWY
 
 ;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
 NTACLDIS_GROUP:
MOV BX,RCX1
MOV AX,FLAGS
PUSH AX
CLDES_INC:
 CMP BP,30
 JNZ CLDES_DEC
 POPF
 INC BL
 MOV RCX1,BX
 JMP OUTT_BARA_CLDIS
 CLDES_DEC:
 CMP BP,31
 JNZ CLDES_MUL
 POPF 
 DEC BL
 MOV RCX1,BX
 JMP OUTT_BARA_CLDIS
 CLDES_MUL:
 MOV AX,RAX1
 CMP BP,32
 JNZ CLDES_DIV
 POPF
 MUL BL
 MOV RAX1,AX
 JMP OUTT_BARA_CLDIS
 CLDES_DIV: 
 MOV AX,RAX1
 POPF
 DIV BL
 MOV RAX1,AX
 JMP OUTT_BARA_CLDIS
 
 OUTT_BARA_CLDIS:
 PUSHF
 POP AX
 MOV FLAGS,AX
 JMP NAG7_AWY
 ;////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTADLDIS_GROUP:
 MOV BX,RDX1
MOV AX,FLAGS
PUSH AX
DLDES_INC:
 CMP BP,30
 JNZ DLDES_DEC
 POPF
 INC BL
 MOV RDX1,BX
 JMP OUTT_BARA_DLDIS
 DLDES_DEC:
 CMP BP,31
 JNZ DLDES_MUL
 POPF 
 DEC BL
 MOV RDX1,BX
 JMP OUTT_BARA_DLDIS
 DLDES_MUL:
 MOV AX,RAX1
 CMP BP,32
 JNZ DLDES_DIV
 POPF
 MUL BL
 MOV RAX1,AX
 JMP OUTT_BARA_DLDIS
 DLDES_DIV:
 MOV AX,RAX1
 POPF
 DIV BL
 MOV RAX1,AX
 JMP OUTT_BARA_DLDIS
 
 OUTT_BARA_DLDIS:
 PUSHF
 POP AX
 MOV FLAGS,AX
 JMP NAG7_AWY
 ;/////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTAAHDIS_GROUP:
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX
AHDES_INC:
 CMP BP,30
 JNZ AHDES_DEC
 POPF
 INC BH
 MOV RAX1,BX
 JMP OUTT_BARA_AHDIS
 AHDES_DEC:
 CMP BP,31
 JNZ AHDES_MUL
 POPF 
 DEC BH
 MOV RAX1,BX
 JMP OUTT_BARA_AHDIS
 AHDES_MUL:
 MOV AX,RAX1
 CMP BP,32
 JNZ AHDES_DIV
 POPF
 MUL BH
 MOV RAX1,AX
 JMP OUTT_BARA_AHDIS
 AHDES_DIV:
 MOV AX,RAX1
 POPF
 DIV BH
 MOV RAX1,AX
 JMP OUTT_BARA_AHDIS
 
 OUTT_BARA_AHDIS:
 PUSHF
 POP AX
 MOV FLAGS,AX
 JMP NAG7_AWY
 ;/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTABHDIS_GROUP:
MOV BX,RBX1
MOV AX,FLAGS
PUSH AX
BHDES_INC:
 CMP BP,30
 JNZ BHDES_DEC
 POPF
 INC BH
 MOV RBX1,BX
 JMP OUTT_BARA_BHDIS
 BHDES_DEC:
 CMP BP,31
 JNZ BHDES_MUL
 POPF 
 DEC BH
 MOV RBX1,BX
 JMP OUTT_BARA_BHDIS
 BHDES_MUL: 
 MOV AX,RAX1
 CMP BP,32
 JNZ BHDES_DIV
 POPF
 MUL BH
 MOV RAX1,AX
 JMP OUTT_BARA_BHDIS
 BHDES_DIV:
 MOV AX,RAX1
 POPF
 DIV BH
 MOV RAX1,AX
 JMP OUTT_BARA_BHDIS
 
 OUTT_BARA_BHDIS:
 PUSHF
 POP AX
 MOV FLAGS,AX
 JMP NAG7_AWY
 ;//////////////////////////////////////////////////////////////////////////////////////////////////////////
NTACHDIS_GROUP:
MOV BX,RCX1
MOV AX,FLAGS
PUSH AX
CHDES_INC:
 CMP BP,30
 JNZ CHDES_DEC
 POPF
 INC BH
 MOV RCX1,BX
 JMP OUTT_BARA_CHDIS
 CHDES_DEC:
 CMP BP,31
 JNZ CHDES_MUL
 POPF 
 DEC BH
 MOV RCX1,BX
 JMP OUTT_BARA_CHDIS
 CHDES_MUL:
 MOV AX,RAX1
 CMP BP,32
 JNZ CHDES_DIV
 POPF
 MUL BH
 MOV RAX1,AX
 JMP OUTT_BARA_CHDIS
 CHDES_DIV:
 MOV AX,RAX1
 POPF
 DIV BH
 MOV RAX1,AX
 JMP OUTT_BARA_CHDIS
 
 OUTT_BARA_CHDIS:
 PUSHF
 POP AX
 MOV FLAGS,AX
 JMP NAG7_AWY
 ;//////////////////////////////////////////////////////////////////////////////////////////////////////
NTADHDIS_GROUP:
MOV BX,RDX1
MOV AX,FLAGS
PUSH AX
DHDES_INC:
 CMP BP,30
 JNZ DHDES_DEC
 POPF
 INC BH
 MOV RDX1,BX
 JMP OUTT_BARA_DHDIS
 DHDES_DEC:
 CMP BP,31
 JNZ DHDES_MUL
 POPF 
 DEC BH
 MOV RDX1,BX
 JMP OUTT_BARA_DHDIS
 DHDES_MUL: 
 MOV AX,RAX1
 CMP BP,32
 JNZ DHDES_DIV
 POPF
 MUL BH
 MOV RAX1,AX
 JMP OUTT_BARA_DHDIS
 DHDES_DIV:
 MOV AX,RAX1
 POPF
 DIV BH
 MOV RAX1,AX
 JMP OUTT_BARA_DHDIS
 
 OUTT_BARA_DHDIS:
 PUSHF
 POP AX
 MOV FLAGS,AX
 JMP NAG7_AWY 
 ERROR_IN:

JMP OUTT_BARA
NAG7_AWY:
  
OUTT_BARA:
       ;AFTER INST:
      JMP NOT_COMMAND

     ;---------------------------------------------------------------NOOP-------------------------------------------------------------------------------------
NOOP:
      MOV AL,ELDOOR
      CMP AL,2
      JZ D2_NO
      D1_NO: 
       GENERAL_INST COMp,COMp1,A_SIZE1
       JMP START_NO
      D2_NO:
        GENERAL_INST COMp,COMp2,A_SIZE2
       START_NO:
       ;INST:
       ;AFTER INST:
      JMP NOT_COMMAND
     ;-------------------------------------------CLEAR CARRY-------------------------------------------------

CLCF:
    MOV AL,ELDOOR
      CMP AL,2
      JZ D2_CL

      D1_CL: 
       GENERAL_INST COMp,COMp1,A_SIZE1
       JMP START_CL

      D2_CL:
        GENERAL_INST COMp,COMp2,A_SIZE2

       START_CL:

       ;INST:
       MOV DX,1111111111111110B
       AND FLAGS,DX

       ;AFTER INST:
       JMP NOT_COMMAND

  ORING:     
      CMP ELDOOR,2
      JZ D2_OR
      D1_OR: 
       GENERAL_INST COMp,COMp1,A_SIZE1
       JMP START_CL

      D2_OR:
        GENERAL_INST COMp,COMp2,A_SIZE2

       START_OR:

       ;INST:
           MOV SI,OFFSET COMp 
    MOV AX,0
    MOV CX,0
    MOV AL,[SI+3] 
    ADD CL,[SI+4]
    ADD AX,CX   
    CMP AX,217
    JZ NTAAXDES_OR
    CMP AX,218                   ;BN3ML JUMP 3LA 7BT 7GAT W M7TNA4 MEMORY1 4OF B2AA
    JZ NTABXDES_OR
    CMP AX,219                    ;ANAM7TAGFUNCTIONT4OFLE 7WAR 2RKAM KDA KDA HOMA BLKTEER 4 2RKAM 3NDE M4AKL KBEERAA FE 2RKAM 5LE BALK ANA HA5OD 2RKAM MN 3LA 2L4MAAAL YAMEGZ 
    JZ NTACXDES_OR
    CMP AX,220
    JZ NTADXORSIDES_OR
    CMP AX,205
    JZ NTADIORALDES_OR
    CMP AX,201
    JZ NTAAHDIS_OR
    CMP AX,206
    JZ NTABLDIS_OR
    CMP AX,202
    JZ NTABHDIS_OR
    CMP AX,207
    JZ NTACLDIS_OR
    CMP AX,203
    JZ NTACHDIS_OR
    CMP AX,208
    JZ NTADLDIS_OR
    CMP AX,204
    JZ NTADHDIS_OR 
    JMP ERROR_OR
NTAAXDES_OR:
  MOV BX,0
  MOV BL,[SI+7]
  CMP BL,104
  JNAE NTAAXDESW_RKM_YA_MEMRKM_YAERROR_OR_OR
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+6] 
  MOV CL,[SI+7]
  ADD AX,CX
  CMP AX,217
  JZ NTAAXDESWAXSOURCE_OR
  CMP AX,218 
  JZ NTAAXDESWBXSOURCE_OR
  CMP AX,219
  JZ NTAAXDESWCXSOURCE_OR
  CMP AX,220
 JZ NTAAXDESWDXORSISOURCE_OR
 CMP AX,205
 JZ NTAAXDESWDIORALLWALERROR_OR_OR
 MOV CX,0
 MOV BX,0                            ;TAKECARE
 MOV BL,[SI+8]
 MOV CL,[SI+9]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ AXDES_WNTA_MEMBX_OR
 CMP AX,404              
 JZ NTA_AXDES_WMEMSI_OR
 CMP AX,389
 JZ NTA_AXDES_WMEMDI_OR
 JMP NTAAXDESW_RKM_YA_MEMRKM_YAERROR_OR_OR 
 
 NTAAXDESWAXSOURCE_OR:  
 MOV BX,RAX1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,BX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,BX
 JMP NAG7_OR
 
 NTAAXDESWBXSOURCE_OR: 
 MOV BX,RAX1
 MOV CX,RBX1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,CX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,BX
 JMP NAG7_OR
 NTAAXDESWCXSOURCE_OR:
 MOV BX,RAX1
 MOV CX,RCX1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,CX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,BX
 JMP NAG7_OR 
 NTAAXDESWDXORSISOURCE_OR:
 MOV AL,[SI+6]
 CMP AL,100
 JZ NTAAXDESWDXSOURCE_OR
  
NTAAXDESWSISOURCE_OR:
 MOV BX,RAX1
 MOV CX,ISI1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,CX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,BX
JMP NAG7_OR
NTAAXDESWDXSOURCE_OR:
 MOV BX,RAX1
 MOV CX,RDX1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,CX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,BX
JMP NAG7_OR
NTAAXDESWDIORALLWALERROR_OR_OR: 
MOV AL,[SI+6]
CMP AL,100 
JZ NTAAXDESWDISOURCE_OR
JMP ERROR_OR
NTAAXDESWDISOURCE_OR:
 MOV BX,RAX1
 MOV CX,IDI1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,CX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RAX1,BX
JMP NAG7_OR
AXDES_WNTA_MEMBX_OR:
MOV BX,RBX1
CMP BX,0FH
JA ERROR_OR
MOV CL, MEMORY1[BX]
MOV CH, MEMORY1[BX+1]
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX
POPF
OR BX,CX
PUSHF
POP AX
MOV FLAGS,AX
MOV RAX1,BX 
JMP NAG7_OR
NTA_AXDES_WMEMSI_OR:
MOV BX,ISI1
CMP BX,0FH
JA ERROR_OR
MOV CH, MEMORY1[BX+1]
MOV CL, MEMORY1[BX]
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX
POPF
OR BX,CX
PUSHF
POP AX
MOV FLAGS,AX
MOV RAX1,BX
JMP NAG7_OR
NTA_AXDES_WMEMDI_OR:
MOV BX,IDI1
CMP BX,0FH
JA ERROR_OR
MOV CH, MEMORY1[BX+1]
MOV CL, MEMORY1[BX]
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX
POPF
OR BX,CX
PUSHF
POP AX
MOV FLAGS,AX
MOV RAX1,BX
JMP NAG7_OR
NTAAXDESW_RKM_YA_MEMRKM_YAERROR_OR_OR:
MOV AL,[SI+6]
CMP AL,91
JZ MEMRKM_ORERROR_OR_AXDES_OR

RKM_ORERROR_OR_AXDES_OR:
MOV AL,[SI+7]
CMP AL,108
JZ ERROR_OR
CMP AL,104
JZ ERROR_OR

RET_NUM_COUNT  COMp+6
CMP CX,4
JA ERROR_OR
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX
POPF 
OR BX,DX
PUSHF
POP AX
MOV FLAGS,AX
MOV RAX1,BX
JMP NAG7_OR
MEMRKM_ORERROR_OR_AXDES_OR:
MOV AL,[SI+8]
CMP AL,108
JZ ERROR_OR
CMP AL,104
JZ ERROR_OR
RET_NUM_COUNT  COMp+7
CMP CX,4
JA ERROR_OR
CMP DX,0FH
JA ERROR_OR
MOV BX,DX
MOV CH, MEMORY1[BX+1]
MOV CL, MEMORY1[BX]
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX
POPF 
OR BX,CX
PUSHF
POP AX
MOV FLAGS,AX
MOV RAX1,BX
JMP NAG7_OR
;/////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTABXDES_OR:
  MOV BX,0
  MOV BL,[SI+7]
  CMP BL,104
  JNAE NTABXDESW_RKM_YA_MEMRKM_YAERROR_OR_OR
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+6] 
  MOV CL,[SI+7]
  ADD AX,CX
  CMP AX,217
  JZ NTABXDESWAXSOURCE_OR
  CMP AX,218 
  JZ NTABXDESWBXSOURCE_OR
  CMP AX,219
  JZ NTABXDESWCXSOURCE_OR
  CMP AX,220
 JZ NTABXDESWDXORSISOURCE_OR
 CMP AX,205
 JZ NTABXDESWDIORALLWALERROR_OR_OR
 MOV BX,0
 MOV CX,0                              ;TAKECARE
 MOV CL,[SI+8]
 MOV BL,[SI+9]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ BXDES_WNTA_MEMBX_OR
 CMP AX,404              
 JZ NTA_BXDES_WMEMSI_OR
 CMP AX,389
 JZ NTA_BXDES_WMEMDI_OR
 JMP NTABXDESW_RKM_YA_MEMRKM_YAERROR_OR_OR  
 
 NTABXDESWAXSOURCE_OR:  
 MOV BX,RAX1
 MOV CX,RBX1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,CX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,BX
 JMP NAG7_OR
 
 NTABXDESWBXSOURCE_OR:
 MOV BX,RBX1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,BX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,BX 
 JMP NAG7_OR
 NTABXDESWCXSOURCE_OR:
 MOV BX,RBX1
 MOV CX,RCX1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,CX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,BX
 JMP NAG7_OR 
 NTABXDESWDXORSISOURCE_OR:
 MOV AL,[SI+6]
 CMP AL,100
 JZ NTABXDESWDXSOURCE_OR
  
NTABXDESWSISOURCE_OR:
 MOV BX,ISI1
 MOV CX,RBX1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,CX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,BX
JMP NAG7_OR
NTABXDESWDXSOURCE_OR:
 MOV BX,RDX1
 MOV CX,RBX1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,CX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,BX
JMP NAG7_OR
NTABXDESWDIORALLWALERROR_OR_OR: 
MOV AL,[SI+6]
CMP AL,100 
JZ NTABXDESWDISOURCE_OR
JMP ERROR_OR
NTABXDESWDISOURCE_OR:
 MOV BX,IDI1
 MOV CX,RBX1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,CX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RBX1,BX
JMP NAG7_OR
BXDES_WNTA_MEMBX_OR:
MOV BX,RBX1
CMP BX,0FH
JA ERROR_OR
MOV CL, MEMORY1[BX]
MOV CH, MEMORY1[BX+1]
MOV BX,RBX1
MOV AX,FLAGS
PUSH AX
POPF
OR BX,CX
PUSHF
POP AX
MOV FLAGS,AX
MOV RBX1,BX 
JMP NAG7_OR
 NTA_BXDES_WMEMSI_OR:
MOV BX,ISI1
CMP BX,0FH
JA ERROR_OR
MOV CL, MEMORY1[BX]
MOV CH, MEMORY1[BX+1]
MOV BX,RBX1
MOV AX,FLAGS
PUSH AX
POPF
OR BX,CX
PUSHF
POP AX
MOV FLAGS,AX
MOV RBX1,BX 
JMP NAG7_OR
NTA_BXDES_WMEMDI_OR:
MOV BX,IDI1
CMP BX,0FH
JA ERROR_OR
MOV CL, MEMORY1[BX]
MOV CH, MEMORY1[BX+1]
MOV BX,RBX1
MOV AX,FLAGS
PUSH AX
POPF
OR BX,CX
PUSHF
POP AX
MOV FLAGS,AX
MOV RBX1,BX 
JMP NAG7_OR
NTABXDESW_RKM_YA_MEMRKM_YAERROR_OR_OR:
MOV AL,[SI+6]
CMP AL,91
JZ MEMRKM_ORERROR_OR_BXDES_OR

RKM_ORERROR_OR_BXDES_OR:
MOV AL,[SI+7]
CMP AL,108
JZ ERROR_OR
CMP AL,104
JZ ERROR_OR
RET_NUM_COUNT  COMp+6
CMP CX,4
JA ERROR_OR
MOV BX,RBX1
MOV AX,FLAGS
PUSH AX
POPF 
OR BX,DX
PUSHF
POP AX
MOV FLAGS,AX
MOV RBX1,BX
JMP NAG7_OR
MEMRKM_ORERROR_OR_BXDES_OR:
MOV AL,[SI+8]
CMP AL,108
JZ ERROR_OR
CMP AL,104
JZ ERROR_OR
RET_NUM_COUNT  COMp+7
CMP CX,4
JA ERROR_OR
CMP DX,0FH
JA ERROR_OR
MOV BX,DX
MOV CH, MEMORY1[BX+1]
MOV CL, MEMORY1[BX]
MOV BX,RBX1
MOV AX,FLAGS
PUSH AX
POPF 
OR BX,CX
PUSHF
POP AX
MOV FLAGS,AX
MOV RBX1,BX
JMP NAG7_OR
;/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTACXDES_OR:
 MOV BX,0
 MOV BL,[SI+7]
 CMP BL,104
 JNAE NTACXDESW_RKM_YA_MEMRKM_YAERROR_OR_OR
 MOV CX,0
 MOV AX,0
 MOV AL,[SI+6] 
 MOV CL,[SI+7]
 ADD AX,CX
 CMP AX,217
 JZ NTACXDESWAXSOURCE_OR
 CMP AX,218 
 JZ NTACXDESWBXSOURCE_OR
 CMP AX,219
 JZ NTACXDESWCXSOURCE_OR
 CMP AX,220
 JZ NTACXDESWDXORSISOURCE_OR
 CMP AX,205
 JZ NTACXDESWDIORALLWALERROR_OR_OR 
 
 MOV BX,0
 MOV CX,0                              ;TAKECARE
 MOV CL,[SI+8]
 MOV BL,[SI+9]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ CXDES_WNTA_MEMBX_OR
 CMP AX,404              
 JZ NTA_CXDES_WMEMSI_OR
 CMP AX,389
 JZ NTA_CXDES_WMEMDI_OR
 JMP NTACXDESW_RKM_YA_MEMRKM_YAERROR_OR_OR  
 
 NTACXDESWAXSOURCE_OR:  
 MOV BX,RAX1
 MOV CX,RCX1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,CX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,BX
 JMP NAG7_OR
 
 NTACXDESWBXSOURCE_OR: 
 MOV BX,RCX1
 MOV CX,RBX1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,CX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,BX
 JMP NAG7_OR
 NTACXDESWCXSOURCE_OR:
 MOV BX,RCX1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,BX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,BX
 JMP NAG7_OR 
 NTACXDESWDXORSISOURCE_OR:
 MOV AL,[SI+6]
 CMP AL,100
 JZ NTACXDESWDXSOURCE_OR
  
NTACXDESWSISOURCE_OR:
 MOV BX,RCX1
 MOV CX,ISI1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,CX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,BX
JMP NAG7_OR
NTACXDESWDXSOURCE_OR:
 MOV BX,RCX1
 MOV CX,RDX1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,CX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,BX
JMP NAG7_OR
NTACXDESWDIORALLWALERROR_OR_OR: 
MOV AL,[SI+6]
CMP AL,100 
JZ NTACXDESWDISOURCE_OR
JMP ERROR_OR
NTACXDESWDISOURCE_OR:
 MOV BX,RCX1
 MOV CX,IDI1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,CX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RCX1,BX
JMP NAG7_OR
CXDES_WNTA_MEMBX_OR:
MOV BX,RBX1
CMP BX,0FH
JA ERROR_OR
MOV CH, MEMORY1[BX+1]
MOV CL, MEMORY1[BX]
MOV BX,RCX1
MOV AX,FLAGS
PUSH AX
POPF
OR BX,CX
PUSHF
POP AX
MOV FLAGS,AX
MOV RCX1,BX 
JMP NAG7_OR
 NTA_CXDES_WMEMSI_OR:
MOV BX,ISI1
CMP BX,0FH
JA ERROR_OR
MOV CH, MEMORY1[BX+1]
MOV CL, MEMORY1[BX]
MOV BX,RCX1
MOV AX,FLAGS
PUSH AX
POPF
OR BX,CX
PUSHF
POP AX
MOV FLAGS,AX
MOV RCX1,BX
JMP NAG7_OR
NTA_CXDES_WMEMDI_OR:
MOV BX,IDI1
CMP BX,0FH
JA ERROR_OR
MOV CH, MEMORY1[BX+1]
MOV CL, MEMORY1[BX]
MOV BX,RCX1
MOV AX,FLAGS
PUSH AX
POPF
OR BX,CX
PUSHF
POP AX
MOV FLAGS,AX
MOV RCX1,BX
JMP NAG7_OR
NTACXDESW_RKM_YA_MEMRKM_YAERROR_OR_OR:
MOV AL,[SI+6]
CMP AL,91
JZ MEMRKM_ORERROR_OR_CXDES_OR

RKM_ORERROR_OR_CXDES_OR:
MOV AL,[SI+7]
CMP AL,108
JZ ERROR_OR
CMP AL,104
JZ ERROR_OR
RET_NUM_COUNT  COMp+6
CMP CX,4
JA ERROR_OR
MOV BX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
OR BX,DX
PUSHF
POP AX
MOV FLAGS,AX
MOV RCX1,BX
JMP NAG7_OR
MEMRKM_ORERROR_OR_CXDES_OR:
MOV AL,[SI+8]
CMP AL,108
JZ ERROR_OR
CMP AL,104
JZ ERROR_OR
RET_NUM_COUNT  COMp+7
CMP CX,4
JA ERROR_OR
CMP DX,0FH
JA ERROR_OR
MOV BX,DX
MOV CH, MEMORY1[BX+1]
MOV CL, MEMORY1[BX]
MOV BX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
OR BX,CX
PUSHF
POP AX
MOV FLAGS,AX
MOV RCX1,BX
JMP NAG7_OR
;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTADXORSIDES_OR:
MOV AL,[SI+3]
CMP AL,115
JZ NTASIDES_OR
NTADXDES_OR:
  MOV BX,0
  MOV BL,[SI+7]
  CMP BL,104
  JNAE NTADXDESW_RKM_YA_MEMRKM_YAERROR_OR_OR
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+6] 
  MOV CL,[SI+7]
  ADD AX,CX
  CMP AX,217
  JZ NTADXDESWAXSOURCE_OR
  CMP AX,218 
  JZ NTADXDESWBXSOURCE_OR
  CMP AX,219
  JZ NTADXDESWCXSOURCE_OR
  CMP AX,220
 JZ NTADXDESWDXORSISOURCE_OR
 CMP AX,205
 JZ NTADXDESWDIORALLWALERROR_OR_OR
 MOV BX,0
 MOV CX,0                              ;TAKECARE
 MOV CL,[SI+8]
 MOV BL,[SI+9]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ DXDES_WNTA_MEMBX_OR
 CMP AX,404              
 JZ NTA_DXDES_WMEMSI_OR
 CMP AX,389
 JZ NTA_DXDES_WMEMDI_OR
 JMP NTADXDESW_RKM_YA_MEMRKM_YAERROR_OR_OR  
 
 NTADXDESWAXSOURCE_OR:  
 MOV BX,RAX1
 MOV CX,RDX1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,CX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,BX
 JMP NAG7_OR
 
 NTADXDESWBXSOURCE_OR: 
 MOV BX,RBX1
 MOV CX,RDX1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,CX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,BX
 JMP NAG7_OR
 NTADXDESWCXSOURCE_OR:
 MOV BX,RCX1
 MOV CX,RDX1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,CX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,BX
 JMP NAG7_OR
 NTADXDESWDXORSISOURCE_OR:
 MOV AL,[SI+6]
 CMP AL,100
 JZ NTADXDESWDXSOURCE_OR
  
NTADXDESWSISOURCE_OR:
 MOV BX,ISI1
 MOV CX,RDX1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,CX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,BX
JMP NAG7_OR
NTADXDESWDXSOURCE_OR:
 MOV BX,RDX1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,BX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,BX
JMP NAG7_OR
NTADXDESWDIORALLWALERROR_OR_OR: 
MOV AL,[SI+6]
CMP AL,100 
JZ NTADXDESWDISOURCE_OR
JMP ERROR_OR
NTADXDESWDISOURCE_OR:
 MOV BX,IDI1
 MOV CX,RDX1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,CX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV RDX1,BX
JMP NAG7_OR
DXDES_WNTA_MEMBX_OR:
MOV BX,RBX1
CMP BX,0FH
JA ERROR_OR
MOV CH, MEMORY1[BX+1]
MOV CL, MEMORY1[BX]
MOV BX,RDX1
MOV AX,FLAGS
PUSH AX
POPF
OR BX,CX
PUSHF
POP AX
MOV FLAGS,AX
MOV RDX1,BX 
JMP NAG7_OR
 NTA_DXDES_WMEMSI_OR:
MOV BX,ISI1
CMP BX,0FH
JA ERROR_OR
MOV CH, MEMORY1[BX+1]
MOV CL, MEMORY1[BX]
MOV BX,RDX1
MOV AX,FLAGS
PUSH AX
POPF
OR BX,CX
PUSHF
POP AX
MOV FLAGS,AX
MOV RDX1,BX 
JMP NAG7_OR
NTA_DXDES_WMEMDI_OR:
MOV BX,IDI1
CMP BX,0FH
JA ERROR_OR
MOV CH, MEMORY1[BX+1]
MOV CL, MEMORY1[BX]
MOV BX,RDX1
MOV AX,FLAGS
PUSH AX
POPF
OR BX,CX
PUSHF
POP AX
MOV FLAGS,AX
MOV RDX1,BX 
JMP NAG7_OR
NTADXDESW_RKM_YA_MEMRKM_YAERROR_OR_OR:
MOV AL,[SI+6]
CMP AL,91
JZ MEMRKM_ORERROR_OR_DXDES_OR

RKM_ORERROR_OR_DXDES_OR:
MOV AL,[SI+7]
CMP AL,108
JZ ERROR_OR
CMP AL,104
JZ ERROR_OR
RET_NUM_COUNT  COMp+6
CMP CX,4
JA ERROR_OR
MOV BX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
OR BX,DX
PUSHF
POP AX
MOV FLAGS,AX
MOV RDX1,BX
JMP NAG7_OR
MEMRKM_ORERROR_OR_DXDES_OR:
MOV AL,[SI+9]
CMP AL,108
JZ ERROR_OR
CMP AL,104
JZ ERROR_OR
RET_NUM_COUNT  COMp+7
CMP CX,4
JA ERROR_OR
CMP DX,0FH
JA ERROR_OR
MOV BX,DX
MOV CH, MEMORY1[BX+1]
MOV CL, MEMORY1[BX]
MOV BX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
OR BX,CX
PUSHF
POP AX
MOV FLAGS,AX
MOV RDX1,BX
JMP NAG7_OR
;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTASIDES_OR:
  MOV BX,0
  MOV BL,[SI+7]
  CMP BL,104
  JNAE NTASIDESW_RKM_YA_MEMRKM_YAERROR_OR_OR
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+6] 
  MOV CL,[SI+7]
  ADD AX,CX
  CMP AX,217
  JZ NTASIDESWAXSOURCE_OR
  CMP AX,218 
  JZ NTASIDESWBXSOURCE_OR
  CMP AX,219
  JZ NTASIDESWCXSOURCE_OR
  CMP AX,220
 JZ NTASIDESWDXORSISOURCE_OR
 CMP AX,205
 JZ NTASIDESWDIORALLWALERROR_OR_OR
 MOV BX,0
 MOV CX,0                              ;TAKECARE
 MOV CL,[SI+8]
 MOV BL,[SI+9]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ SIDES_WNTA_MEMBX_OR
 CMP AX,404              
 JZ NTA_SIDES_WMEMSI_OR
 CMP AX,389
 JZ NTA_SIDES_WMEMDI_OR
 JMP NTASIDESW_RKM_YA_MEMRKM_YAERROR_OR_OR  
 
 NTASIDESWAXSOURCE_OR:  
 MOV BX,RAX1
 MOV CX,ISI1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,CX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV ISI1,BX
 JMP NAG7_OR
 
 NTASIDESWBXSOURCE_OR: 
 MOV BX,RBX1
 MOV CX,ISI1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,CX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV ISI1,BX
 JMP NAG7_OR
 NTASIDESWCXSOURCE_OR:
 MOV BX,RCX1
 MOV CX,ISI1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,CX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV ISI1,BX
 JMP NAG7_OR
 NTASIDESWDXORSISOURCE_OR:
 MOV AL,[SI+6]
 CMP AL,100
 JZ NTASIDESWDXSOURCE_OR
  
NTASIDESWSISOURCE_OR:
 MOV BX,ISI1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,BX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV ISI1,BX
JMP NAG7_OR
NTASIDESWDXSOURCE_OR:
 MOV BX,RDX1
 MOV CX,ISI1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,CX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV ISI1,BX
 JMP NAG7_OR
NTASIDESWDIORALLWALERROR_OR_OR: 
MOV AL,[SI+6]
CMP AL,100 
JZ NTASIDESWDISOURCE_OR
JMP ERROR_OR
NTASIDESWDISOURCE_OR:
 MOV BX,IDI1
 MOV CX,ISI1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,CX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV ISI1,BX
JMP NAG7_OR
SIDES_WNTA_MEMBX_OR:
MOV BX,RBX1
CMP BX,0FH
JA ERROR_OR
MOV CL, MEMORY1[BX]
MOV CH, MEMORY1[BX+1]
MOV BX,ISI1
MOV AX,FLAGS
PUSH AX
POPF
OR BX,CX
PUSHF
POP AX
MOV FLAGS,AX
MOV ISI1,BX 
JMP NAG7_OR
 NTA_SIDES_WMEMSI_OR:
MOV BX,ISI1
CMP BX,0FH
JA ERROR_OR
MOV CL, MEMORY1[BX]
MOV CH, MEMORY1[BX+1]
MOV BX,ISI1
MOV AX,FLAGS
PUSH AX
POPF
OR BX,CX
PUSHF
POP AX
MOV FLAGS,AX
MOV ISI1,BX 
JMP NAG7_OR
NTA_SIDES_WMEMDI_OR:
MOV BX,IDI1
CMP BX,0FH
JA ERROR_OR
MOV CL, MEMORY1[BX]
MOV CH, MEMORY1[BX+1]
MOV BX,ISI1
MOV AX,FLAGS
PUSH AX
POPF
OR BX,CX
PUSHF
POP AX
MOV FLAGS,AX
MOV ISI1,BX 
JMP NAG7_OR
NTASIDESW_RKM_YA_MEMRKM_YAERROR_OR_OR:
MOV AL,[SI+6]
CMP AL,91
JZ MEMRKM_ORERROR_OR_SIDES_OR

RKM_ORERROR_OR_SIDES_OR:
MOV AL,[SI+7]
CMP AL,108
JZ ERROR_OR
CMP AL,104
JZ ERROR_OR
RET_NUM_COUNT  COMp+6
CMP CX,4
JA ERROR_OR
MOV BX,ISI1
MOV AX,FLAGS
PUSH AX
POPF 
OR BX,DX
PUSHF
POP AX
MOV FLAGS,AX
MOV ISI1,BX
JMP NAG7_OR
MEMRKM_ORERROR_OR_SIDES_OR:
MOV AL,[SI+8]
CMP AL,108
JZ ERROR_OR
CMP AL,104
JZ ERROR_OR
RET_NUM_COUNT  COMp+7
CMP CX,4
JA ERROR_OR
CMP DX,0FH
JA ERROR_OR
MOV BX,DX
MOV CH, MEMORY1[BX+1]
MOV CL, MEMORY1[BX]
MOV BX,ISI1
MOV AX,FLAGS
PUSH AX
POPF 
OR BX,CX
PUSHF
POP AX
MOV FLAGS,AX
MOV ISI1,BX
JMP NAG7_OR
;///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

NTADIORALDES_OR:
MOV AL,[SI+3]
CMP AL,97
JZ NTAALDES_OR
NTADIDES_OR:
  MOV BX,0
  MOV BL,[SI+7]
  CMP BL,104
  JNAE NTADIDESW_RKM_YA_MEMRKM_YAERROR_OR_OR
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+6] 
  MOV CL,[SI+7]
  ADD AX,CX
  CMP AX,217
  JZ NTADIDESWAXSOURCE_OR
  CMP AX,218 
  JZ NTADIDESWBXSOURCE_OR
  CMP AX,219
  JZ NTADIDESWCXSOURCE_OR
  CMP AX,220
 JZ NTADIDESWDXORSISOURCE_OR
 CMP AX,205
 JZ NTADIDESWDIORALLWALERROR_OR_OR
 MOV BX,0
 MOV CX,0                              ;TAKECARE
 MOV CL,[SI+8]
 MOV BL,[SI+9]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ DIDES_WNTA_MEMBX_OR
 CMP AX,404              
 JZ NTA_DIDES_WMEMSI_OR
 CMP AX,389
 JZ NTA_DIDES_WMEMDI_OR
 JMP NTADIDESW_RKM_YA_MEMRKM_YAERROR_OR_OR  
 
 NTADIDESWAXSOURCE_OR:  
 MOV BX,RAX1
 MOV CX,IDI1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,CX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV IDI1,BX
 JMP NAG7_OR
 
 NTADIDESWBXSOURCE_OR: 
 MOV BX,RBX1
 MOV CX,IDI1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,CX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV IDI1,BX
 JMP NAG7_OR
 NTADIDESWCXSOURCE_OR:
 MOV BX,RCX1
 MOV CX,IDI1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,CX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV IDI1,BX
 JMP NAG7_OR
 NTADIDESWDXORSISOURCE_OR:
 MOV AL,[SI+6]
 CMP AL,100
 JZ NTADIDESWDXSOURCE_OR
  
NTADIDESWSISOURCE_OR:
 MOV BX,ISI1
 MOV CX,IDI1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,CX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV IDI1,BX
 JMP NAG7_OR
NTADIDESWDXSOURCE_OR:
 MOV BX,RDX1
 MOV CX,IDI1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,CX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV IDI1,BX
 JMP NAG7_OR
NTADIDESWDIORALLWALERROR_OR_OR: 
MOV AL,[SI+6]
CMP AL,100 
JZ NTADIDESWDISOURCE_OR
JMP ERROR_OR
NTADIDESWDISOURCE_OR:
 MOV BX,IDI1
 MOV AX,FLAGS
 PUSH AX
 POPF
 OR BX,BX
 PUSHF
 POP AX
 MOV FLAGS,AX
 MOV IDI1,BX
JMP NAG7_OR
DIDES_WNTA_MEMBX_OR:
MOV BX,RBX1
CMP BX,0FH
JA ERROR_OR
MOV CL, MEMORY1[BX]
MOV CH, MEMORY1[BX+1]
MOV BX,IDI1
MOV AX,FLAGS
PUSH AX
POPF
OR BX,CX
PUSHF
POP AX
MOV FLAGS,AX
MOV IDI1,BX 
JMP NAG7_OR
 NTA_DIDES_WMEMSI_OR:
MOV BX,ISI1
CMP BX,0FH
JA ERROR_OR
MOV CL, MEMORY1[BX]
MOV CH, MEMORY1[BX+1]
MOV BX,IDI1
MOV AX,FLAGS
PUSH AX
POPF
OR BX,CX
PUSHF
POP AX
MOV FLAGS,AX
MOV IDI1,BX
JMP NAG7_OR
NTA_DIDES_WMEMDI_OR:
MOV BX,IDI1
CMP BX,0FH
JA ERROR_OR
MOV CL, MEMORY1[BX]
MOV CH, MEMORY1[BX+1]
MOV BX,IDI1
MOV AX,FLAGS
PUSH AX
POPF
OR BX,CX
PUSHF
POP AX
MOV FLAGS,AX
MOV IDI1,BX
JMP NAG7_OR
NTADIDESW_RKM_YA_MEMRKM_YAERROR_OR_OR:
MOV AL,[SI+6]
CMP AL,91
JZ MEMRKM_ORERROR_OR_DIDES_OR

RKM_ORERROR_OR_DIDES_OR:
MOV AL,[SI+7]
CMP AL,108
JZ ERROR_OR
CMP AL,104
JZ ERROR_OR
RET_NUM_COUNT  COMp+6
CMP CX,4
JA ERROR_OR
MOV BX,IDI1
MOV AX,FLAGS
PUSH AX
POPF 
OR BX,DX
PUSHF
POP AX
MOV FLAGS,AX
MOV IDI1,BX
JMP NAG7_OR
MEMRKM_ORERROR_OR_DIDES_OR:
MOV AL,[SI+8]
CMP AL,108
JZ ERROR_OR
CMP AL,104
JZ ERROR_OR
RET_NUM_COUNT  COMp+7
CMP CX,4
JA ERROR_OR
CMP DX,0FH
JA ERROR_OR
MOV BX,DX
MOV CH, MEMORY1[BX+1]
MOV CL, MEMORY1[BX]
MOV BX,IDI1
MOV AX,FLAGS
PUSH AX
POPF 
OR BX,CX
PUSHF
POP AX
MOV FLAGS,AX
MOV IDI1,BX
JMP NAG7_OR
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTAALDES_OR:
  MOV BX,0
  MOV BL,[SI+7]
  CMP BL,104
  JNAE NTAALDESW_RKM_YA_MEMRKM_YAERROR_OR_OR
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+6] 
  MOV CL,[SI+7]
  ADD AX,CX
  CMP AX,217
  JZ ERROR_OR
  CMP AX,218 
  JZ ERROR_OR
  CMP AX,219
  JZ ERROR_OR
  CMP AX,220
 JZ ERROR_OR
 CMP AX,205
 JZ NTAALDESWDIORALLWDIERROR_OR_OR
 CMP AX,201
 JZ NTAALDESWAHSOURCE_OR
 CMP AX,206
 JZ NTAALDESWBLSOURCE_OR
 CMP AX,202
 JZ NTAALDESWBHSOURCE_OR 
 CMP AX,207
 JZ NTAALDESWCLSOURCE_OR
 CMP AX,203
 JZ NTAALDESWCHSOURCE_OR
 CMP AX,208
 JZ NTAALDESWDLSOURCE_OR
 CMP AX,204
 JZ NTAALDESWDHSOURCE_OR 
 MOV BX,0
 MOV CX,0                              ;TAKECARE
 MOV CL,[SI+8]
 MOV BL,[SI+9]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ ALDES_WNTA_MEMBX_OR
 CMP AX,404              
 JZ NTA_ALDES_WMEMSI_OR
 CMP AX,389
 JZ NTA_ALDES_WMEMDI_OR
 JMP NTAALDESW_RKM_YA_MEMRKM_YAERROR_OR_OR  
 
 
  
NTAALDESWDIORALLWDIERROR_OR_OR: 
MOV AL,[SI+6]
CMP AL,100 
JZ ERROR_OR
NTAALDESWALSOURCE_OR:
MOV BL,BYTE PTR RAX1
MOV CX,RAX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CL,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RAX1,CX
JMP NAG7_OR
ALDES_WNTA_MEMBX_OR:
MOV BX,RBX1
CMP BX,0FH
JA ERROR_OR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX
POPF
OR BL,CL
PUSHF
POP AX
MOV FLAGS,AX
MOV RAX1,BX
JMP NAG7_OR
NTA_ALDES_WMEMSI_OR:
MOV BX,ISI1
CMP BX,0FH
JA ERROR_OR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX
POPF
OR BL,CL
PUSHF
POP AX
MOV FLAGS,AX
MOV RAX1,BX
JMP NAG7_OR
NTA_ALDES_WMEMDI_OR:
MOV BX,IDI1
CMP BX,0FH
JA ERROR_OR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX
POPF
OR BL,CL
PUSHF
POP AX
MOV FLAGS,AX
MOV RAX1,BX
JMP NAG7_OR
NTAALDESWAHSOURCE_OR:
MOV BL,BYTE PTR RAX1+1
MOV DX,RAX1
MOV AX,FLAGS
PUSH AX
POPF 
OR DL,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RAX1,DX
JMP NAG7_OR
NTAALDESWBLSOURCE_OR:
MOV BL,BYTE PTR RBX1
MOV DX,RAX1
MOV AX,FLAGS
PUSH AX
POPF 
OR DL,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RAX1,DX
JMP NAG7_OR
NTAALDESWBHSOURCE_OR:
MOV BL,BYTE PTR RBX1+1
MOV DX,RAX1
MOV AX,FLAGS
PUSH AX
POPF 
OR DL,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RAX1,DX
JMP NAG7_OR
NTAALDESWCLSOURCE_OR:
MOV BL,BYTE PTR RCX1
MOV DX,RAX1
MOV AX,FLAGS
PUSH AX
POPF 
OR DL,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RAX1,DX
JMP NAG7_OR
NTAALDESWCHSOURCE_OR:
MOV BL,BYTE PTR RCX1+1
MOV DX,RAX1
MOV AX,FLAGS
PUSH AX
POPF 
OR DL,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RAX1,DX
JMP NAG7_OR
NTAALDESWDLSOURCE_OR:
MOV BL,BYTE PTR RDX1
MOV DX,RAX1
MOV AX,FLAGS
PUSH AX
POPF 
OR DL,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RAX1,DX
JMP NAG7_OR
NTAALDESWDHSOURCE_OR:
MOV BL,BYTE PTR RDX1+1
MOV DX,RAX1
MOV AX,FLAGS
PUSH AX
POPF 
OR DL,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RAX1,DX
JMP NAG7_OR
NTAALDESW_RKM_YA_MEMRKM_YAERROR_OR_OR:
MOV AL,[SI+6]
CMP AL,91
JZ MEMRKM_ORERROR_OR_ALDES_OR

RKM_ORERROR_OR_ALDES_OR:
MOV AL,[SI+7]
RET_NUM_COUNT  COMp+6
CMP CX,2
JA ERROR_OR
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX
POPF 
OR BL,DL
PUSHF
POP AX
MOV FLAGS,AX
MOV RAX1,BX
JMP NAG7_OR
MEMRKM_ORERROR_OR_ALDES_OR:
MOV AL,[SI+8]
CMP AL,108
JZ ERROR_OR
CMP AL,104
JZ ERROR_OR
RET_NUM_COUNT  COMp+7
CMP CX,4
JA ERROR_OR
CMP DX,0FH
JA ERROR_OR
MOV BX,DX
MOV CX,0
MOV CL,MEMORY1[BX]
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX
POPF 
OR BL,CL
PUSHF
POP AX
MOV FLAGS,AX
MOV RAX1,BX
JMP NAG7_OR
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTABLDIS_OR:
  MOV BX,0
  MOV BL,[SI+7]
  CMP BL,104
  JNAE NTABLDESW_RKM_YA_MEMRKM_YAERROR_OR_OR
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+6] 
  MOV CL,[SI+7]
  ADD AX,CX
  CMP AX,217
  JZ ERROR_OR
  CMP AX,218 
  JZ ERROR_OR
  CMP AX,219
  JZ ERROR_OR
  CMP AX,220
 JZ ERROR_OR
 CMP AX,205
 JZ NTABLDESWDIORALLWDIERROR_OR_OR
 CMP AX,201
 JZ NTABLDESWAHSOURCE_OR
 CMP AX,206
 JZ NTABLDESWBLSOURCE_OR
 CMP AX,202
 JZ NTABLDESWBHSOURCE_OR 
 CMP AX,207
 JZ NTABLDESWCLSOURCE_OR
 CMP AX,203
 JZ NTABLDESWCHSOURCE_OR
 CMP AX,208
 JZ NTABLDESWDLSOURCE_OR
 CMP AX,204
 JZ NTABLDESWDHSOURCE_OR 
 MOV BX,0
 MOV CX,0                              ;TAKECARE
 MOV CL,[SI+8]
 MOV BL,[SI+9]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ BLDES_WNTA_MEMBX_OR
 CMP AX,404              
 JZ NTA_BLDES_WMEMSI_OR
 CMP AX,389
 JZ NTA_BLDES_WMEMDI_OR
 JMP NTABLDESW_RKM_YA_MEMRKM_YAERROR_OR_OR  
 
 
  
NTABLDESWDIORALLWDIERROR_OR_OR: 
MOV AL,[SI+6]
CMP AL,100 
JZ ERROR_OR
NTABLDESWALSOURCE_OR:
MOV BL,BYTE PTR RAX1
MOV DX,RBX1
MOV AX,FLAGS
PUSH AX
POPF 
OR DL,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RBX1,DX
JMP NAG7_OR
BLDES_WNTA_MEMBX_OR:
MOV BX,RBX1
CMP BX,0FH
JA ERROR_OR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RBX1
MOV AX,FLAGS
PUSH AX
POPF
OR BL,CL
PUSHF
POP AX
MOV FLAGS,AX
MOV RBX1,BX
JMP NAG7_OR
NTA_BLDES_WMEMSI_OR:
MOV BX,ISI1
CMP BX,0FH
JA ERROR_OR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RBX1
MOV AX,FLAGS
PUSH AX
POPF
OR BL,CL
PUSHF
POP AX
MOV FLAGS,AX
MOV RBX1,BX
JMP NAG7_OR
NTA_BLDES_WMEMDI_OR:
MOV BX,IDI1
CMP BX,0FH
JA ERROR_OR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RBX1
MOV AX,FLAGS
PUSH AX
POPF
OR BL,CL
PUSHF
POP AX
MOV FLAGS,AX
MOV RBX1,BX
JMP NAG7_OR
NTABLDESWAHSOURCE_OR:
MOV BL,BYTE PTR RAX1+1
MOV DX,RBX1
MOV AX,FLAGS
PUSH AX
POPF 
OR DL,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RBX1,DX
JMP NAG7_OR
NTABLDESWBLSOURCE_OR:
MOV BL,BYTE PTR RBX1
MOV CX,RBX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CL,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RBX1,CX
JMP NAG7_OR
NTABLDESWBHSOURCE_OR:
MOV BL,BYTE PTR RBX1+1
MOV DX,RBX1
MOV AX,FLAGS
PUSH AX
POPF 
OR DL,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RBX1,DX
JMP NAG7_OR
NTABLDESWCLSOURCE_OR:
MOV BL,BYTE PTR RCX1
MOV DX,RBX1
MOV AX,FLAGS
PUSH AX
POPF 
OR DL,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RBX1,DX
JMP NAG7_OR
NTABLDESWCHSOURCE_OR:
MOV BL,BYTE PTR RCX1+1
MOV DX,RBX1
MOV AX,FLAGS
PUSH AX
POPF 
OR DL,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RBX1,DX
JMP NAG7_OR
NTABLDESWDLSOURCE_OR:
MOV BL,BYTE PTR RDX1
MOV DX,RBX1
MOV AX,FLAGS
PUSH AX
POPF 
OR DL,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RBX1,DX
JMP NAG7_OR
NTABLDESWDHSOURCE_OR:
MOV BL,BYTE PTR RDX1+1
MOV DX,RBX1
MOV AX,FLAGS
PUSH AX
POPF 
OR DL,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RBX1,DX
JMP NAG7_OR
NTABLDESW_RKM_YA_MEMRKM_YAERROR_OR_OR:
MOV AL,[SI+6]
CMP AL,91
JZ MEMRKM_ORERROR_OR_BLDES_OR

RKM_ORERROR_OR_BLDES_OR:
MOV AL,[SI+7]
RET_NUM_COUNT  COMp+6
CMP CX,2
JA ERROR_OR
MOV BX,RBX1
MOV AX,FLAGS
PUSH AX
POPF 
OR BL,DL
PUSHF
POP AX
MOV FLAGS,AX
MOV RBX1,BX
JMP NAG7_OR
MEMRKM_ORERROR_OR_BLDES_OR:
MOV AL,[SI+8]
CMP AL,108
JZ ERROR_OR
CMP AL,104
JZ ERROR_OR
RET_NUM_COUNT  COMp+7
CMP CX,4
JA ERROR_OR
CMP DX,0FH
JA ERROR_OR
MOV BX,DX
MOV CX,0
MOV CL,MEMORY1[BX]
MOV BX,RBX1
MOV AX,FLAGS
PUSH AX
POPF 
OR BL,CL
PUSHF
POP AX
MOV FLAGS,AX
MOV RBX1,BX
JMP NAG7_OR
;/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTACLDIS_OR:
  MOV BX,0
  MOV BL,[SI+7]
  CMP BL,104
  JNAE NTACLDESW_RKM_YA_MEMRKM_YAERROR_OR_OR           ;3DLCODEDYAMEGS
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+6] 
  MOV CL,[SI+7]
  ADD AX,CX
  CMP AX,217
  JZ ERROR_OR
  CMP AX,218 
  JZ ERROR_OR
  CMP AX,219
  JZ ERROR_OR
  CMP AX,220
 JZ ERROR_OR
 CMP AX,205
 JZ NTACLDESWDIORALLWDIERROR_OR_OR
 CMP AX,201
 JZ NTACLDESWAHSOURCE_OR
 CMP AX,206
 JZ NTACLDESWBLSOURCE_OR
 CMP AX,202
 JZ NTACLDESWBHSOURCE_OR 
 CMP AX,207
 JZ NTACLDESWCLSOURCE_OR
 CMP AX,203
 JZ NTACLDESWCHSOURCE_OR
 CMP AX,208
 JZ NTACLDESWDLSOURCE_OR
 CMP AX,204
 JZ NTACLDESWDHSOURCE_OR   
 MOV BX,0
 MOV CX,0                              ;TAKECARE
 MOV CL,[SI+8]
 MOV BL,[SI+9]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ CLDES_WNTA_MEMBX_OR
 CMP AX,404              
 JZ NTA_CLDES_WMEMSI_OR
 CMP AX,389
 JZ NTA_CLDES_WMEMDI_OR
  
 JMP NTACLDESW_RKM_YA_MEMRKM_YAERROR_OR_OR  
 
 
  
NTACLDESWDIORALLWDIERROR_OR_OR: 
MOV AL,[SI+6]
CMP AL,100 
JZ ERROR_OR
NTACLDESWALSOURCE_OR:
MOV BL,BYTE PTR RAX1
MOV DX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
OR DL,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RCX1,DX
JMP NAG7_OR
CLDES_WNTA_MEMBX_OR:
MOV BX,RBX1
CMP BX,0FH
JA ERROR_OR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RCX1
MOV AX,FLAGS
PUSH AX
POPF
OR BL,CL
PUSHF
POP AX
MOV FLAGS,AX
MOV RCX1,BX
JMP NAG7_OR
NTA_CLDES_WMEMSI_OR:
MOV BX,ISI1
CMP BX,0FH
JA ERROR_OR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RCX1
MOV AX,FLAGS
PUSH AX
POPF
OR BL,CL
PUSHF
POP AX
MOV FLAGS,AX
MOV RCX1,BX
JMP NAG7_OR
NTA_CLDES_WMEMDI_OR:
MOV BX,IDI1
CMP BX,0FH
JA ERROR_OR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RCX1
MOV AX,FLAGS
PUSH AX
POPF
OR BL,CL
PUSHF
POP AX
MOV FLAGS,AX
MOV RCX1,BX
JMP NAG7_OR
NTACLDESWAHSOURCE_OR:
MOV BL,BYTE PTR RAX1+1
MOV DX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
OR DL,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RCX1,DX
JMP NAG7_OR
NTACLDESWBLSOURCE_OR:
MOV BL,BYTE PTR RBX1
MOV DX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
OR DL,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RCX1,DX
JMP NAG7_OR

NTACLDESWBHSOURCE_OR:
MOV BL,BYTE PTR RBX1+1
MOV DX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
OR DL,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RCX1,DX
JMP NAG7_OR
NTACLDESWCLSOURCE_OR:
MOV BL,BYTE PTR RCX1
MOV CX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CL,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RCX1,CX
JMP NAG7_OR
NTACLDESWCHSOURCE_OR:
MOV BL,BYTE PTR RCX1+1
MOV DX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
OR DL,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RCX1,DX
JMP NAG7_OR
NTACLDESWDLSOURCE_OR:
MOV BL,BYTE PTR RDX1
MOV DX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
OR DL,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RCX1,DX
JMP NAG7_OR
NTACLDESWDHSOURCE_OR:
MOV BL,BYTE PTR RDX1+1
MOV DX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
OR DL,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RCX1,DX
JMP NAG7_OR
NTACLDESW_RKM_YA_MEMRKM_YAERROR_OR_OR:
MOV AL,[SI+6]
CMP AL,91
JZ MEMRKM_ORERROR_OR_CLDES_OR

RKM_ORERROR_OR_CLDES_OR:
MOV AL,[SI+7]
RET_NUM_COUNT  COMp+6
CMP CX,2
JA ERROR_OR
MOV BX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
OR BL,DL
PUSHF
POP AX
MOV FLAGS,AX
MOV RCX1,BX
JMP NAG7_OR
MEMRKM_ORERROR_OR_CLDES_OR:
MOV AL,[SI+8]
CMP AL,108
JZ ERROR_OR
CMP AL,104
JZ ERROR_OR
RET_NUM_COUNT  COMp+7
CMP CX,4
JA ERROR_OR
CMP DX,0FH
JA ERROR_OR
MOV BX,DX
MOV CX,0
MOV CL,MEMORY1[BX]
MOV BX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
OR BL,CL
PUSHF
POP AX
MOV FLAGS,AX
MOV RCX1,BX
JMP NAG7_OR
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTADLDIS_OR:
  MOV BX,0
  MOV BL,[SI+7]
  CMP BL,104
  JNAE NTADLDESW_RKM_YA_MEMRKM_YAERROR_OR_OR           ;3DLCODEDYAMEGS
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+6] 
  MOV CL,[SI+7]
  ADD AX,CX
  CMP AX,217
  JZ ERROR_OR
  CMP AX,218 
  JZ ERROR_OR
  CMP AX,219
  JZ ERROR_OR
  CMP AX,220
 JZ ERROR_OR
 CMP AX,205
 JZ NTADLDESWDIORALLWDIERROR_OR_OR
 CMP AX,201
 JZ NTADLDESWAHSOURCE_OR
 CMP AX,206
 JZ NTADLDESWBLSOURCE_OR
 CMP AX,202
 JZ NTADLDESWBHSOURCE_OR 
 CMP AX,207
 JZ NTADLDESWCLSOURCE_OR
 CMP AX,203
 JZ NTADLDESWCHSOURCE_OR
 CMP AX,208
 JZ NTADLDESWDLSOURCE_OR
 CMP AX,204
 JZ NTADLDESWDHSOURCE_OR    
 MOV BX,0
 MOV CX,0                              ;TAKECARE
 MOV CL,[SI+8]
 MOV BL,[SI+9]
 ADD CX,BX
 ADD AX,CX


 CMP AX,402
 JZ DLDES_WNTA_MEMBX_OR
 CMP AX,404              
 JZ NTA_DLDES_WMEMSI_OR
 CMP AX,389
 JZ NTA_DLDES_WMEMDI_OR
 
 JMP NTADLDESW_RKM_YA_MEMRKM_YAERROR_OR_OR  
 
 
  
NTADLDESWDIORALLWDIERROR_OR_OR: 
MOV AL,[SI+6]
CMP AL,100 
JZ ERROR_OR
NTADLDESWALSOURCE_OR:
MOV BL,BYTE PTR RAX1
MOV DX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
OR DL,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RDX1,DX
JMP NAG7_OR
DLDES_WNTA_MEMBX_OR:
MOV BX,RBX1
CMP BX,0FH
JA ERROR_OR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RDX1
MOV AX,FLAGS
PUSH AX
POPF
OR BL,CL
PUSHF
POP AX
MOV FLAGS,AX
MOV RDX1,BX
JMP NAG7_OR
NTA_DLDES_WMEMSI_OR:
MOV BX,ISI1
CMP BX,0FH
JA ERROR_OR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RDX1
MOV AX,FLAGS
PUSH AX
POPF
OR BL,CL
PUSHF
POP AX
MOV FLAGS,AX
MOV RDX1,BX
JMP NAG7_OR
NTA_DLDES_WMEMDI_OR:
MOV BX,IDI1
CMP BX,0FH
JA ERROR_OR
MOV CX,0
MOV CL,MEMORY1[BX] 
MOV BX,RDX1
MOV AX,FLAGS
PUSH AX
POPF
OR BL,CL
PUSHF
POP AX
MOV FLAGS,AX
MOV RDX1,BX
JMP NAG7_OR
NTADLDESWAHSOURCE_OR:
MOV BL,BYTE PTR RAX1+1
MOV DX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
OR DL,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RDX1,DX
JMP NAG7_OR
NTADLDESWBLSOURCE_OR:
MOV BL,BYTE PTR RBX1
MOV DX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
OR DL,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RDX1,DX
JMP NAG7_OR

NTADLDESWBHSOURCE_OR:
MOV BL,BYTE PTR RBX1+1
MOV DX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
OR DL,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RDX1,DX
JMP NAG7_OR
NTADLDESWCLSOURCE_OR:
MOV BL,BYTE PTR RCX1
MOV DX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
OR DL,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RDX1,DX
JMP NAG7_OR
NTADLDESWCHSOURCE_OR:
MOV BL,BYTE PTR RCX1+1
MOV DX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
OR DL,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RDX1,DX
JMP NAG7_OR
NTADLDESWDLSOURCE_OR:
MOV BL,BYTE PTR RDX1
MOV CX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CL,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RDX1,CX
JMP NAG7_OR
NTADLDESWDHSOURCE_OR:
MOV BL,BYTE PTR RDX1+1
MOV DX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
OR DL,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RDX1,DX
JMP NAG7_OR
NTADLDESW_RKM_YA_MEMRKM_YAERROR_OR_OR:
MOV AL,[SI+6]
CMP AL,91
JZ MEMRKM_ORERROR_OR_DLDES_OR

RKM_ORERROR_OR_DLDES_OR:
MOV AL,[SI+7]
RET_NUM_COUNT  COMp+6
CMP CX,2
JA ERROR_OR
MOV BX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
OR BL,DL
PUSHF
POP AX
MOV FLAGS,AX
MOV RDX1,BX
JMP NAG7_OR
MEMRKM_ORERROR_OR_DLDES_OR:
MOV AL,[SI+8]
CMP AL,108
JZ ERROR_OR
CMP AL,104
JZ ERROR_OR
RET_NUM_COUNT  COMp+7
CMP CX,4
JA ERROR_OR
CMP DX,0FH
JA ERROR_OR
MOV BX,DX
MOV CX,0
MOV CL,MEMORY1[BX]
MOV BX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
OR BL,CL
PUSHF
POP AX
MOV FLAGS,AX
MOV RDX1,BX
JMP NAG7_OR
;/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTAAHDIS_OR:
  MOV BX,0
  MOV BL,[SI+7]
  CMP BL,104
  JNAE NTAAHDESW_RKM_YA_MEMRKM_YAERROR_OR_OR
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+6] 
  MOV CL,[SI+7]
  ADD AX,CX
  CMP AX,217
  JZ ERROR_OR
  CMP AX,218 
  JZ ERROR_OR
  CMP AX,219
  JZ ERROR_OR
  CMP AX,220
 JZ ERROR_OR
 CMP AX,205
 JZ NTAAHDESWDIORALLWDIERROR_OR_OR
 CMP AX,201
 JZ NTAAHDESWAHSOURCE_OR
 CMP AX,206
 JZ NTAAHDESWBLSOURCE_OR
 CMP AX,202
 JZ NTAAHDESWBHSOURCE_OR 
 CMP AX,207
 JZ NTAAHDESWCLSOURCE_OR
 CMP AX,203
 JZ NTAAHDESWCHSOURCE_OR
 CMP AX,208
 JZ NTAAHDESWDLSOURCE_OR
 CMP AX,204
 JZ NTAAHDESWDHSOURCE_OR   
 MOV BX,0
 MOV CX,0                              ;TAKECARE
 MOV CL,[SI+8]
 MOV BL,[SI+9]
 ADD CX,BX
 ADD AX,CX

 CMP AX,402
 JZ AHDES_WNTA_MEMBX_OR
 CMP AX,404              
 JZ NTA_AHDES_WMEMSI_OR
 CMP AX,389
 JZ NTA_AHDES_WMEMDI_OR
  
 JMP NTAAHDESW_RKM_YA_MEMRKM_YAERROR_OR_OR  
 
 
  
NTAAHDESWDIORALLWDIERROR_OR_OR: 
MOV AL,[SI+6]
CMP AL,100 
JZ ERROR_OR
NTAAHDESWALSOURCE_OR:
MOV BL,BYTE PTR RAX1
MOV CX,RAX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CH,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RAX1,CX
JMP NAG7_OR
AHDES_WNTA_MEMBX_OR:
MOV BX,RBX1
CMP BX,0FH
JA ERROR_OR
MOV CX,0
MOV CH,MEMORY1[BX] 
MOV BX,RAX1 
MOV AX,FLAGS
PUSH AX
POPF 
OR BH,CH
PUSHF
POP AX
MOV RAX1,BX
JMP NAG7_OR
NTA_AHDES_WMEMSI_OR:
MOV BX,ISI1
CMP BX,0FH
JA ERROR_OR
MOV CX,0
MOV CH,MEMORY1[BX] 
MOV BX,RAX1 
MOV AX,FLAGS
PUSH AX
POPF 
OR BH,CH
PUSHF
POP AX
MOV RAX1,BX
JMP NAG7_OR
NTA_AHDES_WMEMDI_OR:
MOV BX,IDI1
CMP BX,0FH
JA ERROR_OR
MOV CX,0
MOV CH,MEMORY1[BX] 
MOV BX,RAX1 
MOV AX,FLAGS
PUSH AX
POPF 
OR BH,CH
PUSHF
POP AX
MOV RAX1,BX
JMP NAG7_OR
NTAAHDESWAHSOURCE_OR:
MOV BL,BYTE PTR RAX1+1
MOV CX,RAX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CH,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RAX1,CX
JMP NAG7_OR
NTAAHDESWBLSOURCE_OR:
MOV BL,BYTE PTR RBX1
MOV CX,RAX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CH,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RAX1,CX
JMP NAG7_OR
NTAAHDESWBHSOURCE_OR:
MOV BL,BYTE PTR RBX1+1
MOV CX,RAX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CH,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RAX1,CX
JMP NAG7_OR
NTAAHDESWCLSOURCE_OR:
MOV BL,BYTE PTR RCX1
MOV CX,RAX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CH,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RAX1,CX
JMP NAG7_OR
NTAAHDESWCHSOURCE_OR:
MOV BL,BYTE PTR RCX1+1
MOV CX,RAX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CH,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RAX1,CX
JMP NAG7_OR
NTAAHDESWDLSOURCE_OR:
MOV BL,BYTE PTR RDX1
MOV CX,RAX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CH,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RAX1,CX
JMP NAG7_OR
NTAAHDESWDHSOURCE_OR:
MOV BL,BYTE PTR RDX1+1
MOV CX,RAX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CH,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RAX1,CX
JMP NAG7_OR
NTAAHDESW_RKM_YA_MEMRKM_YAERROR_OR_OR:
MOV AL,[SI+6]
CMP AL,91
JZ MEMRKM_ORERROR_OR_AHDES_OR

RKM_ORERROR_OR_AHDES_OR:
MOV AL,[SI+7]
RET_NUM_COUNT  COMp+6
CMP CX,2
JA ERROR_OR
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX
POPF  
OR BH,DL
POP AX
MOV FLAGS,AX
MOV RAX1,BX
JMP NAG7_OR
MEMRKM_ORERROR_OR_AHDES_OR:
MOV AL,[SI+8]
CMP AL,108
JZ ERROR_OR
CMP AL,104
JZ ERROR_OR
RET_NUM_COUNT  COMp+7
CMP CX,4
JA ERROR_OR
CMP DX,0FH
JA ERROR_OR
MOV BX,DX
MOV CX,0
MOV CH,MEMORY1[BX]
MOV BX,RAX1
MOV AX,FLAGS
PUSH AX
POPF 
OR BH,CH
POP AX
MOV FLAGS,AX
MOV RAX1,BX
JMP NAG7_OR
 ;///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTABHDIS_OR:
  MOV BX,0
  MOV BL,[SI+7]
  CMP BL,104
  JNAE NTABHDESW_RKM_YA_MEMRKM_YAERROR_OR_OR
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+6] 
  MOV CL,[SI+7]
  ADD AX,CX
  CMP AX,217
  JZ ERROR_OR
  CMP AX,218 
  JZ ERROR_OR
  CMP AX,219
  JZ ERROR_OR
  CMP AX,220
 JZ ERROR_OR
 CMP AX,205
 JZ NTABHDESWDIORALLWDIERROR_OR_OR
 CMP AX,201
 JZ NTABHDESWAHSOURCE_OR
 CMP AX,206
 JZ NTABHDESWBLSOURCE_OR
 CMP AX,202
 JZ NTABHDESWBHSOURCE_OR 
 CMP AX,207
 JZ NTABHDESWCLSOURCE_OR
 CMP AX,203
 JZ NTABHDESWCHSOURCE_OR
 CMP AX,208
 JZ NTABHDESWDLSOURCE_OR
 CMP AX,204
 JZ NTABHDESWDHSOURCE_OR   
 MOV BX,0
 MOV CX,0                              ;TAKECARE
 MOV CL,[SI+8]
 MOV BL,[SI+9]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ BHDES_WNTA_MEMBX_OR
 CMP AX,404              
 JZ NTA_BHDES_WMEMSI_OR
 CMP AX,389
 JZ NTA_BHDES_WMEMDI_OR
  
 JMP NTABHDESW_RKM_YA_MEMRKM_YAERROR_OR_OR  
 
 
  
NTABHDESWDIORALLWDIERROR_OR_OR: 
MOV AL,[SI+6]
CMP AL,100 
JZ ERROR_OR
NTABHDESWALSOURCE_OR:
MOV BL,BYTE PTR RAX1
MOV CX,RBX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CH,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RBX1,CX
JMP NAG7_OR
BHDES_WNTA_MEMBX_OR:
MOV BX,RBX1
CMP BX,0FH
JA ERROR_OR
MOV CX,0
MOV CH,MEMORY1[BX] 
MOV BX,RBX1 
MOV AX,FLAGS
PUSH AX
POPF 
OR BH,CH
PUSHF
POP AX
MOV RBX1,BX
JMP NAG7_OR
NTA_BHDES_WMEMSI_OR:
MOV BX,ISI1
CMP BX,0FH
JA ERROR_OR
MOV CX,0
MOV CH,MEMORY1[BX] 
MOV BX,RBX1 
MOV AX,FLAGS
PUSH AX
POPF 
OR BH,CH
PUSHF
POP AX
MOV RBX1,BX
JMP NAG7_OR
NTA_BHDES_WMEMDI_OR:
MOV BX,IDI1
CMP BX,0FH
JA ERROR_OR
MOV CX,0
MOV CH,MEMORY1[BX] 
MOV BX,RBX1 
MOV AX,FLAGS
PUSH AX
POPF 
OR BH,CH
PUSHF
POP AX
MOV RBX1,BX
NTABHDESWAHSOURCE_OR:
MOV BL,BYTE PTR RAX1+1
MOV CX,RBX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CH,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RBX1,CX
JMP NAG7_OR
NTABHDESWBLSOURCE_OR:
MOV BL,BYTE PTR RBX1
MOV CX,RBX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CH,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RBX1,CX
JMP NAG7_OR
NTABHDESWBHSOURCE_OR: 
MOV BL,BYTE PTR RBX1+1
MOV CX,RBX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CH,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RBX1,CX
JMP NAG7_OR
NTABHDESWCLSOURCE_OR:
MOV BL,BYTE PTR RCX1
MOV CX,RBX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CH,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RBX1,CX
JMP NAG7_OR
NTABHDESWCHSOURCE_OR:
MOV BL,BYTE PTR RCX1+1
MOV CX,RBX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CH,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RBX1,CX
JMP NAG7_OR
NTABHDESWDLSOURCE_OR:
MOV BL,BYTE PTR RDX1
MOV CX,RBX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CH,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RBX1,CX
JMP NAG7_OR
NTABHDESWDHSOURCE_OR:
MOV BL,BYTE PTR RDX1+1
MOV CX,RBX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CH,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RBX1,CX
JMP NAG7_OR
NTABHDESW_RKM_YA_MEMRKM_YAERROR_OR_OR:
MOV AL,[SI+6]
CMP AL,91
JZ MEMRKM_ORERROR_OR_BHDES_OR

RKM_ORERROR_OR_BHDES_OR:
MOV AL,[SI+7]
RET_NUM_COUNT  COMp+6
CMP CX,2
JA ERROR_OR
MOV BX,RBX1
MOV AX,FLAGS
PUSH AX
POPF  
OR BH,DL
POP AX
MOV FLAGS,AX
MOV RBX1,BX
JMP NAG7_OR
MEMRKM_ORERROR_OR_BHDES_OR:
MOV AL,[SI+8]
CMP AL,108
JZ ERROR_OR
CMP AL,104
JZ ERROR_OR
RET_NUM_COUNT  COMp+7
CMP CX,4
JA ERROR_OR
CMP DX,0FH
JA ERROR_OR
MOV BX,DX
MOV CX,0
MOV CH,MEMORY1[BX]
MOV BX,RBX1
MOV AX,FLAGS
PUSH AX
POPF 
OR BH,CH
POP AX
MOV FLAGS,AX
MOV RBX1,BX
JMP NAG7_OR
;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTACHDIS_OR:
  MOV BX,0
  MOV BL,[SI+7]
  CMP BL,104
  JNAE NTACHDESW_RKM_YA_MEMRKM_YAERROR_OR_OR
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+6] 
  MOV CL,[SI+7]
  ADD AX,CX
  CMP AX,217
  JZ ERROR_OR
  CMP AX,218 
  JZ ERROR_OR
  CMP AX,219
  JZ ERROR_OR
  CMP AX,220
 JZ ERROR_OR
 CMP AX,205
 JZ NTACHDESWDIORALLWDIERROR_OR_OR
 CMP AX,201
 JZ NTACHDESWAHSOURCE_OR
 CMP AX,206
 JZ NTACHDESWBLSOURCE_OR
 CMP AX,202
 JZ NTACHDESWBHSOURCE_OR 
 CMP AX,207
 JZ NTACHDESWCLSOURCE_OR
 CMP AX,203
 JZ NTACHDESWCHSOURCE_OR
 CMP AX,208
 JZ NTACHDESWDLSOURCE_OR
 CMP AX,204
 JZ NTACHDESWDHSOURCE_OR
 MOV BX,0
 MOV CX,0                              ;TAKECARE
 MOV CL,[SI+8]
 MOV BL,[SI+9]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ CHDES_WNTA_MEMBX_OR
 CMP AX,404              
 JZ NTA_CHDES_WMEMSI_OR
 CMP AX,389
 JZ NTA_CHDES_WMEMDI_OR
  
 JMP NTACHDESW_RKM_YA_MEMRKM_YAERROR_OR_OR  
 
 
  
NTACHDESWDIORALLWDIERROR_OR_OR: 
MOV AL,[SI+6]
CMP AL,100 
JZ ERROR_OR
NTACHDESWALSOURCE_OR:
MOV BL,BYTE PTR RAX1
MOV CX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CH,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RCX1,CX
JMP NAG7_OR
CHDES_WNTA_MEMBX_OR:
MOV BX,RBX1
CMP BX,0FH
JA ERROR_OR
MOV CX,0
MOV CH,MEMORY1[BX] 
MOV BX,RCX1 
MOV AX,FLAGS
PUSH AX
POPF 
OR BH,CH
PUSHF
POP AX
MOV RCX1,BX
JMP NAG7_OR
NTA_CHDES_WMEMSI_OR:
MOV BX,ISI1
CMP BX,0FH
JA ERROR_OR
MOV CX,0
MOV CH,MEMORY1[BX] 
MOV BX,RCX1 
MOV AX,FLAGS
PUSH AX
POPF 
OR BH,CH
PUSHF
POP AX
MOV RCX1,BX
JMP NAG7_OR
NTA_CHDES_WMEMDI_OR:
MOV BX,IDI1
CMP BX,0FH
JA ERROR_OR
MOV CX,0
MOV CH,MEMORY1[BX] 
MOV BX,RCX1 
MOV AX,FLAGS
PUSH AX
POPF 
OR BH,CH
PUSHF
POP AX
MOV RCX1,BX
JMP NAG7_OR
NTACHDESWAHSOURCE_OR:
MOV BL,BYTE PTR RAX1+1
MOV CX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CH,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RCX1,CX
JMP NAG7_OR
NTACHDESWBLSOURCE_OR:
MOV BL,BYTE PTR RBX1
MOV CX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CH,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RCX1,CX
JMP NAG7_OR
NTACHDESWBHSOURCE_OR:
MOV BL,BYTE PTR RBX1+1
MOV CX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CH,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RCX1,CX
JMP NAG7_OR
NTACHDESWCLSOURCE_OR:
MOV BL,BYTE PTR RCX1
MOV CX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CH,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RCX1,CX
JMP NAG7_OR
NTACHDESWCHSOURCE_OR:
MOV BL,BYTE PTR RCX1+1
MOV CX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CH,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RCX1,CX
JMP NAG7_OR
NTACHDESWDLSOURCE_OR:
MOV BL,BYTE PTR RDX1
MOV CX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CH,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RCX1,CX
JMP NAG7_OR
NTACHDESWDHSOURCE_OR:
MOV BL,BYTE PTR RDX1+1
MOV CX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CH,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RCX1,CX
JMP NAG7_OR
NTACHDESW_RKM_YA_MEMRKM_YAERROR_OR_OR:
MOV AL,[SI+6]
CMP AL,91
JZ MEMRKM_ORERROR_OR_CHDES_OR

RKM_ORERROR_OR_CHDES_OR:
MOV AL,[SI+7]
RET_NUM_COUNT  COMp+6
CMP CX,2
JA ERROR_OR
MOV BX,RCX1
MOV AX,FLAGS
PUSH AX
POPF  
OR BH,DL
POP AX
MOV FLAGS,AX
MOV RCX1,BX
JMP NAG7_OR
MEMRKM_ORERROR_OR_CHDES_OR:
MOV AL,[SI+8]
CMP AL,108
JZ ERROR_OR
CMP AL,104
JZ ERROR_OR

RET_NUM_COUNT  COMp+7
CMP CX,4
JA ERROR_OR
CMP DX,0FH
JA ERROR_OR
MOV BX,DX
MOV CX,0
MOV CH,MEMORY1[BX]
MOV BX,RCX1
MOV AX,FLAGS
PUSH AX
POPF 
OR BH,CH
POP AX
MOV FLAGS,AX
MOV RCX1,BX
JMP NAG7_OR
;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
NTADHDIS_OR:
  MOV BX,0
  MOV BL,[SI+7]
  CMP BL,104
  JNAE NTADHDESW_RKM_YA_MEMRKM_YAERROR_OR_OR
  MOV CX,0
  MOV AX,0
  MOV AL,[SI+6] 
  MOV CL,[SI+7]
  ADD AX,CX
  CMP AX,217
  JZ ERROR_OR
  CMP AX,218 
  JZ ERROR_OR
  CMP AX,219
  JZ ERROR_OR
  CMP AX,220
 JZ ERROR_OR
 CMP AX,205
 JZ NTADHDESWDIORALLWDIERROR_OR_OR
 CMP AX,201
 JZ NTADHDESWAHSOURCE_OR
 CMP AX,206
 JZ NTADHDESWBLSOURCE_OR
 CMP AX,202
 JZ NTADHDESWBHSOURCE_OR 
 CMP AX,207
 JZ NTADHDESWCLSOURCE_OR
 CMP AX,203
 JZ NTADHDESWCHSOURCE_OR
 CMP AX,208
 JZ NTADHDESWDLSOURCE_OR
 CMP AX,204
 JZ NTADHDESWDHSOURCE_OR
 MOV BX,0
 MOV CX,0                              ;TAKECARE
 MOV CL,[SI+8]
 MOV BL,[SI+9]
 ADD CX,BX
 ADD AX,CX
 CMP AX,402
 JZ DHDES_WNTA_MEMBX_OR
 CMP AX,404              
 JZ NTA_DHDES_WMEMSI_OR
 CMP AX,389
 JZ NTA_DHDES_WMEMDI_OR
  
 JMP NTADHDESW_RKM_YA_MEMRKM_YAERROR_OR_OR  
 
 
  
NTADHDESWDIORALLWDIERROR_OR_OR: 
MOV AL,[SI+6]
CMP AL,100 
JZ ERROR_OR
NTADHDESWALSOURCE_OR:
MOV BL,BYTE PTR RAX1
MOV CX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CH,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RDX1,CX
JMP NAG7_OR
DHDES_WNTA_MEMBX_OR:
MOV BX,RBX1
CMP BX,0FH
JA ERROR_OR
MOV CX,0
MOV CH,MEMORY1[BX] 
MOV BX,RDX1 
MOV AX,FLAGS
PUSH AX
POPF 
OR BH,CH
PUSHF
POP AX
MOV RDX1,BX
JMP NAG7_OR
NTA_DHDES_WMEMSI_OR:
MOV BX,ISI1
CMP BX,0FH
JA ERROR_OR
MOV CX,0
MOV CH,MEMORY1[BX] 
MOV BX,RDX1 
MOV AX,FLAGS
PUSH AX
POPF 
OR BH,CH
PUSHF
POP AX
MOV RDX1,BX
JMP NAG7_OR
NTA_DHDES_WMEMDI_OR:
MOV BX,IDI1
CMP BX,0FH
JA ERROR_OR
MOV CX,0
MOV CH,MEMORY1[BX] 
MOV BX,RDX1 
MOV AX,FLAGS
PUSH AX
POPF 
OR BH,CH
PUSHF
POP AX
MOV RDX1,BX
JMP NAG7_OR
NTADHDESWAHSOURCE_OR:
MOV BL,BYTE PTR RAX1+1
MOV CX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CH,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RDX1,CX
JMP NAG7_OR
NTADHDESWBLSOURCE_OR:
MOV BL,BYTE PTR RBX1
MOV CX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CH,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RDX1,CX
JMP NAG7_OR
NTADHDESWBHSOURCE_OR:
MOV BL,BYTE PTR RBX1+1
MOV CX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CH,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RDX1,CX
JMP NAG7_OR
NTADHDESWCLSOURCE_OR:
MOV BL,BYTE PTR RCX1
MOV CX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CH,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RDX1,CX
JMP NAG7_OR
NTADHDESWCHSOURCE_OR:
MOV BL,BYTE PTR RCX1+1
MOV CX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CH,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RDX1,CX
JMP NAG7_OR
NTADHDESWDLSOURCE_OR:
MOV BL,BYTE PTR RDX1
MOV CX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CH,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RDX1,CX
JMP NAG7_OR
NTADHDESWDHSOURCE_OR:
MOV BL,BYTE PTR RDX1+1
MOV CX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
OR CH,BL
PUSHF
POP AX
MOV FLAGS,AX
MOV RDX1,CX
JMP NAG7_OR
NTADHDESW_RKM_YA_MEMRKM_YAERROR_OR_OR:
MOV AL,[SI+6]
CMP AL,91
JZ MEMRKM_ORERROR_OR_DHDES_OR

RKM_ORERROR_OR_DHDES_OR:
MOV AL,[SI+7]
RET_NUM_COUNT  COMp+6
CMP CX,2
JA ERROR_OR
MOV BX,RDX1
MOV AX,FLAGS
PUSH AX
POPF  
OR BH,DL
POP AX
MOV FLAGS,AX
MOV RDX1,BX
JMP NAG7_OR
MEMRKM_ORERROR_OR_DHDES_OR:
MOV AL,[SI+8]
CMP AL,108
JZ ERROR_OR
CMP AL,104
JZ ERROR_OR
RET_NUM_COUNT  COMp+7
CMP CX,4
JA ERROR_OR
CMP DX,0FH
JA ERROR_OR
MOV BX,RDX1
MOV AX,FLAGS
PUSH AX
POPF 
OR BH,CH
POP AX
MOV FLAGS,AX
MOV RDX1,BX
JMP NAG7_OR
;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
ERROR_OR:

JMP OUTT_OR
NAG7_OR:
     
OUTT_OR:      
       ;AFTER INST:
       JMP NOT_COMMAND

   ;---------------------------------------------------------------------ILLIGAL COMM--------------------------------------------------------------
NOT_COMMAND:
     CMP PUP_INUSE,1
     JNZ MATNAALSH
     CMP ELDOOR,1
     JNZ MATNAALSH
     JMP OWN_P
     MATNAALSH:
     CMP PUP_INUSE,1
     JNZ COM_AADY
     CALL MIR_REG_P2
     COM_AADY:

     CMP GAMELEVEL,2
     JZ LEVEL_2
     
     CMP ELDOOR,1
     JZ DOOR_PLAYER2
     jmp JUMP_TO_NEXT_LOOP 

     LEVEL_2:
     CMP ELDOOR,1
     JZ D2_AN_L2
     jmp JUMP_TO_NEXT_LOOP 
    
     JUMP_TO_NEXT_LOOP: ;YEROO7 ELDOR ELY BA3DO AW LOOP ELY BA3DHA 3ALA 7ASAB EL PlAYER

;------------------------------------------------------------END COMMAND--------------------------------------------------------------------------------
         CMP PUP_INUSE,1
         JZ HENA
         OWN_P:
        ;RETURN TEMP VALUE TO PLAYER 1 REG THAT IS SELECTED BY 2
         CALL MIR_REG_P1
         ;BANAFEZ 3ALA PROC BETA3Y

         CMP PUP_INUSE,1
         JNZ HENA
         HENA:  
         CMP GAMELEVEL,2
         JZ LEVEL_22
          CMP ELDOOR,1
          JZ DOOR_PLAYER2
          JMP SKIP_PUP

          LEVEL_22:
         CMP ELDOOR,1
         JZ D2_AN_L2
         jmp SKIP_PUP 

  ;-----------------------------------------------------------------------------POWER UP------------------------------------------------------------------------------
         POWER_UP:
         CMP ELDOOR,2
         JZ MEES_P2_PUP
         DISPLAY_GRAPH PLAYER_USE_PUP_MESS,9,18,5
          CALL DELAYING
         DISPLAY_GRAPH CHOSSEN_PUP_MESS,11,18,5

         JMP FOKAK_MENP2
         MEES_P2_PUP:
         DISPLAY_GRAPH PLAYER_USE_PUP_MESS,9,18,26
          CALL DELAYING
         DISPLAY_GRAPH CHOSSEN_PUP_MESS,11,18,26
         FOKAK_MENP2:

         ;HASHOOF ANA ANHY POWER UP
         JMP CHECH_PUP
   
         CHECH0_PUP:
          MOV ah,0                       
                ;CONSUME BUFFER
          INT 16h                       ;7ATETHA HENA 3SHAN COMPARE CHECH_PUP LAZEM YEKON BA3D INTRUPT 
	
           CHECH_PUP:
           MOV AH,1
           INT 16H  
           JZ CHECH_PUP 
   
           CMP AL,31H  
           JZ P_UP_1
   
           CMP AL,32H
           JZ  P_UP_2
   
          CMP AL,33H
          JZ  P_UP_3
   
           CMP AL,34H
           JZ  P_UP_4
   
          CMP AL,35H
           JZ  P_UP_5
           JMP CHECH0_PUP
          ;----------------------POWER UP 1--------------------
            P_UP_1: 
             MOV ah,0                             ;CONSUME BUFFER
             INT 16h  
             CMP ELDOOR,2
             JZ PUP1_P2
     
             ;player 1
             CMP P1_POINTS,5
             JB CONTT
             ;//EXECUTE ON MY PROCCESSOR
              PRINT_POINTS ONE,TEMP_POINTS,18,18               ;DISPLAY POINTS OF PLAYER 1

             CALL OWN_PROCC
             ;AFTER EXEC
             JMP CONTT
             ;player 2
             PUP1_P2:
             CMP P2_POINTS,5
             JB CONTT
             ;//EXECUTE ON MY PROCCESSOR
              PRINT_POINTS ONE,TEMP_POINTS,18,38               ;DISPLAY POINTS OF PLAYER 1
             CALL OWN_PROCC
             ;AFTER EXEC
         
             JMP CONTT

          ;----------------------POWER UP 2--------------------
           P_UP_2:
             MOV ah,0                             ;CONSUME BUFFER
             INT 16h 
             CMP ELDOOR,2
             JZ PUP2_P2
     
             ;player 1
             CMP P1_POINTS,3
             JB CONTT
             
             

             SUB P1_POINTS,3D
             PRINT_POINTS TWO,TEMP_POINTS,18,18             ;DISPLAY POINTS OF PLAYER 1




             JMP CONTT
             ;player 2
             PUP2_P2:
             CMP P2_POINTS,3
             JB CONTT
             
             SUB P2_POINTS,3D
              PRINT_POINTS TWO,TEMP_POINTS,18,38               ;DISPLAY POINTS OF PLAYER 1



             JMP CONTT

    
            JMP CONTT

          ;----------------------POWER UP 3--------------------
            P_UP_3:
             MOV ah,0                             ;CONSUME BUFFER
             INT 16h 
             CMP ELDOOR,2
             JZ PUP3_P2
             ;PLAYER 1
             CMP CHAR1_CHANGE,1
             JZ CONTT
             CMP P1_POINTS,8
             JB CONTT
              PRINT_POINTS THREE,TEMP_POINTS,18,18               ;DISPLAY POINTS OF PLAYER 1
              MOVE_CURSOR 21,1
            	MOV AH,9H
              LEA DX,NEW_FB
              INT 21H
              FORBIDDEN_CHAR CHAR1
              MOV CHAR1_CHANGE,1
               MOV ah,0                             ;CONSUME BUFFER
               INT 16h 
              SUB P1_POINTS,8D
              JMP CONTT
             ;PLAYER 2
             PUP3_P2:
              CMP CHAR2_CHANGE,1
              JZ CONTT
              CMP P2_POINTS,8
              JB CONTT
               PRINT_POINTS THREE,TEMP_POINTS,18,38               ;DISPLAY POINTS OF PLAYER 1

              MOVE_CURSOR 21,21
            	MOV AH,9H
              LEA DX,NEW_FB
              INT 21H
              FORBIDDEN_CHAR CHAR2
               MOV CHAR2_CHANGE,1
               MOV ah,0                             ;CONSUME BUFFER
               INT 16h 
               SUB P2_POINTS,8D
       
               JMP CONTT
          ;----------------------POWER UP 4--------------------
            P_UP_4:
             MOV ah,0                             ;CONSUME BUFFER
             INT 16h
             CMP ELDOOR,2
             JZ PUP4_P2
            ;PLAYER 1
             CMP C_REG3_P1,1
             JZ CONTT
             CMP P1_POINTS,30                     ;IF THE PLAYER DO NOT HAVE ENOUGH POINT 
             JB CONTT
             ;---------------  
             MOV RAX3,0000H
             MOV RBX3,0000H
             MOV RCX3,0000H
             MOV RDX3,0000H
             MOV ISI3,0000H
	           MOV IDI3,0000H
             MOV SSP3,0000H
             MOV BBP3,0000H
             MOV PUP_INUSE,1                      ;HA5ALEHA B WA7ED 3SHAN A7AFEZ 3ALAL VAL REG
             MOV C_REG3_P1,1
             ;------------------
             SUB P1_POINTS,30D
              PRINT_POINTS FOUR,TEMP_POINTS,18,18               ;DISPLAY POINTS OF PLAYER 1

             JMP CONTT
          ;------PLAYER 2--------------
             PUP4_P2:
             CMP C_REG2_P2,1
             JZ CONTT
             CMP P2_POINTS,30                     ;IF THE PLAYER DO NOT HAVE ENOUGH POINT 
             JB CONTT
             ;---------------  
             MOV RAX2,0000H
             MOV RBX2,0000H
             MOV RCX2,0000H
             MOV RDX2,0000H
             MOV ISI2,0000H
	           MOV IDI2,0000H
             MOV SSP2,0000H
             MOV BBP2,0000H
             MOV PUP_INUSE,1                      ;HA5ALEHA B WA7ED 3SHAN A7AFEZ 3ALAL VAL REG
             MOV C_REG2_P2,1
             ;------------------
             SUB P2_POINTS,30D
              PRINT_POINTS FOUR,TEMP_POINTS,18,38               ;DISPLAY POINTS OF PLAYER 1

             JMP CONTT

           ;----------------------POWER UP 5--------------------
          	P_UP_5:
            MOV ah,0                             ;CONSUME BUFFER
            INT 16h  
            CMP GAMELEVEL,1
          	JZ CONTT 
            CMP ELDOOR,2
            JZ PUP5_P2

            ;PLAYER 1
            CMP TARG_CHAGE_P1,1
            JZ CONTT
            CMP P1_POINTS,8
           	JB CONTT 
              PRINT_POINTS FIVE,TEMP_POINTS,18,18               ;DISPLAY POINTS OF PLAYER 1

            MOVE_CURSOR 21,1
            MOV AH,9H
            LEA DX,NEW_TV
            INT 21H 
            READ_COMMAND NEW_TVAL,FOUR,8 
            RET_NUM_COUNT NEW_TVAL
            MOV TARG_VAL,DX                                   ;MOVE NEW VAL INTO TARG VAL
            MOV TARG_CHAGE_P1,1
            SUB P1_POINTS,8D
            JMP CONTT
            
            ;PLAYER 2
            PUP5_P2:
            CMP TARG_CHAGE_P2,1
            JZ CONTT
            CMP P2_POINTS,8
           	JB CONTT 
              PRINT_POINTS FIVE,TEMP_POINTS,18,3               ;DISPLAY POINTS OF PLAYER 1

            MOVE_CURSOR 21,21
            MOV AH,9H
            LEA DX,NEW_TV
            INT 21H
            READ_COMMAND NEW_TVAL,FOUR,28
            RET_NUM_COUNT NEW_TVAL
            MOV TARG_VAL,DX                         ;MOVE NEW VAL INTO TARG VAL
            MOV TARG_CHAGE_P2,1
            SUB P2_POINTS,8D
            JMP CONTT

         ;//HANSHOOF DH DOOR ANHY LA3EB
         ;CMP ELDOOR,1

         ; MOV PUP_INUSE,1

         SKIP_PUP:
        ;AFTER POWER UP: 
         CONTT:

         CMP GAMELEVEL,2
         JZ LEVEL222
         CMP ELDOOR,1
         JZ DOOR_PLAYER2  
         jmp LOOP_MAIN_AGAIN 
         LEVEL222:
         CMP ELDOOR,1
          JZ D2_AN_L2
         jmp LOOP_MAIN_AGAIN 


                         ;----------------------DESCIDE THE LEVEL-------------------
      MOV AL,GAMELEVEL
      CMP AL,2
      JZ LEVEL2
                         ;---------------------------LEVEL 1-----------
    LEVEL1:

       ;SKIP LEVEL 2
       JMP SK_L2
                          ;---------------------------LEVEL 2-------------------------
                          ;-(READ COMM - EXECUTE IT BY JUMPS ON MY OR OTHER PROCCESSOR)-
    LEVEL2:
         
         CMP ELDOOR,2
         JZ MEES_P2_L2
         DISPLAY_GRAPH MESS_L2,11,18,5
         JMP FOKAK_MENP2_L2
         MEES_P2_L2:
         DISPLAY_GRAPH MESS_L2,11,18,26
         FOKAK_MENP2_L2:


        MOV AL,ELDOOR
        CMP AL,2
        JZ D2_AN_L2

         D1_AN_L2: 

         CHECKL2: MOV AH,1
         INT 16h
         JZ CHECKL2
         CMP AL,31H  
         JZ OWN_L2_P1
         CMP AL,32H
          JZ  OPP_L2_P1
          MOV AH,0
          INT 16H
         JMP CHECKL2
         

          OWN_L2_P1:
          MOV AH,0
          INT 16H
         
          CALL OWN_PROCC

          OPP_L2_P1:
          MOV AH,0
          INT 16H
          JMP FAR PTR DOOR_PLAYER1

          ;-----------------------------------------------PLAYER 2
         D2_AN_L2:
         DISPLAY_GRAPH MESS_L2,11,18,26
    
         
         MOV ELDOOR,2
         CHECKL2_P2:
          MOV AH,1
         INT 16h
         JZ CHECKL2_P2
         CMP AL,31H  
         JZ OWN_L2_P2
          CMP AL,32H
          JZ  OPP_L2_P2
          MOV AH,0
          INT 16H
         JMP CHECKL2_P2
       

          OWN_L2_P2:
          MOV AH,0
          INT 16H
          CALL OWN_PROCC

          OPP_L2_P2:
          MOV AH,0
          INT 16H
          JMP FAR PTR DOOR_PLAYER2

    SK_L2:    
    ;---------------------loop_TO_MAIN AGAIN---------------------------------

    LOOP_MAIN_AGAIN:

    ;CHECK ON POINTS NOT ZERO
    MOV AL,P1_POINTS
    MOV BL,P2_POINTS
    DEC AL
    DEC BL 
    CMP AL,0
    JZ  P2_KESEB
    CMP BL,0
    JZ  P1_KESEB
    MOV P1_POINTS,AL
    MOV P2_POINTS,BL

    ;CHECK ON REGISTERS OF 2 PALYERS NOT HAVING TARG VALUE
    ;PLAYER 1 CHECKING(REG_3)
    MOV DX,TARG_VAL
    CMP DX,RAX2
    JZ P1_KESEB
    CMP DX,RBX2
    JZ P1_KESEB
    CMP DX,RCX2
    JZ P1_KESEB
    CMP DX,RDX2
    JZ P1_KESEB
    CMP DX,IDI2
    JZ P1_KESEB
    CMP DX,ISI2
    JZ P1_KESEB
    CMP DX,SSP2
    JZ P1_KESEB
    CMP DX,BBP2
    JZ P1_KESEB

    ;PLAYER 2 CHECKING(REG_2)
    MOV DX,TARG_VAL
    CMP DX,RAX3
    JZ P2_KESEB
    CMP DX,RBX3
    JZ P2_KESEB
    CMP DX,RCX3
    JZ P2_KESEB
    CMP DX,RDX3
    JZ P2_KESEB
    CMP DX,IDI3
    JZ P2_KESEB
    CMP DX,ISI3
    JZ P2_KESEB
    CMP DX,SSP3
    JZ P2_KESEB
    CMP DX,BBP3
    JZ P2_KESEB



    ;RAGA3 ELDOOR B 1
    MOV AH,0
    MOV AL,1
    MOV ELDOOR,AL
    ;RAGA3 PUPINUSE B 1
    MOV PUP_INUSE,0    
    JMP MAIN_LOOP
    
  ;----------------------------------------MEEN KESEB-------------------------------------------
    P1_KESEB:

    MOV AL,1
    MOV KASBAN,AL

    JMP FINISHED
    
    P2_KESEB:

     MOV BL,P2_POINTS
     CMP BL,0
     JZ TAAADOL

    MOV AL,2
    MOV KASBAN,AL

    JMP FINISHED

    TAAADOL:
     MOV AL,3
     MOV KASBAN,AL 

    FINISHED: 
     PRINT_NUMBER NUM_HEXA,RAX3,NUMTODISP,1,4     ;INTIALIZE AX
     PRINT_NUMBER NUM_HEXA,ISI3,NUMTODISP,1,11    ;INTIALIZE SI
     PRINT_NUMBER NUM_HEXA,RBX3,NUMTODISP,3,4     ;INTIALIZE BX
     PRINT_NUMBER NUM_HEXA,IDI3,NUMTODISP,3,11    ;INTIALIZE DI
     PRINT_NUMBER NUM_HEXA,RCX3,NUMTODISP,5,4     ;INTIALIZE cx
     PRINT_NUMBER NUM_HEXA,BBP3,NUMTODISP,5,11    ;INTIALIZE 
     PRINT_NUMBER NUM_HEXA,RDX3,NUMTODISP,8,4     ;INTIALIZE DX
     PRINT_NUMBER NUM_HEXA,SSP3,NUMTODISP,8,11    ;INTIALIZE 

     ;;INTIALIZE REGISTERS PLAYER 2         THAT IS CHANGED IN COMMAND OF [P1]   ETB3HA FE DOOR=1
     PRINT_NUMBER NUM_HEXA,RAX2,NUMTODISP,1,28     ;INTIALIZE AX
     PRINT_NUMBER NUM_HEXA,ISI2,NUMTODISP,1,35     ;INTIALIZE SI
     PRINT_NUMBER NUM_HEXA,RBX2,NUMTODISP,3,28     ;INTIALIZE BX
     PRINT_NUMBER NUM_HEXA,IDI2,NUMTODISP,3,35     ;INTIALIZE DI
     PRINT_NUMBER NUM_HEXA,RCX2,NUMTODISP,5,28     ;INTIALIZE cx
     PRINT_NUMBER NUM_HEXA,BBP2,NUMTODISP,5,35    ;INTIALIZE 
     PRINT_NUMBER NUM_HEXA,RDX2,NUMTODISP,8,28    ;INTIALIZE DX
     PRINT_NUMBER NUM_HEXA,SSP2,NUMTODISP,8,35    ;INTIALIZE 
     ;DELAY SCREEN
      CALL DELAYING
      
       IF_F4_PRESSED:
       MOV AH,0
       MOV al,3
       int 10h

      ;SCREEN OF SCORES 
       MOVE_CURSOR 10,35
       DISPLAY_MESS1  NAMEMESS            ;DISPLAY PLAYER1 NAME
       MOV AL,P1_POINTS
       MOV DH,10
       MOV DL,50
       CALL PRINT_SCORE
     

       MOVE_CURSOR 14,35
       DISPLAY_MESS1 NAMEMESS2       ;DISPLAY PLAYER2 NAME
       MOV AL,P2_POINTS
       MOV DH,14
       MOV DL,50
       CALL PRINT_SCORE

       CMP KASBAN,1
       JZ P1_K
       CMP KASBAN,2
       JZ P2_K
       JMP FAWET

       P1_K:
       MOVE_CURSOR 2,20
       DISPLAY_MESS KASBAN_MESS1
       JMP FAWET

       P2_K:
       MOVE_CURSOR 2,20
       DISPLAY_MESS KASBAN_MESS2
       JMP FAWET


      FAWET:
      ;DELAY 
       CALL DELAYING

       CALL CLEAR_SCREEN_PROC
      
    ;AKHER FINISHED 2ABL MA ABDA2 GAME GEDEDA
    MOV KASBAN,0
    MOV CHAR1_CHANGE,0        ;PUP 3 ONLY ONCE
    MOV CHAR2_CHANGE,0
    MOV C_REG3_P1,0           ;PUP 4 ONLY ONCE
    MOV C_REG2_P2,0
    MOV TARG_CHAGE_P1,0       ;PUP 5 ONLY ONCE
    MOV TARG_CHAGE_P2,0
    MOV PUP_INUSE,0
    MOV ELDOOR,1
    MOV TARG_VAL,1EFAH
    JMP FAR PTR GAME_GEDED


    ;HLT            ;FOR DEBYGGING
    ;clear screen first aftrer change to text mode
    ; mov ax,2
    ;mov bh,0
    ;int 10h  

    ;RETURN CONTROL TO OS
    ;MOV AH,4CH
    ;INT 21H
    
    MAIN ENDP
END MAIN