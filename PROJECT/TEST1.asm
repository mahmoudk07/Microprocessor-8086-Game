 .MODEL SMALL
 .DATA
 ;ARR DB 'o','r',' ','$'  
 COM1 DB 13,?,13 DUP('$')  
 COMp1  DB 13 DUP('$')     ;command player 1
COM2 DB 13,?,13 DUP('$')  
COMp2  DB 13 DUP('$')     ;command player 2    

 
 .CODE 
 INCLUDE COMPINST.INC
 INCLUDE MACROS.INC 
 
 
 SUB1 proc near
    add bl,20h  ;convert upper to lowercase char
    RET
    SUB1 ENDP 
 
 MAIN PROC  
    MOV AX,@DATA
    MOV DS,AX    
    
    mov ax,13h                     ; for 320x200x256 "CGA 256" colors (13h)
    mov bh,0h                      ;bh=0 page number always 0 for 320x200x256 CGA-256
    int 10h       
    
      MOVE_CURSOR 20,1
    READ_USERNAME COM1 
    UP_TO_LOW COM1+2,COMp1,COM1+1  
    
    cmp_inst COMp1             ;CHECK VALID INS
    
    ;COMMANDS
    MOVE: 
      MOV AH,0
      MOV AL,13H
      INT 10H
      MOVE_CURSOR 10,20
      DISPLAY_MESS COMp1        

  
    ADDITION:  
    MOV AH,0
      MOV AH,0
      MOV AL,13H
      INT 10H
      MOVE_CURSOR 10,20
      DISPLAY_MESS COMp1  
    
    SUBTRACT:  
   MOV AH,0
      MOV AL,13H
      INT 10H
      MOVE_CURSOR 10,20
      DISPLAY_MESS COMp1   
    
    MULT:     
    MOV AH,0
      MOV AL,13H
      INT 10H
      MOVE_CURSOR 10,20
      DISPLAY_MESS COMp1    
    
    DIVI:   
    MOV AH,0
      MOV AL,13H
      INT 10H
      MOVE_CURSOR 10,20
      DISPLAY_MESS COMp1  
    
    ANDING:  
    MOV AH,0
      MOV AL,13H
      INT 10H
      MOVE_CURSOR 10,20
      DISPLAY_MESS COMp1  
    
    RO_RIGHT: 
    
    NOOP:
    
    INCR:
    
    DECR:
    
    CLCF:
    
    XORING:
    
    ADDCARRY:
    
    SUBBORROW:
    
    SHIFTL:
    
    ORING: 
     MOV AH,0
      MOV AL,13H
      INT 10H
      MOVE_CURSOR 10,20
      DISPLAY_MESS COMp1  
    
    NOT_COMMAND:
    
    
   
  
   
    
     
    
               ;FOR DEBYGGING
    
    
    MAIN ENDP 
 
  
 
 
     HLT 
 
 END MAIN