;//PRINT NUMBER COMMAND 
PRINT_NUMBER MACRO NUMHEX,REG,NUM_TO_DISP,ROW,COL
 LOCAL PRLOOP

;;CODE SHIFTING NUMBER
    LEA DI,NUM_TO_DISP
    MOV BX,REG
    SHR BX,12  
    MOV [DI],BL
    INC DI 
    MOV BX,REG
    SHL BX,4
    SHR BX,12 
    MOV [DI],BL
    INC DI  
    MOV BX,REG    
    SHL BX,8
    SHR BX,12 
    MOV [DI],BL
    INC DI  
    MOV BX,REG    
    SHL BX,12
    SHR BX,12 
    MOV [DI],BL  
	
    MOV CX,4             ;NO OF LOOPS   
    LEA DI,NUM_TO_DISP   ;MOVE OFFSET OF NUM TO DISPLAY
    MOV DH,ROW           ;INTIALIZE COL AND row
    MOV DL,COL
	
	PRLOOP:
    ;;PRINT NUMBER 
    MOV AL,[DI] 
    LEA BX,NUMHEX
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
    LOOP  PRLOOP
ENDM PRINT_NUMBER

