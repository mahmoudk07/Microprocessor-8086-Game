.model small 
.STACK 256 

.data   
;PLAYER 1 COMMAND DATA 
COM1 DB 13,?,13 DUP('$')  
COMp1  DB 13 DUP('$')     ;command player 1
.code
INCLUDE MACROS.INC
INCLUDE COMPINST.INC
INCLUDE COUNTNUM.INC 
INCLUDE PRINTNUM.INC

MAIN PROC FAR  
    MOV AX,@DATA
    MOV DS,AX  
    
    ;player 1  
    READ_COMMAND COM1,1
    UP_TO_LOW COM1+2,COMp1,COM1+1  
    
    
    

 
    
    ;--------------------------------------------------------- CHECK VALID INS (MOV,ADD,..)---------------------------------------------------   
    ;translate command
     
    
    ;PLAYER 1
    cmp_inst COMp1             

   
    
    main endp
end main