.model small 
.STACK 256 
.data
RAX1 DW ?
RBX1 DW ?
RCX1 DW ?
RDX1 DW ?
ISI1 DW ? 
IDI1 DW ?
SSP1 DW ?
BBP1 DW ?
COM1 DB 13,?,13 DUP('$')  
COM  DB 13 DUP('$')
MEMORY DB 100 DUP(?)

 
.code
INCLUDE MACROS.INC


main proc 
    mov ax,@DATA
    mov ds,ax 
    
;move   

mov ah,0
mov al,13h
int 10h   

MOVE_CURSOR 15,20
READ_USERNAME COM1 

UP_TO_LOW COM1+2,COM,COM1+1  

MOVE_CURSOR 2,20
READ_USERNAME COM1 
UP_TO_LOW COM1+2,COM,COM1+1


  HLT
    
    
main endp
 


SUB1 proc near
    add bl,20h  ;convert upper to lowercase char
    RET
    SUB1 ENDP  

end main 