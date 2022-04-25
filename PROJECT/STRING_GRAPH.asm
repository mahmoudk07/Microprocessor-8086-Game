.model small
.data
m db 5 dup('k'),'$'
.code

main proc   
    mov ax,@data
    mov ds,ax 
    mov es,ax 
    mov ah,0
    mov al,13h
    int 10h 
    mov al, 1    ;WRITE MODE=1
	mov bh, 0    ;PAGE NO
	mov bl, 0ch  ;color
	mov cx, 4    ;message size. 
	mov dl, 10   ;COL
	mov dh, 7    ;Row
	mov bp, offset M        ;OFFSET MESS
	mov ah, 13h
	int 10h
    
    

;BL = attribute if AL bit 1 clear

    hlt
        
    main endp
end main