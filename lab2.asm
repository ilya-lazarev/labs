d1 segment 
mess db 'Hello','$'
mess1 db 10,13, 'vvod',10,13,'$'
mess2 db 10,13, 'vivod', 10,13,'$'
in_s db 20 dup(0)
out_s db 20 dup('$')
d1 ends

code1 segment 
assume ds:d1,cs:code1

start: mov ax,d1
	mov ds,ax

	mov ah,9
	mov dx, offset mess
	int 21h

	mov ah,9
	mov dx, offset mess1
	int 21h

	mov ah,10
	mov in_s,18
	mov dx, offset in_s
	int 21h

	mov cl,in_s+1
	xor ch,ch
	mov si,offset in_s+2
	mov di,offset out_s


c0: 	mov al, [si]
	push cx

	mov bx, 0 ; bx - count number of bits
	mov cx, 8 ; bit loop counter
;	mov dx, 7
;c1:     bt ax, dx
c1:	ror al,1
	jnc c2
	inc bx
;c2: 	dec dx
c2:	loop c1

	pop cx
	cmp bx, 3; 3 - total bits to check
	jne c3

  	mov [di], al
  	inc di
c3: 	inc si
	loop c0


        mov ah,9
	mov dx, offset mess2
	int 21h
        mov ah,9
	mov dx, offset out_s
	int 21h
        mov ax,4c00h
	int 21h
code1 ends

end start