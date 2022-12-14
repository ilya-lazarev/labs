.8086
model small

outstr	macro str
	mov ah, 9
	lea dx, str
	int 21h
endm

inpstr	macro dst
	mov dx, dst
	mov ah, 0ah
	int 21h
endm

putch	macro c
	mov ah, 2
	mov dl, c
	int 21h
endm

maxlen equ 7
kolvo  equ 8

assume cs:code, ds:data, ss:stacks


stacks segment stack
  dw 1000h dup(55h)
ends

data segment

howto  db 'Enter ', '0'+kolvo, ' numbers. Number of positives and negatives must be equal',10,13,'$'
prig   db 'Enter number '
prig1  db '0:$'
errinp db 'Wrong number, try again', 10, 13, '$'
crlf   db 10, 13, '$'
notmt  db 'Number of positive and negative do not match', 10, 13, '$'
oflstr  db ' Overflow$'
align 16
buffer db kolvo*(maxlen+1) dup(0)
align 16
arr    dw 12, 45, 78, 32767, -1, -2, -3, -5 ;kolvo dup(0)
arrpos dw kolvo dup(0)
arrneg dw kolvo dup(0)
arrmul dw kolvo dup(0)
arrsum dw kolvo dup(0)
strbuf db 32 dup(0)

ends



code segment

main proc far
start:
  mov ax, seg data
  mov ds, ax
  mov es, ax

  outstr howto

  lea di, buffer
  xor ax, ax
  mov cx, kolvo
  lea si, arr
loop1:
  lea di, buffer

retr1:
  mov ax, kolvo
  sub ax, cx
  inc al
  call input1
  add di, 2
  call to_number
  sub di, 2
  cmp bl, 1
  jne oknum1
  outstr errinp

  jmp retr1

oknum1:
  mov [si], ax
  inc si
  inc si
  add di, maxlen+1
  loop loop1

; числа введены, вычисляем
gofor1:
; разделим на +-
  mov cx, kolvo
  lea bx, arr
  lea di, arrneg
  lea si, arrpos
  xor dx, dx ; считаем +- числа, dh - полож., dl - отриц.
loopdvd:
  mov ax, [bx]
  cmp ax, 0
  jl mvneg1
; положительные
  inc dh
  mov [si], ax
  inc si
  inc si
  jmp dvd1
mvneg1:
  inc dl
  mov [di], ax
  inc di
  inc di
dvd1:
  inc bx
  inc bx
  loop loopdvd

; если кол-во +- чисел не равно - выход.

  cmp dh, dl
  je domath

  outstr notmt
  jmp endprog

; произведения
domath:
  mov cx, 0
  lea di, arrneg
  lea si, arrpos
loopm1:
  mov ax, [si]
  call outax
  putch '*'
  mov ax, [di]
  call outax
  putch '='

  mov ax, [si]
  imul word ptr [di]
  call outax
nxt1:

  outstr crlf
  add si, 2
  add di, 2
  inc cx
  cmp cx, kolvo/2
  jb loopm1

; суммы
  mov cx, 0
  lea di, arrneg
  lea si, arrpos
loopa1:
  mov ax, [si]
  call outax
  putch '+'

  mov ax, [di]
  call outax
  putch '='

  mov ax, [si]
  add ax, [di]
  pushf
  call outax
  popf
  jno nxta1
  outstr oflstr

nxta1:
  outstr crlf
  add si, 2
  add di, 2
  inc cx
  cmp cx, kolvo/2
  jb loopa1


endprog:
  mov ah, 4ch
  int 21h

main endp


;*************************************
; вывод числа в ax
outax proc
  push di
  lea di, strbuf
  call to_str
  outstr strbuf
  pop di
  ret
outax endp


;*************************************
; разделить bx на 10
div10 proc
  push ax
  push dx
  mov ax, bx
  xor dx, dx
  mov bx, 10
  idiv bx
  mov bx, ax
  pop dx
  pop ax
  ret
div10 endp

;*************************************
; преобразовать число в ах в строку в [di]
; меняет ax
to_str proc
  push di
  push dx
  push bx
  push bp
  xor bp, bp; флаг, означает, что ненулевая цифра уже попалась
  test ax, 8000h
  jns pos1
  mov [di], byte ptr '-'
  inc di
  neg ax

pos1:
  mov bx, 10000
  
rdiv1:
  xor dx, dx
  idiv bx
  cmp ax, 0
  jnz save1
  cmp bp, 0
  jz next1
  
save1:
  mov bp, 1 ; установить флаг
  add al, '0' ; сохранить цифру
  mov [di], al
  inc di
next1:
  mov ax, dx; остаток от деления на делитель в bx
  call div10 ; разделить bx на 10
  cmp bx, 0
  je put1
  jmp rdiv1
put1:
  cmp bp, 0
  jnz ret1
  mov [di], byte ptr '0'
  inc di
; если число было 0 - сохранить '0'
ret1:
  mov [di], byte ptr '$'

  pop bp
  pop bx
  pop dx
  pop  di
  ret
to_str endp

;*************************************
; преобр строки в [di], оканчивающейся 0, в число
; вход: di - адрес буфера
; выход: ax - число, bl - наличие ошибки
; меняет ax, bx
to_number proc

  push di
  xor bx, bx
  xor ax, ax ; число-результат
  cmp [di], byte ptr '-'
  jnz lp1
  mov dl, 1; 1 - признак отрицательного
  inc di

lp1:
  cmp [di], byte ptr '0'
  jb  err1
  cmp [di], byte ptr '9'
  ja err1
; умножить ax на 10
  mov bp, ax
  shl bp, 1
  jo err1
  jc err1
  shl ax, 3
  jo err1
  jc err1
  add ax, bp
  jo err1
  jc err1

; добавить очередную цифру
  mov bl, [di]
  sub bl, '0'
  add ax, bx
  jo err1
  jc err1
  inc di
; повторяем, пока не 0 в буфуре
  cmp [di], byte ptr 0
  jnz lp1
  
  cmp dl, 1
  jne noneg1
  neg ax
noneg1:
  pop di
  xor bx, bx
  ret
err1:
  mov bl, 1
  pop di
  ret
to_number endp


;*************************************
; ввод одного числа
; вход: al - № числа, di - буфер для входной строки
; использует dx, ax, bx
;
input1 proc
  push cx
  lea bx, prig1
  add al, '0'
  mov [bx], al
  outstr prig

; обнулим буфер
  push di
  mov cx, maxlen+1
  cld
  mov al, 0
  rep stosb
  pop di

; запишем maxlen в начало буфера
  mov byte ptr [di], maxlen
  inpstr di

  outstr crlf

; запишем 0 вместо 'lf'
  xor bx, bx
  mov bl, [di+1] ; кол-во введенных
  mov [bx+di+2], byte ptr 0
  pop cx
  ret
input1 endp

ends

end start
