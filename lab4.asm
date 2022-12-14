.8086
model small

LOCALS

setcur macro
	mov ah, 2
	mov bh, 0
	int 10h
endm

setcurxy	macro x, y
	mov dh, y
	mov dl, x
  setcur
endm

clrwin macro l, t, r, b, bg
  push ax
  push bx
  push cx
  push dx
	mov cl, l
	mov ch, t
	mov dh, b
	mov dl, r
	mov bh, bg
	mov al, b-t+1
	mov ah, 6
	int 10h
  pop dx
  pop cx
  pop bx
  pop ax
endm

clrmsg macro bg
  clrwin wmsgl, wmsgt, wmsgr, wmsgb, bg
endm

shmsg macro str, len
  clrmsg wmsgbg
  setcurxy <wmsgl+1>, <wmsgt+1>
  outstr str
endm


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

maxlen  equ 7
kolvo   equ 8

infl    equ  0
inft    equ  0
infr    equ  79
infb    equ  2
infbg   equ  2fh

; main win: left top right bottom
winl    equ 0
wint    equ infb+1
winr    equ 39
winb    equ wint+10
winbg   equ 1fh ; white on blue

inpcol  equ 2
inprow  equ 2

wresl   equ  winr+1
wrest   equ  wint
wresr   equ  79
wresb   equ  winb
wresbg  equ  5fh

; message win
wmsgl  equ 2
wmsgt  equ 21
wmsgr  equ 77
wmsgb  equ 23
wmsgbg equ 40h ; black on red

assume cs:code, ds:data, ss:stacks


stacks segment stack
  dw 1000h dup(55h)
ends

data segment

howto  db 'Enter ', '0'+kolvo, ' numbers. Number of positives and negatives must be equal$'
prig   db 'Enter number '
prig1  db '0:$'
priglen equ $-prig

errinp db 'Wrong number, try again$'
errmsglen equ $-errinp

crlf   db 10, 13, '$'
notmt  db 'Number of positive and negative do not match.$'
notmtlen equ $-notmt
oncem  db 'Try again? (y/n)$'
oncemlen equ $-oncem

oflstr  db ' Overflow$'
align 16
buffer db kolvo*(maxlen+1) dup(0)
align 16
arr    dw 122, 453, 789, 32767, -111, -2, -313, -5 ;kolvo dup(0)
arrpos dw kolvo dup(0)
arrneg dw kolvo dup(0)
arrmul dw kolvo dup(0)
arrsum dw kolvo dup(0)
strbuf db 32 dup(0)

ends


code segment


;*****************************************************
; Start here

main proc far
start:
  mov ax, seg data
  mov ds, ax
  mov es, ax
  cld

  clrwin 0, 0, 79, 24, 0fh
  clrwin infl, inft, infr, infb, infbg
  clrwin winl, wint, winr, winb, winbg
  setcurxy infl+1, inft+1
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

  mov dh, wint+1
  add dh, al
  mov dl, winl+1
  setcur

  inc al
  call input1
  add di, 2
  call to_number
  sub di, 2
  cmp bl, 1
  jne oknum1

  shmsg errinp, errmsglen
  jmp retr1

oknum1:
  clrmsg 0fh
  mov [si], ax
  add si, 2
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

  shmsg notmt, notmtlen
  jmp endprog

; произведения
domath:

  clrwin wresl, wrest, wresr, wresb, wresbg

  mov cx, 0
  lea di, arrneg
  lea si, arrpos
loopm1:
  mov dh, cl
  add dh, wrest+1
  mov dl, wresl+1
  setcur
  mov ax, [si]
  call outax
  putch '*'
  mov ax, [di]
  call outax
  putch '='

  mov ax, [si]
  imul word ptr [di]
;   cmp dx, 0
;   jz  @@o1
;   push ax
;   mov ax, dx
;   call outax
;   pop ax

; @@o1:
  call outax
nxt1:

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
  mov dh, cl
  add dh, wrest+1+kolvo/2
  mov dl, wresl+1
  setcur
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

nxta1:
  add si, 2
  add di, 2
  inc cx
  cmp cx, kolvo/2
  jb loopa1

  clrmsg wmsgbg

endprog:
  setcurxy <wmsgr - oncemlen - 1>, <wmsgt + 1>
  outstr oncem
  call getyn
  jnc @@end
  jmp start

@@end:
  mov ah, 4ch
  int 21h

main endp


;*************************************
; ввод с клавиатуры ответа y/n
; возвращает: флаг C = 1, если Y, = 0, если N
getyn  proc
@@k:
  mov ah, 8
  int 21h
  cmp al, 'y'
  je @@a
  cmp al, 'Y'
  je @@a
  cmp al, 'n'
  je @@q
  cmp al, 'N'
  je @@q
  jmp @@k
@@a:
  stc
  jmp @@r
@@q:
  clc
@@r:
  ret
getyn endp

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
  jns @@p
  mov al, '-'
  stosb
  neg ax

@@p:
  mov bx, 10000
  
@@d:
  xor dx, dx
  idiv bx
  cmp ax, 0
  jnz @@s
  cmp bp, 0
  jz @@n
  
@@s:
  mov bp, 1 ; установить флаг
  add al, '0' ; сохранить цифру
  stosb
@@n:
  mov ax, dx; остаток от деления на делитель в bx
  call div10 ; разделить bx на 10
  cmp bx, 0
  je @@t
  jmp @@d
@@t:
  cmp bp, 0
  jnz @@r
  mov [di], byte ptr '0'
  inc di
; если число было 0 - сохранить '0'
@@r:
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
  jnz @@lp
  mov dl, 1; 1 - признак отрицательного
  inc di

@@lp:
  cmp [di], byte ptr '0'
  jb  @@e
  cmp [di], byte ptr '9'
  ja @@e
; умножить ax на 10
  mov bp, ax
  shl bp, 1
  jo @@e
  jc @@e
  shl ax, 3
  jo @@e
  jc @@e
  add ax, bp
  jo @@e
  jc @@e

; добавить очередную цифру
  mov bl, [di]
  sub bl, '0'
  add ax, bx
  jo @@e
  jc @@e
  inc di
; повторяем, пока не 0 в буфуре
  cmp [di], byte ptr 0
  jnz @@lp

  cmp dl, 1
  jne @@n
  neg ax
@@n:
  pop di
  xor bx, bx
  ret
@@e:
  mov bl, 1
  pop di
  ret
to_number endp


;*************************************
; ввод одного числа
; вход: al - N числа, di - буфер для входной строки
; использует dx, ax, bx
;
input1 proc

  push cx
  push ax
  mov ax, 0920h ; вывести пробелы
  mov bx, winbg
  mov cx, priglen + maxlen -1
  int 10h
  pop ax

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
