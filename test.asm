section        .text
_f:
    ret
global         _start          
_start:
test:   ; labels can go anywhere
    mov edx, len 
    mov ecx, msg 
    mov ebx, 1
    mov eax, 4
    call _f
    int 0x80
    mov eax, 1
    int 0x80
section        .data             
    msg        db "Hello world!", 0xa  
    len        equ $ -msg
