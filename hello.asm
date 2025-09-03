bits 64
default rel

section .data
    x.a db 'hello world', 0
    fmt db '%s', 10, 0    ; 10 is newline character, 0 is null terminator

section .text
    extern printf
    global main
    align 4
main:
    push rbp
    mov rbp, rsp
    
    ; Prepare arguments for printf
    mov rdi, fmt          ; format string
    mov rsi, x.a            ; string to print
    xor eax, eax          ; clear eax (no floating point args)
    ; call printf
    call     printf wrt ..plt ; to allow for PIE
    ; jmp     printf wrt ..plt ; to allow for PIE

    ; Return 0
    xor eax, eax
    
    pop rbp
    ret