    .file   "gcd.c"
    .text
.globl gcd
    .type   gcd, @function
gcd:
.LFB0:
    .cfi_startproc
    pushq   %rbp
    .cfi_def_cfa_offset 16
    movq   %rsp, %rbp
    .cfi_offset 6, -16
    .cfi_def_cfa_register 6
    movl %edi, -20(%rbp)
    movl %esi, -24(%rbp)
        jmp _2
_1:
    movl -24(%rbp), %eax
    movl %eax, -4(%rbp)
    movl -20(%rbp), %eax
    movl %eax, %edx
    sarl $31, %edx
    idivl -24(%rbp)
    movl %edx, -24(%rbp)
    movl -4(%rbp), %eax
    movl %eax, -20(%rbp)
    jmp _1
_2:
    cmp $0, -24(%rbp)
    jne _2
    movl -20(%rbp), %eax
    leave
    ret
    .cfi_endproc
.LFE0
    .size gcd, .-gcd
    .ident "Tagatha"
	.section	.note.GNU-stack,"",@progbits
