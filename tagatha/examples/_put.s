    .file   "put.adb"
    .text
.globl _put
    .type   _put, @function
_put:
.LFB0:
    .cfi_startproc
    pushq   %rbp
    .cfi_def_cfa_offset 16
    movq   %rsp, %rbp
    .cfi_offset 6, -16
    .cfi_def_cfa_register 6
    movl %edi, -4(%rbp)
    movl -12(%rbp), @-4(%rbp)
    leave
    ret
    .cfi_endproc
.LFE0
    .size _put, .-_put
    .ident "Tagatha"
