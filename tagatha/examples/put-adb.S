    .file   "example.adb"
    .text
.globl put__adb
    .type   put__adb, @function
put__adb:
.LFB0:
    .cfi_startproc
    pushq   %rbp
    .cfi_def_cfa_offset 16
    movq   %rsp, %rbp
    .cfi_offset 6, -16
    .cfi_def_cfa_register 6
    subl $12, %esp
    movl -8(%ebp), @8(%ebp)
    leave
    ret
    .cfi_endproc
.LFE0
    .size put__adb, .-put__adb
    .ident "Tagatha"
