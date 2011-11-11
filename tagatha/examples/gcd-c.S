    .file   "example.adb"
    .text
.globl gcd__c
    .type   gcd__c, @function
gcd__c:
.LFB0:
    .cfi_startproc
    pushq   %rbp
    .cfi_def_cfa_offset 16
    movq   %rsp, %rbp
    .cfi_offset 6, -16
    .cfi_def_cfa_register 6
    subl $4, %esp
_1:
    cmp $0, 12(%ebp)
    je _2
    movl 12(%ebp), %eax
    movl %eax, -4(%ebp)
    movl 12(%ebp), %edx
    movl %edx, %eax
    sarl $31, %edx
    idivl 8(%ebp)
    movl %eax, 12(%ebp)
    movl -4(%ebp), %eax
    movl %eax, 12(%ebp)
    jmp _1
_2:
    movl 8(%ebp), %eax
    leave
    ret
    .cfi_endproc
.LFE0
    .size gcd__c, .-gcd__c
    .ident "Tagatha"
