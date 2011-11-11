	.file	"example6.c"
	.text
.globl gcd
	.type	gcd, @function
gcd:
.LFB0:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	movq	%rsp, %rbp
	.cfi_offset 6, -16
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	%edx, -12(%rbp)
	movl	%ecx, -16(%rbp)
	movl	%r8d, -20(%rbp)
	movl	%r9d, -24(%rbp)
	movl	-4(%rbp), %eax
	leave
	ret
	.cfi_endproc
.LFE0:
	.size	gcd, .-gcd
	.ident	"GCC: (GNU) 4.4.0 20090506 (Red Hat 4.4.0-4)"
	.section	.note.GNU-stack,"",@progbits
