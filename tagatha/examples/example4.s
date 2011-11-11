	.file	"example4.c"
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
	movl	%edi, -68(%rbp)
	movl	-68(%rbp), %eax
	movl	%eax, -64(%rbp)
	movl	-68(%rbp), %eax
	movl	%eax, -4(%rbp)
	movl	-64(%rbp), %edx
	movl	-4(%rbp), %eax
	leal	(%rdx,%rax), %eax
	leave
	ret
	.cfi_endproc
.LFE0:
	.size	gcd, .-gcd
	.ident	"GCC: (GNU) 4.4.0 20090506 (Red Hat 4.4.0-4)"
	.section	.note.GNU-stack,"",@progbits
