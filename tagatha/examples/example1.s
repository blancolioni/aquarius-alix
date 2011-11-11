	.file	"example1.c"
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
	movl	%edi, -20(%rbp)
	movl	%esi, -24(%rbp)
	jmp	.L2
.L3:
	movl	-24(%rbp), %eax
	movl	%eax, -4(%rbp)
	movl	-20(%rbp), %eax
	movl	%eax, %edx
	sarl	$31, %edx
	idivl	-24(%rbp)
	movl	%edx, -24(%rbp)
	movl	-4(%rbp), %eax
	movl	%eax, -20(%rbp)
.L2:
	cmpl	$0, -24(%rbp)
	jne	.L3
	movl	-20(%rbp), %eax
	leave
	ret
	.cfi_endproc
.LFE0:
	.size	gcd, .-gcd
	.ident	"GCC: (GNU) 4.4.0 20090506 (Red Hat 4.4.0-4)"
	.section	.note.GNU-stack,"",@progbits
