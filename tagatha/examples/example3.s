	.file	"example3.c"
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
	subq	$8, %rsp
	movzbl	-128(%rbp), %eax
	movsbl	%al,%edx
	movzbl	-16(%rbp), %eax
	movsbl	%al,%eax
	leal	(%rdx,%rax), %eax
	leave
	ret
	.cfi_endproc
.LFE0:
	.size	gcd, .-gcd
	.ident	"GCC: (GNU) 4.4.0 20090506 (Red Hat 4.4.0-4)"
	.section	.note.GNU-stack,"",@progbits
