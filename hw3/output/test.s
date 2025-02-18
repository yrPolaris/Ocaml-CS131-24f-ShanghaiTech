	.text
	.globl	foo
foo:
	pushq	%rbp
	movq	%rsp, %rbp
	addq	$-208, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	%rcx, -32(%rbp)
	movq	%r8 , -40(%rbp)
	movq	%r9 , -48(%rbp)
	movq	-8(%rbp), %r8 
	movq	$1, %rcx
	xorq	%r9 , %r9 
	cmpq	%rcx, %r8 
	sete	%r9b
	movq	%r9 , -56(%rbp)
	movq	-16(%rbp), %r8 
	movq	$2, %rcx
	xorq	%r9 , %r9 
	cmpq	%rcx, %r8 
	sete	%r9b
	movq	%r9 , -64(%rbp)
	movq	-24(%rbp), %r8 
	movq	$3, %rcx
	xorq	%r9 , %r9 
	cmpq	%rcx, %r8 
	sete	%r9b
	movq	%r9 , -72(%rbp)
	movq	-32(%rbp), %r8 
	movq	$4, %rcx
	xorq	%r9 , %r9 
	cmpq	%rcx, %r8 
	sete	%r9b
	movq	%r9 , -80(%rbp)
	movq	-40(%rbp), %r8 
	movq	$5, %rcx
	xorq	%r9 , %r9 
	cmpq	%rcx, %r8 
	sete	%r9b
	movq	%r9 , -88(%rbp)
	movq	-48(%rbp), %r8 
	movq	$6, %rcx
	xorq	%r9 , %r9 
	cmpq	%rcx, %r8 
	sete	%r9b
	movq	%r9 , -96(%rbp)
	movq	16(%rbp), %r8 
	movq	$7, %rcx
	xorq	%r9 , %r9 
	cmpq	%rcx, %r8 
	sete	%r9b
	movq	%r9 , -104(%rbp)
	movq	24(%rbp), %r8 
	movq	$8, %rcx
	xorq	%r9 , %r9 
	cmpq	%rcx, %r8 
	sete	%r9b
	movq	%r9 , -112(%rbp)
	movq	32(%rbp), %r8 
	movq	$9, %rcx
	xorq	%r9 , %r9 
	cmpq	%rcx, %r8 
	sete	%r9b
	movq	%r9 , -120(%rbp)
	movq	40(%rbp), %r8 
	movq	$10, %rcx
	xorq	%r9 , %r9 
	cmpq	%rcx, %r8 
	sete	%r9b
	movq	%r9 , -128(%rbp)
	movq	-56(%rbp), %r8 
	movq	-64(%rbp), %rcx
	andq	%rcx, %r8 
	movq	%r8 , -136(%rbp)
	movq	-136(%rbp), %r8 
	movq	-72(%rbp), %rcx
	andq	%rcx, %r8 
	movq	%r8 , -144(%rbp)
	movq	-144(%rbp), %r8 
	movq	-80(%rbp), %rcx
	andq	%rcx, %r8 
	movq	%r8 , -152(%rbp)
	movq	-152(%rbp), %r8 
	movq	-88(%rbp), %rcx
	andq	%rcx, %r8 
	movq	%r8 , -160(%rbp)
	movq	-160(%rbp), %r8 
	movq	-96(%rbp), %rcx
	andq	%rcx, %r8 
	movq	%r8 , -168(%rbp)
	movq	-168(%rbp), %r8 
	movq	-104(%rbp), %rcx
	andq	%rcx, %r8 
	movq	%r8 , -176(%rbp)
	movq	-176(%rbp), %r8 
	movq	-112(%rbp), %rcx
	andq	%rcx, %r8 
	movq	%r8 , -184(%rbp)
	movq	-184(%rbp), %r8 
	movq	-120(%rbp), %rcx
	andq	%rcx, %r8 
	movq	%r8 , -192(%rbp)
	movq	-192(%rbp), %r8 
	movq	-128(%rbp), %rcx
	andq	%rcx, %r8 
	movq	%r8 , -200(%rbp)
	movq	-200(%rbp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
	.globl	main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	addq	$-32, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	subq	$8, %rsp
	movq	%r12, 0(%rsp)
	movq	%rsp, %r12
	andq	$-16, %rsp
	leaq	foo(%rip), %rdi
	leaq	ll_callback(%rip), %rax
	callq	*%rax
	movq	%r12, %rsp
	popq	%r12
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	