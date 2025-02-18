	.text
	.globl	one
one:
	pushq	%rbp
	movq	%rsp, %rbp
	addq	$-16, %rsp
	movq	%rdi, -8(%rbp)
	subq	$8, %rsp
	movq	%r12, 0(%rsp)
	movq	%rsp, %r12
	andq	$-16, %rsp
	leaq	print(%rip), %rax
	callq	*%rax
	movq	%r12, %rsp
	popq	%r12
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
	.globl	two
two:
	pushq	%rbp
	movq	%rsp, %rbp
	addq	$-24, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	subq	$8, %rsp
	movq	%r12, 0(%rsp)
	movq	%rsp, %r12
	andq	$-16, %rsp
	leaq	print(%rip), %rax
	callq	*%rax
	movq	%r12, %rsp
	popq	%r12
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
	.globl	three
three:
	pushq	%rbp
	movq	%rsp, %rbp
	addq	$-32, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	subq	$8, %rsp
	movq	%r12, 0(%rsp)
	movq	%rsp, %r12
	andq	$-16, %rsp
	leaq	print(%rip), %rax
	callq	*%rax
	movq	%r12, %rsp
	popq	%r12
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
	.globl	four
four:
	pushq	%rbp
	movq	%rsp, %rbp
	addq	$-40, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	%rcx, -32(%rbp)
	subq	$8, %rsp
	movq	%r12, 0(%rsp)
	movq	%rsp, %r12
	andq	$-16, %rsp
	leaq	print(%rip), %rax
	callq	*%rax
	movq	%r12, %rsp
	popq	%r12
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
	.globl	five
five:
	pushq	%rbp
	movq	%rsp, %rbp
	addq	$-48, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	%rcx, -32(%rbp)
	movq	%r8 , -40(%rbp)
	subq	$8, %rsp
	movq	%r12, 0(%rsp)
	movq	%rsp, %r12
	andq	$-16, %rsp
	leaq	print(%rip), %rax
	callq	*%rax
	movq	%r12, %rsp
	popq	%r12
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
	.globl	six
six:
	pushq	%rbp
	movq	%rsp, %rbp
	addq	$-56, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	%rcx, -32(%rbp)
	movq	%r8 , -40(%rbp)
	movq	%r9 , -48(%rbp)
	subq	$8, %rsp
	movq	%r12, 0(%rsp)
	movq	%rsp, %r12
	andq	$-16, %rsp
	leaq	print(%rip), %rax
	callq	*%rax
	movq	%r12, %rsp
	popq	%r12
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
	.globl	seven
seven:
	pushq	%rbp
	movq	%rsp, %rbp
	addq	$-56, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	%rcx, -32(%rbp)
	movq	%r8 , -40(%rbp)
	movq	%r9 , -48(%rbp)
	subq	$8, %rsp
	movq	%r12, 0(%rsp)
	movq	%rsp, %r12
	andq	$-16, %rsp
	leaq	print(%rip), %rax
	callq	*%rax
	movq	%r12, %rsp
	popq	%r12
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
	.globl	eight
eight:
	pushq	%rbp
	movq	%rsp, %rbp
	addq	$-56, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	%rcx, -32(%rbp)
	movq	%r8 , -40(%rbp)
	movq	%r9 , -48(%rbp)
	subq	$8, %rsp
	movq	%r12, 0(%rsp)
	movq	%rsp, %r12
	andq	$-16, %rsp
	leaq	print(%rip), %rax
	callq	*%rax
	movq	%r12, %rsp
	popq	%r12
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
	.globl	nine
nine:
	pushq	%rbp
	movq	%rsp, %rbp
	addq	$-56, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	%rcx, -32(%rbp)
	movq	%r8 , -40(%rbp)
	movq	%r9 , -48(%rbp)
	subq	$8, %rsp
	movq	%r12, 0(%rsp)
	movq	%rsp, %r12
	andq	$-16, %rsp
	leaq	print(%rip), %rax
	callq	*%rax
	movq	%r12, %rsp
	popq	%r12
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
	.globl	ten
ten:
	pushq	%rbp
	movq	%rsp, %rbp
	addq	$-56, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	%rcx, -32(%rbp)
	movq	%r8 , -40(%rbp)
	movq	%r9 , -48(%rbp)
	subq	$8, %rsp
	movq	%r12, 0(%rsp)
	movq	%rsp, %r12
	andq	$-16, %rsp
	leaq	print(%rip), %rax
	callq	*%rax
	movq	%r12, %rsp
	popq	%r12
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
	.globl	ll_main
ll_main:
	pushq	%rbp
	movq	%rsp, %rbp
	addq	$-8, %rsp
	subq	$8, %rsp
	movq	%r12, 0(%rsp)
	movq	%rsp, %r12
	andq	$-16, %rsp
	leaq	print(%rip), %rax
	callq	*%rax
	movq	%r12, %rsp
	popq	%r12
	subq	$8, %rsp
	movq	%r12, 0(%rsp)
	movq	%rsp, %r12
	andq	$-16, %rsp
	movq	$1, %rdi
	leaq	one(%rip), %rax
	callq	*%rax
	movq	%r12, %rsp
	popq	%r12
	subq	$8, %rsp
	movq	%r12, 0(%rsp)
	movq	%rsp, %r12
	andq	$-16, %rsp
	movq	$1, %rdi
	movq	$1, %rsi
	leaq	two(%rip), %rax
	callq	*%rax
	movq	%r12, %rsp
	popq	%r12
	subq	$8, %rsp
	movq	%r12, 0(%rsp)
	movq	%rsp, %r12
	andq	$-16, %rsp
	movq	$1, %rdi
	movq	$1, %rsi
	movq	$1, %rdx
	leaq	three(%rip), %rax
	callq	*%rax
	movq	%r12, %rsp
	popq	%r12
	subq	$8, %rsp
	movq	%r12, 0(%rsp)
	movq	%rsp, %r12
	andq	$-16, %rsp
	movq	$1, %rdi
	movq	$1, %rsi
	movq	$1, %rdx
	movq	$1, %rcx
	leaq	four(%rip), %rax
	callq	*%rax
	movq	%r12, %rsp
	popq	%r12
	subq	$8, %rsp
	movq	%r12, 0(%rsp)
	movq	%rsp, %r12
	andq	$-16, %rsp
	movq	$1, %rdi
	movq	$1, %rsi
	movq	$1, %rdx
	movq	$1, %rcx
	movq	$1, %r8 
	leaq	five(%rip), %rax
	callq	*%rax
	movq	%r12, %rsp
	popq	%r12
	subq	$8, %rsp
	movq	%r12, 0(%rsp)
	movq	%rsp, %r12
	andq	$-16, %rsp
	movq	$1, %rdi
	movq	$1, %rsi
	movq	$1, %rdx
	movq	$1, %rcx
	movq	$1, %r8 
	movq	$1, %r9 
	leaq	six(%rip), %rax
	callq	*%rax
	movq	%r12, %rsp
	popq	%r12
	subq	$8, %rsp
	movq	%r12, 0(%rsp)
	movq	%rsp, %r12
	andq	$-16, %rsp
	movq	$1, %rdi
	movq	$1, %rsi
	movq	$1, %rdx
	movq	$1, %rcx
	movq	$1, %r8 
	movq	$1, %r9 
	pushq	$1
	subq	$8, %rsp
	leaq	seven(%rip), %rax
	callq	*%rax
	movq	%r12, %rsp
	popq	%r12
	subq	$8, %rsp
	movq	%r12, 0(%rsp)
	movq	%rsp, %r12
	andq	$-16, %rsp
	movq	$1, %rdi
	movq	$1, %rsi
	movq	$1, %rdx
	movq	$1, %rcx
	movq	$1, %r8 
	movq	$1, %r9 
	pushq	$1
	pushq	$1
	leaq	eight(%rip), %rax
	callq	*%rax
	movq	%r12, %rsp
	popq	%r12
	subq	$8, %rsp
	movq	%r12, 0(%rsp)
	movq	%rsp, %r12
	andq	$-16, %rsp
	movq	$1, %rdi
	movq	$1, %rsi
	movq	$1, %rdx
	movq	$1, %rcx
	movq	$1, %r8 
	movq	$1, %r9 
	pushq	$1
	pushq	$1
	pushq	$1
	subq	$8, %rsp
	leaq	nine(%rip), %rax
	callq	*%rax
	movq	%r12, %rsp
	popq	%r12
	subq	$8, %rsp
	movq	%r12, 0(%rsp)
	movq	%rsp, %r12
	andq	$-16, %rsp
	movq	$1, %rdi
	movq	$1, %rsi
	movq	$1, %rdx
	movq	$1, %rcx
	movq	$1, %r8 
	movq	$1, %r9 
	pushq	$1
	pushq	$1
	pushq	$1
	pushq	$1
	leaq	ten(%rip), %rax
	callq	*%rax
	movq	%r12, %rsp
	popq	%r12
	movq	%rbp, %rsp
	popq	%rbp
	retq	