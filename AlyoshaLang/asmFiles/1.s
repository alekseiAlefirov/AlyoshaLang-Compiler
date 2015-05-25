.global main

.text

main:
	pushq  $1
	pushq  $1
	popq   %rax
	popq   %rbx
	add    %rax, %rbx
	pushq  %rbx
	popq   %rsi
	movq   $msg, %rdi
	movq   $0, %rax
	call   printf

	movq   $0, %rbx
	movq   $1, %rax
	int    $0x80

.data

msg:
	.ascii "Answer: %d.\n"
