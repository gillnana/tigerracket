	.globl	main

	.data

	.text
main:
	sub $sp, $sp, 4
	sw $ra, 4($sp)

	li $a0, 0
	jal lt_not

	move $a0, $v0
	jal lt_print_int

	lw $ra, 4($sp)
	add $sp, $sp, 4
	jr $ra
