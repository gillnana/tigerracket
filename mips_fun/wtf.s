	.globl	main

	.data


	.text
main:
	sub $sp, $sp, 4
	sw $ra, 4($sp)

	li $a0, 35
	li $a1, 33  # the letter 'N'
#	jal alloc_block
	jal alloc_array
	move $s0, $v0
	

#	add $t0, $s0, 0
#	lw $a0, ($t0)
#	jal lt_print_int
	
#	add $t0, $s0, 4
#	lw $a0, ($t0)
#	jal lt_print_int
	
	
	# $s0 is pointer to block of 7 letter 'N'
#	move $a0, $s0
#	jal lt_print


	
	

	lw $ra, 4($sp)
	add $sp, $sp, 4
	jr $ra

