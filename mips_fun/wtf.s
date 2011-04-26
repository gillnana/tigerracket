	.globl	main

	.data


	.text
main:
	sub $sp, $sp, 8
	sw $ra, 4($sp)
	sw $s0, 8($sp)

	li $a0, 35
	li $a1, 33  # the letter 'N'
	sub $sp, $sp, 20 # just to be safe
	jal alloc_block
#	jal alloc_array
	add $sp, $sp, 20 # just to be safe
 	move $s0, $v0

	# DEBUG: print the stack pointer...
#	move $a0, $sp
#	sub $sp, $sp, 20 # just to be safe
#	jal lt_print_int
#	add $sp, $sp, 20 # just to be safe
	
#	add $t0, $s0, 0
#	lw $a0, ($t0)
#	sub $sp, $sp, 20 # just to be safe
#	jal lt_print_int
#	add $sp, $sp, 20 # just to be safe
	
#	add $t0, $s0, 4
#	lw $a0, ($t0)
#	sub $sp, $sp, 20 # just to be safe
#	jal lt_print_int
#	add $sp, $sp, 20 # just to be safe
	
	
	# $s0 is pointer to block of 7 letter 'N'
	move $a0, $s0
	sub $sp, $sp, 20 # just to be safe
	jal lt_print
	add $sp, $sp, 20 # just to be safe


	
	
	lw $s0, 8($sp)
	lw $ra, 4($sp)
	add $sp, $sp, 8
	jr $ra

