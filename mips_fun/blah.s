	.globl main
	.data
	
	.text
main:
	sub $sp, $sp, 4
	sw $ra, 0($sp)

	
	sub $sp, $sp, 24
	
	li $a0, 111 #0
	li $a1, 222 #4
	li $a2, 333 #8
	li $a3, 444 #12
	
	li $t0, 555
	sw $t0, 16($sp)
	
	li $t1, 666
	sw $t1, 20($sp)
	
	jal call_test
	
	add $sp, $sp, 24
	

	lw $ra, 0($sp)
	add $sp, $sp, 4
	jr $ra
