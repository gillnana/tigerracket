	.globl	main

	.data
str1:        .ascii "----TTTThhhhiiiissss    iiiissss    aaaa    ssssttttrrrriiiinnnngggg\n\n\n\n"
newline:     .ascii "----\n\n\n\n"
str2:        .ascii " This is another string"
somebyte:    .byte  0

	
	.text
main:
	sub $sp, $sp, 4
	sw $ra, 4($sp)
	
#	li $a0, 1
#	la $a1, str1
#	li $a2, 10
#	jal write

	la $a0, str1
	li $t0, 17
	sw $t0, ($a0)
	jal lt_print

	
	li $a0, 25
	jal lt_print_int

	la $a0, newline
	li $t0, 1
	sw $t0, ($a0)
	jal lt_print

	
	li $a0, 0
	jal lt_print_int

	la $a0, newline
	li $t0, 1
	sw $t0, ($a0)
	jal lt_print

	
	li $a0, -31
	jal lt_print_int

	la $a0, newline
	li $t0, 1
	sw $t0, ($a0)
	jal lt_print

	
	li $a0, 536870913
	jal lt_print_int

	la $a0, newline
	li $t0, 1
	sw $t0, ($a0)
	jal lt_print

		
	li $a0, 4294967295
	jal lt_print_int

	la $a0, newline
	li $t0, 1
	sw $t0, ($a0)
	jal lt_print

		
	li $a0, 100
	jal lt_print_int

	la $a0, newline
	li $t0, 1
	sw $t0, ($a0)
	jal lt_print

	
	lw $ra, 4($sp)
	add $sp, $sp, 4
	jr $ra

