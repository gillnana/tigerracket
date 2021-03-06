	.file	1 "libtiger.c"
	.section .mdebug.abi32
	.previous
	.gnu_attribute 4, 1
	.rdata
	.align	2
$LC0:
	.ascii	"Error: out of memory\012\000"
	.text
	.align	2
	.globl	out_of_memory_fail
	.set	nomips16
	.ent	out_of_memory_fail
	.type	out_of_memory_fail, @function
out_of_memory_fail:
	.frame	$fp,32,$31		# vars= 8, regs= 2/0, args= 16, gp= 0
	.mask	0xc0000000,-4
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	
	addiu	$sp,$sp,-32
	sw	$31,28($sp)
	sw	$fp,24($sp)
	move	$fp,$sp
	lui	$2,%hi($LC0)
	addiu	$2,$2,%lo($LC0)
	sw	$2,20($fp)
	li	$2,21			# 0x15
	sw	$2,16($fp)
	li	$4,2			# 0x2
	lw	$5,20($fp)
	lw	$6,16($fp)
	jal	write
	nop

	jal	abort
	nop

	.set	macro
	.set	reorder
	.end	out_of_memory_fail
	.size	out_of_memory_fail, .-out_of_memory_fail
	.align	2
	.globl	alloc_block
	.set	nomips16
	.ent	alloc_block
	.type	alloc_block, @function
alloc_block:
	.frame	$fp,32,$31		# vars= 8, regs= 2/0, args= 16, gp= 0
	.mask	0xc0000000,-4
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	
	addiu	$sp,$sp,-32
	sw	$31,28($sp)
	sw	$fp,24($sp)
	move	$fp,$sp
	sw	$4,32($fp)
	sw	$5,36($fp)
	lw	$2,32($fp)
	nop
	sll	$2,$2,2
	move	$4,$2
	jal	malloc
	nop

	sw	$2,20($fp)
	lw	$2,20($fp)
	nop
	bne	$2,$0,$L4
	nop

	jal	out_of_memory_fail
	nop

$L4:
	sw	$0,16($fp)
	j	$L5
	nop

$L6:
	lw	$2,16($fp)
	nop
	sll	$2,$2,2
	lw	$3,20($fp)
	nop
	addu	$2,$3,$2
	lw	$3,36($fp)
	nop
	sw	$3,0($2)
	lw	$2,16($fp)
	nop
	addiu	$2,$2,1
	sw	$2,16($fp)
$L5:
	lw	$3,16($fp)
	lw	$2,32($fp)
	nop
	slt	$2,$3,$2
	bne	$2,$0,$L6
	nop

	lw	$2,20($fp)
	move	$sp,$fp
	lw	$31,28($sp)
	lw	$fp,24($sp)
	addiu	$sp,$sp,32
	j	$31
	nop

	.set	macro
	.set	reorder
	.end	alloc_block
	.size	alloc_block, .-alloc_block
	.align	2
	.globl	alloc_array
	.set	nomips16
	.ent	alloc_array
	.type	alloc_array, @function
alloc_array:
	.frame	$fp,32,$31		# vars= 8, regs= 2/0, args= 16, gp= 0
	.mask	0xc0000000,-4
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	
	addiu	$sp,$sp,-32
	sw	$31,28($sp)
	sw	$fp,24($sp)
	move	$fp,$sp
	sw	$4,32($fp)
	sw	$5,36($fp)
	lw	$2,32($fp)
	nop
	addiu	$2,$2,1
	move	$4,$2
	lw	$5,36($fp)
	jal	alloc_block
	nop

	sw	$2,16($fp)
	lw	$2,16($fp)
	lw	$3,32($fp)
	nop
	sw	$3,0($2)
	lw	$2,16($fp)
	move	$sp,$fp
	lw	$31,28($sp)
	lw	$fp,24($sp)
	addiu	$sp,$sp,32
	j	$31
	nop

	.set	macro
	.set	reorder
	.end	alloc_array
	.size	alloc_array, .-alloc_array
	.align	2
	.globl	alloc_string
	.set	nomips16
	.ent	alloc_string
	.type	alloc_string, @function
alloc_string:
	.frame	$fp,24,$31		# vars= 0, regs= 2/0, args= 16, gp= 0
	.mask	0xc0000000,-4
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	
	addiu	$sp,$sp,-24
	sw	$31,20($sp)
	sw	$fp,16($sp)
	move	$fp,$sp
	sw	$4,24($fp)
	lw	$2,24($fp)
	nop
	addiu	$2,$2,1
	move	$4,$2
	move	$5,$0
	jal	alloc_array
	nop

	move	$sp,$fp
	lw	$31,20($sp)
	lw	$fp,16($sp)
	addiu	$sp,$sp,24
	j	$31
	nop

	.set	macro
	.set	reorder
	.end	alloc_string
	.size	alloc_string, .-alloc_string
	.align	2
	.globl	alloc_closure
	.set	nomips16
	.ent	alloc_closure
	.type	alloc_closure, @function
alloc_closure:
	.frame	$fp,32,$31		# vars= 8, regs= 2/0, args= 16, gp= 0
	.mask	0xc0000000,-4
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	
	addiu	$sp,$sp,-32
	sw	$31,28($sp)
	sw	$fp,24($sp)
	move	$fp,$sp
	sw	$4,32($fp)
	sw	$5,36($fp)
	li	$4,8			# 0x8
	jal	malloc
	nop

	sw	$2,16($fp)
	lw	$2,16($fp)
	nop
	bne	$2,$0,$L13
	nop

	jal	out_of_memory_fail
	nop

$L13:
	lw	$2,16($fp)
	lw	$3,32($fp)
	nop
	sw	$3,0($2)
	lw	$2,16($fp)
	lw	$3,36($fp)
	nop
	sw	$3,4($2)
	lw	$2,16($fp)
	move	$sp,$fp
	lw	$31,28($sp)
	lw	$fp,24($sp)
	addiu	$sp,$sp,32
	j	$31
	nop

	.set	macro
	.set	reorder
	.end	alloc_closure
	.size	alloc_closure, .-alloc_closure
	.rdata
	.align	2
$LC1:
	.ascii	"Null pointer: tried to read or write a field of nil\012\000"
	.text
	.align	2
	.globl	assert_nonnil
	.set	nomips16
	.ent	assert_nonnil
	.type	assert_nonnil, @function
assert_nonnil:
	.frame	$fp,32,$31		# vars= 8, regs= 2/0, args= 16, gp= 0
	.mask	0xc0000000,-4
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	
	addiu	$sp,$sp,-32
	sw	$31,28($sp)
	sw	$fp,24($sp)
	move	$fp,$sp
	sw	$4,32($fp)
	lw	$2,32($fp)
	nop
	bne	$2,$0,$L17
	nop

	lui	$2,%hi($LC1)
	addiu	$2,$2,%lo($LC1)
	sw	$2,20($fp)
	li	$2,52			# 0x34
	sw	$2,16($fp)
	li	$4,2			# 0x2
	lw	$5,20($fp)
	lw	$6,16($fp)
	jal	write
	nop

	jal	abort
	nop

$L17:
	move	$sp,$fp
	lw	$31,28($sp)
	lw	$fp,24($sp)
	addiu	$sp,$sp,32
	j	$31
	nop

	.set	macro
	.set	reorder
	.end	assert_nonnil
	.size	assert_nonnil, .-assert_nonnil
	.rdata
	.align	2
$LC2:
	.ascii	"Array index: out of bounds; index was \000"
	.align	2
$LC3:
	.ascii	" but size was \000"
	.align	2
$LC4:
	.ascii	"\012\000"
	.text
	.align	2
	.globl	assert_inbounds
	.set	nomips16
	.ent	assert_inbounds
	.type	assert_inbounds, @function
assert_inbounds:
	.frame	$fp,32,$31		# vars= 8, regs= 2/0, args= 16, gp= 0
	.mask	0xc0000000,-4
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	
	addiu	$sp,$sp,-32
	sw	$31,28($sp)
	sw	$fp,24($sp)
	move	$fp,$sp
	sw	$4,32($fp)
	sw	$5,36($fp)
	lw	$3,36($fp)
	lw	$2,32($fp)
	nop
	subu	$2,$3,$2
	bgez	$2,$L19
	nop

	addiu	$2,$2,3
$L19:
	sra	$2,$2,2
	addiu	$2,$2,-1
	sw	$2,20($fp)
	lw	$2,32($fp)
	nop
	lw	$2,0($2)
	nop
	sw	$2,16($fp)
	lw	$2,20($fp)
	nop
	bltz	$2,$L20
	nop

	lw	$3,20($fp)
	lw	$2,16($fp)
	nop
	slt	$2,$3,$2
	bne	$2,$0,$L22
	nop

$L20:
	li	$4,2			# 0x2
	lui	$2,%hi($LC2)
	addiu	$5,$2,%lo($LC2)
	li	$6,38			# 0x26
	jal	write
	nop

	lw	$4,20($fp)
	jal	lt_print_int
	nop

	li	$4,2			# 0x2
	lui	$2,%hi($LC3)
	addiu	$5,$2,%lo($LC3)
	li	$6,14			# 0xe
	jal	write
	nop

	lw	$4,16($fp)
	jal	lt_print_int
	nop

	li	$4,2			# 0x2
	lui	$2,%hi($LC4)
	addiu	$5,$2,%lo($LC4)
	li	$6,1			# 0x1
	jal	write
	nop

	jal	abort
	nop

$L22:
	move	$sp,$fp
	lw	$31,28($sp)
	lw	$fp,24($sp)
	addiu	$sp,$sp,32
	j	$31
	nop

	.set	macro
	.set	reorder
	.end	assert_inbounds
	.size	assert_inbounds, .-assert_inbounds
	.align	2
	.globl	string_comp
	.set	nomips16
	.ent	string_comp
	.type	string_comp, @function
string_comp:
	.frame	$fp,16,$31		# vars= 8, regs= 1/0, args= 0, gp= 0
	.mask	0x40000000,-4
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	
	addiu	$sp,$sp,-16
	sw	$fp,12($sp)
	move	$fp,$sp
	sw	$4,16($fp)
	sw	$5,20($fp)
	lw	$2,16($fp)
	nop
	lw	$2,0($2)
	nop
	sw	$2,4($fp)
	lw	$2,20($fp)
	nop
	lw	$3,0($2)
	lw	$2,4($fp)
	nop
	beq	$3,$2,$L24
	nop

	move	$2,$0
	j	$L25
	nop

$L24:
	sw	$0,0($fp)
	j	$L26
	nop

$L28:
	lw	$2,0($fp)
	lw	$3,16($fp)
	sll	$2,$2,2
	addu	$2,$3,$2
	lw	$3,4($2)
	lw	$2,0($fp)
	lw	$4,20($fp)
	sll	$2,$2,2
	addu	$2,$4,$2
	lw	$2,4($2)
	nop
	beq	$3,$2,$L27
	nop

	move	$2,$0
	j	$L25
	nop

$L27:
	lw	$2,0($fp)
	nop
	addiu	$2,$2,1
	sw	$2,0($fp)
$L26:
	lw	$3,0($fp)
	lw	$2,4($fp)
	nop
	slt	$2,$3,$2
	bne	$2,$0,$L28
	nop

	li	$2,1			# 0x1
$L25:
	move	$sp,$fp
	lw	$fp,12($sp)
	addiu	$sp,$sp,16
	j	$31
	nop

	.set	macro
	.set	reorder
	.end	string_comp
	.size	string_comp, .-string_comp
	.align	2
	.globl	lt_print
	.set	nomips16
	.ent	lt_print
	.type	lt_print, @function
lt_print:
	.frame	$fp,40,$31		# vars= 16, regs= 2/0, args= 16, gp= 0
	.mask	0xc0000000,-4
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	
	addiu	$sp,$sp,-40
	sw	$31,36($sp)
	sw	$fp,32($sp)
	move	$fp,$sp
	sw	$4,40($fp)
	lw	$2,40($fp)
	nop
	lw	$2,0($2)
	nop
	sw	$2,20($fp)
	sw	$0,16($fp)
	sw	$0,16($fp)
	j	$L31
	nop

$L32:
	lw	$2,16($fp)
	lw	$3,40($fp)
	sll	$2,$2,2
	addu	$2,$3,$2
	lw	$2,4($2)
	nop
	andi	$2,$2,0x00ff
	sb	$2,24($fp)
	addiu	$2,$fp,24
	li	$4,1			# 0x1
	move	$5,$2
	li	$6,1			# 0x1
	jal	write
	nop

	lw	$2,16($fp)
	nop
	addiu	$2,$2,1
	sw	$2,16($fp)
$L31:
	lw	$3,16($fp)
	lw	$2,20($fp)
	nop
	slt	$2,$3,$2
	bne	$2,$0,$L32
	nop

	move	$sp,$fp
	lw	$31,36($sp)
	lw	$fp,32($sp)
	addiu	$sp,$sp,40
	j	$31
	nop

	.set	macro
	.set	reorder
	.end	lt_print
	.size	lt_print, .-lt_print
	.align	2
	.globl	lt_print_int
	.set	nomips16
	.ent	lt_print_int
	.type	lt_print_int, @function
lt_print_int:
	.frame	$fp,48,$31		# vars= 24, regs= 2/0, args= 16, gp= 0
	.mask	0xc0000000,-4
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	
	addiu	$sp,$sp,-48
	sw	$31,44($sp)
	sw	$fp,40($sp)
	move	$fp,$sp
	sw	$4,48($fp)
	li	$2,1			# 0x1
	sw	$2,24($fp)
	lw	$2,48($fp)
	nop
	sw	$2,20($fp)
	lw	$2,20($fp)
	nop
	bgez	$2,$L36
	nop

	lw	$2,20($fp)
	nop
	subu	$2,$0,$2
	sw	$2,20($fp)
	j	$L36
	nop

$L37:
	lw	$2,24($fp)
	nop
	sll	$2,$2,1
	sll	$3,$2,2
	addu	$2,$2,$3
	sw	$2,24($fp)
$L36:
	lw	$3,24($fp)
	lw	$2,20($fp)
	nop
	slt	$2,$2,$3
	beq	$2,$0,$L37
	nop

	lw	$2,20($fp)
	nop
	beq	$2,$0,$L38
	nop

	lw	$3,24($fp)
	li	$2,10			# 0xa
	bne	$2,$0,1f
	div	$0,$3,$2
	break	7
1:
	mfhi	$3
	mflo	$2
	sw	$2,24($fp)
$L38:
	sw	$0,16($fp)
	lw	$2,48($fp)
	nop
	bgez	$2,$L39
	nop

	lw	$2,16($fp)
	addiu	$3,$fp,16
	addu	$2,$3,$2
	li	$3,45			# 0x2d
	sb	$3,12($2)
	lw	$2,16($fp)
	nop
	addiu	$2,$2,1
	sw	$2,16($fp)
$L39:
	lw	$2,16($fp)
	lw	$4,20($fp)
	lw	$3,24($fp)
	nop
	bne	$3,$0,1f
	div	$0,$4,$3
	break	7
1:
	mfhi	$4
	mflo	$3
	andi	$3,$3,0x00ff
	addiu	$3,$3,48
	andi	$3,$3,0x00ff
	sll	$3,$3,24
	sra	$3,$3,24
	addiu	$4,$fp,16
	addu	$2,$4,$2
	sb	$3,12($2)
	lw	$3,20($fp)
	lw	$2,24($fp)
	nop
	bne	$2,$0,1f
	div	$0,$3,$2
	break	7
1:
	mfhi	$2
	sw	$2,20($fp)
	lw	$3,24($fp)
	li	$2,10			# 0xa
	bne	$2,$0,1f
	div	$0,$3,$2
	break	7
1:
	mfhi	$3
	mflo	$2
	sw	$2,24($fp)
	lw	$2,16($fp)
	nop
	addiu	$2,$2,1
	sw	$2,16($fp)
	lw	$2,24($fp)
	nop
	bne	$2,$0,$L39
	nop

	addiu	$2,$fp,28
	li	$4,1			# 0x1
	move	$5,$2
	lw	$6,16($fp)
	jal	write
	nop

	move	$sp,$fp
	lw	$31,44($sp)
	lw	$fp,40($sp)
	addiu	$sp,$sp,48
	j	$31
	nop

	.set	macro
	.set	reorder
	.end	lt_print_int
	.size	lt_print_int, .-lt_print_int
	.align	2
	.globl	lt_getchar
	.set	nomips16
	.ent	lt_getchar
	.type	lt_getchar, @function
lt_getchar:
	.frame	$fp,32,$31		# vars= 8, regs= 2/0, args= 16, gp= 0
	.mask	0xc0000000,-4
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	
	addiu	$sp,$sp,-32
	sw	$31,28($sp)
	sw	$fp,24($sp)
	move	$fp,$sp
	li	$4,1			# 0x1
	jal	alloc_string
	nop

	sw	$2,16($fp)
	addiu	$2,$fp,20
	move	$4,$0
	move	$5,$2
	li	$6,1			# 0x1
	jal	read
	nop

	lbu	$2,20($fp)
	nop
	move	$3,$2
	lw	$2,16($fp)
	nop
	sw	$3,4($2)
	lw	$2,16($fp)
	move	$sp,$fp
	lw	$31,28($sp)
	lw	$fp,24($sp)
	addiu	$sp,$sp,32
	j	$31
	nop

	.set	macro
	.set	reorder
	.end	lt_getchar
	.size	lt_getchar, .-lt_getchar
	.align	2
	.globl	lt_ord
	.set	nomips16
	.ent	lt_ord
	.type	lt_ord, @function
lt_ord:
	.frame	$fp,8,$31		# vars= 0, regs= 1/0, args= 0, gp= 0
	.mask	0x40000000,-4
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	
	addiu	$sp,$sp,-8
	sw	$fp,4($sp)
	move	$fp,$sp
	sw	$4,8($fp)
	lw	$2,8($fp)
	nop
	lw	$2,0($2)
	nop
	beq	$2,$0,$L44
	nop

	lw	$2,8($fp)
	nop
	lw	$2,4($2)
	j	$L45
	nop

$L44:
	li	$2,-1			# 0xffffffffffffffff
$L45:
	move	$sp,$fp
	lw	$fp,4($sp)
	addiu	$sp,$sp,8
	j	$31
	nop

	.set	macro
	.set	reorder
	.end	lt_ord
	.size	lt_ord, .-lt_ord
	.align	2
	.globl	lt_chr
	.set	nomips16
	.ent	lt_chr
	.type	lt_chr, @function
lt_chr:
	.frame	$fp,32,$31		# vars= 8, regs= 2/0, args= 16, gp= 0
	.mask	0xc0000000,-4
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	
	addiu	$sp,$sp,-32
	sw	$31,28($sp)
	sw	$fp,24($sp)
	move	$fp,$sp
	sw	$4,32($fp)
	li	$4,1			# 0x1
	jal	alloc_string
	nop

	sw	$2,16($fp)
	lw	$2,16($fp)
	lw	$3,32($fp)
	nop
	sw	$3,4($2)
	move	$sp,$fp
	lw	$31,28($sp)
	lw	$fp,24($sp)
	addiu	$sp,$sp,32
	j	$31
	nop

	.set	macro
	.set	reorder
	.end	lt_chr
	.size	lt_chr, .-lt_chr
	.align	2
	.globl	lt_size
	.set	nomips16
	.ent	lt_size
	.type	lt_size, @function
lt_size:
	.frame	$fp,8,$31		# vars= 0, regs= 1/0, args= 0, gp= 0
	.mask	0x40000000,-4
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	
	addiu	$sp,$sp,-8
	sw	$fp,4($sp)
	move	$fp,$sp
	sw	$4,8($fp)
	lw	$2,8($fp)
	nop
	lw	$2,0($2)
	move	$sp,$fp
	lw	$fp,4($sp)
	addiu	$sp,$sp,8
	j	$31
	nop

	.set	macro
	.set	reorder
	.end	lt_size
	.size	lt_size, .-lt_size
	.align	2
	.globl	lt_substring
	.set	nomips16
	.ent	lt_substring
	.type	lt_substring, @function
lt_substring:
	.frame	$fp,32,$31		# vars= 8, regs= 2/0, args= 16, gp= 0
	.mask	0xc0000000,-4
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	
	addiu	$sp,$sp,-32
	sw	$31,28($sp)
	sw	$fp,24($sp)
	move	$fp,$sp
	sw	$4,32($fp)
	sw	$5,36($fp)
	sw	$6,40($fp)
	lw	$4,40($fp)
	jal	alloc_string
	nop

	sw	$2,20($fp)
	sw	$0,16($fp)
	j	$L52
	nop

$L53:
	lw	$5,16($fp)
	lw	$3,40($fp)
	lw	$2,16($fp)
	nop
	addu	$2,$3,$2
	lw	$3,32($fp)
	sll	$2,$2,2
	addu	$2,$3,$2
	lw	$3,4($2)
	lw	$4,20($fp)
	sll	$2,$5,2
	addu	$2,$4,$2
	sw	$3,4($2)
	lw	$2,16($fp)
	nop
	addiu	$2,$2,1
	sw	$2,16($fp)
$L52:
	lw	$3,16($fp)
	lw	$2,40($fp)
	nop
	slt	$2,$3,$2
	bne	$2,$0,$L53
	nop

	lw	$2,20($fp)
	move	$sp,$fp
	lw	$31,28($sp)
	lw	$fp,24($sp)
	addiu	$sp,$sp,32
	j	$31
	nop

	.set	macro
	.set	reorder
	.end	lt_substring
	.size	lt_substring, .-lt_substring
	.align	2
	.globl	lt_concat
	.set	nomips16
	.ent	lt_concat
	.type	lt_concat, @function
lt_concat:
	.frame	$fp,48,$31		# vars= 24, regs= 2/0, args= 16, gp= 0
	.mask	0xc0000000,-4
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	
	addiu	$sp,$sp,-48
	sw	$31,44($sp)
	sw	$fp,40($sp)
	move	$fp,$sp
	sw	$4,48($fp)
	sw	$5,52($fp)
	lw	$2,48($fp)
	nop
	lw	$2,0($2)
	nop
	sw	$2,32($fp)
	lw	$2,52($fp)
	nop
	lw	$2,0($2)
	nop
	sw	$2,28($fp)
	lw	$3,32($fp)
	lw	$2,28($fp)
	nop
	addu	$2,$3,$2
	sw	$2,24($fp)
	lw	$4,24($fp)
	jal	alloc_string
	nop

	sw	$2,20($fp)
	sw	$0,16($fp)
	j	$L56
	nop

$L57:
	lw	$5,16($fp)
	lw	$2,16($fp)
	lw	$3,48($fp)
	sll	$2,$2,2
	addu	$2,$3,$2
	lw	$3,4($2)
	lw	$4,20($fp)
	sll	$2,$5,2
	addu	$2,$4,$2
	sw	$3,4($2)
	lw	$2,16($fp)
	nop
	addiu	$2,$2,1
	sw	$2,16($fp)
$L56:
	lw	$3,16($fp)
	lw	$2,32($fp)
	nop
	slt	$2,$3,$2
	bne	$2,$0,$L57
	nop

	sw	$0,16($fp)
	j	$L58
	nop

$L59:
	lw	$3,16($fp)
	lw	$2,32($fp)
	nop
	addu	$5,$3,$2
	lw	$2,16($fp)
	lw	$3,52($fp)
	sll	$2,$2,2
	addu	$2,$3,$2
	lw	$3,4($2)
	lw	$4,20($fp)
	sll	$2,$5,2
	addu	$2,$4,$2
	sw	$3,4($2)
	lw	$2,16($fp)
	nop
	addiu	$2,$2,1
	sw	$2,16($fp)
$L58:
	lw	$3,16($fp)
	lw	$2,28($fp)
	nop
	slt	$2,$3,$2
	bne	$2,$0,$L59
	nop

	lw	$2,20($fp)
	move	$sp,$fp
	lw	$31,44($sp)
	lw	$fp,40($sp)
	addiu	$sp,$sp,48
	j	$31
	nop

	.set	macro
	.set	reorder
	.end	lt_concat
	.size	lt_concat, .-lt_concat
	.align	2
	.globl	lt_not
	.set	nomips16
	.ent	lt_not
	.type	lt_not, @function
lt_not:
	.frame	$fp,8,$31		# vars= 0, regs= 1/0, args= 0, gp= 0
	.mask	0x40000000,-4
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	
	addiu	$sp,$sp,-8
	sw	$fp,4($sp)
	move	$fp,$sp
	sw	$4,8($fp)
	lw	$2,8($fp)
	nop
	beq	$2,$0,$L62
	nop

	move	$2,$0
	j	$L63
	nop

$L62:
	li	$2,1			# 0x1
$L63:
	move	$sp,$fp
	lw	$fp,4($sp)
	addiu	$sp,$sp,8
	j	$31
	nop

	.set	macro
	.set	reorder
	.end	lt_not
	.size	lt_not, .-lt_not
	.align	2
	.globl	lt_exit
	.set	nomips16
	.ent	lt_exit
	.type	lt_exit, @function
lt_exit:
	.frame	$fp,24,$31		# vars= 0, regs= 2/0, args= 16, gp= 0
	.mask	0xc0000000,-4
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	
	addiu	$sp,$sp,-24
	sw	$31,20($sp)
	sw	$fp,16($sp)
	move	$fp,$sp
	sw	$4,24($fp)
	lw	$4,24($fp)
	jal	exit
	nop

	.set	macro
	.set	reorder
	.end	lt_exit
	.size	lt_exit, .-lt_exit
	.align	2
	.globl	lt_flush
	.set	nomips16
	.ent	lt_flush
	.type	lt_flush, @function
lt_flush:
	.frame	$fp,8,$31		# vars= 0, regs= 1/0, args= 0, gp= 0
	.mask	0x40000000,-4
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	
	addiu	$sp,$sp,-8
	sw	$fp,4($sp)
	move	$fp,$sp
	move	$sp,$fp
	lw	$fp,4($sp)
	addiu	$sp,$sp,8
	j	$31
	nop

	.set	macro
	.set	reorder
	.end	lt_flush
	.size	lt_flush, .-lt_flush
	.align	2
	.globl	call_test
	.set	nomips16
	.ent	call_test
	.type	call_test, @function
call_test:
	.frame	$fp,32,$31		# vars= 8, regs= 2/0, args= 16, gp= 0
	.mask	0xc0000000,-4
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	
	addiu	$sp,$sp,-32
	sw	$31,28($sp)
	sw	$fp,24($sp)
	move	$fp,$sp
	sw	$4,32($fp)
	sw	$5,36($fp)
	sw	$6,40($fp)
	sw	$7,44($fp)
	lui	$2,%hi($LC4)
	addiu	$2,$2,%lo($LC4)
	sw	$2,16($fp)
	li	$4,1			# 0x1
	lw	$5,16($fp)
	li	$6,1			# 0x1
	jal	write
	nop

	lw	$4,32($fp)
	jal	lt_print_int
	nop

	li	$4,1			# 0x1
	lw	$5,16($fp)
	li	$6,1			# 0x1
	jal	write
	nop

	lw	$4,36($fp)
	jal	lt_print_int
	nop

	li	$4,1			# 0x1
	lw	$5,16($fp)
	li	$6,1			# 0x1
	jal	write
	nop

	lw	$4,40($fp)
	jal	lt_print_int
	nop

	li	$4,1			# 0x1
	lw	$5,16($fp)
	li	$6,1			# 0x1
	jal	write
	nop

	lw	$4,44($fp)
	jal	lt_print_int
	nop

	li	$4,1			# 0x1
	lw	$5,16($fp)
	li	$6,1			# 0x1
	jal	write
	nop

	lw	$4,48($fp)
	jal	lt_print_int
	nop

	li	$4,1			# 0x1
	lw	$5,16($fp)
	li	$6,1			# 0x1
	jal	write
	nop

	lw	$4,52($fp)
	jal	lt_print_int
	nop

	move	$sp,$fp
	lw	$31,28($sp)
	lw	$fp,24($sp)
	addiu	$sp,$sp,32
	j	$31
	nop

	.set	macro
	.set	reorder
	.end	call_test
	.size	call_test, .-call_test
	.ident	"GCC: (GNU) 4.4.3"
