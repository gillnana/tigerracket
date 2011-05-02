.globl main
.text


#      begin def main
main:
sub $sp, $sp, 4  #push $ra
sw $ra, ($sp)
li $a0, 26
li $a1, 0
sub $sp, $sp, 16
jal alloc_block
add $sp, $sp, 16
sw $s0, ($v0) # put the static parent's activation record in the first slot of the new activation record
move $s0, $v0 # set the current activation record to be the new one
#      done setting up activation record
la $a0, lt_print_int
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #(struct:label-loc lt640) #(struct:label main))
move $t3, $s0
sw $v0, 4($t3)
#             end   (lnstore $v0 #(struct:label-loc lt640) #(struct:label main))
la $a0, lt_exit
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #(struct:label-loc lt639) #(struct:label main))
move $t3, $s0
sw $v0, 8($t3)
#             end   (lnstore $v0 #(struct:label-loc lt639) #(struct:label main))
la $a0, lt_not
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #(struct:label-loc lt638) #(struct:label main))
move $t3, $s0
sw $v0, 12($t3)
#             end   (lnstore $v0 #(struct:label-loc lt638) #(struct:label main))
la $a0, lt_concat
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #(struct:label-loc lt637) #(struct:label main))
move $t3, $s0
sw $v0, 16($t3)
#             end   (lnstore $v0 #(struct:label-loc lt637) #(struct:label main))
la $a0, lt_substring
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #(struct:label-loc lt636) #(struct:label main))
move $t3, $s0
sw $v0, 20($t3)
#             end   (lnstore $v0 #(struct:label-loc lt636) #(struct:label main))
la $a0, lt_size
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #(struct:label-loc lt635) #(struct:label main))
move $t3, $s0
sw $v0, 24($t3)
#             end   (lnstore $v0 #(struct:label-loc lt635) #(struct:label main))
la $a0, lt_chr
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #(struct:label-loc lt634) #(struct:label main))
move $t3, $s0
sw $v0, 28($t3)
#             end   (lnstore $v0 #(struct:label-loc lt634) #(struct:label main))
la $a0, lt_ord
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #(struct:label-loc lt633) #(struct:label main))
move $t3, $s0
sw $v0, 32($t3)
#             end   (lnstore $v0 #(struct:label-loc lt633) #(struct:label main))
la $a0, lt_getchar
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #(struct:label-loc lt632) #(struct:label main))
move $t3, $s0
sw $v0, 36($t3)
#             end   (lnstore $v0 #(struct:label-loc lt632) #(struct:label main))
la $a0, lt_print
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #(struct:label-loc lt631) #(struct:label main))
move $t3, $s0
sw $v0, 40($t3)
#             end   (lnstore $v0 #(struct:label-loc lt631) #(struct:label main))
#            begin (lnload #(struct:temp-loc t642) $a0 #(struct:label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 12
lw $a0, 52($t3)
#            end (lnload #(struct:temp-loc t642) $a0 #(struct:label main))
li $a1, 0
sub $sp, $sp, 16
jal alloc_block
add $sp, $sp, 16
#             begin (lnstore $v0 #(struct:mem-loc m643) #(struct:label main))
move $t3, $s0
sw $v0, 48($t3)
#             end   (lnstore $v0 #(struct:mem-loc m643) #(struct:label main))
li $t0, 5
#             begin (lnstore $t0 #(struct:temp-loc t645) #(struct:label main))
move $t3, $s0
sw $t0, 60($t3)
#             end   (lnstore $t0 #(struct:temp-loc t645) #(struct:label main))
li $t0, 0
#             begin (lnstore $t0 #(struct:mem-loc m644) #(struct:label main))
move $t3, $s0
sw $t0, 56($t3)
#             end   (lnstore $t0 #(struct:mem-loc m644) #(struct:label main))
#   BEGIN binop op=+, src1=#(struct:mem-loc m643), src2=#(struct:mem-loc m644), dest=#(struct:mem-loc m644)
#            begin (lnload #(struct:mem-loc m643) $t1 #(struct:label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 11
lw $t1, 48($t3)
#            end (lnload #(struct:mem-loc m643) $t1 #(struct:label main))
#            begin (lnload #(struct:mem-loc m644) $t2 #(struct:label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 13
lw $t2, 56($t3)
#            end (lnload #(struct:mem-loc m644) $t2 #(struct:label main))
add $t0, $t1, $t2
#             begin (lnstore $t0 #(struct:mem-loc m644) #(struct:label main))
move $t3, $s0
sw $t0, 56($t3)
#             end   (lnstore $t0 #(struct:mem-loc m644) #(struct:label main))
#   END   binop op=+, src1=#(struct:mem-loc m643), src2=#(struct:mem-loc m644), dest=#(struct:mem-loc m644)
#  DEREF ASSIGN INS #(struct:mem-loc m644) #(struct:temp-loc t645)
#            begin (lnload #(struct:mem-loc m644) $t1 #(struct:label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 13
lw $t1, 56($t3)
#            end (lnload #(struct:mem-loc m644) $t1 #(struct:label main))
#            begin (lnload #(struct:temp-loc t645) $t2 #(struct:label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 14
lw $t2, 60($t3)
#            end (lnload #(struct:temp-loc t645) $t2 #(struct:label main))
sw $t2, ($t1)
li $t0, 7
#             begin (lnstore $t0 #(struct:temp-loc t645) #(struct:label main))
move $t3, $s0
sw $t0, 60($t3)
#             end   (lnstore $t0 #(struct:temp-loc t645) #(struct:label main))
li $t0, 1
#             begin (lnstore $t0 #(struct:mem-loc m644) #(struct:label main))
move $t3, $s0
sw $t0, 56($t3)
#             end   (lnstore $t0 #(struct:mem-loc m644) #(struct:label main))
#   BEGIN binop op=+, src1=#(struct:mem-loc m643), src2=#(struct:mem-loc m644), dest=#(struct:mem-loc m644)
#            begin (lnload #(struct:mem-loc m643) $t1 #(struct:label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 11
lw $t1, 48($t3)
#            end (lnload #(struct:mem-loc m643) $t1 #(struct:label main))
#            begin (lnload #(struct:mem-loc m644) $t2 #(struct:label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 13
lw $t2, 56($t3)
#            end (lnload #(struct:mem-loc m644) $t2 #(struct:label main))
add $t0, $t1, $t2
#             begin (lnstore $t0 #(struct:mem-loc m644) #(struct:label main))
move $t3, $s0
sw $t0, 56($t3)
#             end   (lnstore $t0 #(struct:mem-loc m644) #(struct:label main))
#   END   binop op=+, src1=#(struct:mem-loc m643), src2=#(struct:mem-loc m644), dest=#(struct:mem-loc m644)
#  DEREF ASSIGN INS #(struct:mem-loc m644) #(struct:temp-loc t645)
#            begin (lnload #(struct:mem-loc m644) $t1 #(struct:label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 13
lw $t1, 56($t3)
#            end (lnload #(struct:mem-loc m644) $t1 #(struct:label main))
#            begin (lnload #(struct:temp-loc t645) $t2 #(struct:label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 14
lw $t2, 60($t3)
#            end (lnload #(struct:temp-loc t645) $t2 #(struct:label main))
sw $t2, ($t1)
#            begin (lnload #(struct:mem-loc m643) $t0 #(struct:label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 11
lw $t0, 48($t3)
#            end (lnload #(struct:mem-loc m643) $t0 #(struct:label main))
#             begin (lnstore $t0 #(struct:temp-loc t641) #(struct:label main))
move $t3, $s0
sw $t0, 44($t3)
#             end   (lnstore $t0 #(struct:temp-loc t641) #(struct:label main))
#            begin (lnload #(struct:label-loc lt640) $t0 #(struct:label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 0
lw $t0, 4($t3)
#            end (lnload #(struct:label-loc lt640) $t0 #(struct:label main))
#             begin (lnstore $t0 #(struct:label-loc lt646) #(struct:label main))
move $t3, $s0
sw $t0, 64($t3)
#             end   (lnstore $t0 #(struct:label-loc lt646) #(struct:label main))
#               begin (lnref #(struct:temp-loc t641) $t0 #(struct:label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 10
add $t0, $t3, 44
#             begin (lnstore $t0 #(struct:mem-loc m651) #(struct:label main))
move $t3, $s0
sw $t0, 80($t3)
#             end   (lnstore $t0 #(struct:mem-loc m651) #(struct:label main))
li $t0, 0
#             begin (lnstore $t0 #(struct:mem-loc m650) #(struct:label main))
move $t3, $s0
sw $t0, 76($t3)
#             end   (lnstore $t0 #(struct:mem-loc m650) #(struct:label main))
#   BEGIN binop op=+, src1=#(struct:mem-loc m651), src2=#(struct:mem-loc m650), dest=#(struct:mem-loc m650)
#            begin (lnload #(struct:mem-loc m651) $t1 #(struct:label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 19
lw $t1, 80($t3)
#            end (lnload #(struct:mem-loc m651) $t1 #(struct:label main))
#            begin (lnload #(struct:mem-loc m650) $t2 #(struct:label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 18
lw $t2, 76($t3)
#            end (lnload #(struct:mem-loc m650) $t2 #(struct:label main))
add $t0, $t1, $t2
#             begin (lnstore $t0 #(struct:mem-loc m650) #(struct:label main))
move $t3, $s0
sw $t0, 76($t3)
#             end   (lnstore $t0 #(struct:mem-loc m650) #(struct:label main))
#   END   binop op=+, src1=#(struct:mem-loc m651), src2=#(struct:mem-loc m650), dest=#(struct:mem-loc m650)
#            begin (lnload #(struct:mem-loc m650) $t0 #(struct:label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 18
lw $t0, 76($t3)
#            end (lnload #(struct:mem-loc m650) $t0 #(struct:label main))
lw $t0, ($t0)
#             begin (lnstore $t0 #(struct:temp-loc t647) #(struct:label main))
move $t3, $s0
sw $t0, 72($t3)
#             end   (lnstore $t0 #(struct:temp-loc t647) #(struct:label main))
#     BEGIN FUNCALL labloc=#(struct:label-loc lt646), args=(#(struct:temp-loc t647)), dest=ans
sub $sp, $sp, 4  #push $s0
sw $s0, ($sp)
sub $sp, $sp, 16
#            begin (lnload #(struct:temp-loc t647) $a0 #(struct:label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 17
lw $a0, 72($t3)
#            end (lnload #(struct:temp-loc t647) $a0 #(struct:label main))
#            begin (lnload #(struct:temp-loc t647) $t0 #(struct:label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 17
lw $t0, 72($t3)
#            end (lnload #(struct:temp-loc t647) $t0 #(struct:label main))
sw $t0, 0($sp)
#            begin (lnload #(struct:label-loc lt646) $t2 #(struct:label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 15
lw $t2, 64($t3)
#            end (lnload #(struct:label-loc lt646) $t2 #(struct:label main))
lw $s0, 4($t2)
lw $t2, ($t2)
jalr $t2
add $sp, $sp, 16
lw $s0, ($sp)
add $sp, $sp, 4  #pop $s0
#             begin (lnstore $v0 ans #(struct:label main))
#ignore store into ans
#             end   (lnstore $v0 ans #(struct:label main))
#     END   FUNCALL labloc=#(struct:label-loc lt646), args=(#(struct:temp-loc t647)), dest=ans
#            begin (lnload #(struct:label-loc lt640) $t0 #(struct:label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 0
lw $t0, 4($t3)
#            end (lnload #(struct:label-loc lt640) $t0 #(struct:label main))
#             begin (lnstore $t0 #(struct:label-loc lt652) #(struct:label main))
move $t3, $s0
sw $t0, 84($t3)
#             end   (lnstore $t0 #(struct:label-loc lt652) #(struct:label main))
#               begin (lnref #(struct:temp-loc t641) $t0 #(struct:label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 10
add $t0, $t3, 44
#             begin (lnstore $t0 #(struct:mem-loc m657) #(struct:label main))
move $t3, $s0
sw $t0, 100($t3)
#             end   (lnstore $t0 #(struct:mem-loc m657) #(struct:label main))
li $t0, 1
#             begin (lnstore $t0 #(struct:mem-loc m656) #(struct:label main))
move $t3, $s0
sw $t0, 96($t3)
#             end   (lnstore $t0 #(struct:mem-loc m656) #(struct:label main))
#   BEGIN binop op=+, src1=#(struct:mem-loc m657), src2=#(struct:mem-loc m656), dest=#(struct:mem-loc m656)
#            begin (lnload #(struct:mem-loc m657) $t1 #(struct:label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 24
lw $t1, 100($t3)
#            end (lnload #(struct:mem-loc m657) $t1 #(struct:label main))
#            begin (lnload #(struct:mem-loc m656) $t2 #(struct:label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 23
lw $t2, 96($t3)
#            end (lnload #(struct:mem-loc m656) $t2 #(struct:label main))
add $t0, $t1, $t2
#             begin (lnstore $t0 #(struct:mem-loc m656) #(struct:label main))
move $t3, $s0
sw $t0, 96($t3)
#             end   (lnstore $t0 #(struct:mem-loc m656) #(struct:label main))
#   END   binop op=+, src1=#(struct:mem-loc m657), src2=#(struct:mem-loc m656), dest=#(struct:mem-loc m656)
#            begin (lnload #(struct:mem-loc m656) $t0 #(struct:label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 23
lw $t0, 96($t3)
#            end (lnload #(struct:mem-loc m656) $t0 #(struct:label main))
lw $t0, ($t0)
#             begin (lnstore $t0 #(struct:temp-loc t653) #(struct:label main))
move $t3, $s0
sw $t0, 92($t3)
#             end   (lnstore $t0 #(struct:temp-loc t653) #(struct:label main))
#     BEGIN FUNCALL labloc=#(struct:label-loc lt652), args=(#(struct:temp-loc t653)), dest=ans
sub $sp, $sp, 4  #push $s0
sw $s0, ($sp)
sub $sp, $sp, 16
#            begin (lnload #(struct:temp-loc t653) $a0 #(struct:label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 22
lw $a0, 92($t3)
#            end (lnload #(struct:temp-loc t653) $a0 #(struct:label main))
#            begin (lnload #(struct:temp-loc t653) $t0 #(struct:label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 22
lw $t0, 92($t3)
#            end (lnload #(struct:temp-loc t653) $t0 #(struct:label main))
sw $t0, 0($sp)
#            begin (lnload #(struct:label-loc lt652) $t2 #(struct:label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 20
lw $t2, 84($t3)
#            end (lnload #(struct:label-loc lt652) $t2 #(struct:label main))
lw $s0, 4($t2)
lw $t2, ($t2)
jalr $t2
add $sp, $sp, 16
lw $s0, ($sp)
add $sp, $sp, 4  #pop $s0
#             begin (lnstore $v0 ans #(struct:label main))
#ignore store into ans
#             end   (lnstore $v0 ans #(struct:label main))
#     END   FUNCALL labloc=#(struct:label-loc lt652), args=(#(struct:temp-loc t653)), dest=ans
lw $ra, ($sp)
add $sp, $sp, 4  #pop $ra
jr $ra
#     end def main

