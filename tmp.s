.globl main
.text


#      begin def main
main:
sub $sp, $sp, 4  #push $ra
sw $ra, ($sp)
li $a0, 25
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
#             begin (lnstore $v0 #(struct:label-loc lt638) #(struct:label main))
move $t3, $s0
sw $v0, 4($t3)
#             end   (lnstore $v0 #(struct:label-loc lt638) #(struct:label main))
la $a0, lt_exit
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #(struct:label-loc lt637) #(struct:label main))
move $t3, $s0
sw $v0, 8($t3)
#             end   (lnstore $v0 #(struct:label-loc lt637) #(struct:label main))
la $a0, lt_not
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #(struct:label-loc lt636) #(struct:label main))
move $t3, $s0
sw $v0, 12($t3)
#             end   (lnstore $v0 #(struct:label-loc lt636) #(struct:label main))
la $a0, lt_concat
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #(struct:label-loc lt635) #(struct:label main))
move $t3, $s0
sw $v0, 16($t3)
#             end   (lnstore $v0 #(struct:label-loc lt635) #(struct:label main))
la $a0, lt_substring
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #(struct:label-loc lt634) #(struct:label main))
move $t3, $s0
sw $v0, 20($t3)
#             end   (lnstore $v0 #(struct:label-loc lt634) #(struct:label main))
la $a0, lt_size
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #(struct:label-loc lt633) #(struct:label main))
move $t3, $s0
sw $v0, 24($t3)
#             end   (lnstore $v0 #(struct:label-loc lt633) #(struct:label main))
la $a0, lt_chr
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #(struct:label-loc lt632) #(struct:label main))
move $t3, $s0
sw $v0, 28($t3)
#             end   (lnstore $v0 #(struct:label-loc lt632) #(struct:label main))
la $a0, lt_ord
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #(struct:label-loc lt631) #(struct:label main))
move $t3, $s0
sw $v0, 32($t3)
#             end   (lnstore $v0 #(struct:label-loc lt631) #(struct:label main))
la $a0, lt_getchar
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #(struct:label-loc lt630) #(struct:label main))
move $t3, $s0
sw $v0, 36($t3)
#             end   (lnstore $v0 #(struct:label-loc lt630) #(struct:label main))
la $a0, lt_print
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #(struct:label-loc lt629) #(struct:label main))
move $t3, $s0
sw $v0, 40($t3)
#             end   (lnstore $v0 #(struct:label-loc lt629) #(struct:label main))
la $a0, lt_print_int
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #(struct:label-loc lt648) #(struct:label main))
move $t3, $s0
sw $v0, 44($t3)
#             end   (lnstore $v0 #(struct:label-loc lt648) #(struct:label main))
la $a0, lt_exit
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #(struct:label-loc lt647) #(struct:label main))
move $t3, $s0
sw $v0, 48($t3)
#             end   (lnstore $v0 #(struct:label-loc lt647) #(struct:label main))
la $a0, lt_not
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #(struct:label-loc lt646) #(struct:label main))
move $t3, $s0
sw $v0, 52($t3)
#             end   (lnstore $v0 #(struct:label-loc lt646) #(struct:label main))
la $a0, lt_concat
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #(struct:label-loc lt645) #(struct:label main))
move $t3, $s0
sw $v0, 56($t3)
#             end   (lnstore $v0 #(struct:label-loc lt645) #(struct:label main))
la $a0, lt_substring
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #(struct:label-loc lt644) #(struct:label main))
move $t3, $s0
sw $v0, 60($t3)
#             end   (lnstore $v0 #(struct:label-loc lt644) #(struct:label main))
la $a0, lt_size
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #(struct:label-loc lt643) #(struct:label main))
move $t3, $s0
sw $v0, 64($t3)
#             end   (lnstore $v0 #(struct:label-loc lt643) #(struct:label main))
la $a0, lt_chr
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #(struct:label-loc lt642) #(struct:label main))
move $t3, $s0
sw $v0, 68($t3)
#             end   (lnstore $v0 #(struct:label-loc lt642) #(struct:label main))
la $a0, lt_ord
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #(struct:label-loc lt641) #(struct:label main))
move $t3, $s0
sw $v0, 72($t3)
#             end   (lnstore $v0 #(struct:label-loc lt641) #(struct:label main))
la $a0, lt_getchar
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #(struct:label-loc lt640) #(struct:label main))
move $t3, $s0
sw $v0, 76($t3)
#             end   (lnstore $v0 #(struct:label-loc lt640) #(struct:label main))
la $a0, lt_print
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #(struct:label-loc lt639) #(struct:label main))
move $t3, $s0
sw $v0, 80($t3)
#             end   (lnstore $v0 #(struct:label-loc lt639) #(struct:label main))
la $a0, l649
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #(struct:label-loc lt650) #(struct:label main))
move $t3, $s0
sw $v0, 84($t3)
#             end   (lnstore $v0 #(struct:label-loc lt650) #(struct:label main))
#     BEGIN FUNCALL labloc=#(struct:label-loc lt650), args=(), dest=#(struct:temp-loc t652)
sub $sp, $sp, 4  #push $s0
sw $s0, ($sp)
sub $sp, $sp, 16
#            begin (lnload #(struct:label-loc lt650) $t2 #(struct:label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 20
lw $t2, 84($t3)
#            end (lnload #(struct:label-loc lt650) $t2 #(struct:label main))
lw $s0, 4($t2)
lw $t2, ($t2)
jalr $t2
add $sp, $sp, 16
lw $s0, ($sp)
add $sp, $sp, 4  #pop $s0
#             begin (lnstore $v0 #(struct:temp-loc t652) #(struct:label main))
move $t3, $s0
sw $v0, 92($t3)
#             end   (lnstore $v0 #(struct:temp-loc t652) #(struct:label main))
#     END   FUNCALL labloc=#(struct:label-loc lt650), args=(), dest=#(struct:temp-loc t652)
#     BEGIN FUNCALL labloc=#(struct:label-loc lt648), args=(#(struct:temp-loc t652)), dest=ans
sub $sp, $sp, 4  #push $s0
sw $s0, ($sp)
sub $sp, $sp, 16
#            begin (lnload #(struct:temp-loc t652) $t0 #(struct:label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 22
lw $t0, 92($t3)
#            end (lnload #(struct:temp-loc t652) $t0 #(struct:label main))
sw $t0, 0($sp)
#            begin (lnload #(struct:label-loc lt648) $t2 #(struct:label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 10
lw $t2, 44($t3)
#            end (lnload #(struct:label-loc lt648) $t2 #(struct:label main))
lw $s0, 4($t2)
lw $t2, ($t2)
jalr $t2
add $sp, $sp, 16
lw $s0, ($sp)
add $sp, $sp, 4  #pop $s0
#             begin (lnstore $v0 ans #(struct:label main))
#ignore store into ans
#             end   (lnstore $v0 ans #(struct:label main))
#     END   FUNCALL labloc=#(struct:label-loc lt648), args=(#(struct:temp-loc t652)), dest=ans
lw $ra, ($sp)
add $sp, $sp, 4  #pop $ra
jr $ra
#     end def main



#      begin def l649
l649:
sub $sp, $sp, 4  #push $ra
sw $ra, ($sp)
li $a0, 2
li $a1, 0
sub $sp, $sp, 16
jal alloc_block
add $sp, $sp, 16
sw $s0, ($v0) # put the static parent's activation record in the first slot of the new activation record
move $s0, $v0 # set the current activation record to be the new one
#      done setting up activation record
li $t0, 12
#             begin (lnstore $t0 #(struct:temp-loc t651) #(struct:label l649))
move $t3, $s0
sw $t0, 4($t3)
#             end   (lnstore $t0 #(struct:temp-loc t651) #(struct:label l649))
#            begin (lnload #(struct:temp-loc t651) $v0 #(struct:label l649))
move $t3, $s0 # loading variable at nest-depth 0 and offset 0
lw $v0, 4($t3)
#            end (lnload #(struct:temp-loc t651) $v0 #(struct:label l649))
lw $ra, ($sp)
add $sp, $sp, 4  #pop $ra
jr $ra
#     end def l649

