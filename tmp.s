.globl main
.text


#      begin def main
main:
sub $sp, $sp, 4  #push $ra
sw $ra, ($sp)
li $a0, 31
li $a1, 0
sub $sp, $sp, 16
jal alloc_block
add $sp, $sp, 16
sw $s0, ($v0) # put the static parent's activation record in the first slot of the new activation record
move $s0, $v0 # set the current activation record to be the new one
mainrecur:
#      done setting up activation record
la $a0, lt_print_int
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #s(label-loc lt706) #s(label main))
move $t3, $s0
sw $v0, 4($t3)
#             end   (lnstore $v0 #s(label-loc lt706) #s(label main))
la $a0, lt_exit
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #s(label-loc lt705) #s(label main))
move $t3, $s0
sw $v0, 8($t3)
#             end   (lnstore $v0 #s(label-loc lt705) #s(label main))
la $a0, lt_not
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #s(label-loc lt704) #s(label main))
move $t3, $s0
sw $v0, 12($t3)
#             end   (lnstore $v0 #s(label-loc lt704) #s(label main))
la $a0, lt_concat
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #s(label-loc lt703) #s(label main))
move $t3, $s0
sw $v0, 16($t3)
#             end   (lnstore $v0 #s(label-loc lt703) #s(label main))
la $a0, lt_substring
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #s(label-loc lt702) #s(label main))
move $t3, $s0
sw $v0, 20($t3)
#             end   (lnstore $v0 #s(label-loc lt702) #s(label main))
la $a0, lt_size
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #s(label-loc lt701) #s(label main))
move $t3, $s0
sw $v0, 24($t3)
#             end   (lnstore $v0 #s(label-loc lt701) #s(label main))
la $a0, lt_chr
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #s(label-loc lt700) #s(label main))
move $t3, $s0
sw $v0, 28($t3)
#             end   (lnstore $v0 #s(label-loc lt700) #s(label main))
la $a0, lt_ord
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #s(label-loc lt699) #s(label main))
move $t3, $s0
sw $v0, 32($t3)
#             end   (lnstore $v0 #s(label-loc lt699) #s(label main))
la $a0, lt_getchar
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #s(label-loc lt698) #s(label main))
move $t3, $s0
sw $v0, 36($t3)
#             end   (lnstore $v0 #s(label-loc lt698) #s(label main))
la $a0, lt_flush
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #s(label-loc lt697) #s(label main))
move $t3, $s0
sw $v0, 40($t3)
#             end   (lnstore $v0 #s(label-loc lt697) #s(label main))
la $a0, lt_print
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #s(label-loc lt696) #s(label main))
move $t3, $s0
sw $v0, 44($t3)
#             end   (lnstore $v0 #s(label-loc lt696) #s(label main))
li $t0, 3
#             begin (lnstore $t0 #s(temp-loc t709) #s(label main))
move $t3, $s0
sw $t0, 56($t3)
#             end   (lnstore $t0 #s(temp-loc t709) #s(label main))
li $t0, 0
#             begin (lnstore $t0 #s(temp-loc t710) #s(label main))
move $t3, $s0
sw $t0, 60($t3)
#             end   (lnstore $t0 #s(temp-loc t710) #s(label main))
#            begin (lnload #s(temp-loc t709) $a0 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 13
lw $a0, 56($t3)
#            end (lnload #s(temp-loc t709) $a0 #s(label main))
#            begin (lnload #s(temp-loc t710) $a1 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 14
lw $a1, 60($t3)
#            end (lnload #s(temp-loc t710) $a1 #s(label main))
sub $sp, $sp, 16
jal alloc_array
add $sp, $sp, 16
#             begin (lnstore $v0 #s(temp-loc t708) #s(label main))
move $t3, $s0
sw $v0, 52($t3)
#             end   (lnstore $v0 #s(temp-loc t708) #s(label main))
#               begin (lnref #s(temp-loc t708) $t0 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 12
add $t0, $t3, 52
#             begin (lnstore $t0 #s(mem-loc m712) #s(label main))
move $t3, $s0
sw $t0, 72($t3)
#             end   (lnstore $t0 #s(mem-loc m712) #s(label main))
#            begin (lnload #s(mem-loc m712) $a0 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 17
lw $a0, 72($t3)
#            end (lnload #s(mem-loc m712) $a0 #s(label main))
sub $sp, $sp, 16
jal assert_nonnil
add $sp, $sp, 16
#            begin (lnload #s(mem-loc m712) $t0 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 17
lw $t0, 72($t3)
#            end (lnload #s(mem-loc m712) $t0 #s(label main))
lw $t0, ($t0)
#             begin (lnstore $t0 #s(mem-loc m712) #s(label main))
move $t3, $s0
sw $t0, 72($t3)
#             end   (lnstore $t0 #s(mem-loc m712) #s(label main))
#            begin (lnload #s(temp-loc t710) $t0 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 14
lw $t0, 60($t3)
#            end (lnload #s(temp-loc t710) $t0 #s(label main))
#             begin (lnstore $t0 #s(temp-loc t713) #s(label main))
move $t3, $s0
sw $t0, 76($t3)
#             end   (lnstore $t0 #s(temp-loc t713) #s(label main))
li $t0, 4
#             begin (lnstore $t0 #s(mem-loc m711) #s(label main))
move $t3, $s0
sw $t0, 64($t3)
#             end   (lnstore $t0 #s(mem-loc m711) #s(label main))
#   BEGIN binop op=*, src1=#s(temp-loc t713), src2=#s(mem-loc m711), dest=#s(temp-loc t713)
#            begin (lnload #s(temp-loc t713) $t1 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 18
lw $t1, 76($t3)
#            end (lnload #s(temp-loc t713) $t1 #s(label main))
#            begin (lnload #s(mem-loc m711) $t2 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 15
lw $t2, 64($t3)
#            end (lnload #s(mem-loc m711) $t2 #s(label main))
mul $t0, $t1, $t2
#             begin (lnstore $t0 #s(temp-loc t713) #s(label main))
move $t3, $s0
sw $t0, 76($t3)
#             end   (lnstore $t0 #s(temp-loc t713) #s(label main))
#   END   binop op=*, src1=#s(temp-loc t713), src2=#s(mem-loc m711), dest=#s(temp-loc t713)
#   BEGIN binop op=+, src1=#s(mem-loc m712), src2=#s(mem-loc m711), dest=#s(mem-loc m711)
#            begin (lnload #s(mem-loc m712) $t1 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 17
lw $t1, 72($t3)
#            end (lnload #s(mem-loc m712) $t1 #s(label main))
#            begin (lnload #s(mem-loc m711) $t2 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 15
lw $t2, 64($t3)
#            end (lnload #s(mem-loc m711) $t2 #s(label main))
add $t0, $t1, $t2
#             begin (lnstore $t0 #s(mem-loc m711) #s(label main))
move $t3, $s0
sw $t0, 64($t3)
#             end   (lnstore $t0 #s(mem-loc m711) #s(label main))
#   END   binop op=+, src1=#s(mem-loc m712), src2=#s(mem-loc m711), dest=#s(mem-loc m711)
#   BEGIN binop op=+, src1=#s(temp-loc t713), src2=#s(mem-loc m711), dest=#s(mem-loc m711)
#            begin (lnload #s(temp-loc t713) $t1 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 18
lw $t1, 76($t3)
#            end (lnload #s(temp-loc t713) $t1 #s(label main))
#            begin (lnload #s(mem-loc m711) $t2 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 15
lw $t2, 64($t3)
#            end (lnload #s(mem-loc m711) $t2 #s(label main))
add $t0, $t1, $t2
#             begin (lnstore $t0 #s(mem-loc m711) #s(label main))
move $t3, $s0
sw $t0, 64($t3)
#             end   (lnstore $t0 #s(mem-loc m711) #s(label main))
#   END   binop op=+, src1=#s(temp-loc t713), src2=#s(mem-loc m711), dest=#s(mem-loc m711)
#        start array bounds check
#            begin (lnload #s(mem-loc m712) $a0 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 17
lw $a0, 72($t3)
#            end (lnload #s(mem-loc m712) $a0 #s(label main))
#            begin (lnload #s(mem-loc m711) $a1 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 15
lw $a1, 64($t3)
#            end (lnload #s(mem-loc m711) $a1 #s(label main))
sub $sp, $sp, 16
jal assert_inbounds
add $sp, $sp, 16
#        end array bounds check
li $t0, 100
#             begin (lnstore $t0 #s(temp-loc t714) #s(label main))
move $t3, $s0
sw $t0, 68($t3)
#             end   (lnstore $t0 #s(temp-loc t714) #s(label main))
#  DEREF ASSIGN INS #s(mem-loc m711) #s(temp-loc t714)
#    first check non-nil
#            begin (lnload #s(mem-loc m711) $a0 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 15
lw $a0, 64($t3)
#            end (lnload #s(mem-loc m711) $a0 #s(label main))
sub $sp, $sp, 16
jal assert_nonnil
add $sp, $sp, 16
#    end check non-nil
#            begin (lnload #s(mem-loc m711) $t1 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 15
lw $t1, 64($t3)
#            end (lnload #s(mem-loc m711) $t1 #s(label main))
#            begin (lnload #s(temp-loc t714) $t2 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 16
lw $t2, 68($t3)
#            end (lnload #s(temp-loc t714) $t2 #s(label main))
sw $t2, ($t1)
#               begin (lnref #s(temp-loc t708) $t0 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 12
add $t0, $t3, 52
#             begin (lnstore $t0 #s(mem-loc m716) #s(label main))
move $t3, $s0
sw $t0, 88($t3)
#             end   (lnstore $t0 #s(mem-loc m716) #s(label main))
#            begin (lnload #s(mem-loc m716) $a0 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 21
lw $a0, 88($t3)
#            end (lnload #s(mem-loc m716) $a0 #s(label main))
sub $sp, $sp, 16
jal assert_nonnil
add $sp, $sp, 16
#            begin (lnload #s(mem-loc m716) $t0 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 21
lw $t0, 88($t3)
#            end (lnload #s(mem-loc m716) $t0 #s(label main))
lw $t0, ($t0)
#             begin (lnstore $t0 #s(mem-loc m716) #s(label main))
move $t3, $s0
sw $t0, 88($t3)
#             end   (lnstore $t0 #s(mem-loc m716) #s(label main))
li $t0, 1
#             begin (lnstore $t0 #s(temp-loc t717) #s(label main))
move $t3, $s0
sw $t0, 92($t3)
#             end   (lnstore $t0 #s(temp-loc t717) #s(label main))
li $t0, 4
#             begin (lnstore $t0 #s(mem-loc m715) #s(label main))
move $t3, $s0
sw $t0, 80($t3)
#             end   (lnstore $t0 #s(mem-loc m715) #s(label main))
#   BEGIN binop op=*, src1=#s(temp-loc t717), src2=#s(mem-loc m715), dest=#s(temp-loc t717)
#            begin (lnload #s(temp-loc t717) $t1 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 22
lw $t1, 92($t3)
#            end (lnload #s(temp-loc t717) $t1 #s(label main))
#            begin (lnload #s(mem-loc m715) $t2 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 19
lw $t2, 80($t3)
#            end (lnload #s(mem-loc m715) $t2 #s(label main))
mul $t0, $t1, $t2
#             begin (lnstore $t0 #s(temp-loc t717) #s(label main))
move $t3, $s0
sw $t0, 92($t3)
#             end   (lnstore $t0 #s(temp-loc t717) #s(label main))
#   END   binop op=*, src1=#s(temp-loc t717), src2=#s(mem-loc m715), dest=#s(temp-loc t717)
#   BEGIN binop op=+, src1=#s(mem-loc m716), src2=#s(mem-loc m715), dest=#s(mem-loc m715)
#            begin (lnload #s(mem-loc m716) $t1 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 21
lw $t1, 88($t3)
#            end (lnload #s(mem-loc m716) $t1 #s(label main))
#            begin (lnload #s(mem-loc m715) $t2 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 19
lw $t2, 80($t3)
#            end (lnload #s(mem-loc m715) $t2 #s(label main))
add $t0, $t1, $t2
#             begin (lnstore $t0 #s(mem-loc m715) #s(label main))
move $t3, $s0
sw $t0, 80($t3)
#             end   (lnstore $t0 #s(mem-loc m715) #s(label main))
#   END   binop op=+, src1=#s(mem-loc m716), src2=#s(mem-loc m715), dest=#s(mem-loc m715)
#   BEGIN binop op=+, src1=#s(temp-loc t717), src2=#s(mem-loc m715), dest=#s(mem-loc m715)
#            begin (lnload #s(temp-loc t717) $t1 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 22
lw $t1, 92($t3)
#            end (lnload #s(temp-loc t717) $t1 #s(label main))
#            begin (lnload #s(mem-loc m715) $t2 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 19
lw $t2, 80($t3)
#            end (lnload #s(mem-loc m715) $t2 #s(label main))
add $t0, $t1, $t2
#             begin (lnstore $t0 #s(mem-loc m715) #s(label main))
move $t3, $s0
sw $t0, 80($t3)
#             end   (lnstore $t0 #s(mem-loc m715) #s(label main))
#   END   binop op=+, src1=#s(temp-loc t717), src2=#s(mem-loc m715), dest=#s(mem-loc m715)
#        start array bounds check
#            begin (lnload #s(mem-loc m716) $a0 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 21
lw $a0, 88($t3)
#            end (lnload #s(mem-loc m716) $a0 #s(label main))
#            begin (lnload #s(mem-loc m715) $a1 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 19
lw $a1, 80($t3)
#            end (lnload #s(mem-loc m715) $a1 #s(label main))
sub $sp, $sp, 16
jal assert_inbounds
add $sp, $sp, 16
#        end array bounds check
li $t0, 111
#             begin (lnstore $t0 #s(temp-loc t718) #s(label main))
move $t3, $s0
sw $t0, 84($t3)
#             end   (lnstore $t0 #s(temp-loc t718) #s(label main))
#  DEREF ASSIGN INS #s(mem-loc m715) #s(temp-loc t718)
#    first check non-nil
#            begin (lnload #s(mem-loc m715) $a0 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 19
lw $a0, 80($t3)
#            end (lnload #s(mem-loc m715) $a0 #s(label main))
sub $sp, $sp, 16
jal assert_nonnil
add $sp, $sp, 16
#    end check non-nil
#            begin (lnload #s(mem-loc m715) $t1 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 19
lw $t1, 80($t3)
#            end (lnload #s(mem-loc m715) $t1 #s(label main))
#            begin (lnload #s(temp-loc t718) $t2 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 20
lw $t2, 84($t3)
#            end (lnload #s(temp-loc t718) $t2 #s(label main))
sw $t2, ($t1)
#               begin (lnref #s(temp-loc t708) $t0 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 12
add $t0, $t3, 52
#             begin (lnstore $t0 #s(mem-loc m720) #s(label main))
move $t3, $s0
sw $t0, 104($t3)
#             end   (lnstore $t0 #s(mem-loc m720) #s(label main))
#            begin (lnload #s(mem-loc m720) $a0 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 25
lw $a0, 104($t3)
#            end (lnload #s(mem-loc m720) $a0 #s(label main))
sub $sp, $sp, 16
jal assert_nonnil
add $sp, $sp, 16
#            begin (lnload #s(mem-loc m720) $t0 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 25
lw $t0, 104($t3)
#            end (lnload #s(mem-loc m720) $t0 #s(label main))
lw $t0, ($t0)
#             begin (lnstore $t0 #s(mem-loc m720) #s(label main))
move $t3, $s0
sw $t0, 104($t3)
#             end   (lnstore $t0 #s(mem-loc m720) #s(label main))
li $t0, 2
#             begin (lnstore $t0 #s(temp-loc t721) #s(label main))
move $t3, $s0
sw $t0, 108($t3)
#             end   (lnstore $t0 #s(temp-loc t721) #s(label main))
li $t0, 4
#             begin (lnstore $t0 #s(mem-loc m719) #s(label main))
move $t3, $s0
sw $t0, 96($t3)
#             end   (lnstore $t0 #s(mem-loc m719) #s(label main))
#   BEGIN binop op=*, src1=#s(temp-loc t721), src2=#s(mem-loc m719), dest=#s(temp-loc t721)
#            begin (lnload #s(temp-loc t721) $t1 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 26
lw $t1, 108($t3)
#            end (lnload #s(temp-loc t721) $t1 #s(label main))
#            begin (lnload #s(mem-loc m719) $t2 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 23
lw $t2, 96($t3)
#            end (lnload #s(mem-loc m719) $t2 #s(label main))
mul $t0, $t1, $t2
#             begin (lnstore $t0 #s(temp-loc t721) #s(label main))
move $t3, $s0
sw $t0, 108($t3)
#             end   (lnstore $t0 #s(temp-loc t721) #s(label main))
#   END   binop op=*, src1=#s(temp-loc t721), src2=#s(mem-loc m719), dest=#s(temp-loc t721)
#   BEGIN binop op=+, src1=#s(mem-loc m720), src2=#s(mem-loc m719), dest=#s(mem-loc m719)
#            begin (lnload #s(mem-loc m720) $t1 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 25
lw $t1, 104($t3)
#            end (lnload #s(mem-loc m720) $t1 #s(label main))
#            begin (lnload #s(mem-loc m719) $t2 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 23
lw $t2, 96($t3)
#            end (lnload #s(mem-loc m719) $t2 #s(label main))
add $t0, $t1, $t2
#             begin (lnstore $t0 #s(mem-loc m719) #s(label main))
move $t3, $s0
sw $t0, 96($t3)
#             end   (lnstore $t0 #s(mem-loc m719) #s(label main))
#   END   binop op=+, src1=#s(mem-loc m720), src2=#s(mem-loc m719), dest=#s(mem-loc m719)
#   BEGIN binop op=+, src1=#s(temp-loc t721), src2=#s(mem-loc m719), dest=#s(mem-loc m719)
#            begin (lnload #s(temp-loc t721) $t1 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 26
lw $t1, 108($t3)
#            end (lnload #s(temp-loc t721) $t1 #s(label main))
#            begin (lnload #s(mem-loc m719) $t2 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 23
lw $t2, 96($t3)
#            end (lnload #s(mem-loc m719) $t2 #s(label main))
add $t0, $t1, $t2
#             begin (lnstore $t0 #s(mem-loc m719) #s(label main))
move $t3, $s0
sw $t0, 96($t3)
#             end   (lnstore $t0 #s(mem-loc m719) #s(label main))
#   END   binop op=+, src1=#s(temp-loc t721), src2=#s(mem-loc m719), dest=#s(mem-loc m719)
#        start array bounds check
#            begin (lnload #s(mem-loc m720) $a0 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 25
lw $a0, 104($t3)
#            end (lnload #s(mem-loc m720) $a0 #s(label main))
#            begin (lnload #s(mem-loc m719) $a1 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 23
lw $a1, 96($t3)
#            end (lnload #s(mem-loc m719) $a1 #s(label main))
sub $sp, $sp, 16
jal assert_inbounds
add $sp, $sp, 16
#        end array bounds check
li $t0, 10
#             begin (lnstore $t0 #s(temp-loc t722) #s(label main))
move $t3, $s0
sw $t0, 100($t3)
#             end   (lnstore $t0 #s(temp-loc t722) #s(label main))
#  DEREF ASSIGN INS #s(mem-loc m719) #s(temp-loc t722)
#    first check non-nil
#            begin (lnload #s(mem-loc m719) $a0 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 23
lw $a0, 96($t3)
#            end (lnload #s(mem-loc m719) $a0 #s(label main))
sub $sp, $sp, 16
jal assert_nonnil
add $sp, $sp, 16
#    end check non-nil
#            begin (lnload #s(mem-loc m719) $t1 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 23
lw $t1, 96($t3)
#            end (lnload #s(mem-loc m719) $t1 #s(label main))
#            begin (lnload #s(temp-loc t722) $t2 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 24
lw $t2, 100($t3)
#            end (lnload #s(temp-loc t722) $t2 #s(label main))
sw $t2, ($t1)
#            begin (lnload #s(temp-loc t708) $t0 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 12
lw $t0, 52($t3)
#            end (lnload #s(temp-loc t708) $t0 #s(label main))
#             begin (lnstore $t0 #s(temp-loc t707) #s(label main))
move $t3, $s0
sw $t0, 48($t3)
#             end   (lnstore $t0 #s(temp-loc t707) #s(label main))
la $a0, l723
move $a1, $s0
sub $sp, $sp, 16
jal alloc_closure
add $sp, $sp, 16
#             begin (lnstore $v0 #s(label-loc lt725) #s(label main))
move $t3, $s0
sw $v0, 112($t3)
#             end   (lnstore $v0 #s(label-loc lt725) #s(label main))
#            begin (lnload #s(label-loc lt725) $t0 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 27
lw $t0, 112($t3)
#            end (lnload #s(label-loc lt725) $t0 #s(label main))
#             begin (lnstore $t0 #s(label-loc lt734) #s(label main))
move $t3, $s0
sw $t0, 116($t3)
#             end   (lnstore $t0 #s(label-loc lt734) #s(label main))
#     BEGIN FUNCALL labloc=#s(label-loc lt734), args=(), dest=ans
sub $sp, $sp, 4  #push $s0
sw $s0, ($sp)
sub $sp, $sp, 16
#            begin (lnload #s(label-loc lt734) $t2 #s(label main))
move $t3, $s0 # loading variable at nest-depth 0 and offset 28
lw $t2, 116($t3)
#            end (lnload #s(label-loc lt734) $t2 #s(label main))
lw $s0, 4($t2)
lw $t2, ($t2)
jalr $t2
add $sp, $sp, 16
lw $s0, ($sp)
add $sp, $sp, 4  #pop $s0
#             begin (lnstore $v0 ans #s(label main))
#ignore store into ans
#             end   (lnstore $v0 ans #s(label main))
#     END   FUNCALL labloc=#s(label-loc lt734), args=(), dest=ans
mainbreak:  # break label for function main
lw $ra, ($sp)
add $sp, $sp, 4  #pop $ra
jr $ra
#     end def main



#      begin def l723
l723:
sub $sp, $sp, 4  #push $ra
sw $ra, ($sp)
li $a0, 7
li $a1, 0
sub $sp, $sp, 16
jal alloc_block
add $sp, $sp, 16
sw $s0, ($v0) # put the static parent's activation record in the first slot of the new activation record
move $s0, $v0 # set the current activation record to be the new one
l723recur:
#      done setting up activation record
#            begin (lnload #s(label-loc lt696) $t0 #s(label l723))
move $t3, $s0 # loading variable at nest-depth 1 and offset 10
lw $t3, ($t3)
lw $t0, 44($t3)
#            end (lnload #s(label-loc lt696) $t0 #s(label l723))
#             begin (lnstore $t0 #s(label-loc lt727) #s(label l723))
move $t3, $s0
sw $t0, 8($t3)
#             end   (lnstore $t0 #s(label-loc lt727) #s(label l723))
#            begin (lnload #s(temp-loc t707) $t0 #s(label l723))
move $t3, $s0 # loading variable at nest-depth 1 and offset 11
lw $t3, ($t3)
lw $t0, 48($t3)
#            end (lnload #s(temp-loc t707) $t0 #s(label l723))
#             begin (lnstore $t0 #s(temp-loc t728) #s(label l723))
move $t3, $s0
sw $t0, 16($t3)
#             end   (lnstore $t0 #s(temp-loc t728) #s(label l723))
#     BEGIN FUNCALL labloc=#s(label-loc lt727), args=(#s(temp-loc t728)), dest=#s(temp-loc t726)
sub $sp, $sp, 4  #push $s0
sw $s0, ($sp)
sub $sp, $sp, 16
#            begin (lnload #s(temp-loc t728) $a0 #s(label l723))
move $t3, $s0 # loading variable at nest-depth 0 and offset 3
lw $a0, 16($t3)
#            end (lnload #s(temp-loc t728) $a0 #s(label l723))
#            begin (lnload #s(temp-loc t728) $t0 #s(label l723))
move $t3, $s0 # loading variable at nest-depth 0 and offset 3
lw $t0, 16($t3)
#            end (lnload #s(temp-loc t728) $t0 #s(label l723))
sw $t0, 0($sp)
#            begin (lnload #s(label-loc lt727) $t2 #s(label l723))
move $t3, $s0 # loading variable at nest-depth 0 and offset 1
lw $t2, 8($t3)
#            end (lnload #s(label-loc lt727) $t2 #s(label l723))
lw $s0, 4($t2)
lw $t2, ($t2)
jalr $t2
add $sp, $sp, 16
lw $s0, ($sp)
add $sp, $sp, 4  #pop $s0
#             begin (lnstore $v0 #s(temp-loc t726) #s(label l723))
move $t3, $s0
sw $v0, 4($t3)
#             end   (lnstore $v0 #s(temp-loc t726) #s(label l723))
#     END   FUNCALL labloc=#s(label-loc lt727), args=(#s(temp-loc t728)), dest=#s(temp-loc t726)
#            begin (lnload #s(label-loc lt725) $t0 #s(label l723))
move $t3, $s0 # loading variable at nest-depth 1 and offset 27
lw $t3, ($t3)
lw $t0, 112($t3)
#            end (lnload #s(label-loc lt725) $t0 #s(label l723))
#             begin (lnstore $t0 #s(label-loc lt731) #s(label l723))
move $t3, $s0
sw $t0, 20($t3)
#             end   (lnstore $t0 #s(label-loc lt731) #s(label l723))
#     BEGIN RECURSIVE FUNCALL labloc=#s(label-loc lt731), args=(), dest=#s(temp-loc t726)
j l723recur
#     END  TAIL-RECURSIVE FUNCALL labloc=#s(label-loc lt731), args=(), dest=#s(temp-loc t726)
#            begin (lnload #s(temp-loc t726) $v0 #s(label l723))
move $t3, $s0 # loading variable at nest-depth 0 and offset 0
lw $v0, 4($t3)
#            end (lnload #s(temp-loc t726) $v0 #s(label l723))
l723break:  # break label for function l723
lw $ra, ($sp)
add $sp, $sp, 4  #pop $ra
jr $ra
#     end def l723

