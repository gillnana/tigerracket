#lang racket

#;(require rnrs/io/ports-6)
(require test-engine/racket-tests)
(require "intermediate.rkt")
(require "canonicalize.rkt")
(require "parser.rkt")

(provide (all-defined-out))

(define TEMP0 "$t0")
(define TEMP1 "$t1")
(define TEMP2 "$t2")
(define TEMP3 "$t3")
(define COMMA ", ")
(define RETURN_REGISTER "$v0")
(define SP "$sp")
(define CURRENT_AR "$s0")
(define AR_CURRENT CURRENT_AR)
(define ☃ "$at")

; TODO: global stateful variables are evil and we should fix this
; possibly using with-output-to
; or something 
; -dpercy
;(define cur-prog (open-output-string))

(define (gen-code ir [cur-block #f])
  (match ir
    [(program (list) fxnlist)
     (begin (map gen-code fxnlist) (void))]
    [(program inslist fxnlist)
     (error "found a program that failed to functionize")]
    [(and cb (fxn-block (label lbl) inslist static-link locals))
     (begin
       (ln)
       (ln)
       (ln "#      begin def " lbl)
       (ln (labelize lbl) ":")
       (ln (stack-setup locals))
       (create-activation-record locals cb)
       (ln (labelize lbl) "recur:")
       (ln "#      done setting up activation record")
       ; assume the variables this function has access to are sorted (?), including parameters to this fxn 
       
       

       
       (map (λ (ins) (gen-code ins cb)) inslist)
       (ln lbl "break:  # break label for function " lbl)
       (ln (stack-teardown locals))
       (ln "jr $ra")
       (ln "#     end def " lbl)
       (ln)
       )]
    
    ; below here i demand you have a cur-block
    [(allocation (mem-block sym size))
     (begin
       (lnload size "$a0" cur-block)
       (ln "li $a1, 0")
       (ln "sub $sp, $sp, 16")
       (ln "jal alloc_block")
       (ln "add $sp, $sp, 16"))]
    [(allocation _) (void)]
    [(lim-ins imm 'ans) (void)]
    [(lim-ins (label l) dest)
     (if (label-loc? dest)
         (begin (ln "la " TEMP0 COMMA l)
                ;(ln "sw " TEMP0 COMMA (get-offset dest temps) "($sp)")
                (lnstore TEMP0 dest cur-block)
                )
         
         ; TODO: register onionization
         (error (format "internal error: label assigned to location ~a that cannot hold labels" dest)))]
    [(lim-ins (? number? imm) dest)
     (if (number-location? dest)
         (begin
           (ln "li " TEMP0 COMMA imm)
           ;(ln "sw " TEMP0 COMMA (get-offset dest temps) "($sp)")
           (lnstore TEMP0 dest cur-block)
           )
         
         ; TODO: register onionization 
         (error (format "internal error: number assigned to location ~a that cannot hold numbers" dest)))]
    [(closure-ins (label l) dest-loc)
     (if (tail-recursive? cur-block)
         (void) ; ...
         (begin (ln "la $a0" COMMA l)
                (ln "move $a1" COMMA AR_CURRENT) ;lscd
                (ln "sub $sp, $sp, 16")
                (ln "jal alloc_closure")
                (ln "add $sp, $sp, 16")
                (lnstore RETURN_REGISTER dest-loc cur-block)
                ))]
    [(malloc-ins size dest-loc)
     (ln "li $a0, " size)
     (ln "li $a1, 0")
     (ln "sub $sp, $sp, 16")
     (ln "jal alloc_block")
     (ln "add $sp, $sp, 16")
     (lnstore RETURN_REGISTER dest-loc cur-block)
      
     ]
    
    [(array-malloc-ins size-loc initval-loc dest-loc)
     (lnload size-loc "$a0" cur-block)
     (lnload initval-loc "$a1" cur-block)
     (ln "sub $sp, $sp, 16")
     (ln "jal alloc_array")
     (ln "add $sp, $sp, 16")
     (lnstore RETURN_REGISTER dest-loc cur-block)
     ]
    
    [(move-ins src 'ans) (void)]
    [(move-ins src dest)
     (lnload src TEMP0 cur-block)
     (lnstore TEMP0 dest cur-block)
     ]
    
    ; this instruction corresponds to x=&y, putting the l-value of y into the r-value of x
    [(ref-ins dest src)
     (lnref src TEMP0 cur-block)
     (lnstore TEMP0 dest cur-block)
     ]
    
    ; this instruction corresponds to x=*y, putting the r-value of y into the r-value of x
    [(deref-ins src1 src2)
     (lnload src2 "$a0" cur-block)
     (ln "sub $sp, $sp, 16")
     (ln "jal assert_nonnil")
     (ln "add $sp, $sp, 16")
     (lnload src2 TEMP0 cur-block)
     (ln "lw " TEMP0 COMMA "(" TEMP0 ")")
     (lnstore TEMP0 src1 cur-block)]
    
    ; this instruction is x*=y, putting the r-value of y into the l-value of x
    [(deref-assign-ins src1 src2) 
     (ln (format "#  DEREF ASSIGN INS ~a ~a" src1 src2))
     (ln "#    first check non-nil")
     (lnload src1 "$a0" cur-block)
     (ln "sub $sp, $sp, 16")
     (ln "jal assert_nonnil")
     (ln "add $sp, $sp, 16")
     (ln "#    end check non-nil")
     (lnload src1 TEMP1 cur-block)
     (lnload src2 TEMP2 cur-block)
     (ln "sw " TEMP2 COMMA "(" TEMP1 ")")]
    
    [(array-bounds-check-ins array-loc index-loc)
     (ln "#        start array bounds check")
     (lnload array-loc "$a0" cur-block)
     (lnload index-loc "$a1" cur-block)
     (ln "sub $sp, $sp, 16")
     (ln "jal assert_inbounds")
     (ln "add $sp, $sp, 16")
     (ln "#        end array bounds check")
     ]
   
    [(return-ins src)
     ;(ln "lw " RETURN_REGISTER COMMA (get-offset src temps) "($sp)")
     (lnload src RETURN_REGISTER cur-block)
     ]
    
    [(break-ins)
     (match cur-block
       [(fxn-block (label lbl) _ _ _) (ln "j " lbl "break # jump to break-label")])]
     
    
    [(binary-ins op src1 src2 'ans) (void)]
    [(binary-ins 'string= src1 src2 dest) 
     (ln "#  BEGIN string comparison")
     (lnload src1 "$a0" cur-block)
     (lnload src2 "$a1" cur-block)
     (ln "sub $sp, $sp, 16")
     (ln "jal string_comp")
     (ln "add $sp, $sp, 16")
     (lnstore "$v0" dest cur-block)
     (ln "#  END string comparison")
     ]
    [(binary-ins op src1 src2 dest)
     (ln "#   BEGIN binop op="op", src1="src1", src2="src2", dest="dest)
     ;(ln "lw " TEMP1 COMMA (get-offset src1 temps) "($sp)")
     ;(ln "lw " TEMP2 COMMA (get-offset src2 temps) "($sp)")
     (lnload src1 TEMP1 cur-block)
     (lnload src2 TEMP2 cur-block)
     (ln (match op
           ['+ "add"]
           ['- "sub"]
           ['* "mul"]
           ['/ "div"]
           ['= "seq"]
           ['< "slt"]
           ['> "sgt"]
           ['<= "sle"]
           ['>= "sge"]
           ['<> "sne"]
           ['or "or"] ; maybe unnecessary
           ['and "and"] ; maybe unnecessary
           )
         " " TEMP0 COMMA TEMP1 COMMA TEMP2)
    ; (ln "sw " TEMP0 COMMA (get-offset dest temps) "($sp)")
     (lnstore TEMP0 dest cur-block)
     (ln "#   END   binop op="op", src1="src1", src2="src2", dest="dest)
]
    
    [(unary-ins op src 'ans) (void)]
    [(unary-ins op src dest)
     ;(ln "lw " TEMP1 COMMA (get-offset src temps) "($sp)")
     (lnload src TEMP1 cur-block)
     (match op
           ['- (ln "sub " TEMP0 COMMA "$0" COMMA TEMP1)]
           ; TODO...
           )
        
     ;(ln "sw " TEMP0 COMMA (get-offset dest temps) "($sp)")
     (lnstore TEMP0 dest cur-block)
     ]
    
    [(label lbl) (ln lbl ":")]
    [(uncond-jump-ins (label lbl)) (ln "j " lbl)]
    [(cond-jump-ins src (label lbl))
     ;(ln "lw " TEMP1 COMMA (get-offset src temps) "($sp)")
     (lnload src TEMP1 cur-block)
     (ln "beqz " TEMP1 COMMA lbl)]
    [(cond-jump-relop-ins op src1 src2 (label lbl))
;     (ln "lw " TEMP1 COMMA (get-offset src1 temps) "($sp)")
;     (ln "lw " TEMP2 COMMA (get-offset src2 temps) "($sp)")
     (lnload src1 TEMP1 cur-block)
     (lnload src2 TEMP2 cur-block)
     (ln
      (match op ;note that cond-jump relop ALWAYS inverts the comparator -- means jump if false
        ['= "bneq"]
        ['< "bge"]
        ['> "ble"]
        ['<> "beq"]
        ['<= "bgt"]
        ['>= "blt"])
      " " TEMP1 COMMA TEMP2 COMMA lbl)]
    
    [(funcall-ins labloc args dest)
     (begin
       (ln "#     BEGIN FUNCALL labloc="labloc", args="args", dest="dest)
       (ln (push AR_CURRENT))
       (ln "sub $sp, $sp, " (* 4 (max (length args) 4)))
       (map (λ (arg num)
              ; note: a param-loc always refers to the parameters of the *current* function.
              (when (> 4 num) 
                (lnload arg (string-append "$a" (number->string num)) cur-block))
              
              (lnload arg TEMP0 cur-block)
              (ln "sw " TEMP0 COMMA (* 4 num) "($sp)")
              )
            args
            (build-list (length args) values))
       
       (lnload labloc TEMP2 cur-block)

       
       ; need to get the static closure in order to pass it to the callee in AR
       ; this closure is labloc
       (ln "lw " AR_CURRENT COMMA "4(" TEMP2 ")")
       ; get the code pointer out of the closure
       (ln "lw " TEMP2 COMMA "(" TEMP2 ")")
       
       (ln "jalr " TEMP2)
       (ln "add $sp, $sp, " (* 4 (max (length args) 4))) 
       (ln (pop AR_CURRENT))
       (lnstore RETURN_REGISTER dest cur-block)
       (ln "#     END   FUNCALL labloc="labloc", args=" args ", dest="dest)
       )]
       
       
    
    ))


;; DUMB IDIOT HELPER FUNCTIONS

; creates a new activation record for this function.  this code goes inside the function block.
; puts the current activation record ( $s0 ) in the activation record to be created.
; assumes that the parent AR is already pushed on the stack but still is in $s0
(define (create-activation-record local-vars cur-block)
  (begin 
    (ln "li $a0, " (+ 1 (length local-vars)))
    (ln "li $a1, 0")
    (ln "sub $sp, $sp, 16")
    (ln "jal alloc_block")
    (ln "add $sp, $sp, 16")
    (ln "sw " AR_CURRENT COMMA "(" RETURN_REGISTER ") # put the static parent's activation record in the first slot of the new activation record")
    (ln "move " AR_CURRENT COMMA RETURN_REGISTER " # set the current activation record to be the new one")
    
    ; now we have a blank AR linked to previous
    ; now copy params in from stack
    (let* ([params (filter param-loc? local-vars)]
           [nums (build-list (length params) values)])
      (map (λ (loc num) 
             (ln "lw " TEMP0 COMMA (* 4 (+ 1 #|TODO MAGIC MAGIC|# num)) "($sp) # copy arg " num)
             (lnstore TEMP0 loc cur-block)
             )
           params
           nums)
      )
    )
  )

; takes a predicate and a list
; returns (values index item) where (eq? (list-ref ls index) item)
;      or (values #f #f) if no such item is found
(define (ormap-index pred? ls [index 0])
  (cond
    [(empty? ls) (values #f #f)]
    [(cons? ls) (if (pred? (first ls))
                    (values index (pred? (first ls)))
                    (ormap-index pred? (rest ls) (+ index 1)))]))

; finds a variable in this fxn-block's scope (not its parents')
; returns (values index fxn-block)
(define (find-local loc block)
  (match block
    [(fxn-block _ _ _ temp-list) 
     (ormap-index (λ (x) (eq? x loc))
                  temp-list)]
    [(? symbol? s) (values #f #f)]
    ))

; returns (values num-static-links-to-follow 
;                 offset-in-that-fn)
(define (find-static loc block)
  (match block
    [(fxn-block _ _ static-links temp-list)
     (let-values [((a b) (ormap-index (λ (x) ; x is a fxn-block, either itself or a static parent
                   (let-values ([(offset fb) (find-local loc x)])
                     offset)
                   )
                 (cons block static-links)))]
       (if a
           (values a b)
           (error (format "failed to find location ~a" loc))))
       ]))

(define (tail-recursive? block)
  #f)


(define (get-offset temp temp-list)
  (* 4 (+ 1 (index-of temp temp-list))))

(define (stack-setup temps)
  (appendln
   ;(push CURRENT_AR)
   (push "$ra")
   ;(string-append "sub $sp, $sp, 4")
   ))

(define (stack-teardown temps)
  (appendln
   ;(string-append "add $sp, $sp, 4")
   (pop "$ra")
   ;(pop CURRENT_AR)
   ))

(define (lnload src-loc dest-reg cur-block)
  (ln (format "#            begin (lnload ~a ~a ~a)" src-loc dest-reg (fxn-block-label cur-block)))
  (match src-loc
    [(? symbol? src-loc) (error "internal error: trying to load value from 'ans")]
    [(or (temp-loc sym) (label-loc sym) (param-loc sym _) (mem-loc sym))
     (let-values ([(nest-depth offset) (find-static src-loc cur-block)])
       ;(displayln (fxn-block-local-vars cur-block)) ; MESS
       ;(displayln src-loc)
       (ln "move " TEMP3 COMMA AR_CURRENT " # loading variable at nest-depth " nest-depth " and offset " offset)
       (jump-back-one nest-depth cur-block)
       (ln "lw " dest-reg COMMA (* 4 (+ 1 offset)) "(" TEMP3 ")"))]
    [(mem-loc sym) (error 'do-this)] ; we will need an environment to keep track of all the random stuff on the heap
    )
  (ln (format "#            end (lnload ~a ~a ~a)" src-loc dest-reg (fxn-block-label cur-block))))

(define (lnstore src-reg dest-loc cur-block)
  ;(displayln cur-block)
  (ln (format "#             begin (lnstore ~a ~a ~a)" src-reg dest-loc (fxn-block-label cur-block)))
  (match dest-loc
    [(? symbol? sym) (ln "#ignore store into ans")]
    [(or (temp-loc sym) (label-loc sym) (param-loc sym _) (mem-loc sym))
     (let-values ([(nest-depth offset) (find-static dest-loc cur-block)])
       ; this named let jumps back
       (ln "move " TEMP3 COMMA AR_CURRENT)
       (jump-back-one nest-depth cur-block)
       (ln "sw " src-reg COMMA (* 4 (+ 1 offset)) "(" TEMP3 ")"))])
  (ln (format "#             end   (lnstore ~a ~a ~a)" src-reg dest-loc (fxn-block-label cur-block))))

(define (lnref src-loc dest-reg cur-block)
  (ln (format "#               begin (lnref ~a ~a ~a)"  src-loc dest-reg (fxn-block-label cur-block)))
  (match src-loc
    [(? symbol? src-loc) (error "internal error: trying to load address of 'ans")]
    [(or (temp-loc sym) (label-loc sym) (param-loc sym _) (mem-loc sym))
     (let-values ([(nest-depth offset) (find-static src-loc cur-block)])
       ;(displayln (fxn-block-local-vars cur-block)) ; MESS
       ;(displayln src-loc)
       (ln "move " TEMP3 COMMA AR_CURRENT " # loading variable at nest-depth " nest-depth " and offset " offset)
       (jump-back-one nest-depth cur-block)
       
       ;(ln "lw " dest-reg COMMA (* 4 (+ 1 offset)) "(" TEMP3 ")")
       (ln "add " dest-reg COMMA TEMP3 COMMA (* 4 (+ 1 offset)))
       
       )]
    [(mem-loc sym) (error 'do-this)] ; we will need an environment to keep track of all the random stuff on the heap
    )
  )

(define (get-fp-offset block)
  (match block
    [(fxn-block _ _ _ temps)
     (* 4 (+ 2 (length temps)))]))

; follows links between activation records until TEMP1 holds a pointer to the AR of the desired temp
(define (jump-back-one n cb)
  (when (> n 0)
    (match cb
        [(fxn-block _ _ (list* parent _) _)
         (begin
           (ln "lw " TEMP3 COMMA   #;(get-fp-offset cb) #|no longer necessary|#   "(" TEMP3 ")")
           ; get-fp-offset was being called because static link pointed to bottom of frame,
           ; but static link was stored at top of frame, 
           ; now just deref a whole bunch of times
           (jump-back-one (- n 1) parent))])))

(define (index-of item ls (count 0))
  (if (empty? ls)
      (error (format "item ~a not in list ~a" item ls))
      (if (equal? item (first ls))
          count
          (index-of item (rest ls) (+ count 1))
          )
      )) 

  
(define (push name)
  (string-append (format "sub $sp, $sp, 4  #push ~a" name)
                 "\n"
                 "sw " name ", ($sp)" ; GCC convention
                 ))

(define (pop name)
  (string-append "lw " name ", ($sp)" ; GCC convention
                 "\n"
                 (format "add $sp, $sp, 4  #pop ~a" name)
                 ))

(define (appendln . lines)
  (apply string-append (add-between lines "\n")))


; ln calls display on each of its arguments,
; then displays a new line
(define (ln . textlist)
  (map (λ (s)
         (when (and (not (empty? s)) 
                    ((listof void?) s))
           (error "ln void"))
         
         (display s))
       textlist)
  (displayln ""))

(define (labelize label)
  (string-append (symbol->string label)))

(check-expect (begin (ln "wossar") (ln "foop") (get-output-string)) "wossar\nfoop\n")
