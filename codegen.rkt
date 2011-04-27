#lang racket

#;(require rnrs/io/ports-6)
(require test-engine/racket-tests)
(require "intermediate.rkt")
(require "parser.rkt")

(provide (all-defined-out))

(define TEMP0 "$t0")
(define TEMP1 "$t1")
(define TEMP2 "$t2")
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
       (ln (labelize lbl))
       (ln (stack-setup locals))
       (create-activation-record locals) ; assume the variables this function has access to are sorted, including parameters to this fxn 
       (map (λ (ins) (gen-code ins cb)) inslist)
       (ln (stack-teardown locals))
       (ln "jr $ra")
       )]
    
    ; below here i demand you have a cur-block
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
    [(closure-ins (label l) dest)
     (begin (ln "la $a0" COMMA l)
            (ln "move $a1" COMMA AR_CURRENT) ;lscd
            (ln "sub $sp, $sp, 16")
            (ln "jal alloc_closure") ; assume alloc_closure exists, works
            (ln "add $sp, $sp, 16")
            (ln "move " dest COMMA RETURN_REGISTER)
            )]
    
    [(move-ins src 'ans) (void)]
    [(move-ins src dest)
     ;(ln "lw " TEMP0 COMMA (get-offset src temps) "($sp)")
     ;(ln "sw " TEMP0 COMMA (get-offset dest temps) "($sp)")
     (lnload src TEMP0 cur-block)
     (lnstore TEMP0 dest cur-block)
     ]
    
    [(return-ins src)
     ;(ln "lw " RETURN_REGISTER COMMA (get-offset src temps) "($sp)")
     (lnload src RETURN_REGISTER cur-block)
     ]
     
    
    [(binary-ins op src1 src2 'ans) (void)]
    [(binary-ins op src1 src2 dest)
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
         
;    [(funcall-ins labloc args dest)
;     (begin
;       (ln "#BEGIN FUNCALL")
;       (when (> (length args) 4) 
;         (error "TODO more than 4 arguments to function not yet supported"))
;       ; load args
;       (map (λ (arg num)
;              ;(ln "lw $a" num ", " (get-offset arg temps) "($sp)")
;              ; note: a param-loc always refers to the parameters of the *current* function.
;              (ln "  #BEGIN PUT ARG " num)
;              (lnload arg (string-append "$a" (number->string num)) cur-block)
;              (ln "  #END PUT ARG " num)
;              )
;            args
;            (build-list (length args) values))
;       
;       ; jump!
;       ;(ln "lw $t0, " (get-offset labloc cur-block) "($sp)")    
;       (lnload labloc TEMP2 cur-block)
;       
;       (ln "move " TEMP0 COMMA SP)
;       (ln (push TEMP0)) ; can't push sp directly because it would change the sp
;       
;       (ln "jalr " TEMP2)
;       
;       (ln "add " SP COMMA SP COMMA "4")
;       
;       ; retrieve return val
;       ;(when (not (eq? dest 'ans))
;       ;  (ln "sw $v0, " (get-offset dest cur-block) "($sp)")
;       ;  )
;       (lnstore RETURN_REGISTER dest cur-block)
;       (ln "#END FUNCALL")
;       )]
    
    [(funcall-ins labloc args dest)
     (begin
       #;(ln "#BEGIN FUNCALL")
       
       (when (> (length args) 4) 
         (error "TODO more than 4 arguments to function not yet supported"))
       ; load args
       (map (λ (arg num)
              ;(ln "lw $a" num ", " (get-offset arg temps) "($sp)")
              ; note: a param-loc always refers to the parameters of the *current* function.
              #;(ln "  #BEGIN PUT ARG " num)
              (lnload arg (string-append "$a" (number->string num)) cur-block)
              #;(ln "  #END PUT ARG " num)
              )
            args
            (build-list (length args) values))
       
       ; jump!
       ;(ln "lw $t0, " (get-offset labloc cur-block) "($sp)")    
       (lnload labloc TEMP2 cur-block)
       (ln "sub $sp, $sp, 16") ; todo more than 4 args
       (ln "jalr " TEMP2)
       (ln "add $sp, $sp, 16") ; todo more than 4 args
       
       #;(ln "add " SP COMMA SP COMMA "4")
       
       )]
       
       
    
    ))


;; DUMB IDIOT HELPER FUNCTIONS

; creates a new activation record for this function.  this code goes inside the function block.
; puts the current activation record ( $s0 ) in the activation record to be created.
; assumes that the parent AR is already pushed on the stack but still is in $s0
(define (create-activation-record local-vars)
  (begin 
    (ln ("li $a0, " (+ 1 (length local-vars))))
    (ln ("li $a1, 0"))
    (ln ("sub $sp, $sp, 16"))
    (ln ("jal alloc_block"))
    (ln ("add $sp, $sp, 16"))
    (ln ("sw " AR_CURRENT COMMA "(" RETURN_REGISTER ")")) ; TODO remember to add one on every access.  you will obviously look at this extremely obviously placed comment to understand this.
    (ln ("move " AR_CURRENT COMMA RETURN_REGISTER))))

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
     (ormap-index (λ (x) ; x is a fxn-block, either itself or a static parent
                   (let-values ([(offset fb) (find-local loc x)])
                     offset)
                   )
                 (cons block static-links))]))


(define (get-offset temp temp-list)
  (* 4 (+ 1 (index-of temp temp-list))))

(define (stack-setup temps)
  (appendln
   (push CURRENT_AR)
   (push "$ra")
   (string-append "sub $sp, $sp, 8")
   ))

(define (stack-teardown temps)
  (appendln
   (string-append "add $sp, $sp, ")
   (pop "$ra")
   (pop CURRENT_AR)
   ))

(define (lnload src-loc dest-reg cur-block)
  (match src-loc
    [(? symbol? src-loc) (error "internal error: trying to load value from 'ans")]
    [(param-loc sym num)
     (if (<= 0 num 3)
         (ln "move " dest-reg COMMA "$a" num)
         (error 'UnsupportedOperationException);(ln "lw " dest-reg COMMA)
         )]
    [(or (temp-loc sym) (label-loc sym))
     (let-values ([(nest-depth offset) (find-static src-loc cur-block)])
       (ln "move " TEMP1 COMMA SP)
       (jump-back-one nest-depth cur-block)
       (ln "lw " dest-reg COMMA (* 4 (+ 1 offset)) "(" TEMP1 ")"))]))

(define (lnstore src-reg dest-loc cur-block)
  ;(displayln cur-block)
  (match dest-loc
    [(? symbol? sym) (ln "#ignore store into ans")]
    [(param-loc sym num)
     (if (<= 0 num 3)
         (ln "move " "$a" num COMMA src-reg)
         (error 'UnsupportedOperationException);(ln "lw " dest-reg COMMA 
         )
     ]
    [(or (temp-loc sym) (label-loc sym))
     (let-values ([(nest-depth offset) (find-static dest-loc cur-block)])
       ; this named let jumps back
       (ln "move " TEMP1 COMMA SP)
       (jump-back-one nest-depth cur-block)
       (ln "sw " src-reg COMMA (* 4 (+ 1 offset)) "(" TEMP1 ")"))]))

(define (get-fp-offset block)
  (match block
    [(fxn-block _ _ _ temps)
     (* 4 (+ 2 (length temps)))]))

(define (jump-back-one n cb)
  (when (> 0 n)
    (begin
      (get-fp-offset cb)
      (match cb
        [(fxn-block _ _ (list* parent _) _)
         (begin
           (ln "lw " TEMP1 COMMA (get-fp-offset cb) "(" TEMP1 ")")
           (jump-back-one (- n 1) parent))]))))

(define (index-of item ls (count 0))
  (if (empty? ls)
      (error (format "item ~a not in list ~a" item ls))
      (if (equal? item (first ls))
          count
          (index-of item (rest ls) (+ count 1))
          )
      )) 

  
(define (push name)
  (string-append "sub $sp, $sp, 4"
                 "\n"
                 "sw " name ", 4($sp)"
                 ))

(define (pop name)
  (string-append "lw " name ", 4($sp)"
                 "\n"
                 "add $sp, $sp, 4"
                 ))

(define (appendln . lines)
  (apply string-append (add-between lines "\n")))


; ln calls display on each of its arguments,
; then displays a new line
(define (ln . textlist)
  #;(when (ormap void? (flatten textlist)) (error "ln void"))
  (map (λ (s)
         (when ((listof void?) s) (error "ln void"))
         
         (display s)) textlist)
  (displayln ""))

(define (labelize label)
  (string-append (symbol->string label) ":"))

(check-expect (begin (ln "wossar") (ln "foop") (get-output-string)) "wossar\nfoop\n")