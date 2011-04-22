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
         
    ;[(funcall-ins labloc args 'ans) (void)]
    [(funcall-ins labloc args dest)
     (begin
       (ln "#BEGIN FUNCALL")
       (when (> (length args) 4) 
         (error "TODO more than 4 arguments to function not yet supported"))
       ; load args
       (map (λ (arg num)
              ;(ln "lw $a" num ", " (get-offset arg temps) "($sp)")
              ; note: a param-loc always refers to the parameters of the *current* function.
              (ln "  #BEGIN PUT ARG " num)
              (lnload arg (string-append "$a" (number->string num)) cur-block)
              (ln "  #END PUT ARG " num)
              )
            args
            (build-list (length args) values))
       
       ; jump!
       ;(ln "lw $t0, " (get-offset labloc cur-block) "($sp)")
       (ln "  #BEGIN LOAD THING TO JALR LATER")
       (lnload labloc TEMP2 cur-block)
       (ln "  #END LOAD THING TO JALR LATER")
       
       (ln "  #BEGIN PUSH ACCESS LINK")
       (ln "move " TEMP0 COMMA SP)
       (ln (push TEMP0)) ; can't push sp directly because it would change the sp
       (ln "  #END PUSH ACCESS LINK")
       
       (ln "  #BEGIN ACTUALLY JALR")
       (ln "jalr " TEMP2)
       (ln "  #END ACTUALLY JALR")
       
       (ln "  #BEGIN POP STATIC LINK")
       (ln "add " SP COMMA SP COMMA "4")
       (ln "  #END POP STATIC LINK")
       
       ; retrieve return val
       ;(when (not (eq? dest 'ans))
       ;  (ln "sw $v0, " (get-offset dest cur-block) "($sp)")
       ;  )
       (lnstore RETURN_REGISTER dest cur-block)
       (ln "#END FUNCALL")
       )]
    
    
    ))
  

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
   (push "$ra")
   (string-append "sub $sp, $sp, " (number->string (* 4 (length temps))))
   ))

(define (stack-teardown temps)
  (appendln
   (string-append "add $sp, $sp, " (number->string (* 4 (length temps))))
   (pop "$ra")
   ))

;(define (lnload src-loc dest-reg stack-env)
;  (match src-loc
;    [(? symbol? src-loc) (error "internal error: trying to load value from 'ans")]
;    [(param-loc sym num)
;     (if (<= 0 num 3)
;         (ln "move " dest-reg COMMA "$a" num)
;         (error 'UnsupportedOperationException);(ln "lw " dest-reg COMMA)
;         ]
;    [(or (temp-loc sym) (label-loc sym))
;     (ln "lw " dest-reg COMMA (get-offset src-loc stack-env) "($sp)")]
;  ))

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

      ; annoying function that extracts a list of locations from an instruction
      ; called to statically allocate space for local variables
;(define (get-locs ins)
;  (let [(location? (lambda (l)
;                     (and (location? l)
;                          (not (param-loc? l)))))]
;    (match ins
;      [(move-ins src dest) (filter location? (list src dest))]
;      [(lim-ins imm dest) (filter location? (list dest))]
;      [(binary-ins op src1 src2 dest) (filter location? (list src1 src2 dest))]
;      [(unary-ins op src dest) (filter location? (list src dest))]
;      [(uncond-jump-ins dest) (filter location? (list dest))]
;      [(cond-jump-ins src dest) (filter location? (list src dest))]
;      [(cond-jump-relop-ins op src1 src2 dest) (filter location? (list src1 src2 dest))]
;      [(push-ins src) (filter location? (list src))]
;      [(array-allocate-ins src dest) (filter location? (list src dest))]
;      [(deref-ins src1 src2) (filter location? (list src1 src2))]
;      [(ref-ins src1 src2) (filter location? (list src1 src2))]
;      [(deref-assign-ins src1 src2) (filter location? (list src1 src2))]
;      [(funcall-ins labloc params dest) (filter location? (list* labloc dest params))]
;      [(return-ins return-val-loc) (filter location? (list return-val-loc))]
;      [other-ins empty] ; all of the other instructions have no arguments that could possibly be locations
;      )))

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