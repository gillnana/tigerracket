#lang racket
(require "parser.rkt")
;(require "canonicalize.rkt")
(require test-engine/racket-tests)

(define-syntax-rule (check-match exp pat)
  (check-expect
   (match exp
     [pat #t]
     [else #f])
   #t))


; INSTRUCTIONS
; each instruction also needs to potentially contain a label

(struct move-ins (src dest) #:transparent)
(struct lim-ins (imm dest) #:transparent) ; constant value imm is put into dest
(struct binary-ins (op src1 src2 dest) #:transparent)
(struct unary-ins (op src dest) #:transparent)

(struct cond-jump-ins (src dest) #:transparent) ; conditionally jumps if src is true
(struct cond-jump-relop-ins (op src1 src2 dest) #:transparent) ; conditionally jumps if (relop src1 src2) is true

(struct array-allocate-ins (src1 dest) #:transparent) ;this instruction allocates an array to some initial value, which most backends will do for free. src1 is the address of the expression to be inserted into the array.  dest is the mem-block struct which is the location of the array.

(struct pointer-set-ins (src1 src2) #:transparent) ; this instruction corresponds to x=*y, putting the r-value of y into the r-value of x
(struct deref-ins (src1 src2) #:transparent) ; this instruction corresponds to x=&y, putting the l-value of y into the r-value of x


; LOCATIONS

(struct location-binding (var loc) #:transparent)
(struct temp-loc (t) #:transparent) ; each location can either represent a location in memory, or a register
(struct mem-block (m size) #:transparent) ; the size is the location of the register or temporary holding the size of this block, which is itself an expression that must be computed at runtime
(struct mem-loc (block offset) #:transparent) ; the offset of a mem-loc is the location within the block, indexed from 0.  the block is represented by a location holding the address of that block.


(struct label (l) #:transparent)

; GENSYM PROCEDURES

(define (gen-temp) (temp-loc (gensym 't)))
(define (gen-mem size) (mem-block (gensym 'm) size)) ; we decided that size refers to the number of words this takes in the machine.  for
;most purposes a single word will hold one item, be that a pointer or integer.

(define (gen-label) (label (gensym 'l)))


(define (lookup id loc-env)
  (let [(result (ormap (match-lambda 
                         [(location-binding var loc)
                          (and (eq? var id)
                               loc)])
                       loc-env))]
    (if result
        result
        (error (format "internal error: unbound identifier ~a" id)))))
  

; gen ast symbol listof-location-binding -> listof-instruction
; takes an ast and returns a list of horrible spaghetti instructions with gotos and unreadable garbage and things
(define (gen-prog ast)
  (gen ast 'ans empty))

(define (gen ast result-sym loc-env)
  (match ast
    [(binary-op (op op) arg1 arg2) 
     (let [(sym1 (gen-temp))
           (sym2 (gen-temp))]
       (append (gen arg1 sym1 loc-env)
               (gen arg2 sym2 loc-env)
               (list (binary-ins op sym1 sym2 result-sym))))]
    [(unary-op (op op) arg)
     (let [(sym (gen-temp))]
       (append (gen arg sym loc-env) (list (unary-ins op sym result-sym))))]
    [(int-literal val)
     (list (move-ins val result-sym))]
    [(id name)
     (let [(sym (lookup name loc-env))]
       (if (temp-loc? sym)
           (list (move-ins sym result-sym))
           (error (format "internal error: identifier ~a found in wrong location type" name))))]
    
    [(array-creation type-id size-expr initval)
     (let* [(size-register (gen-temp)) 
            (size-gen-code (gen size-expr size-register loc-env))
            (block (gen-mem size-register))
            (initval-register (gen-temp))
            (initval-gen-code (gen initval initval-register loc-env))]
       (append size-gen-code initval-gen-code
               (list
                (deref-ins result-sym block)
                (array-allocate-ins initval-register block)))
       )]
      
    [(record-creation type-id fieldvals)
     (let* [(size-register (gen-temp))
            (size-gen-code (lim-ins (length fieldvals) size-register))
            (block (gen-mem size-register))]
       ;TODO should there be an malloc instruction that basically generates the code to do this first?
       ;TODO gen-mem is sort of malloc.  we need to make sure that it gets rid of the register it's using when we do register allocation
       (append (list size-gen-code)
               (map (λ (field offset)
                      (match field
                        [(fieldval name val)
                         (gen val (mem-loc block offset) loc-env)]))
                    fieldvals
                    (build-list (length fieldvals) values))))]
    
    ; assignment doesn't overwrite ans. this is ok.
    [(assignment (id name) expr)
     (let [(dest-loc (lookup name loc-env))]
       (if (temp-loc? dest-loc)
           (gen expr dest-loc loc-env)
           (error (format "internal error: identifier ~a or bound to wrong location type" name)))
       )]
    
;    [(assignment (record-access rec-id field-id) val) ; TODO: but depends on record declarations
;     ...]
    
    [(assignment (and ast-node (array-access (id array-id) index)) val)
     ;(displayln ast-node)
     ;(displayln loc-env)
     (let [(dest-loc (lookup array-id loc-env))
           (val-temp (gen-temp))
           ]
       (if (temp-loc? dest-loc)
           (append
             (gen val val-temp loc-env)
             (list (pointer-set-ins (mem-loc dest-loc index) val-temp)))
           (error (format "internal error: array ~a bound to wrong location type" array-id))))]
    
    
    ; leaving something in ans is ok. the program has already been typechecked.
    [(expseq exprs) (apply append (map (λ (expr) (gen expr result-sym loc-env)) exprs))]
    
    [(let-vars decs body)
     (let-values [((inner-loc-env decs-instructions)
                   (for/fold ([le loc-env]
                              [instructions empty])
                     [(dec decs)]
                     
                     (match dec
                       [(vardec id t-id expr)
                        (let [(sym (gen-temp))]
                          (values 
                           (cons (location-binding id sym) le)
                           (append instructions (gen expr sym le))))])))]
       (append decs-instructions
               (gen body result-sym inner-loc-env)))]
    
    ;[(let-types decs body)
    ; to do record declarations we need to decide on the structure of our records table
     
    [(if-statement cond then else)
     (let* [(then-register (gen-temp))
            (then-gen-code (gen then then-register loc-env))
            (else-register (gen-temp))
            (else-label (gen-label)) ; TODO figure out how this label can find something
            (else-gen-code (gen else else-register loc-env))
            (cond-gen-code
             (match cond
               [(binary-op (and op (or '> '< '>= '<=)) arg1 arg2)
                (let [(arg1-register (gen-temp))
                      (arg2-register (gen-temp))]
                  (append (gen arg1 arg1-register loc-env)
                          (gen arg2 arg2-register loc-env)
                          (list (cond-jump-relop-ins op arg1-register arg2-register else-label))))]
               [else
                (let [(cond-register (gen-temp))]
                  (append (gen cond cond-register loc-env)
                          (list (cond-jump-ins cond-register else-label))))]))]
       (append cond-gen-code
               then-gen-code
               (list else-label)
               else-gen-code))]
    )
  )


(check-match (gen-prog (parse-string "let var x := 0 in x := 3; x end")) 
             (list 
              (move-ins 0 loc1)
              (move-ins 3 loc1)
              (move-ins loc1 'ans)))

(check-match (gen-prog (parse-string "let var x := 0 in x := x+2; x end"))
             (list
              (move-ins 0 loc1)
              (move-ins loc1 loc2)
              (move-ins 2 loc3)
              (binary-ins '+ loc2 loc3 loc1)
              (move-ins loc1 'ans)))

(check-match (gen-prog (parse-string "let var x := 0 in x := x+2; x; () end"))  
             (list
              (move-ins 0 loc1)
              (move-ins loc1 loc2)
              (move-ins 2 loc3)
              (binary-ins '+ loc2 loc3 loc1)
              (move-ins loc1 'ans)))

(check-match (gen-prog (parse-string "let var y := 0 in let var x := (y := 2; 7) in y end end"))
             (list
              (move-ins 0 loc1)
              (move-ins 2 loc1)
              (move-ins 7 loc2)
              (move-ins loc1 'ans)))

(check-match (gen-prog (parse-string "int[10] of 15+3")) ;note that this fails to type check but we don't care
             (list
              (move-ins 10 (temp-loc t0))
              (move-ins 15 (temp-loc t3))
              (move-ins 3 (temp-loc t4))
              (binary-ins '+ (temp-loc t3) (temp-loc t4) (temp-loc t2))
              (deref-ins 'ans (mem-block m1 (temp-loc t0)))
              (array-allocate-ins (temp-loc t2) (mem-block m1 _))))

(check-match (gen-prog (parse-string "if 3 then ()"))
             (list
              (move-ins 3 (temp-loc t5))
              (cond-jump-ins (temp-loc t5) (label l4))
              (label l4)))

(check-match (gen-prog (parse-string "if 3 then 4 else 5"))
             (list
              (move-ins 3 (temp-loc t5))
              (cond-jump-ins (temp-loc t5) (label l4))
              (move-ins 4 (temp-loc t2))
              (label l4)
              (move-ins 5 (temp-loc t3))))

(check-match (gen-prog (parse-string "let var a := int[10] of 1 in a[5] := 6 end"))
             (list
              (move-ins 10 (temp-loc t0))
              (move-ins 1 (temp-loc t2))
              (deref-ins (temp-loc t9) (mem-block m1 (temp-loc t0)))
              (array-allocate-ins (temp-loc t2) (mem-block m1 _))
              (move-ins 6 (temp-loc t3))
              (pointer-set-ins (mem-loc (temp-loc t9) (int-literal 5)) (temp-loc t3))))


(test)