#lang racket
(require "parser.rkt")
(require test-engine/racket-tests)

(define-syntax-rule (check-match exp pat)
  (check-expect
   (match exp
     [pat #t])
   #t))

(struct move-ins (src dest) #:transparent)
(struct lim-ins (imm dest) #:transparent) ; constant value imm is put into dest
(struct binary-ins (op src1 src2 dest) #:transparent)
(struct unary-ins (op src dest) #:transparent)
;(struct deref-ins (op src1 src2 dest) #:transparent) ; this instruction corresponds to x=*y, putting the r-value of y into the r-value of x

(struct location-binding (var loc) #:transparent)
(struct temp-loc (t) #:transparent) ; each location can either represent a location in memory, or a register
(struct mem-block (m size) #:transparent) ; the size is the location of the register or temporary holding the size of this block
(struct mem-loc (block offset) #:transparent)

(define (gen-temp) (temp-loc (gensym 't)))
(define (gen-mem size) (mem-block (gensym 'm) size)) ; we decided that size refers to the number of words this takes in the machine.  for
;most purposes a single word will hold one item, be that a pointer or integer.

; gen->
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
     (let [(sym (ormap (match-lambda 
                         [(location-binding var loc)
                          (and (eq? var name)
                               loc)])
                       loc-env))]
       (if sym
           (list (move-ins sym result-sym))
           (error (format "unbound variable ~a" name))))]
    
    #;[(array-creation type-id size-expr initval)
     (let* [(size-register (gen-temp)) 
            (size-gen-code (gen size-expr size-register loc-env))
            (block (gen-mem size-register))
            (initval-register (gen-temp))
            (initval-gen-code (gen initval initval-register loc-env))]
       (append (list size-gen-code initval-gen-code)
               (loop-over-this-array-and-initialize-everything))
       
       
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
    
    ; assignment doesn't overwrite ans. this is probably ok.
    [(assignment (id name) expr) ; TODO: record access and array access
     (let [(dest-loc (ormap (match-lambda [(and (location-binding var loc) binding)
                                            (if (eq? var name)
                                                loc
                                                #f)])
                             loc-env))]
       (gen expr dest-loc loc-env)
       )]
;    [(assignment (record-access rec-id field-id) val) ; depends on record declarations
;     ...]
;    [(assignment (array-access ...) val) ; TODO: array access
;     ... ]
    
    
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
               (gen body
                         result-sym
                         inner-loc-env)))]
    
    [(expseq exps) (apply append (map (λ (exp) (gen exp result-sym loc-env))
                                             exps))]
    
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
(test)