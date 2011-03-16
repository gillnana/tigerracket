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

(struct uncond-jump-ins (dest) #:transparent)
(struct cond-jump-ins (src dest) #:transparent) ; conditionally jumps if src is true
(struct cond-jump-relop-ins (op src1 src2 dest) #:transparent) ; conditionally jumps if (relop src1 src2) is true

(struct push-ins (src) #:transparent) ; pushes the contents of src onto the stack as a function parameter

(struct array-allocate-ins (src1 dest) #:transparent) ;this instruction allocates an array to some initial value, which most backends will do for free. src1 is the address of the expression to be inserted into the array.  dest is the mem-block struct which is the location of the array.

(struct pointer-set-ins (src1 src2) #:transparent) ; this instruction corresponds to x=*y, putting the r-value of y into the r-value of x
(struct ref-ins (src1 src2) #:transparent) ; this instruction corresponds to x=&y, putting the l-value of y into the r-value of x
(struct deref-ins (src src2) #:transparent) ; this instruction corresponds to x*=y, putting the l-value of y into the l-value of x

; LOCATIONS

(struct location-binding (var loc) #:transparent)

; a word-sized loc is either:
;   - a temp-loc, which may be either a register or a word in memory
(struct temp-loc (t) #:transparent)

; a malloc'd block of memory is:
;   m is a gensym to uniquely identify the block
;   size is a location that (at runtime) holds the size of the block to be allocated
(struct mem-block (m size) #:transparent) ; the size is the location of the register or temporary holding the size of this block, which is itself an expression that must be computed at runtime
(struct mem-loc (block offset) #:transparent) ; the offset of a mem-loc is the location within the block, described as the number of words from the beginning, indexed from 0.  the block is represented by a location holding the address of that block.


(struct label-loc (l) #:transparent)
(struct param-loc (p param-number) #:transparent)

(struct return-val-loc () #:transparent) ;location of the return value

;TODO figure out how to hold on to the size for bounds checking


(struct record-table-entry (name pointer? offset) #:transparent) ; the pointer? of a record-table-entry is a boolean describing whether or not this word of the record is a pointer or if the bits of the word actually contain the desired data.  this is #t for integers and #f otherwise.  the offset is given as number of words indexed from 0.

(struct label (l) #:transparent)

(struct stack-setup-ins () #:transparent) ; in MIPS, push ra and fp onto stack, sets new fp to current value of sp
(struct stack-teardown-ins () #:transparent) ; pops fp and ra from stack into registers, sets stack to old value of sp
(struct jump-to-return-address-ins () #:transparent)

(struct funcall-ins (dest num-params return-val) #:transparent) ;num-params is a statically determined integer.  the return value of the funcall is placed in the return-val location
(struct return-ins (return-val-loc) #:transparent) ;represents an instruction that puts the return value of a function in the location it should go, wherever that may be


; GENSYM PROCEDURES

(define (gen-temp) (temp-loc (gensym 't)))
(define (gen-label-temp) (label-loc (gensym 'lt)))
(define (gen-param param-num) (param-loc (gensym 'p) param-num))
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
(define (gen-prog prog)
  (gen (canonicalize (parse-string prog)) 'ans empty (make-immutable-hash empty)))

(define (gen ast result-sym loc-env record-table)
  (match ast
    [(binary-op (op op) arg1 arg2) 
     (let [(sym1 (gen-temp))
           (sym2 (gen-temp))]
       (append (gen arg1 sym1 loc-env record-table
                    )
               (gen arg2 sym2 loc-env record-table)
               (list (binary-ins op sym1 sym2 result-sym))))]
    [(unary-op (op op) arg)
     (let [(sym (gen-temp))]
       (append (gen arg sym loc-env) (list (unary-ins op sym result-sym))))]
    [(int-literal val)
     (list (lim-ins val result-sym))]
    [(id name)
     (let [(sym (lookup name loc-env))]
       (if (or (temp-loc? sym) (param-loc? sym) (label-loc? sym))
           (list (move-ins sym result-sym))
           (error (format "internal error: identifier ~a found in wrong location type ~a" name sym))))]
    
    [(array-creation type-id size-expr initval)
     (let* [(size-register (gen-temp)) 
            (size-gen-code (gen size-expr size-register loc-env record-table))
            (block (gen-mem size-register))
            (initval-register (gen-temp))
            (initval-gen-code (gen initval initval-register loc-env record-table))]
       (append size-gen-code initval-gen-code
               (list
                (deref-ins result-sym block)
                (array-allocate-ins initval-register block)))
       )]
      
    [(record-creation type-id fieldvals)
     (let* [(size-register (gen-temp))
            (size-gen-code (list (lim-ins (length fieldvals) size-register)))
            (block (gen-mem size-register))
            
            ]
       ;TODO should there be an malloc instruction that basically generates the code to do this first?
       ;TODO gen-mem is sort of malloc.  we need to make sure that it gets rid of the register it's using when we do register allocation
       (apply append size-gen-code
              (list (deref-ins result-sym block))
              (map (λ (field offset)
                     (match field
                       [(fieldval name val)
                        (gen val (mem-loc block offset) loc-env record-table)]))
                   fieldvals
                   (build-list (length fieldvals) values))))]
;    
;    [(record-creation type-id fieldvals)
;     (let* [(size-register (gen-temp))
;            (size-gen-code (lim-ins (length fieldvals) size-register)
;    
    ; assignment doesn't overwrite ans. this is ok.
    [(assignment (id name) expr)
     (let [(dest-loc (lookup name loc-env))]
       (if (temp-loc? dest-loc)
           (gen expr dest-loc loc-env record-table)
           (error (format "internal error: identifier ~a or bound to wrong location type" name)))
       )]
    
;    [(assignment (record-access rec-id field-id) val) ; TODO: but depends on record declarations
;     ...]
    
    [(assignment (and ast-node (array-access (id array-id) index)) val)
     ;(displayln ast-node)
     ;(displayln loc-env)
     (let* [(dest-loc (lookup array-id loc-env))
           (val-temp (gen-temp))
           (val-gen-code (gen val val-temp loc-env record-table))
           (index-temp (gen-temp))
           (index-gen-code (gen index index-temp loc-env record-table))
           ]
       (if (temp-loc? dest-loc)
           (append
            index-gen-code
            val-gen-code
            (list (pointer-set-ins (mem-loc dest-loc index-temp) val-temp)))
           (error (format "internal error: array ~a bound to wrong location type" array-id))))]
    
    
    ; leaving something in ans is ok. the program has already been typechecked.
    [(expseq exprs) (apply append (map (λ (expr) (gen expr result-sym loc-env record-table)) exprs))]
    
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
                           (append instructions (gen expr sym le record-table))))])))]
       (append decs-instructions
               (gen body result-sym inner-loc-env record-table)))]
    
    [(let-types decs body)
     (let [(updated-record-table
            (foldl (λ (dec table)
                     (match dec
                       [(tydec ty-id (record-of tyfields))
                        (hash-set table
                                  ty-id
                                  (map (λ (tyf offset)
                                         (match tyf
                                           [(tyfield tyf-id tyf-ty)
                                            (record-table-entry tyf-id (not (equal? (type-id 'int) tyf-ty)) offset)]))
                                       ;TODO: find a smarter way to figure out if the type of the thing in this record is a pointer
                                       ; this is only a problem because some stupid programmer might decide to rebind int
                                       ; i'm okay with letting it be a pointer if someone does type a = int and declares it of type a
                                       tyfields
                                       (build-list (length tyfields) values)))]
                       [else table]))
                   record-table
                   decs))]
       
       ;(displayln updated-record-table)
       (gen body result-sym loc-env updated-record-table))]
     
    [(if-statement cond then (expseq empty))
     (let* [(end-label (gen-label))
            (then-register (gen-temp))
            (then-gen-code (append (gen then then-register loc-env record-table) ))
            (cond-gen-code (create-conditional-jump cond end-label loc-env record-table))]
       (append cond-gen-code
               then-gen-code
               (list end-label)))]
    
    [(if-statement cond then else)
     (let* [(end-label (gen-label))
            (then-register (gen-temp))
            (else-label (gen-label))
            (else-register (gen-temp))
            (then-gen-code (append (gen then then-register loc-env record-table)
                                   (list (move-ins then-register result-sym)
                                         (uncond-jump-ins end-label)
                                         else-label)))
            (else-gen-code (gen else else-register loc-env record-table))
            (cond-gen-code (create-conditional-jump cond else-label loc-env record-table))]
       (append cond-gen-code
               then-gen-code
               else-gen-code
               (list (move-ins else-register result-sym) end-label)))]
    
    [(while-statement cond body)
     (let* [(start-label (gen-label))
            (end-label (gen-label))
            (cond-gen-code (create-conditional-jump cond end-label loc-env record-table))
            (dummy-location (gen-temp))
            (body-gen-code (gen body dummy-location loc-env record-table))]
       (append (list start-label) cond-gen-code body-gen-code
               (list (uncond-jump-ins start-label) end-label)))]
    
    [(nil) (list (lim-ins 0 result-sym))]
    
    
    [(let-funs decs body)
     (let [(skip-label (gen-label))]
       
       (let-values 
           [((inner-loc-env fun-instructions )
             (for/fold ([le loc-env]
                        [instructions empty])
               [(dec decs)]
               
               (match dec
                 [(fundec id tyfields type-id fun-body)
                  (let [;(fun-id-sym (gen-temp))
                        (fun-label (gen-label))
                        (loc-of-label (gen-label-temp))
                        (loc-of-result (gen-temp))] ; TODO: maybe return-val-loc??
                    
                    (values 
                     (cons (location-binding id loc-of-label) le)
                     (append instructions 
                             (list fun-label (stack-setup-ins))
                             (gen 
                              fun-body
                              loc-of-result
                              (append
                               (map (λ (tyf param-num)
                                      (match tyf
                                        [(tyfield ty-name ty-ty)
                                         (location-binding ty-name 
                                                           (gen-param param-num))]))
                                    tyfields
                                    (build-list (length tyfields) add1))
                               le)
                              record-table)
                             (list (return-ins loc-of-result)
                                   (stack-teardown-ins)
                                   (jump-to-return-address-ins)
                                   ))))])))]
         (append (list (uncond-jump-ins skip-label))
                 fun-instructions
                 (list skip-label)
                 (gen body result-sym inner-loc-env record-table))))]
    
    [(funcall fun-id args)
     ;TODO evaluate fun-id which might be any expression
     (let* [(f (lookup (id-name fun-id) loc-env))
           (arg-sym-list (build-list (length args) (λ (ignore) (gen-temp))))
           (label-here (gen-label))
           (label-holder (gen-temp))
           (param-gen-code (append
                            (map (λ (arg param-sym)
                                   (gen arg param-sym loc-env record-table))
                                 args arg-sym-list)))
           ]
       (if (label-loc? f)
           
           (append param-gen-code
                   (map push-ins arg-sym-list)
                   (list (funcall-ins f (length args) result-sym) 
                         ) ; the last argument is the return address
                   )
           
           
           (error (format "internal error: function ~a bound to wrong location type" fun-id f))))]
     
    )
  )

(define (create-conditional-jump condition to-label loc-env record-table)
  (match condition
    [(binary-op (op (and op (or '> '< '>= '<=))) arg1 arg2)
     (let [(arg1-register (gen-temp))
           (arg2-register (gen-temp))]
       (append (gen arg1 arg1-register loc-env record-table)
               (gen arg2 arg2-register loc-env record-table)
               (list (cond-jump-relop-ins op arg1-register arg2-register to-label))))]
    [else
     (let [(cond-register (gen-temp))]
       (append (gen condition cond-register loc-env record-table)
               (list (cond-jump-ins cond-register to-label))))]))


(check-match (gen-prog "let var x := 0 in x := 3; x end") 
             (list 
              (lim-ins 0 loc1)
              (lim-ins 3 loc1)
              (move-ins loc1 'ans)))

(check-match (gen-prog "let var x := 0 in x := x+2; x end")
             (list
              (lim-ins 0 loc1)
              (move-ins loc1 loc2)
              (lim-ins 2 loc3)
              (binary-ins '+ loc2 loc3 loc1)
              (move-ins loc1 'ans)))

(check-match (gen-prog "let var x := 0 in x := x+2; x; () end")  
             (list
              (lim-ins 0 loc1)
              (move-ins loc1 loc2)
              (lim-ins 2 loc3)
              (binary-ins '+ loc2 loc3 loc1)
              (move-ins loc1 'ans)))

(check-match (gen-prog "let var y := 0 in let var x := (y := 2; 7) in y end end")
             (list
              (lim-ins 0 loc1)
              (lim-ins 2 loc1)
              (lim-ins 7 loc2)
              (move-ins loc1 'ans)))

(check-match (gen-prog "int[10] of 15+3") ;note that this fails to type check but we don't care
             (list
              (lim-ins 10 (temp-loc t0))
              (lim-ins 15 (temp-loc t3))
              (lim-ins 3 (temp-loc t4))
              (binary-ins '+ (temp-loc t3) (temp-loc t4) (temp-loc t2))
              (deref-ins 'ans (mem-block m1 (temp-loc t0)))
              (array-allocate-ins (temp-loc t2) (mem-block m1 _))))

(check-match (gen-prog "if 3 then ()")
             (list
              (lim-ins 3 (temp-loc t5))
              (cond-jump-ins (temp-loc t5) (label l4))
              (label l4)))

(check-match (gen-prog "if 3 then (5;())")
             (list
              (lim-ins 3 (temp-loc t5))
              (cond-jump-ins (temp-loc t5) (label l4))
              (lim-ins 5 (temp-loc t6))
              (label l4)))


(check-match (gen-prog "let var x : int := 26 in if 3 then x := 7 end")
             (list
              (lim-ins 26 (temp-loc t3))
              (lim-ins 3 (temp-loc t5))
              (cond-jump-ins (temp-loc t5) (label l4))
              (lim-ins 7 (temp-loc t3))
              (label l4)))

(check-match (gen-prog "if 3 then 4 else 5")
             (list
              (lim-ins 3 (temp-loc t5))
              (cond-jump-ins (temp-loc t5) (label l4))
              (lim-ins 4 (temp-loc t2))
              (move-ins (temp-loc t2) 'ans)
              (uncond-jump-ins (label l1))
              (label l4)
              (lim-ins 5 (temp-loc t3))
              (move-ins (temp-loc t3) 'ans)
              (label l1)))

(check-match (gen-prog "let var a := int[10] of 1 in a[5] := 6 end")
             (list
              (lim-ins 10 (temp-loc t0))
              (lim-ins 1 (temp-loc t2))
              (deref-ins (temp-loc t9) (mem-block m1 (temp-loc t0)))
              (array-allocate-ins (temp-loc t2) (mem-block m1 _))
              (lim-ins 5 (temp-loc t4))
              (lim-ins 6 (temp-loc t3))
              (pointer-set-ins (mem-loc (temp-loc t9) (temp-loc t4)) (temp-loc t3))))

(check-match (gen-prog "if 4>1 then 0 else 16")
             (list
              (lim-ins 4 (temp-loc t0))
              (lim-ins 1 (temp-loc t1))
              (cond-jump-relop-ins '> (temp-loc t0) (temp-loc t1) (label l9))
              (lim-ins 0 (temp-loc t7))
              (move-ins (temp-loc t7) 'ans)
              (uncond-jump-ins (label l6))
              (label l9)
              (lim-ins 16 (temp-loc t8))
              (move-ins (temp-loc t8) 'ans)
              (label l6)))

(check-match (gen-prog "let type a = {x:int} var z : a := nil in z := a{x=5} end")
             (list
              (lim-ins 0 (temp-loc t1))
              (lim-ins 1 (temp-loc t2))
              (deref-ins (temp-loc t1) (mem-block m3 (temp-loc t2)))
              (lim-ins 5 (mem-loc (mem-block m3 _) 0))))

(check-match (gen-prog "while 3 do (7;())")
             (list
              (label l2)
              (lim-ins 3 (temp-loc t4))
              (cond-jump-ins (temp-loc t4) (label l3))
              (lim-ins 7 (temp-loc t5))
              (uncond-jump-ins (label l2))
              (label l3)))

(test)
