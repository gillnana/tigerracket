#lang racket
(require "parser.rkt")
(require "typecheck.rkt")
(require "canonicalize.rkt")
;(require "dag.rkt")
(require test-engine/racket-tests)

(provide (all-defined-out))

(define-syntax-rule (check-match exp pat)
  (check-expect
   (match exp
     [pat #t]
     [else #f])
   #t))



(struct fxn-block (label ins-list) #:transparent)

; INSTRUCTIONS
; each instruction also needs to potentially contain a label

(define (location? item)
  (or (temp-loc? item) (mem-loc? item) (label-loc? item) (param-loc? item) (return-val-loc? item)))

(struct move-ins (src dest) #:transparent)
(struct lim-ins (imm dest) #:transparent) ; constant value imm is put into dest
(struct binary-ins (op src1 src2 dest) #:transparent)
(struct unary-ins (op src dest) #:transparent)

(struct uncond-jump-ins (dest) #:transparent)
(struct cond-jump-ins (src dest) #:transparent) ; conditionally jumps if src is true
(struct cond-jump-relop-ins (op src1 src2 dest) #:transparent) ; conditionally jumps if (relop src1 src2) is true

(struct push-ins (src) #:transparent) ; pushes the contents of src onto the stack as a function parameter

(struct break-ins () #:transparent) ;breaks out of a current for loop.  expects loops to be implemented as tail-recursive functions

(struct array-allocate-ins (src1 dest) #:transparent) ;this instruction allocates an array to some initial value, which most backends will do for free. src1 is the address of the expression to be inserted into the array.  dest is the mem-block struct which is the location of the array.

(struct deref-ins (src1 src2) #:transparent) ; this instruction corresponds to x=*y, putting the r-value of y into the r-value of x
(struct ref-ins (src1 src2) #:transparent) ; this instruction corresponds to x=&y, putting the l-value of y into the r-value of x
(struct deref-assign-ins (src1 src2) #:transparent) ; this instruction is x*=y, putting the r-value of y into the l-value of x



(struct stack-setup-ins () #:transparent) ; in MIPS, push ra and fp onto stack, sets new fp to current value of sp
(struct stack-teardown-ins () #:transparent) ; pops fp and ra from stack into registers, sets stack to old value of sp
(struct jump-to-return-address-ins () #:transparent)

;(struct funcall-ins (labloc num-params return-val) #:transparent) ;num-params is a statically determined integer.  the return value of the funcall is placed in the return-val location

; this is a better representation; you don't always put arguments on the stack. you sometimes use $a0-$a3
(struct funcall-ins (labloc params dest) #:transparent)

(struct return-ins (return-val-loc) #:transparent) ;represents an instruction that puts the return value of a function in the location it should go, wherever that may be


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


;(struct record-table-entry (name pointer? offset) #:transparent) ; the pointer? of a record-table-entry is a boolean describing whether or not this word of the record is a pointer or if the bits of the word actually contain the desired data.  this is #t for integers and #f otherwise.  the offset is given as number of words indexed from 0.

(struct label (l) #:transparent)



; GENSYM PROCEDURES

(define (gen-temp) (temp-loc (gensym 't)))
(define (gen-label-loc) (label-loc (gensym 'lt)))
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
  (gen prog 'ans empty))

(define (reset-dag-table!) (set! dag-table (make-hash)))

(define (gen ast result-sym loc-env)
  (reset-dag-table!)
  (fxn-block 'main (dag-gen ast result-sym loc-env)))

(define dag-table (make-hash))

; 
(define (dag-gen ast result-sym loc-env)
  ;(displayln dag-table)
  (let [(cached-node (hash-ref dag-table ast #f))]
    (if cached-node
        (list (move-ins cached-node result-sym))
        (begin
          (hash-set! dag-table ast result-sym)
          (gen-helper ast result-sym loc-env)))))

(define (gen-lv ast result-sym loc-env)
  (displayln ast)
  (match ast
    [(id name)
     (let [(sym (lookup name loc-env))]
       (displayln loc-env)
       (if (temp-loc? sym)
           (list (ref-ins result-sym sym))
           (error "huge error.")))]
    [(array-access arr indx)
     (let* [(indx-temp (gen-temp))
            (indx-gen-code (gen indx indx-temp loc-env))
            (arr-temp (gen-temp))
            (arr-gen-code (gen-lv arr arr-temp loc-env))]
       (append 
        indx-gen-code
        arr-gen-code
        (list (binary-ins '+ result-sym arr-temp indx-temp))))]
    [(record-access rec indx offset)
     (let* [(offset-temp (gen-temp))
            (rec-temp (gen-temp))
            (rec-gen-code (gen-lv rec rec-temp loc-env))]
       (append rec-gen-code
               (list (lim-ins offset offset-temp)
                     (binary-ins '+ result-sym rec-temp offset-temp))))]))

(define (gen-helper ast result-sym loc-env)
  (match ast
    ;ARITHMETIC
    [(binary-op (op op) arg1 arg2) 
     (let [(sym1 (gen-temp))
           (sym2 (gen-temp))]
       (append (dag-gen arg1 sym1 loc-env)
               (dag-gen arg2 sym2 loc-env)
               (list (binary-ins op sym1 sym2 result-sym))))]
    [(unary-op (op op) arg)
     (let [(sym (gen-temp))]
       (append (dag-gen arg sym loc-env) (list (unary-ins op sym result-sym))))]
    ;LITERAL OR STUPID VALUES
    [(int-literal val)
     (list (lim-ins val result-sym))]
    
    [(string-literal val)
     (list (lim-ins val result-sym))]
    
    [(id name)
     (let [(sym (lookup name loc-env))]
       (if (or (temp-loc? sym) (param-loc? sym) (label-loc? sym))
           (list (move-ins sym result-sym))
           (error (format "internal error: identifier ~a found in wrong location type ~a" name sym))))]
    
    ; STRUCTURE CREATION
    [(array-creation type-id size-expr initval)
     (let* [(size-register (gen-temp)) 
            (size-gen-code (dag-gen size-expr size-register loc-env))
            (block (gen-mem size-register))
            (initval-register (gen-temp))
            (initval-gen-code (dag-gen initval initval-register loc-env))]
       (append size-gen-code initval-gen-code
               (list
                (ref-ins result-sym block)
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
              (list (ref-ins result-sym block))
              (map (λ (field offset)
                     (match field
                       [(fieldval name val)
                        (dag-gen val (mem-loc block offset) loc-env )]))
                   fieldvals
                   (build-list (length fieldvals) values))))]
    ;    
    ;    [(record-creation type-id fieldvals)
    ;     (let* [(size-register (gen-temp))
    ;            (size-gen-code (lim-ins (length fieldvals) size-register)
    ;    
    ; assignment doesn't overwrite ans. this is ok.
    [(assignment (id name) expr)
     (let* [(dest-loc (lookup name loc-env))
            (rval-gen-code (dag-gen expr dest-loc loc-env ))]
       (reset-dag-table!)
       (if (temp-loc? dest-loc)
           rval-gen-code
           (error (format "internal error: identifier ~a or bound to wrong location type" name)))
       )]
    
    ;    [(assignment (record-access rec-id field-id) val) ; TODO: but depends on record declarations
    ;     ...]
    
    [(assignment (and ast-node (array-access (id array-id) index)) val)
     ;(displayln ast-node)
     ;(displayln loc-env)
     (let* [(dest-loc (lookup array-id loc-env))
            (val-temp (gen-temp))
            (val-gen-code (dag-gen val val-temp loc-env ))
            (index-temp (gen-temp))
            (index-gen-code (dag-gen index index-temp loc-env ))
            ]
       (reset-dag-table!)
       (if (temp-loc? dest-loc)
           (append
            index-gen-code
            val-gen-code
            (list (deref-ins (mem-loc dest-loc index-temp) val-temp)))
           (error (format "internal error: array ~a bound to wrong location type" array-id))))]
    
    
    ; leaving something in ans is ok. the program has already been typechecked.
    [(expseq exprs) (apply append (map (λ (expr) (dag-gen expr result-sym loc-env )) exprs))]
;    
;    [(record-creation type-id fieldvals)
;     (let* [(size-register (gen-temp))
;            (size-gen-code (lim-ins (length fieldvals) size-register)
;    
    #;[(assignment lval val)
     (let* [(lval-temp (gen-temp))
            (lval-gen-code (gen-lv lval lval-temp loc-env))
            (val-temp (gen-temp))
            (val-gen-code (gen val val-temp loc-env))]
       (append
        lval-gen-code
        val-gen-code
        (list (deref-assign-ins lval-temp val-temp))))] 
    
    
    ; leaving something in ans is ok. the program has already been typechecked.
    
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
                           (append instructions (dag-gen expr sym le))))])))]
       (append decs-instructions
               (dag-gen body result-sym inner-loc-env)))]
    
    [(let-types decs body)
     (gen body result-sym loc-env)]
     
    [(if-statement cond then (expseq empty))
     (let* [(end-label (gen-label))
            (then-register (gen-temp))
            (then-gen-code (append (dag-gen then then-register loc-env) ))
            (cond-gen-code (create-conditional-jump cond end-label loc-env))]
       (append cond-gen-code
               then-gen-code
               (list end-label)))]
    
    [(if-statement cond then else)
     (let* [(end-label (gen-label))
            (then-register (gen-temp))
            (else-label (gen-label))
            (else-register (gen-temp))
            (then-gen-code (append (dag-gen then then-register loc-env )
                                   (list (move-ins then-register result-sym)
                                         (uncond-jump-ins end-label)
                                         else-label)))
            (else-gen-code (dag-gen else else-register loc-env ))
            (cond-gen-code (create-conditional-jump cond else-label loc-env ))]
       (reset-dag-table!)
       (append cond-gen-code
               then-gen-code
               else-gen-code
               (list (move-ins else-register result-sym) end-label)))]
    
    [(while-statement cond body)
     (begin
       (reset-dag-table!)
       (let* [(start-label (gen-label))
              (end-label (gen-label))
              (cond-gen-code (create-conditional-jump cond end-label loc-env ))
              (dummy-location (gen-temp))
              (body-gen-code (dag-gen body dummy-location loc-env ))]
         (append (list start-label) cond-gen-code body-gen-code
                 (list (uncond-jump-ins start-label) end-label))))]
    
    [(nil) (list (lim-ins 0 result-sym))]
    
    
;    [(let-funs decs body)
;     (begin
;       (reset-dag-table!)
;       (let [(skip-label (gen-label))
;             (fun-labels (build-list (length decs) (λ (ignore) (gen-label))))
;             (loc-of-labels (build-list (length decs) (λ (ignore) (gen-label-loc))))
;             
;             ]
;         
;         (let-values 
;             [((inner-loc-env fun-instructions )
;               (for/fold ([le loc-env]
;                          [instructions empty])
;                 [(dec decs)
;                  (fun-label fun-labels)
;                  (loc-of-label loc-of-labels)]
;                 
;                 (match dec
;                   [(fundec id tyfields type-id fun-body)
;                    (let [;(fun-id-sym (gen-temp))
;                          ;(fun-label (gen-label))
;                          ;(loc-of-label (gen-label-temp))
;                          (loc-of-result (gen-temp))] ; TODO: maybe return-val-loc??
;                      
;                      (values 
;                       (cons (location-binding id loc-of-label) le)
;                       (append instructions 
;                               (list fun-label (stack-setup-ins))
;                               (dag-gen fun-body
;                                        loc-of-result
;                                        (append
;                                         (map (λ (tyf param-num)
;                                                (match tyf
;                                                  [(tyfield ty-name ty-ty)
;                                                   (location-binding ty-name 
;                                                                     (gen-param param-num))]))
;                                              tyfields
;                                              (build-list (length tyfields) add1))
;                                         le)
;                                       
;                                        )
;                               (list (return-ins loc-of-result)
;                                     (stack-teardown-ins)
;                                     (jump-to-return-address-ins)))))])))]
;           (append (map (λ (fun-label loc-of-label) (lim-ins fun-label loc-of-label)) fun-labels loc-of-labels)
;                   ;(list (uncond-jump-ins skip-label))
;                   fun-instructions
;                   ;(list skip-label)
;                   (dag-gen body result-sym inner-loc-env )))))]
    
    [(let-funs decs body)
     (begin
       (reset-dag-table!)
       ; TODO: this is EXTREMELY SILLY for now
       ; assume that this is the top-level let-standard-library thingy
       (let-values [((inner-loc-env stdlib-instructions)
                   (for/fold ([le loc-env]
                              [instructions empty])
                     [(dec decs)]
                     (match dec
                       [(fundec id tyfields type-id (stdlibfxn label _))
                        (let [(sym (gen-label-loc))]
                          (values 
                           (cons (location-binding id sym) le)
                           (cons (lim-ins (string->symbol (string-append "lt_" (symbol->string label))) sym) instructions)
                           ))])))]
       (append stdlib-instructions
               (dag-gen body result-sym inner-loc-env)))
       )]
     
    
    [(funcall fun-id args)
     ;TODO evaluate fun-id which might be any expression
     (let* [(f (lookup (id-name fun-id) loc-env))
            (arg-sym-list (build-list (length args) (λ (ignore) (gen-temp))))
            (label-here (gen-label))
            (label-holder (gen-temp))
            (param-gen-code (apply append
                             (map (λ (arg param-sym)
                                    (dag-gen arg param-sym loc-env ))
                                  args arg-sym-list)))
            ]
       (if (label-loc? f)
           
           (append param-gen-code
                   ;(map push-ins arg-sym-list)
                   (list (funcall-ins f arg-sym-list result-sym)
                         ) ; the last argument is the return address
                   )
           
           
           (error (format "internal error: function ~a bound to wrong location type" fun-id f))))]
    
    [(break) (list (break-ins))]
    ))


(define (create-conditional-jump condition to-label loc-env )
  (match condition
    [(binary-op (op (and op (or '> '< '>= '<=))) arg1 arg2)
     (let [(arg1-register (gen-temp))
           (arg2-register (gen-temp))]
       (append (dag-gen arg1 arg1-register loc-env )
               (dag-gen arg2 arg2-register loc-env )
               (list (cond-jump-relop-ins op arg1-register arg2-register to-label))))]
    [else
     (let [(cond-register (gen-temp))]
       (append (dag-gen condition cond-register loc-env )

               (list (cond-jump-ins cond-register to-label))))]))



;(check-match (gen-prog (canonicalize (parse-string "let var x := 0 in x := 3; x end"))) 
;             (list 
;              (lim-ins 0 x)                   ; int x = 0;
;              (ref-ins dest x)                ; int* destination = &x;
;              (lim-ins 3 val)                 ; int value = 3;
;              (deref-assign-ins dest val)     ; *destination = value;
;              (move-ins x 'ans)))             ; ans = x;
;
;(check-match (gen-prog (canonicalize (parse-string  "let var x := 0 in x := x+2; x end")))
;             (list
;              (lim-ins 0 x)                   ; int x = 0;
;              (ref-ins dest x)                ; int* destination = &x;
;              (move-ins x arg1)               ; int plusarg1 = x;
;              (lim-ins 2 arg2)                ; int plusarg2 = 2;
;              (binary-ins '+ arg1 arg2 res)   ; int plusresult = plusarg1 + plusarg2;
;              (deref-assign-ins dest res)     ; *destination = plusresult;
;              (move-ins x 'ans)))             ; ans = x;
;
;(check-match (gen-prog (canonicalize (parse-string  "let var x := 0 in x := x+2; x; () end")))  
;             (list
;              (lim-ins 0 x)                   ; int x = 0;
;              (ref-ins dest x)                ; int* destination = &x; (canonicalize (parse-string 
;              (move-ins x arg1)               ; int plusarg1 = x;
;              (lim-ins 2 arg2)                ; int plusarg2 = 2;
;              (binary-ins '+ arg1 arg2 res)   ; int plusresult = plusarg1 + plusarg2;
;              (deref-assign-ins dest res)     ; *destination = plusresult;
;              (move-ins x 'ans)))             ; ans = x;
;
;(check-match (gen-prog (canonicalize (parse-string  "let var y := 0 in let var x := (y := 2; 7) in y end end")))
;             (list
;              (lim-ins 0 y)                   ; int y := 0
;              (ref-ins dest y)                ; int* destination = &y;
;              (lim-ins 2 val)                 ; int val = 2;
;              (deref-assign-ins dest val)     ; *destination = val;
;              (lim-ins 7 x)                   ; int x = 7;
;              (move-ins y 'ans)))             ; ans = y;
;
;(check-match (gen-prog (canonicalize (parse-string  "int[10] of 15+3"))) ;note that this fails to type check but we don't care
;             (list
;              (lim-ins 10 (temp-loc t0))
;              (lim-ins 15 (temp-loc t3))
;              (lim-ins 3 (temp-loc t4))
;              (binary-ins '+ (temp-loc t3) (temp-loc t4) (temp-loc t2))
;              (ref-ins 'ans (mem-block m1 (temp-loc t0)))
;              (array-allocate-ins (temp-loc t2) (mem-block m1 _))))
;
;(check-match (gen-prog (canonicalize (parse-string  "if 3 then ()")))
;             (list
;              (lim-ins 3 (temp-loc t5))
;              (cond-jump-ins (temp-loc t5) (label l4))
;              (label l4)))
;
;(check-match (gen-prog (canonicalize (parse-string  "if 3 then (5;())")))
;             (list
;              (lim-ins 3 (temp-loc t5))
;              (cond-jump-ins (temp-loc t5) (label l4))
;              (lim-ins 5 (temp-loc t6))
;              (label l4)))
;
;
;(check-match (gen-prog (canonicalize (parse-string  "let var x : int := 26 in if 3 then x := 7 end")))
;             (list
;              (lim-ins 26 (temp-loc t3))
;              (lim-ins 3 (temp-loc t5))
;              (cond-jump-ins (temp-loc t5) (label l4))
;              (lim-ins 7 (temp-loc t3))
;              (label l4)))
;
;(check-match (gen-prog (canonicalize (parse-string  "if 3 then 4 else 5")))
;             (list
;              (lim-ins 3 (temp-loc t5))
;              (cond-jump-ins (temp-loc t5) (label l4))
;              (lim-ins 4 (temp-loc t2))
;              (move-ins (temp-loc t2) 'ans)
;              (uncond-jump-ins (label l1))
;              (label l4)
;              (lim-ins 5 (temp-loc t3))
;              (move-ins (temp-loc t3) 'ans)
;              (label l1)))
;
;(check-match (gen-prog (canonicalize (parse-string  "let var a := int[10] of 1 in a[5] := 6 end")))
;             (list
;              (lim-ins 10 (temp-loc t0))
;              (lim-ins 1 (temp-loc t2))
;              (ref-ins (temp-loc t9) (mem-block m1 (temp-loc t0)))
;              (array-allocate-ins (temp-loc t2) (mem-block m1 _))
;              (lim-ins 5 (temp-loc t4))
;              (lim-ins 6 (temp-loc t3))
;              (deref-ins (mem-loc (temp-loc t9) (temp-loc t4)) (temp-loc t3))))
;
;(check-match (gen-prog (canonicalize (parse-string  "if 4>1 then 0 else 16")))
;             (list
;              (lim-ins 4 (temp-loc t0))
;              (lim-ins 1 (temp-loc t1))
;              (cond-jump-relop-ins '> (temp-loc t0) (temp-loc t1) (label l9))
;              (lim-ins 0 (temp-loc t7))
;              (move-ins (temp-loc t7) 'ans)
;              (uncond-jump-ins (label l6))
;              (label l9)
;              (lim-ins 16 (temp-loc t8))
;              (move-ins (temp-loc t8) 'ans)
;              (label l6)))
;
;(check-match (gen-prog (canonicalize (parse-string  "let type a = {x:int} var z : a := nil in z := a{x=5} end")))
;             (list
;              (lim-ins 0 (temp-loc t1))
;              (lim-ins 1 (temp-loc t2))
;              (ref-ins (temp-loc t1) (mem-block m3 (temp-loc t2)))
;              (lim-ins 5 (mem-loc (mem-block m3 _) 0))))
;
;(check-match (gen-prog (canonicalize (parse-string  "while 3 do (7;())")))
;             (list
;              (label l2)
;              (lim-ins 3 (temp-loc t4))
;              (cond-jump-ins (temp-loc t4) (label l3))
;              (lim-ins 7 (temp-loc t5))
;              (uncond-jump-ins (label l2))
;              (label l3)))

(test)
