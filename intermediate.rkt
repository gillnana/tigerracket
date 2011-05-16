#lang racket
(require "parser.rkt")
(require "typecheck.rkt")
(require "canonicalize.rkt")
(require "semantic-loop.rkt")
;(require "dag.rkt")
(require test-engine/racket-tests)

(provide (all-defined-out))

(define-syntax-rule (check-match exp pat)
  (check-expect
   (match exp
     [pat #t]
     [else #f])
   #t))


(struct program (inslist fxnlist) #:prefab)

(struct fxn-block (label ins-list static-link local-vars)  #:prefab); static links do not yet involve escaping
; static links are other function blocks

(define parent-fxn (make-parameter (list 'TOP_LEVEL)))

; INSTRUCTIONS
; each instruction also needs to potentially contain a label

(define (location? item)
  (or (temp-loc? item) (mem-loc? item) (label-loc? item) (param-loc? item) #;(return-val-loc? item)))

(define (number-location? item)
   (or (temp-loc? item) (mem-loc? item) (param-loc? item) #;(return-val-loc? item)))

; allocation represents allocating space for a variable
; it is completely internal to intermediate.rkt and should be ignored by later phases
(struct allocation (loc) #:prefab)
; this struct is completely generic and includes all types of locs.  later (at codegen time) these locs should be
; unpacked and put into the right part of the stack frame depending on their type

(struct move-ins (src dest) #:prefab)
(struct lim-ins (imm dest) #:prefab) ; constant value imm is put into dest
(struct binary-ins (op src1 src2 dest) #:prefab)
(struct unary-ins (op src dest) #:prefab)

(struct uncond-jump-ins (dest) #:prefab)
(struct cond-jump-ins (src dest) #:prefab) ; conditionally jumps if src is FALSE
(struct cond-jump-relop-ins (op src1 src2 dest) #:prefab) ; conditionally jumps if (relop src1 src2) is FALSE

(struct push-ins (src) #:prefab) ; pushes the contents of src onto the stack as a function parameter

(struct break-ins () #:prefab) ;breaks out of a current for loop.  expects loops to be implemented as tail-recursive functions

(struct array-allocate-ins (src1 dest) #:prefab) ;this instruction allocates an array to some initial value, which most backends will do for free. src1 is the address of the expression to be inserted into the array.  dest is the mem-block struct which is the location of the array.

(struct deref-ins (src1 src2) #:prefab) ; this instruction corresponds to x=*y, putting the r-value of y into the r-value of x
(struct ref-ins (src1 src2) #:prefab) ; this instruction corresponds to x=&y, putting the l-value of y into the r-value of x
(struct deref-assign-ins (src1 src2) #:prefab) ; this instruction is *x=y, putting the r-value of y into the l-value of x

(struct array-bounds-check-ins (array-loc index-loc) #:prefab)


(struct stack-setup-ins () #:prefab) ; in MIPS, push ra and fp onto stack, sets new fp to current value of sp
(struct stack-teardown-ins () #:prefab) ; pops fp and ra from stack into registers, sets stack to old value of sp
(struct jump-to-return-address-ins () #:prefab)

;(struct funcall-ins (labloc num-params return-val) #:transparent) ;num-params is a statically determined integer.  the return value of the funcall is placed in the return-val location

; this is a better representation; you don't always put arguments on the stack. you sometimes use $a0-$a3
(struct funcall-ins (labloc params dest) #:prefab)

(struct return-ins (return-val-loc) #:prefab) ;represents an instruction that puts the return value of a function in the location it should go, wherever that may be

(struct closure-ins (label dest) #:prefab) ; creates a closure over the specified label (function pointer) and puts it in the dest register.  in MIPS code, lim.

(struct malloc-ins (size dest) #:prefab)
(struct array-malloc-ins (size initval dest) #:prefab)

; LOCATIONS

(struct location-binding (var loc) #:prefab)

; a word-sized loc is either:
;   - a temp-loc, which may be either a register or a word in memory
(struct temp-loc (t) #:prefab)

; a malloc'd block of memory is:
;   m is a gensym to uniquely identify the block
;   size is a location that (at runtime) holds the size of the block to be allocated
(struct mem-block (m size) #:prefab) ; the size is the location of the register or temporary holding the size of this block, which is itself an expression that must be computed at runtime
; the offset of a mem-loc is the location within the block, described as the number of words from the beginning, indexed from 0.  the block is represented by a location holding the address of that block.

(struct mem-loc (m) #:prefab) ; currently a mem-loc only refers to an element of memory on the heap.  pointer arithmetic does the rest.


(struct label-loc (l) #:prefab)
(struct param-loc (p param-number) #:prefab)

;(struct return-val-loc () #:transparent) ;location of the return value

;TODO figure out how to hold on to the size for bounds checking


;(struct record-table-entry (name pointer? offset) #:transparent) ; the pointer? of a record-table-entry is a boolean describing whether or not this word of the record is a pointer or if the bits of the word actually contain the desired data.  this is #t for integers and #f otherwise.  the offset is given as number of words indexed from 0.

(struct label (l) #:prefab)



; GENSYM PROCEDURES

(define (gen-temp) (temp-loc (gensym 't)))
(define (gen-label-loc) (label-loc (gensym 'lt)))
(define (gen-param param-num) (param-loc (gensym 'p) param-num))
(define (gen-mem-block size) (mem-block (gensym 'm) size)) ; we decided that size refers to the number of words this takes in the machine.  for
;most purposes a single word will hold one item, be that a pointer or integer.
(define (gen-mem) (mem-loc (gensym 'm)))

(define (gen-label) (label (gensym 'l)))


(define (lookup id loc-env)
  (or (ormap (match-lambda 
               [(location-binding var loc)
                (and (eq? var id)
                     loc)])
             loc-env)
      (error (format "internal error: unbound identifier ~a" id))))
  


(define (gen-prog prog)
  (gen prog 'ans empty))

(define (reset-dag-table!) (set! dag-table (make-hash)))

; gen ast symbol listof-location-binding? -> program
; takes an ast and returns a list of horrible spaghetti instructions with gotos and unreadable garbage and things
(define (gen ast result-sym loc-env)
  (reset-dag-table!)
  (parameterize ([parent-fxn (list (make-placeholder 'dummy))])
    (match (dag-gen ast result-sym loc-env)
      [(and main-prog (program main-inslist funs))
       (make-reader-graph 
        (functionize (label 'main) main-prog (first (parent-fxn)))
        
        )])))

(define (functionize name prog placeholder-fxn)
  ;(displayln (parent-fxn))
  (match prog
    [(program inslist fxnlist)
     
     (let [(static-link (fxn-block name 
                                   inslist 
                                   (rest (parent-fxn))
                                   (filter-map (match-lambda
                                                 [(allocation loc) loc]
                                                 [_ #f])
                                               inslist)
                                   ))]
       ;(displayln static-link)
       (placeholder-set! placeholder-fxn static-link)
       (program empty
                (cons static-link fxnlist)))]))

(define (ins-combine . args)
  (program (flatten args) empty))

(define (program-append . plist)
  ;(displayln plist)
  ;ASSERT EVRYTHING A PROGRAM
  (map (λ (prog)
         (when (not (program? prog))
           (error (format "program-append expected a program but found garbage ~a" prog))))
       plist)
  
  (let [(ins-list (apply append (map program-inslist (flatten plist))))
        (prog-list (apply append (map program-fxnlist (flatten plist))))]
    (program ins-list prog-list)))
            
(define dag-table (make-hash))


(define (dag-gen ast result-sym loc-env)
  ;(displayln dag-table)
  (let [(cached-node (hash-ref dag-table ast #f))]
    (if cached-node #;false
        (begin
          ;(displayln "i dagged ")
          ;(display cached-node)
          (ins-combine (move-ins cached-node result-sym)))
        (begin
          (hash-set! dag-table ast result-sym)
          (gen-helper ast result-sym loc-env)))))

; gen-helper ast location? listof-location-binding? -> program
(define (gen-helper ast result-sym loc-env)
  (match ast
    ;ARITHMETIC
    [(binary-op (op op) arg1 arg2) 
     (let [(sym1 (gen-temp))
           (sym2 (gen-temp))]
       (program-append (ins-combine (allocation sym1) (allocation sym2))
                       (dag-gen arg1 sym1 loc-env)
                       (dag-gen arg2 sym2 loc-env)
                       (program (list (binary-ins op sym1 sym2 result-sym)) empty)))]
    [(unary-op (op op) arg)
     (let [(sym (gen-temp))]
       (program-append (ins-combine (allocation sym))
                       (dag-gen arg sym loc-env) 
                       (program (list (unary-ins op sym result-sym)) empty)))]
    ;LITERAL OR STUPID VALUES
    [(int-literal val)
     (ins-combine (lim-ins val result-sym))]
    
    [(string-literal val)
     (ins-combine (lim-ins val result-sym))]
    
    [(id name) (program-append (ins-combine (move-ins (lookup name loc-env) result-sym)))]

    [(array-creation type-id size-expr initval)
     (let* [(size-loc (gen-temp))
            (size-gen-code (dag-gen size-expr size-loc loc-env))
            (initval-loc (gen-temp))
            (initval-gen-code (dag-gen initval initval-loc loc-env)) 
            (array-malloc-gen-code (ins-combine (array-malloc-ins size-loc initval-loc result-sym)))]
       (program-append (ins-combine (allocation size-loc) (allocation initval-loc) (list (list (list))))
                       size-gen-code
                       initval-gen-code
                       array-malloc-gen-code))]
   
    
    [(record-creation type-id fieldvals)
     (let* [(result-holder (gen-mem))
            (cur-element-loc (gen-mem))
            (fieldval-loc (gen-temp))
            ]
       
       (program-append (ins-combine (allocation result-holder) 
                                    (allocation cur-element-loc)
                                    (allocation fieldval-loc)
                                    (malloc-ins (length fieldvals) result-holder))
                       (apply 
                        program-append 
                        (map (λ (field offset)
                               (match field
                                 [(fieldval name val)
                                  (program-append 
                                   (begin
                                     (let [(cur-fieldval (dag-gen val fieldval-loc loc-env))]
                                       (reset-dag-table!) ; we use the fieldval-loc temporary for each iteration, making
                                       ; it volatile here if multiple fields of this record are identical
                                       ; consider a{x=4, y=5, z=4}
                                       ; TODO: optimize later
                                       cur-fieldval))
                                   (ins-combine
                                    (lim-ins (* 4 offset) cur-element-loc)
                                    (binary-ins '+ result-holder cur-element-loc cur-element-loc)
                                    (deref-assign-ins cur-element-loc fieldval-loc)))]))
                             fieldvals
                             (build-list (length fieldvals) values)))
                       (ins-combine 
                        (move-ins result-holder result-sym))
                       ))]
    

    [(and accessor (or (array-access _ _ _) (record-access _ _ _ _)))
     (let* [(worker-loc (gen-mem))
            (gen-lval-code (gen-lval accessor worker-loc loc-env))]
       (program-append (ins-combine (allocation worker-loc))
                       gen-lval-code
                       (ins-combine (deref-ins result-sym worker-loc))))]
      
    ; assignment doesn't overwrite ans. this is ok.
    [(assignment (id name) expr)
     (let* [(dest-loc (lookup name loc-env)) 
            (rval-gen-code (dag-gen expr dest-loc loc-env ))]
       (reset-dag-table!)
       rval-gen-code  
       )]
    
    [(assignment lvalue expr)
     (let* [(dest-loc (gen-mem))
            (lval-gen-code (gen-lval lvalue dest-loc loc-env))
            (rval-loc (gen-temp))
            (rval-gen-code (dag-gen expr rval-loc loc-env))]
       (reset-dag-table!)
       (program-append
        (ins-combine (allocation dest-loc) (allocation rval-loc))
        lval-gen-code
        rval-gen-code
        (ins-combine (deref-assign-ins dest-loc rval-loc))))
     ]
    
    ;    [(assignment (record-access rec-id field-id) val) ; TODO: but depends on record declarations
    ;     ...]
    
;    [(assignment (and ast-node (array-access (id array-id) index)) val)
;     ;(displayln ast-node)
;     ;(displayln loc-env)
;     (let* [(dest-loc (lookup array-id loc-env))
;            (val-temp (gen-temp))
;            (val-gen-code (dag-gen val val-temp loc-env ))
;            (index-temp (gen-temp))
;            (index-gen-code (dag-gen index index-temp loc-env ))
;            ]
;       (reset-dag-table!)
;       (if (temp-loc? dest-loc)
;           (program-append (ins-combine (allocation val-temp)
;                                        (allocation index-temp))
;                           index-gen-code
;                           val-gen-code
;                           (ins-combine (deref-ins (mem-loc dest-loc index-temp) val-temp)))
;           (error (format "internal error: array ~a bound to wrong location type" array-id))))]
    
    
    ; leaving something in ans is ok. the program has already been typechecked.
    [(expseq exprs) (apply program-append (map (λ (expr) (dag-gen expr result-sym loc-env)) exprs))]
;    
;    [(record-creation type-id fieldvals)
;     (let* [(size-register (gen-temp))
;            (size-gen-code (lim-ins (length fieldvals) size-register)
;    
   
    
    ; leaving something in ans is (label l)ok. the program has already been typechecked.
    
    [(let-vars decs body)
     (let-values [((inner-loc-env decs-instructions)
                   (for/fold ([le loc-env]
                              [instructions (ins-combine)])
                     [(dec decs)]
                     (match dec
                       [(vardec id t-id expr)
                        (let [(sym (gen-temp))]
                          (values 
                           (cons (location-binding id sym) le)
                           (program-append (ins-combine (allocation sym))
                                           instructions 
                                           (dag-gen expr sym le ))))])))]
       (program-append decs-instructions
               (dag-gen body result-sym inner-loc-env)))]
    
    [(let-types decs body)
     (dag-gen body result-sym loc-env)]
     
    [(if-statement cond then (expseq empty))
     (let* [(end-label (gen-label))
            (then-register (gen-temp))
            (cond-gen-code (create-conditional-jump cond end-label loc-env))
            ; the conditional code always executes so doesnt need to drop the dag table
            (then-gen-code (dag-gen then then-register loc-env))
            ]
       (reset-dag-table!)
       (program-append (ins-combine (allocation then-register))
                       cond-gen-code
                       then-gen-code
                       (ins-combine end-label)))]
    
    [(if-statement cond then else)
     (let* [(end-label (gen-label))
            (then-register (gen-temp))
            (else-label (gen-label))
            (else-register (gen-temp))
            (cond-gen-code (create-conditional-jump cond else-label loc-env))
            (then-gen-code (program-append (ins-combine (allocation then-register)
                                                        (allocation else-register))
                                           (dag-gen then then-register loc-env )
                                           (ins-combine (move-ins then-register result-sym)
                                                        (uncond-jump-ins end-label)
                                                        else-label)))
            (else-gen-code (begin 
                             (reset-dag-table!)
                             (dag-gen else else-register loc-env )))]
       (reset-dag-table!)
       (program-append cond-gen-code
                       then-gen-code
                       else-gen-code
                       (ins-combine (move-ins else-register result-sym) end-label)))]
    
    [(while-statement cond body)
     (begin
       (reset-dag-table!)
       (let* [(start-label (gen-label))
              (end-label (gen-label))
              (cond-gen-code (create-conditional-jump cond end-label loc-env ))
              (dummy-location (gen-temp))
              (body-gen-code (dag-gen body dummy-location loc-env ))]
         (program-append (ins-combine start-label
                                      (allocation dummy-location))
                         cond-gen-code body-gen-code
                         (ins-combine (uncond-jump-ins start-label) end-label))))]
    
    [(nil) (ins-combine (lim-ins 0 result-sym))]
    
    [(let-funs decs body)
     (begin
       (reset-dag-table!)
       (let [(fn-placeholder-locs
              (foldl (λ (dec placeholder-bindings)
                       (match dec
                         [(fundec _ _ _ (stdlibfxn _ _)) placeholder-bindings]
                         [(fundec id _ _ _)
                          (cons (location-binding id (make-placeholder 'fish)) placeholder-bindings)
                          ]))
                     empty
                     decs)
              )]
         ; TODO: this is EXTREMELY SILLY for now
         ; assume that this is the top-level let-standard-library thingy
         (let-values [((inner-loc-env fn-assign-program)
                       (for/fold ([le (append fn-placeholder-locs loc-env)]
                                  [prog (program empty empty)] ; the accumulated library of fundefs
                                  )
                         [(dec decs)]
                         (match dec
                           ; case for external compiled function
                           [(fundec id tyfields type-id (stdlibfxn lbl _))
                            (let [(sym (gen-label-loc))]
                              (values 
                               (cons (location-binding id sym) le)
                               ;(cons (lim-ins (label (string->symbol (string-append "lt_" (symbol->string lbl)))) sym) instructions)
                               (program-append (ins-combine 
                                                (allocation sym)
                                                (closure-ins (label (string->symbol (string-append "lt_" 
                                                                                                   (symbol->string lbl))))
                                                             sym))
                                               prog)
                               ))]
                           ; case for tiger-defined function
                           [(fundec id tyfields type-id body)
                            (let* ([fun-label (gen-label)]
                                   [recur-label (gen-label)]
                                   [sym (gen-label-loc)] ; the location that holds the label of this function
                                   [return-val-loc (gen-temp)] ; the location that the function will put its answer into 
                                   ;during the return-ins
                                   ; add all the parameters into the location environment
                                   ;TODO: params currently start from 0. is there reason to start from 1?
                                   [body-new-le (map (λ (tyf param-num)
                                                       (match tyf
                                                         [(tyfield ty-name ty-ty)
                                                          (location-binding ty-name 
                                                                            (gen-param param-num))]))
                                                     tyfields
                                                     (build-list (length tyfields) values))]
                                   [body-le (append body-new-le
                                                    le)]
                                   ; generate the function
                                   [sub-fxn-ph (make-placeholder 'dummy)]
                                   [fn-body-prog 
                                    
                                    
                                    
                                    (parameterize [(parent-fxn (cons sub-fxn-ph (parent-fxn)))]
                                      (reset-dag-table!)
                                      (functionize fun-label 
                                                   (program-append
                                                    (ins-combine (allocation return-val-loc)
                                                                 (map (match-lambda
                                                                        [(location-binding _ param)
                                                                         (allocation param)])
                                                                      body-new-le))
                                                    (dag-gen body return-val-loc body-le)
                                                    (ins-combine (return-ins return-val-loc))) sub-fxn-ph)
                                      )
                                    ])
                              (begin
                                (placeholder-set! (lookup id le) sym)
                                (values 
                                 (cons (location-binding id sym) le)
                                 (program-append (ins-combine (allocation sym))
                                                 fn-body-prog
                                                 (ins-combine (closure-ins fun-label sym))
                                                 prog)
                                 )))
                            ]
                           )))]
         ;(displayln fn-assign-program)
         (reset-dag-table!)
         (program-append fn-assign-program
                         (dag-gen body result-sym inner-loc-env)))))]
     
    [(funcall (id fun-id) args)
     (reset-dag-table!)
     (let* [(f (lookup (id-name fun-id) loc-env))
            (arg-sym-list (build-list (length args) (λ (ignore) (gen-temp))))
            (label-here (gen-label))
            (label-holder (gen-temp))
            ; TODO: does a param-loc represent parameters passed to *this* function,
            ;       or parameters that this function passes to called functions
            (param-gen-code (apply program-append
                                   (ins-combine (allocation label-holder))
                                   (map (λ (arg param-sym)
                                          (program-append (ins-combine (allocation param-sym))
                                                          (dag-gen arg param-sym loc-env )))
                                        args arg-sym-list)))]
       (program-append param-gen-code
                       (ins-combine (funcall-ins f 
    
    [(funcall fun-id args)
     (reset-dag-table!)
     ;TODO evaluate fun-id which might be any expression
     (let* [;(f (lookup (id-name fun-id) loc-env))
            (f-loc (gen-label-loc))
            (f-load-prog (dag-gen fun-id f-loc loc-env))
            (arg-sym-list (build-list (length args) (λ (ignore) (gen-temp))))
            (label-here (gen-label))
            (label-holder (gen-temp))
            ; TODO: does a param-loc represent parameters passed to *this* function,
            ;       or parameters that this function passes to called functions
            (param-gen-code (apply program-append
                                   (ins-combine (allocation label-holder))
                                   (map (λ (arg param-sym)
                                          (program-append (ins-combine (allocation param-sym))
                                                          (dag-gen arg param-sym loc-env )))
                                        args arg-sym-list)))
            ]
       ;(reset-dag-table!)
           
       (program-append (ins-combine (allocation f-loc))
                       f-load-prog 
                       param-gen-code
                       ;(map push-ins arg-sym-list)
                       (ins-combine (funcall-ins f-loc arg-sym-list result-sym)
                                    ) ; the last argument is the return address
                       ))]

    [(break) (ins-combine (break-ins))]
    ))

; TODO: make gen-lval dag things correctly?
(define (gen-lval ast result-sym loc-env)
  (match ast
    [(id name) (program-append (ins-combine (ref-ins result-sym (lookup name loc-env))))]
    ["captain kirk is climbing a mountain. why is he climbing a mountain?" (void)]
    [(array-access lval index return-t)
     (let* [(lval-loc (gen-mem))
            (lval-gen-code (gen-lval lval lval-loc loc-env)) ; I win at dominoes
            (index-loc (gen-temp))
            (index-gen-code (dag-gen index index-loc loc-env))]
       (reset-dag-table!) ; in the worst case, the lvalue that called gen-lval generates some volatile code.  but if it
       ; does, that code will call reset-dag-table.
       ; TODO our program needs this but we don't know why.  figure out why.
       (program-append (ins-combine (allocation lval-loc) (allocation index-loc))
                       lval-gen-code ; puts a pointer to arr into lval-loc
                       (ins-combine (deref-ins lval-loc lval-loc)) ; puts the arr itself into lval-loc
                       index-gen-code
                       (ins-combine (lim-ins 4 result-sym) ; offset by 1 word
                                    (binary-ins '* index-loc result-sym index-loc) ; multiply index by 4
                                    (binary-ins '+ lval-loc result-sym result-sym) 
                                    (binary-ins '+ index-loc result-sym result-sym)
                                    ; result-sym has pointer to element
                                    ; lval-loc should still be the array itself (pointer to array's size)
                                    (array-bounds-check-ins lval-loc result-sym)
                                    )))]
    [(record-access lval field-id offset return-t)
     (let* [(lval-loc (gen-mem))
            (lval-gen-code (gen-lval lval lval-loc loc-env)) ; pickles
            ]
       (program-append (ins-combine (allocation lval-loc))
                       lval-gen-code
                       (ins-combine (deref-ins lval-loc lval-loc)
                                    (lim-ins (* 4 offset) result-sym)
                                    (binary-ins '+ lval-loc result-sym result-sym))))]
       ))


(define (create-conditional-jump condition to-label loc-env )
   (match condition
     [(binary-op (op (and op (or '> '< '>= '<=))) arg1 arg2)
      (let [(arg1-register (gen-temp))
            (arg2-register (gen-temp))]
        (program-append (ins-combine (allocation arg1-register)
                                     (allocation arg2-register))
                        (dag-gen arg1 arg1-register loc-env )
                        (dag-gen arg2 arg2-register loc-env )
                        (ins-combine (cond-jump-relop-ins op arg1-register arg2-register to-label))))]
     [else
      (let [(cond-register (gen-temp))]
        (program-append (ins-combine (allocation cond-register))
                        (dag-gen condition cond-register loc-env )
                        (ins-combine (cond-jump-ins cond-register to-label))))]))



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
