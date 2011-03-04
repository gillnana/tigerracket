#lang racket

(require "parser.rkt")
(require test-engine/racket-tests)

;iterator? is true iff the local variable is a for loop indexing variable
(struct iterator-binding (id iterator?))

; verify-loop-sematics : ast-node bool listof-iterator-binding -> void
; throws an error if
;    a) break occurs outside a loop
;    b) an iterator variable is mutated
(define (verify-loop-semantics node)
  (vls node #f empty))

(define (vls node in-loop? env)
  (match node
    [(for-statement var start end body) 
     (vls body true (cons (iterator-binding var true) env))]
    
    [(while-statement cond body)
     (begin
       (vls cond in-loop? env)
       (vls body true env))]
    
    [(assignment (id name) expr)
     (begin (map (match-lambda
                   [(iterator-binding iter-id #t) 
                   (when (equal? iter-id name)   
                     (error (format "semantic error: illegal assignment to iterator ~a" iter-id)))])
                 env)
            (vls expr in-loop? env))]
    [(assignment l-expr r-expr)
     (begin
     (vls l-expr in-loop? env)
     (vls r-expr in-loop? env))]
    
    [(let-types bindings body) (vls body in-loop? env)]
    
    [(let-vars bindings body)
     (vls body in-loop? 
                            (foldl (match-lambda* 
                                     [(list (vardec id t-id val) env-accum)
                                      (begin
                                        (vls val in-loop? env-accum)
                                        (cons (iterator-binding id false) env-accum))])
                                   env
                                   bindings))]
    
    [(let-funs bindings body)
     (map (match-lambda
            [(fundec id tyfields type-id body)
             (vls body in-loop?
                                    (foldl (match-lambda*
                                             [(list (tyfield name type-id) env-accum)
                                              (cons (iterator-binding name #f) env-accum)])
                                           env
                                           tyfields))
             ])
            bindings)
            ]
    
    [(int-literal val) (void)]
    [(string-literal val) (void)]
    
    [(binary-op op arg1 arg2)
     (begin (vls arg1 in-loop? env)
            (vls arg2 in-loop? env))]
    [(unary-op op arg1)
     (vls arg1 in-loop? env)]
    
    [(funcall fun-id args)
     (map (λ (exp) (vls exp in-loop? env)) args)]
    
    [(record-creation type-id fieldvals)
     (map (match-lambda
            [(fieldval name val)
             (vls val in-loop? env)])
          fieldvals)]
     
     [(array-creation type-id size initval)
      (begin
        (vls size in-loop? env)
        (vls initval in-loop? env))]
     
     [(id name) (void)]
     [(record-access rec-id field-id) (void)]
     [(array-access id index) (vls index in-loop? env)]
    
    [(if-statement c t e)
       (begin
        (vls c in-loop? env)
        (vls t in-loop? env)
        (when e
          (vls e in-loop? env))
        )]
    
    [(expseq seq)
     (map (λ (exp) (vls exp in-loop? env)) seq)]
    [(sequence expseq)
     (vls expseq in-loop? env)]
    
    [(break) (unless in-loop? (error "semantic error: found a break statement not inside a loop"))]
    )
  (void))   
    

;IT'S ALL BROKEN
     
(check-expect (verify-loop-semantics (parse-file "./tests/queens.tig")) (void))
(test)