#lang racket
(require "parser.rkt")
(require test-engine/racket-tests)



(struct instruction (op a b r) #:transparent)
(struct location-binding (var loc) #:transparent)

; ast-int->
; takes an ast and returns a list of horrible spaghetti instructions with gotos and unreadable garbage and things
(define (ast->int ast result-sym loc-env)
  (match ast
    [(binary-op (op op) arg1 arg2) 
     (let [(sym1 (gensym))
           (sym2 (gensym))]
       (append (ast->int arg1 sym1 loc-env)
               (ast->int arg2 sym2 loc-env)
               (list (instruction op sym1 sym2 result-sym))))]
    [(unary-op (op op) arg)
     (let [(sym (gensym 'foob))]
       (append (ast->int arg sym loc-env) (list (instruction op sym #f result-sym))))]
    [(int-literal val)
     (list (instruction 'store val #f result-sym))]
    [(id name)
     (let [(sym  (ormap (match-lambda [(location-binding var loc)
                                           (and (eq? var name)
                                                loc)])
                        loc-env))]
       (if sym
           (list (instruction 'move sym #f result-sym))
           (error (format "unbound variable ~a" name))))]
    [(let-vars decs body)
     
     (let-values [((inner-loc-env decs-instructions)
                   (for/fold ([le loc-env]
                              [instructions empty])
                     [(dec decs)]
                     
                     (match dec
                       [(vardec id t-id expr)
                        (let [(sym (gensym))]
                          (values 
                           (cons (location-binding id sym) le)
                           (append instructions (ast->int expr sym le))))])))]
       (append decs-instructions
               (ast->int body
                         result-sym
                         inner-loc-env))
     )
     ]
    [(sequence expseq) (ast->int expseq result-sym loc-env)]
    [(expseq exps) (apply append (map (λ (exp) (ast->int exp result-sym loc-env))
                                             exps))
                   
                   ]
    
    )
  )