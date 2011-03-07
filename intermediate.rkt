#lang racket
(require "parser.rkt")
(require test-engine/racket-tests)



(struct move-ins (src dest) #:transparent)
(struct binary-ins (op src1 src2 dest) #:transparent)
(struct unary-ins (op src dest) #:transparent)

(struct location-binding (var loc) #:transparent)

; ast-int->
; takes an ast and returns a list of horrible spaghetti instructions with gotos and unreadable garbage and things
(define (ast->int ast result-sym loc-env)
  (match ast
    [(binary-op (op op) arg1 arg2) 
     (let [(sym1 (gensym 'loc))
           (sym2 (gensym 'loc))]
       (append (ast->int arg1 sym1 loc-env)
               (ast->int arg2 sym2 loc-env)
               (list (binary-ins op sym1 sym2 result-sym))))]
    [(unary-op (op op) arg)
     (let [(sym (gensym 'loc))]
       (append (ast->int arg sym loc-env) (list (unary-ins op sym result-sym))))]
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
    ; assignment doesn't overwrite ans. this is probably ok.
    [(assignment (id name) expr) ; TODO: record access and array access
     (let [(dest-loc (ormap (match-lambda [(and (location-binding var loc) binding)
                                            (if (eq? var name)
                                                loc
                                                #f)])
                             loc-env))]
       (ast->int expr dest-loc loc-env)
       )]
;    [(assignment (record-access ...) val) ; TODO: record access
;     ...]
;    [(assignment (array-access ...) val) ; TODO: array access
;     ... ]
    
    
    ; leaving something in ans is ok. the program has already been typechecked.
    [(expseq exprs) (apply append (map (λ (expr) (ast->int expr result-sym loc-env)) exprs))]
    
    [(let-vars decs body)
     (let-values [((inner-loc-env decs-instructions)
                   (for/fold ([le loc-env]
                              [instructions empty])
                     [(dec decs)]
                     
                     (match dec
                       [(vardec id t-id expr)
                        (let [(sym (gensym 'loc))]
                          (values 
                           (cons (location-binding id sym) le)
                           (append instructions (ast->int expr sym le))))])))]
       (append decs-instructions
               (ast->int body
                         result-sym
                         inner-loc-env)))]
    
    [(expseq exps) (apply append (map (λ (exp) (ast->int exp result-sym loc-env))
                                             exps))
                   
                   ]
    
    )
  )


(check-expect (match (ast->int (parse-string "let var x := 0 in x := 3; x end") 'ans empty) 
                [(list 
                  (move-ins 0 loc1)
                  (move-ins 3 loc1)
                  (move-ins loc1 'ans)) #t]
                [else else])
              #t)

(check-expect (match (ast->int (parse-string "let var x := 0 in x := x+2; x end") 'ans empty)
                [(list
                  (move-ins 0 loc1)
                  (move-ins loc1 loc2)
                  (move-ins 2 loc3)
                  (binary-ins '+ loc2 loc3 loc1)
                  (move-ins loc1 'ans)) #t]
                [else else])
              #t)

(check-expect (match (ast->int (parse-string "let var x := 0 in x := x+2; x; () end") 'ans empty)
                [(list
                  (move-ins 0 loc1)
                  (move-ins loc1 loc2)
                  (move-ins 2 loc3)
                  (binary-ins '+ loc2 loc3 loc1)
                  (move-ins loc1 'ans)) #t]
                [else else])
              #t)
(check-expect (match (ast->int (parse-string "let var y := 0 in let var x := (y := 2; 7) in y end end") 'ans empty)
                [(list (move-ins 0 loc1)
                       (move-ins 2 loc1)
                       (move-ins 7 loc2)
                       (move-ins loc1 'ans)) #t]
                [else else])
              #t)
(test)