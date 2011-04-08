#lang racket

(require "parser.rkt")
(require "intermediate.rkt")
(require test-engine/racket-tests)

(provide (all-defined-out))

;; daggify ast -> ast
;; returns a new ast with shared nodes if they appear in a simple expression
;(define (daggify ast)
;  (define nodes (make-hash))
;  (define (d ast)
;    (let [(cached (hash-ref nodes ast #f))]
;      (if cached
;          cached
;          (let [(new
;                 (match ast
;                   [(int-literal forty) (int-literal forty)]
;                   [(id id-id) (id id-id)]
;                   [(binary-op op l r)
;                    (binary-op op (d l) (d r))]
;                   [(unary-op op arg)
;                    (unary-op op (d arg))]
;                   [(expseq exprs)
;                    (expseq (map (λ (expr) (d expr)) exprs))]
;                   [(while-statement cond body)
;                    (while-statement (d cond) (d body))]
;                   [(let [(new (gen-helper ast result-sym loc-env record-table ))](funcall fun-id args)
;                    (funcall (d fun-id) (d args)))]
;                   [(assignment lvalue val)
;                    (begin
;                      (let [(assgn-rval (d val))] ;TODO: this is slightly incorrect: 
;                        (set! nodes (make-hash))
;                        (assignment lvalue #|<---WTF!?!      |# assgn-rval)))]
;                   [(let-vars decs body)
;                    (let-vars 
;                     (map (match-lambda
;                            [(vardec id type-id val) (vardec id type-id (d val))])
;                          decs)
;                     (d body))]
;                   #;[else (error "not match cloz faeund")]))]
;            (hash-set! nodes new new)
;            new
;            ))
;      )
;    )
;  (d ast)
;  )