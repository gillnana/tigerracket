#lang racket
(require "parser.rkt")
(require test-engine/racket-tests)

(define (canonicalize ast)
  
  ; TODO make sure that fields are specified in the same order as declared for record creation
  (match ast
    
    [(for-statement index start end body)
     (expseq
      (list
       (assignment index start)
       (while-statement (binary-op (op '<=) index end) 
                        (expseq
                         (list
                          (canonicalize body)
                          (binary-op (op '+) index (int-literal 1)))))))]
    
    [(while-statement cond body) (while-statement (canonicalize cond) (canonicalize body))]
    
    [(if-statement c t e) (if-statement (canonicalize c) (canonicalize t) (canonicalize e))]
    
    [(binary-op (op '&) a b)
     (if-statement (canonicalize a)
                   (if-statement (canonicalize b) (int-literal 1) (int-literal 0))
                   (int-literal 0))]
    
    [(binary-op (op 'or) a b)
     (if-statement (canonicalize a)
                   (int-literal 1)
                   (if-statement (canonicalize b) (int-literal 1) (int-literal 0)))]
    
    [(binary-op other a b) (binary-op other (canonicalize a) (canonicalize b))]
    [(unary-op op a) (unary-op op (canonicalize a))]
    [(expseq seq) (expseq (map canonicalize seq))] 
    [(array-access id index) (array-access (canonicalize id) (canonicalize index))]
    
    [(funcall fun-id args) (funcall fun-id (map canonicalize args))]
    [(record-creation type-id fieldvals) (record-creation type-id 
                                                          (map
                                                           (match-lambda
                                                             [(fieldval name val)
                                                              (fieldval name (canonicalize val))])))]
    [(array-creation type-id size initval) (array-creation type-id (canonicalize size) (canonicalize initval))]
    [(assignment lvalue val) (assignment (canonicalize lvalue) (canonicalize val))]
    
    [(vardec id type-id val) (vardec id type-id (canonicalize val))]
    [(fundec id tyfields type-id body) (fundec id tyfields type-id (canonicalize body))]
    
    [(let-vars bindings body) (let-vars (map canonicalize bindings) (canonicalize body))]
    [(let-funs bindings body) (let-funs (map canonicalize bindings) (canonicalize body))]
    [(let-types bindings body) (let-types bindings (canonicalize body))]
    
    
    [(int-literal val) (int-literal val)]
    [(string-literal val) (string-literal val)]
    [(nil) (nil)]
    [(id a) (id a)]
    [(break) (break)]
    [(record-access rec-id field-id) (record-access rec-id field-id)]
    [(tydec type-id ty) (tydec type-id ty)]
    
    [else else]))
   
(check-expect (begin (canonicalize (parse-file "./tests/queens.tig")) (call/cc (λ (k) {k (k "pizza")}))) "pizza")

(test)