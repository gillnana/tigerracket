#lang racket
(require "parser.rkt")
(require "typecheck.rkt")
(require test-engine/racket-tests)

(provide (all-defined-out))

; translates a tiger arithmetic operator
; into the corresponding Racket operator
(define (op-lookup op-sym)
  (match op-sym
    ['+ +]
    ['- -]
    ['* *]
    ['/ quotient]
    ['= =]
    ['<> (λ (a b) (not (= a b)))]
    ['< <]
    ['> >]
    ['<= <=]
    ['>= >=]))

(define (canonicalize ast)
  
  ; TODO make sure that fields are specified in the same order as declared for record creation
  ; do this by alphabetizing the field order at canonicalization
  ; note type checking must follow canonicalization in this case
  (match ast
    #;[(for-statement index start end body)
       (let-vars
        (list (vardec index #f start))
        (expseq
         (list
          (while-statement (binary-op (op '<=) (id index) end) 
                           (expseq
                            (list
                             (canonicalize body)
                             (binary-op (op '+) (id index) (int-literal 1))))))))]
    
    [(for-statement index start end body)
     (let-vars
      (list (vardec 'startval 'int (canonicalize start))
            (vardec 'endval 'int (canonicalize end)))
      (expseq (list (let-funs
               (list (fundec 'f 
                             (list (tyfield index (type-id 'int)))
                             #f
                             (if-statement
                              (binary-op (op '<=) (id index) (id 'endval))
                              (expseq
                               (list
                                (canonicalize body)
                                (funcall (id 'f) (list (binary-op (op '+) (id index) (int-literal 1))))))
                              (expseq empty))))
               (expseq (list (funcall (id 'f) (list (id 'startval)))))))))]
                                
    
    [(while-statement cond body)
     (let-funs
      (list (fundec 'f empty #f
                    (if-statement (canonicalize cond)
                                  (expseq (list (canonicalize body) (funcall (id 'f) empty)))
                                  (expseq empty))))
      (expseq (list (funcall (id 'f) empty))))]
    
    ; an if-statement of a literal value can have one branch eliminated
    [(if-statement c t e)
     (let ([c (canonicalize c)]
           [t (canonicalize t)]
           [e (canonicalize e)])
       (match c
         [(int-literal 0) e]
         [(int-literal _) t]
         [_ (if-statement c t e)]))]
    
    
    [(binary-op (op '&) a b)
     (canonicalize (if-statement a
                                 (if-statement b
                                               (int-literal 1)
                                               (int-literal 0))
                                 (int-literal 0)))]
    
    [(binary-op (op 'or) a b)
     (canonicalize (if-statement a
                                 (int-literal 1)
                                 (if-statement b
                                               (int-literal 1)
                                               (int-literal 0))))]
    
    ; evaluate constant arithmetic expressions
    [(binary-op (op arith) a b)
     (let ([a (canonicalize a)]
           [b (canonicalize b)])
       (if (and (int-literal? a)
                (int-literal? b))
           (int-literal ((op-lookup arith) (int-literal-value a)
                                           (int-literal-value b)))
           (binary-op (op arith) a b)))]
    
    [(unary-op op a) (unary-op op (canonicalize a))]
    [(expseq seq) (expseq (map canonicalize seq))] 
    [(array-access id index return-t) (array-access (canonicalize id) (canonicalize index) return-t)]
    
    [(funcall fun-id args) (funcall fun-id (map canonicalize args))]
    [(record-creation type-id fieldvals) (record-creation type-id 
                                                          (map
                                                           (match-lambda
                                                             [(fieldval name val)
                                                              (fieldval name (canonicalize val))])
                                                           fieldvals))]
    [(array-creation type-id size initval) (array-creation type-id (canonicalize size) (canonicalize initval))]
    [(assignment lvalue val) (assignment (canonicalize lvalue) (canonicalize val))]
    
    [(vardec id type-id val) (vardec id type-id (canonicalize val))]
    [(fundec id tyfields type-id body) (fundec id tyfields type-id (canonicalize body))]
    
    [(let-vars bindings body) (let-vars (map canonicalize bindings) (canonicalize body))]
    [(let-funs bindings body) (let-funs (map canonicalize bindings) (canonicalize body))]
    [(let-types bindings body) (let-types bindings (canonicalize body))]
    
    [(string-literal val)
     (let [(array (gensym 'array))]
       (let-vars
        (list (vardec array #f (array-creation #f (int-literal (string-length val)) (int-literal 0)))) ;TODO: type information?
        (expseq
         (append (map (λ (char offset)
                       (assignment (array-access (id array) (int-literal offset) (t-int)) (int-literal char)))
                     (map char->integer (string->list val))
                     (build-list (string-length val) values))
                 (list (id array)))
                 )))]
    
    
    [(int-literal val) (int-literal val)]
    [(nil) (nil)]
    [(id a) (id a)]
    [(break) (break)]
    [(record-access rec-id field-id offset return-t) (record-access rec-id field-id offset return-t)]
    [(tydec type-id ty) (tydec type-id ty)]
    
    [else else]))


(check-expect (begin (canonicalize (parse-file "./tests/queens.tig")) (call/cc (λ (k) {k (k "pizza")}))) "pizza")

(test)