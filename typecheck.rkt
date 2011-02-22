#lang racket

(require "parser.rkt")
(require test-engine/racket-tests)
; type-binding represents creating a new type
; let type a = int in ... end
(struct type-binding (id ty) #:transparent)

; var-binding represents a variable that hold a value of a certain type
; let x = 3 in ... end
; let x : int = 3 in ... end
(struct var-binding (id ty) #:transparent)

(struct t-int () #:transparent)
(struct t-string () #:transparent)
(struct t-void () #:transparent) ; NOT TRANSPARENT, FUCK YOU RACKET
(struct t-nil () #:transparent)

(struct t-fun (args result) #:transparent) ; args is list of types
(struct t-array (elem-type) #:transparent)
(struct t-record (fields) #:transparent)
(struct field (name ty) #:transparent)

(define (type-lookup type-symbol type-env) 
  (cond 
    [(equal? type-symbol 'int) (t-int)]
    [(equal? type-symbol 'string) (t-string)]
    ;[(equal? type-
    ;[(type-id? type-symbol) (type-lookup (type-id-name type-symbol) type-env)]
    [else (or (ormap (lambda (binding) (if (equal? (type-binding-id binding) type-symbol)
                               (type-binding-ty binding)
                               false))
                     type-env)
              (error (format "unbound type ~a" type-symbol)))]))

(define (var-lookup var-symbol var-env)
  ;(print var-env)
  (or (ormap (lambda (binding) (if (equal? (var-binding-id binding) var-symbol)
                                   (var-binding-ty binding)
                                   false))
             var-env)
      (error (format "unbound identifier ~a" var-symbol))))

; takes the type of an identifier or function argument, and the type of a thing you want to put in it
; tells you if that's ok
(define (assignable-to? variable value)
  (or (equal? value (t-nil))
      (equal? value variable)))


(define (type-of expr)
  (type-of-env expr empty empty))

(define (type-of-env expr te ve) ; type-env and var-env
  (match expr
    [(int-literal a) (t-int)]
    [(string-literal a) (t-string)]
    [(nil) (t-nil)]
    
    [(id a) (var-lookup a ve)]
    
    [(array-creation (type-id type) size initval)
     (cond
       [(not (t-int? (type-of-env size te ve))) (error "type error: type of array size not int")]
       [(not (equal? type (type-of-env initval te ve))) (error "type error: type of array not same as initial value")]
       [else (t-array (type-lookup type te))])]
    
    [(binary-op (op sym) arg1 arg2)
     (cond [(symbol=? sym '=) (if (equal? (type-of-env arg1 te ve) (type-of-env arg2 te ve))
                                  (t-int)
                                  (error "type error: arguments for equality comparison must be of same type"))]
           [(and (t-int? (type-of-env arg1 te ve))
                 (t-int? (type-of-env arg2 te ve)))
            (t-int)]
           [else (error (format "type error: args for operator ~a must be integers" sym))])]
    
    [(unary-op (op '-) arg1)
     (if (not (t-int? (type-of-env arg1 te ve)))
         (error "type error: arg to unary minus must be int")
         (t-int))]
    
    [(sequence expseq) (type-of-env expseq te ve)]
    [(expseq explist) ; foldl is sexy
     (foldl (λ (exp type)
             (type-of-env exp te ve))
           (t-void)
           explist)]
    
    [(if-statement c t (list))
     (cond [(not (t-int? (type-of-env c te ve))) (error "type error: condition of if statement must have boolean/int value")]
           [(not (t-void? (type-of-env t te ve))) (error "type error: then branch of an if statement must have no value")]
           [else (t-void)])]
    [(if-statement c t e)
     (let ([type-of-t (type-of-env t te ve)]) 
       (cond [(not (t-int? (type-of-env c te ve))) (error "type error: condition of if statement must have boolean/int value")]
             [(not (equal? type-of-t (type-of-env e te ve))) (error "type error: then and else branches of if statement must have same type")]
             [else type-of-t]))]
    [(while-statement c body)
     (if (t-int? (type-of-env c te ve))
         (type-of-env body te ve)
         (error "type error: while statement condition must have boolean/int value"))]
    [(for-statement var start end body)
     (if (and ;l-value-of var = int
              (t-int? (type-of-env start te ve))
              (t-int? (type-of-env end te ve)))
         (type-of-env body te ve)
         (error "type error: for statement must increment an int from start to end int values"))]

    ;begin let statement
    [(let-statement decs expseq)
     (local [
             
             (define (accumulate-type-declarations decl ty-env)
                (match decl
                  [(vardec id id-type val) ty-env]
                  [(fundec id tyfields type-id body) ty-env]
                  [(tydec type-id ty) 
                   (cons (type-binding type-id ty) ty-env)]))
             
             (define (accumulate-var-declarations decl v-env)
               (match decl
                 [(vardec id id-type val)
                 
                  (let [(expression-type (type-of-env val te ve))]
                      (if (not id-type)
                        (if (equal? (t-nil) expression-type)
                            (error (format "variable ~a has value nil but no type" id))
                            (cons (var-binding id expression-type) v-env))
                        (let [(declared-type (type-lookup id-type te))]
                          (if (assignable-to? declared-type expression-type)
                            (cons (var-binding id declared-type) v-env)
                            (error (format "type error: type mismatch, found ~a; expected ~a"
                                           declared-type expression-type))))))]
                 ; TODO: ensure that function argument names don't repeat
                 ; f(x, y) ok but f(x, x) bad
                 [(fundec id tyfields type-id body)
                  (let* [(body-type (type-of-env body te ve))
                         (new-v-env (cons (var-binding id  
                                                       (t-fun (map (λ (tyfield) (type-lookup (tyfield-type-id tyfield) te))
                                                                   tyfields)
                                                              body-type)) ; TODO: modify ve to contain self for recursion!
                                          v-env))]
                    (if (not type-id)
                        new-v-env
                        (let [(declared-type (type-lookup type-id te))]
                          (if (assignable-to? declared-type body-type)
                              new-v-env
                              (error (format "type error: type mismatch, found ~a; expected ~a"
                                             declared-type body-type))))))]
                 [(tydec a b) v-env]
                 [(while-statement cond body) (error "while statement found in odd place")] ; just in case
                 ))]
           
       ;TODO: test function definitions
       (type-of-env expseq
                    (foldl accumulate-type-declarations
                           te ; the environment
                           decs)
                    (foldl accumulate-var-declarations
                           ve ; the environment
                           decs)))]
     ;end let statement
              
    ;[(record-creation (type-id type) fields)
    ;(if (check-record-fields fields (type-lookup type te))
    ))

;(define (infer-primitive expr ve)
;  (cond [(int-literal? expr) (t-int)]
;        [(string-literal? expr) (t-string)]
;        [(nil? expr) (error "type error: nil expressions must have a declared type")]
;        [(id? expr) (var-lookup (type-id-name expr) ve)]
;        [else (error (format "type error: failed to infer a basic primitive type from initial value ~a.  polymorphic types are not allowed" expr))]))


;;TESTS
(check-expect (type-of (parse-string "4"))
              (t-int))

(check-expect (type-of (parse-string "\"hello\""))
              (t-string))
(check-expect (type-of (parse-string "4-7")) 
              (t-int))
(check-expect (type-of (parse-string "4>7")) 
              (t-int))
(check-expect (type-of (parse-string "4=7")) 
              (t-int))
(check-expect (type-of (parse-string "\"zoomba\"=\"pizza\"")) 
              (t-int))

(check-expect (type-of (parse-string "-4"))
              (t-int))
(check-expect (type-of (parse-string "(4>7)=(5<6)"))
              (t-int))
(check-expect (type-of (parse-string "if 4 then 7 else 6"))
              (t-int))
(check-error (type-of (parse-string "if 4 then \"hamburger\" else 12")) "type error: then and else branches of if statement must have same type")
(check-expect (type-of (parse-string "while 1 do (27; \"spain\")"))
              (t-string))

(check-expect (type-of (parse-string "let type a = int in 4 end"))
              (t-int))
(check-expect (type-of (parse-string "let type f = int type g = int in 5 end"))
              (t-int))
(check-expect (type-of (parse-string "let var y := 4 in 29 end"))
              (t-int))
(check-expect (type-of (parse-string "let var u : int := 13 in 100 end"))
              (t-int))

(check-error (type-of (parse-string "let var w : string := 22 in 1 end")) "type error: type mismatch, found #(struct:t-string); expected #(struct:t-int)")
(check-error (type-of (parse-string "let type a = int in let var x : a := \"green\" in 37 end end")) "type error: type mismatch, found #(struct:type-id int); expected #(struct:t-string)")

(check-expect (type-of (parse-string "let var m : int := 4+4 in m end"))
              (t-int))
(check-expect (type-of (parse-string "let var n := 5*3+2-12*66-304440403 in \"waaaa\" end"))
              (t-string))
(check-error (type-of (parse-string "let var z := nil in end")) "variable z has value nil but no type") ; TODO: handle nil values more intelligently
(check-error (type-of (parse-string "let var z := nil in zz end")) "variable z has value nil but no type") ; TODO: handle nil values more intelligently
(check-expect (type-of (parse-string "let var zz : int := nil in end")) (t-void))
;(check-expect (type-of (parse-string "let type a = int var x : a := 2 in 154 end")) (t-int)) ; TODO: let*
;(check-expect (type-of (parse-string "let type a = int type b = a in let var nobbish : b := 48 in 23 end end")) (t-int)) ; TODO: let = letrec*

(test)