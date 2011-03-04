#lang racket

(require "parser.rkt")
(require test-engine/racket-tests)

; type-binding represents creating a new type
; let type a = int in ... end
(struct type-binding (id ty) #:transparent #:mutable)

; var-binding represents a variable that hold a value of a certain type
; let x = 3 in ... end
; let x : int = 3 in ... end
(struct var-binding (id ty) #:transparent)

(struct t-int () #:transparent)
(struct t-string () #:transparent)
(struct t-void () #:transparent) ; NOT TRANSPARENT, FUCK YOU RACKET
(struct t-nil () #:transparent)

(struct fun-arg (type) #:transparent #:mutable)

(struct t-fun (args result) #:transparent #:mutable) ; args is list of types
(struct t-array (elem-type) #:transparent #:mutable)
(struct t-record (fields) #:transparent)
(struct field (name ty) #:transparent #:mutable)

(struct t-dummy () #:transparent)

; type-lookup symbol listof-type-binding -> t-type
(define (type-lookup type-symbol type-env)
  (cond 
    [(equal? type-symbol 'int) (t-int)]
    [(equal? type-symbol 'string) (t-string)]
    ;[(type-id? type-symbol) (type-lookup (type-id-name type-symbol) type-env)]
    [else (or (ormap (lambda (binding) (if (equal? (type-binding-id binding) type-symbol)
                                           (type-binding-ty binding)
                                           false))
                     type-env)
              (error (format "unbound type ~a" type-symbol)))]))

; var-lookup symbol listof-var-binding -> t-type
(define (var-lookup var-symbol var-env)
  ;(print var-env)
  (or (ormap (lambda (binding) (if (equal? (var-binding-id binding) var-symbol)
                                   (var-binding-ty binding)
                                   false))
             var-env)
      (error (format "unbound identifier ~a in environment ~a" var-symbol var-env))))

; assignable-to? t-type t-type -> boolean
; takes the type of an identifier or function argument, and the type of a thing you want to put in it
; tells you if that's ok
(define (assignable-to? variable value)
  ;(print variable)
  ;(print value)
  (when (symbol? variable) (error "internal error: assignable-to received a symbol"))
  (and (not (t-void? value))
       (or (and (equal? value (t-nil)) (t-record? variable))
           (same-type? value variable))))

(define (same-type? a b)
  (cond [(and (t-nil? a) (t-nil? b)) #t] 
        [(or (t-nil? a) (t-nil? b)) (or (t-record? a) (t-record? b))]
        [(or (t-int? a) (t-string? a) (t-void? a)) (equal? a b)]
        [(or (t-array? a) (t-record? a)) (eq? a b)]
        [(t-fun? a) (and (andmap same-type? (t-fun-args a) (t-fun-args b))
                         (same-type? (t-fun-result a) (t-fun-result b)))]
        [(fun-arg a) (same-type? (fun-arg-type a) (fun-arg-type b))]
        [else (error "internal error: unknown types specified")]))


(define (ast-node->t-type ast-node te)
  ;(displayln (format "made a call to ast-node with node ~a" ast-node))
  (match ast-node
    [(type-id name) (begin #;(displayln (format "~a is of type ~a" name (type-lookup name te))) (type-lookup name te))]
    [(array-of ast-sub-node) (t-array (ast-node->t-type ast-sub-node te))]
    [(record-of tyfields) (begin
                            #;(displayln "calling ast func. on record-of node")
                            (t-record (map (lambda (tyf) 
                                           (match tyf
                                             [(tyfield id ast-sub-node)
                                              (field id (ast-node->t-type ast-sub-node te))]))
                                         tyfields)))]
    [(function-type arg-nodes val-node) (t-fun (map (λ (an) 
                                                      (fun-arg (ast-node->t-type an te))) 
                                                    arg-nodes)
                                               (ast-node->t-type val-node te))]
  ))



; type-of expr -> t-type
(define (type-of expr)
  (type-of-env expr empty empty))

(define (type-of-env expr te ve) ; type-env and var-env
  (match expr
    [(int-literal a) (t-int)]
    [(string-literal a) (t-string)]
    [(nil) (t-nil)]
    
    [(id a) (var-lookup a ve)]
    [(break) (t-void)]
    ;arrays
    [(array-creation (type-id type) size initval)
     (let [(size-type (type-of-env size te ve))]
            (if (not (t-int? size-type))
                (error (format "type error: expected type of array size to be int, instead it was ~a" size-type))
                (let [(declared-type (type-lookup type te))]
                  (if (not (t-array? declared-type))
                      (error (format "type error: type of array must be declared as an array, instead found ~a" declared-type))
                      (let [(initval-type (type-of-env initval te ve))]
                        (if (not (assignable-to? (t-array-elem-type declared-type) initval-type))
                            (error (format "type error: type mismatch; initial value ~a must match array type ~a"
                                           initval-type declared-type))
                            declared-type))))))]
    [(array-access (id name) index)
     (if (t-int? (type-of-env index te ve))
         (t-array-elem-type (var-lookup name ve))
         (error (format "type error: attempted to access array ~a with non-integer index" name)))]
          
    ;records      
    [(record-creation (type-id type) var-fields)
     (let [(decl-fields (type-lookup type te))]
       (cond [(not (= (length var-fields) (length (t-record-fields decl-fields))))
              ;TODO: improve this error message by telling you which fields you missed/overspecified
              (error (format "type error: type mismatch; wrong number of fields ~a specified for creation of record ~a"
                          var-fields decl-fields))]
             [(andmap
               (λ (field-of-type)
                 (or (ormap (λ (field-of-var) 
                              (if (equal? (field-name field-of-type)
                                          (fieldval-name field-of-var))
                                  (let [(varfield-type (type-of-env (fieldval-val field-of-var) te ve))]
                                    (if (assignable-to? (field-ty field-of-type) varfield-type)
                                        #t
                                        (error 
                                         (format "type error: type mismatch; field ~a was given value of type ~a; expected ~a"
                                                 (field-name field-of-type)
                                                 varfield-type
                                                 (field-ty field-of-type)))))
                                  #f
                                  ))
                            var-fields
                            )
                     (error (format "type mismatch; no value specified for field ~a in ~a" field-of-type type))))
               (t-record-fields decl-fields))
              decl-fields]
             [else (error "internal error: code 8726")]))]
    [(record-access rec-id field-id)
     (or (ormap (λ (field) (if (equal? (field-name field) field-id)
                               (field-ty field)
                               #f))
                (t-record-fields (var-lookup rec-id ve)))
         (error (format "semantic error: unknown field ~a of record ~a" rec-id field-id)))]
                  ;; rec-id has a field-id
     
    
    [(binary-op (op sym) arg1 arg2)
     (let [(t1 (type-of-env arg1 te ve))
           (t2 (type-of-env arg2 te ve))]
       (cond [(symbol=? sym '=) (cond [(and (t-nil? t1) (t-nil? t2)) (error "type error: found illegal expression \"nil == nil\"")]
                                      [(same-type? (type-of-env arg1 te ve) (type-of-env arg2 te ve)) (t-int)]
                                      [else (error "type error: arguments for equality comparison must be of same type")])]
             [(and (t-int? (type-of-env arg1 te ve))
                   (t-int? (type-of-env arg2 te ve)))
              (t-int)]
             [else (error (format "type error: args for operator ~a must be integers" sym))]))]

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
    
    ; control flow operators
    [(if-statement c t (list))
     (cond [(not (t-int? (type-of-env c te ve))) (error "type error: condition of if statement must have boolean/int value")]
           [(not (t-void? (type-of-env t te ve))) (error "type error: then branch of an if statement must have no value")]
           [else (t-void)])]
    [(if-statement c t e)
     (let ([type-of-t (type-of-env t te ve)]
           [type-of-e (type-of-env e te ve)] )
       (cond [(not (t-int? (type-of-env c te ve))) (error "type error: condition of if statement must have boolean/int value")]
             [(not (same-type? type-of-t type-of-e)) (error "type error: then and else branches of if statement must have same type")]
             [else (if (not (t-nil? type-of-t))
                       type-of-t
                       type-of-e)]))]
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
    
    ;let statements
    ;let-vars
    [(let-vars decs exp)
     (local [(define (accumulate-var-declarations decl v-env)
               (match decl
                 [(vardec id type-id val) ; type-id is symbol or false, NOT an ast-node ex: 'int or 'string or 'zoomba or 'whatever or false or #f
                  (let [(expression-type (type-of-env val te v-env))]
                    (if (not type-id)
                        (if (equal? (t-nil) expression-type)
                            (error (format "type error: variable ~a has value nil but no type" id))
                            (cons (var-binding id expression-type) v-env))
                        (let [(declared-type (type-lookup type-id te))]
                          (if (assignable-to? declared-type expression-type)
                              (cons (var-binding id declared-type) v-env)
                              (error (format "type error: type mismatch, found type ~a; expected ~a"
                                             type-id (unpack-error-annotation expression-type val)))))))]))]
                                                             
                                                             ;(type-id-name (array-creation-type-id val))))))))]))]
       (type-of-env exp
                    te
                    (foldl accumulate-var-declarations
                           ve
                           decs)))]
    ;let-funs
    [(let-funs decs exp)
     (local [; fundec ve -> new-ve
             (define (accumulate-fun-declaration decl v-env)
               (match decl
                 ; TODO: ensure that function argument names don't repeat
                 ; f(x, y) ok but f(x, x) bad
                 [(fundec id tyfields return-type body)
                  (cons (var-binding id (t-fun (map (lambda (tf)
                                                      (fun-arg (match tf
                                                                 [(tyfield name (type-id type-sym))
                                                         (type-lookup type-sym te)])))
                                                    tyfields)
                                               (if (not return-type)
                                                   (t-void)
                                                   (type-lookup return-type te))))
                        v-env)]))
             ; takes a fundec and a ve
             ; the ve includes everything except the argument types
             ; checks whether the body is valid in {that ve extended with the args}
             ; throws an error if the body is invalid
             ; otherwise returns some
             (define (verify-fun-declaration decl v-env)
               (match decl
                 ; TODO: ensure that function argument names don't repeat
                 ; f(x, y) ok but f(x, x) bad
                 [(fundec id tyfields return-type body)
                  (let [(v-env-plus-args (foldl (lambda (tf var-env)
                                                  (match tf
                                                    [(tyfield id (type-id type-sym))
                                                     (cons (var-binding id (type-lookup type-sym te))
                                                           var-env)]))
                                                v-env
                                                tyfields))]
                    (or (same-type? (type-of-env body te v-env-plus-args)
                                    (if (not return-type)
                                        (t-void)
                                        (type-lookup return-type te)))
                        (error (format "type error: type mismatch, declaration of function ~a was declared as ~a but body returns type ~a"
                                       id
                                       (if (not return-type)
                                           (t-void)
                                           (type-lookup return-type te))
                                       (type-of-env body te v-env-plus-args)))))
                                       
                        ]))
             (define working-env (foldl accumulate-fun-declaration ve decs))]
       (begin
         ;(print working-env)
         (map (lambda (fdecl) (verify-fun-declaration fdecl working-env)) decs) ; throw error if any function doesn't type
         (type-of-env exp
                      te
                      working-env))
       )]
    ;let-types
    ;
    
    [(let-types decs exp)
     (local [(define (accumulate-type-declarations decl ty-env)
               (match decl
                 [(tydec name ast-type-node) (cons (type-binding name (t-dummy)) ty-env)]))
                                              ;(type-binding name (ast-node->t-type ast-type-node ty-env))

             (define new-bindings (foldr accumulate-type-declarations empty decs))
             (define new-te (append new-bindings te))
             
             (define (contains-dummy? t-env)
               ;(displayln t-env)
               (ormap (match-lambda
                        [(type-binding type-id type)
                         (is-dummy? type empty)])
                      t-env))
             
             (define (is-dummy? t visited)
               (and (not (ormap (λ (node) (eq? node t)) visited))
                    (let [(new-visited (cons t visited))]
                    (match t
                      [(t-dummy) #t]
                      [(t-record fieldlist) (ormap (λ (fld) (is-dummy? (field-ty fld) new-visited)) fieldlist)]
                      [(t-array elem) (is-dummy? elem new-visited)]
                      [(t-fun args result) (or (ormap (λ (arg) (is-dummy? arg new-visited)) args) (is-dummy? result new-visited))]
                      [else #f]))))
             
             (define (fix-decs! new-t-env new-bindings tydecs num-tries-left)
               #;(displayln (format "~a tries left" num-tries-left))
               (cond [(not (contains-dummy? new-bindings)) (void)]
                     [(<= num-tries-left 0) (error (format "illegal cycle in type declarations, environment was ~a" new-t-env))]
                     [else
                      (begin 
                        (map (λ (bind dec)
                                 (fix-dec! bind (tydec-ty dec) new-t-env))
                             new-bindings
                             tydecs)
                        ;(displayln new-t-env)
                        (fix-decs! new-t-env new-bindings tydecs (sub1 num-tries-left))
                        ;(displayln new-t-env)
                        )]))
             
             (define (fix-dec! bind dec new-t-env)
               (match (type-binding-ty bind)
                 [(or (t-dummy) (t-array (t-dummy)))
                  (set-type-binding-ty! bind (ast-node->t-type dec new-t-env))]
                 [(t-record fields)
                  (map (λ (record-tyfield bound-field)
                         (when (t-dummy? (field-ty bound-field))
                           (set-field-ty! bound-field (ast-node->t-type (tyfield-type-id record-tyfield) new-t-env))))
                       (record-of-tyfields dec)
                       fields)]
                 [(t-fun args result)
                  (map (λ (decl-arg bound-arg)
                         (when (t-dummy? (fun-arg-type bound-arg))
                           (set-fun-arg-type! bound-arg (ast-node->t-type decl-arg new-t-env))))
                       (function-type-dom dec)
                       args)
                  (when (t-dummy? result)
                    (set-t-fun-result! (type-binding-ty) (ast-node->t-type (function-type-rng dec) new-t-env)))]
                 [else (void)]))]
             
       (fix-decs! new-te new-bindings decs 10)
             
       (type-of-env exp new-te ve))]
              
    
    [(funcall fun-id caller-args)
     (let* [(f (var-lookup (id-name fun-id) ve))
            (fundef-args (t-fun-args f))]
       (if (andmap (λ (fundef-arg caller-arg) (assignable-to? (fun-arg-type fundef-arg) (type-of-env caller-arg te ve)))
                   (t-fun-args f)
                   caller-args)
           (t-fun-result f)
           (error (format "type error: mismatched type applied to function argument, expected ~a, found ~a"
                          (t-fun-args f) caller-args))))]
     
    ))

(define (unpack-error-annotation type-expr ast-node)
  (cond [(array-creation? ast-node) (type-id-name (array-creation-type-id ast-node))]
        [(record-creation? ast-node) (type-id-name (record-creation-type-id ast-node))]
        [else type-expr]))


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

; array creation/access tests
(check-error (type-of (parse-string "a[\"zoomba\"]")) "type error: attempted to access array a with non-integer index")
(check-error (type-of (parse-string "let type intarray = array of int var x := intarray[4] of \"pizza\" in end")) "type error: type mismatch; initial value #(struct:t-string) must match array type #(struct:t-array #(struct:t-int))")
(check-error (type-of (parse-string "let var x := int[10] of 338 in end")) "type error: type of array must be declared as an array, instead found #(struct:t-int)")
(check-expect (type-of (parse-string "let type intarray = array of int var y := intarray[26] of 0 in y end")) (t-array (t-int)))
(check-expect (type-of (parse-string "let type intarray = array of int var y := intarray[26] of 0 in y[3] end")) (t-int))

(check-expect (type-of (parse-string "let type point = {x : int, y:int} type pointarray = array of point var y := pointarray[50] of nil in y end")) (t-array (t-record (list (field 'x (t-int)) (field 'y (t-int))))))

(check-error (type-of (parse-string "a[4]")) "unbound identifier a in environment ()")

(check-expect (type-of (parse-string "let type intarray = array of int var ab := intarray[10] of 5 in ab[24] end")) (t-int))
(check-error (type-of (parse-string "let type intarray = array of int var z := intarray[10] of 5 in z[\"pizza\"] end")) "type error: attempted to access array z with non-integer index")
(check-error (type-of (parse-string "let type intarray = array of int var aa := intarray[10] of 5 in aa[nil] end")) "type error: attempted to access array aa with non-integer index")
(check-expect (type-of (parse-string "let type a = array of int var x : a := a[7] of 1 in x end"))
              (t-array (t-int)))

(check-error (type-of (parse-string "let type a = array of int type b = array of int var x : a := b[10] of 0 in end")) "type error: type mismatch, found type a; expected b") ;

; record creation/access tests

(check-expect (type-of (parse-string "let type foo = {} var x : foo := foo{} in x end")) (t-record empty))
(check-expect (type-of (parse-string "let type wazza = {x : int} var w : wazza := wazza{x=5} in w end")) (t-record (list (field 'x (t-int)))))
(check-expect (type-of (parse-string "let type pizza = {x : int, y : int} var z := pizza{x=3,y=-39} in z end")) (t-record (list (field 'x (t-int)) (field 'y (t-int)))))
(check-error (type-of (parse-string "let type oatmeal = {x : int} var m : oatmeal := oatmeal{x=\"i hate oatmeal\"} in m end")) "type error: type mismatch; field x was given value of type #(struct:t-string); expected #(struct:t-int)")
(check-error (type-of (parse-string "let type soda = {x : int} var y : soda := soda{p=3} in y end")) "type mismatch; no value specified for field #(struct:field x #(struct:t-int)) in soda")
(check-error (type-of (parse-string "let type bagels = {x : int, y : blarg} in end")) "unbound type blarg")
(check-error (type-of (parse-string "let type sandwich = {x : string} var turkey := sandwich{x = \"tomato\", y = \"pickles\"} in turkey end")) "type error: type mismatch; wrong number of fields (#(struct:fieldval x #(struct:string-literal tomato)) #(struct:fieldval y #(struct:string-literal pickles))) specified for creation of record #(struct:t-record (#(struct:field x #(struct:t-string))))")
(check-error (type-of (parse-string "let type greem = {x : int} var z : greem := greem{x=12,m=22} in z end")) "type error: type mismatch; wrong number of fields (#(struct:fieldval x #(struct:int-literal 12)) #(struct:fieldval m #(struct:int-literal 22))) specified for creation of record #(struct:t-record (#(struct:field x #(struct:t-int))))")
(check-expect (type-of (parse-string "let type pt = {x : int, y: int} in let type line = { a : pt, b : pt} in line{a=pt{x=1,y=44},b=pt{x=98,y=6000000}} end end")) (t-record (list (field 'a (t-record (list (field 'x (t-int)) (field 'y (t-int))))) (field 'b (t-record (list (field 'x (t-int)) (field 'y (t-int))))))))



(check-expect (type-of (parse-string "(34; 27)")) (t-int))

; let-statement tests

(check-error (type-of (parse-string "let var w : string := 22 in 1 end")) "type error: type mismatch, found type string; expected #(struct:t-int)")
(check-error (type-of (parse-string "let type a = int in let var x : a := \"green\" in 37 end end")) "type error: type mismatch, found type a; expected #(struct:t-string)")
(check-expect (type-of (parse-string "let var m : int := 4+4 in m end"))
              (t-int))
(check-expect (type-of (parse-string "let var n := 5*3+2-12*66-304440403 in \"waaaa\" end"))
              (t-string))
(check-error (type-of (parse-string "let var z := nil in end")) "type error: variable z has value nil but no type")
(check-error (type-of (parse-string "let var z := nil in zz end")) "type error: variable z has value nil but no type")
(check-error (type-of (parse-string "let var zz : int := nil in end")) "type error: type mismatch, found type int; expected #(struct:t-nil)")
(check-expect (type-of (parse-string "let type a = int var x : a := 2 in 154 end")) (t-int)) ; TODO: let*
(check-expect (type-of (parse-string "let type a = int type b = a in let var nobbish : b := 48 in 23 end end")) (t-int)) ; TODO: let = letrec*
(check-expect (type-of (parse-string "let var x := 7   var y := x in y  end"))
              (t-int))
(check-expect (type-of (parse-string "let var x := 7 in let var y := x in y end end"))
              (t-int))



; fundec/funcall tests
(check-expect (type-of (parse-string "let function f() : int = 25 in f end")) (t-fun empty (t-int)))
(check-expect (type-of (parse-string "let function f(x : int) : int = 25 in f(12) end")) (t-int))
(check-expect (type-of (parse-string "let function f(x : int) = (25;()) in f end")) (t-fun (list (fun-arg (t-int))) (t-void)))

(check-expect (type-of (parse-string "let function g(x : int) : int = 12 in g end")) (t-fun (list (fun-arg (t-int))) (t-int)))
(check-expect (type-of (parse-string "let type fun = int -> int function f(x : fun) : fun = let function g(x : int) : int = 7 in g end in f end")) (t-fun (list (fun-arg (t-fun (list (fun-arg (t-int))) (t-int)))) (t-fun (list (fun-arg (t-int))) (t-int))))

(check-expect (type-of (parse-string "let type fun = int -> int function g(x : int) : int = 7 in let function f(x : fun) : fun = g in f end end")) (t-fun (list (fun-arg (t-fun (list (fun-arg (t-int))) (t-int)))) (t-fun (list (fun-arg (t-int))) (t-int))))

(check-expect (type-of (parse-string "let type fun = int -> int function g(x : int) : int = 7 function f(x : fun ) : fun = g in f end")) (t-fun (list (fun-arg (t-fun (list (fun-arg (t-int))) (t-int)))) (t-fun (list (fun-arg (t-int))) (t-int))))

(check-expect (type-of (parse-string "let function f(x:int):int = 1+f(x) in f end")) (t-fun (list (fun-arg (t-int))) (t-int)))
(check-expect (type-of (parse-string "let function f(x:int):int = g(x)+3 function g(x:int):int = if g(x) then f(x)+4 else 26 in g(5) end")) (t-int))

; misc tests
(check-expect (type-of (parse-string "if 1 then nil else nil")) (t-nil))
(check-error (type-of (parse-string "let var x := if 1 then nil else nil in end")) "type error: variable x has value nil but no type")

(check-expect (type-of (parse-string "let type a = {x:int} in if 1 then nil else a{x=1} end")) (t-record (list (field 'x (t-int)))))
(check-expect (type-of (parse-string "let type a = {x:int} in if 1 then a{x=1} else nil end")) (t-record (list (field 'x (t-int)))))


; recursive types tests
(check-expect (type-of (parse-string "let type intlist = {hd:int, tl:intlist} var x := intlist{hd=1, tl=intlist{hd=2, tl=intlist{hd=3, tl=nil}}} in x end"))
              ; shared is broken with (struct ...) structures. 
              #;(shared [(-a- (t-record (list (field 'hd (t-int)) (field 'tl -a-))))] -a-)
              (local [(define f (field 'tl 'something))
                      (define r (t-record (list (field 'hd (t-int)) f)))]
                (set-field-ty! f r)
                r))
(check-expect (type-of (parse-string "let type intlist = {hd:int, tl:intlist} var x := intlist{hd=1, tl=intlist{hd=2, tl=intlist{hd=3, tl=nil}}} in x end"))
              ; shared is broken with (struct ...) structures. 
              #;(shared [(-a- (t-record (list (field 'hd (t-int)) (field 'tl -a-))))] -a-)
              (local [(define f (field 'tl 'something))
                      (define r (t-record (list (field 'hd (t-int)) f)))]
                (set-field-ty! f r)
                r))
(check-expect (type-of (parse-string "let type tree = {key:int, children:treelist} type treelist = {hd:tree, tl:treelist} var x : tree := nil in x end"))
              #;(shared ([t (t-record (list (field 'key (t-int)) (field 'children tl)))]
                         [tl (t-record (list (field 'hd t) (field 'tl tl)))])
                  t)
              (local [(define children (field 'children #f))
                      (define t (t-record (list (field 'key (t-int))
                                                children)))
                      (define tail (field 'tl #f))
                      (define tl (t-record (list (field 'hd t)
                                                 tail)))]
                (set-field-ty! children tl)
                (set-field-ty! tail tl)
                t))

(check-expect (type-of (parse-string "let type a = b type b = c type c = d type d = int var x : a := 6 in x end")) (t-int))
(check-expect (type-of (parse-string "let type a = array of b type b = c type c = int var x := a[6] of 0 in x end"))
              (t-array (t-int)))

(check-error (type-of (parse-string "let type a = b type b = a in end")) "illegal cycle in type declarations, environment was (#(struct:type-binding a #(struct:t-dummy)) #(struct:type-binding b #(struct:t-dummy)))")

;(check-error (type-of (parse-string "let type b = int -> intfun type intfun = b -> int in end")) "wossar")
(check-expect (type-of (parse-string "let type a = array of int type alist = {x:a,y:alist} in alist{x=a[10] of 3, y=nil} end"))
              (local [(define x (field 'x (t-array (t-int))))            
                      (define y (field 'y (t-dummy)))
                      (define alist (t-record (list x y)))]
                (set-field-ty! y alist)
                alist))
(check-expect (type-of (parse-string "let type a = int -> int type arec = {x:a,y:arec} type fun = arec -> a in end")) (t-void)) 
(check-expect (type-of (parse-string "break")) (t-void))
              

(test)
; TODO more test cases
