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

(define top-level-te
  (list (type-binding 'int (box (t-int)))
        (type-binding 'string (box (t-string)))))

; type-lookup symbol listof-type-binding -> (box t-type) ; TODO: actually returns a box. change this?
(define (type-lookup type-symbol type-env)
  (or (ormap (lambda (binding) (if (equal? (type-binding-id binding) type-symbol)
                                   (type-binding-ty binding)
                                   false))
             (append type-env top-level-te)) ; user types plus predefined types. the predefined ones can be shadowed.
      (error (format "unbound type ~a" type-symbol))))

; var-lookup symbol listof-var-binding -> t-type
(define (var-lookup var-symbol var-env)
  (or (ormap (lambda (binding) (if (equal? (var-binding-id binding) var-symbol)
                                   (var-binding-ty binding)
                                   false))
             var-env)
      (error (format "unbound identifier ~a in environment ~a" var-symbol var-env))))

; assignable-to? t-type t-type -> boolean
; takes the type of an identifier or function argument, and the type of a thing you want to put in it
; tells you if that's ok
(define (assignable-to? variable value)
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
        [(box? a) (same-type? (unbox a) (unbox b))]
        [else (error "internal error: unknown types specified")]))

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
    
    [(assignment lvalue exp)
     (if (assignable-to? (type-of-env lvalue te ve) (type-of-env exp te ve))
         (t-void)
         (error (format "type error: illegal assignment of type ~a to type ~a"
                        (type-of-env lvalue te ve)
                        (type-of-env exp te ve))))]
    ;arrays
    [(array-creation (type-id type) size initval)
     (let [(size-type (type-of-env size te ve))]
            (if (not (t-int? size-type))
                (error (format "type error: expected type of array size to be int, instead it was ~a" size-type))
                (let [(declared-type (unbox (type-lookup type te)))]
                  (if (not (t-array? declared-type))
                      (error (format "type error: type of array must be declared as an array, instead found ~a" declared-type))
                      (let [(initval-type (type-of-env initval te ve))]
                        (if (not (assignable-to? (unbox (t-array-elem-type declared-type)) initval-type))
                            (error (format "type error: type mismatch; initial value ~a must match array type ~a"
                                           initval-type declared-type))
                            declared-type))))))]
    
    [(array-access sub-node index)
     (match (type-of-env sub-node te ve)
       [(t-array (box type)) (if (t-int? (type-of-env index te ve))
                                 type
                                 (error (format "type error: attempted to access array ~a with non-integer index" sub-node)))]
       [else (error (format "type error: illegal access of non-array variable ~a" sub-node))])]
          
    ;records      
    [(record-creation (type-id type) var-fields)
     (let [(decl-fields (unbox (type-lookup type te)))]
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
                                    (if (assignable-to? (unbox (field-ty field-of-type)) varfield-type)
                                        #t
                                        (error (format "type error: type mismatch; field ~a was given value of type ~a; expected ~a"
                                                       (field-name field-of-type)
                                                       varfield-type
                                                       (field-ty field-of-type)))))
                                  #f))
                            var-fields)
                     (error (format "type mismatch; no value specified for field ~a in ~a" field-of-type type))))
               (t-record-fields decl-fields))
              decl-fields]
             [else (error "internal error: code 8726")]))]
    [(record-access rec field-id)
     (or (ormap (λ (field) (if (equal? (field-name field) field-id)
                               (unbox (field-ty field))
                               #f))
                (t-record-fields (type-of-env rec te ve)))
         (error (format "semantic error: unknown field ~a of record ~a" field-id rec)))]
                  ;; rec-id has a field-id
     
    
    [(binary-op (op sym) arg1 arg2)
     (let [(t1 (type-of-env arg1 te ve))
           (t2 (type-of-env arg2 te ve))]
       (cond [(eq? sym '=) (cond [(and (t-nil? t1) (t-nil? t2)) (error "type error: found illegal expression \"nil == nil\"")]
                                 [(same-type? (type-of-env arg1 te ve) (type-of-env arg2 te ve)) (t-int)]
                                 [else (error "type error: arguments for equality comparison must be of same type")])]
             [(and (t-int? t1) (t-int? t2)) (t-int)]
             [else (error (format "type error: args for operator ~a must be integers. found ~a and ~a" sym
                                  (type-of-env arg1 te ve)
                                  (type-of-env arg2 te ve)))]))]

    [(unary-op (op '-) arg1)
     (if (not (t-int? (type-of-env arg1 te ve)))
         (error "type error: arg to unary minus must be int")
         (t-int))]
    
    [(expseq explist) ; foldl is sexy
     (foldl (λ (exp type)
              (type-of-env exp te ve))
            (t-void)
            explist)]
    
    ; control flow operators
    [(if-statement c t e)
     (let ([type-of-t (type-of-env t te ve)]
           [type-of-e (type-of-env e te ve)] )
       (cond [(not (t-int? (type-of-env c te ve))) (error "type error: condition of if statement must have boolean/int value")]
             [(not (same-type? type-of-t type-of-e)) (error "type error: then and else branches of if statement must have same type")]
             [else (if (not (t-nil? type-of-t))
                       type-of-t
                       type-of-e)]))]
    
    [(while-statement c body)
     (cond [(not (t-int? (type-of-env c te ve))) (error "type error: while statement condition must have boolean/int value")]
           [(not (t-void? (type-of-env body te ve))) (error "type error: while statement body must return no value")]
           [else (t-void)])]
    
    [(for-statement var start end body)
     (cond [(not (and (t-int? (type-of-env start te ve))
                      (t-int? (type-of-env end te ve))))
            (error "type error: for statement must increment an int from start to end int values")]
           [(not (t-void? (type-of-env body te ve))) (error "type error: body of for statement must return no value")]
           [else (t-void)])]
    
    ;let statements
    ;let-vars
    [(let-vars decs exp)
     (local [(define (accumulate-var-declarations decl v-env)
               (match decl
                 [(vardec id type-id val) ; type-id is symbol or false, NOT an ast-node ex: 'int or 'string or 'zoomba or 'whatever or false or #f
                  (let [(expression-type (type-of-env val te v-env))]
                    (if (not type-id)
                        (cond [(t-nil? expression-type)
                               (error (format "type error: variable ~a has value nil but no type" id))]
                              [(t-void? expression-type)
                               (error (format "type error: cannot assign void type to variable ~a" id))]
                              [else (cons (var-binding id expression-type) v-env)])
                        (let [(declared-type (unbox (type-lookup type-id te)))]
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
                 [(fundec id tyfields return-type body)
                  (cons (var-binding id (t-fun (map (lambda (tf)
                                                      (match tf
                                                        [(tyfield name (type-id type-sym))
                                                         (type-lookup type-sym te)]))
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
                 ; f(x) and f(x) should never be declared, independent of scope!
                 ; also ensure that no two of f() f(x) f(a) f(a, b) etc are declared at once
                 [(fundec id tyfields return-type body)
                  (if (contains-dupes? (map tyfield-id tyfields))
                      (error 
                       (format "semantic error: definition of function ~a contains multiple arguments with the same identifier ~a" 
                               id (contains-dupes? (map tyfield-id tyfields))))
                  
                      (let [(v-env-plus-args (foldl (lambda (tf var-env)
                                                      (match tf
                                                        [(tyfield id (type-id type-sym))
                                                         (cons (var-binding id (unbox (type-lookup type-sym te)))
                                                               var-env)]))
                                                v-env
                                                tyfields))]
                        (or (same-type? (type-of-env body te v-env-plus-args); TODO (should be assignable-to?)?
                                        (if (not return-type)
                                            (t-void)
                                            (unbox (type-lookup return-type te))))
                            (error (format "type error: type mismatch, declaration of function ~a was declared as ~a but body returns type ~a"
                                           id
                                           (if (not return-type)
                                               (t-void)
                                               (unbox (type-lookup return-type te)))
                                           (type-of-env body te v-env-plus-args))))))
                  
                  ]))
             (define working-env (foldl accumulate-fun-declaration ve decs))
             
             (define (check-repeat-fundecs decs)
               (when (contains-dupes? (map fundec-id decs))
                 (error (format "semantic error: multiple function declarations for same identifier ~a in same block"
                                (contains-dupes? (map fundec-id decs))))))]
       
       
       (begin
         ;(print working-env)
         (check-repeat-fundecs decs)
         (map (lambda (fdecl) (verify-fun-declaration fdecl working-env)) decs) ; throw error if any function doesn't type
         (type-of-env exp
                      te
                      working-env))
       )]
    ;let-types
    ;
    
    [(let-types decs exp)
     (local 
       [(define (accumulate-type-declarations decl ty-env)
          (match decl
            [(tydec name ast-type-node) (cons (type-binding name (box #f)) ty-env)]))
        
        (define new-bindings (foldr accumulate-type-declarations empty decs))
        
        ; after we append the new bindings, we just mutate same new-te
        ; so we don't need to pass it around anymore
        (define new-te (append new-bindings te))
        
        ; ast-node -> t-type
        (define (instantiate-t-type ast-node)
            (match ast-node
              [(array-of (type-id name)) (t-array (type-lookup name new-te))]
              [(record-of tyfields)
                (t-record
                 (map (match-lambda
                        [(tyfield id (type-id name))
                         (field id (type-lookup name new-te))])
                      tyfields))]
              [(function-type arg-nodes (type-id name))
                (t-fun 
                 (map (match-lambda
                        [(type-id name)
                         (type-lookup name new-te)])
                      arg-nodes)
                 (type-lookup name new-te))]))
        
        ; t-type ast-node -> bool       
        (define (resolve-dec? t-box dec)
          (and (false? (unbox t-box)) ; break and return false if it's not an empty box
               (let [(new-t (match dec
                              ; if the dec is just binding it to another type,
                              ; look it up
                              [(type-id name) (unbox (type-lookup name new-te))]
                              ; if it's creating a complex type,
                              ; instantiate it and look up the fields of the type
                              [complex-type (instantiate-t-type complex-type)]))]
                 (and new-t                  ; if it did not remain an empty box
                      (set-box! t-box new-t) ; make the change
                      #t))))                         ; return true so we know to keep going
        
        ; (listof box t-type) (listof ast-node) -> void
        (define (fix-decs! new-bindings tydecs)
          (if (ormap resolve-dec? new-bindings tydecs)
              (fix-decs! new-bindings tydecs) ; keep going if something changed
              (unless (andmap unbox new-bindings)
                ; here we have reached fixed point of fix-decs! if there are empty boxes, we have a cycle
                (error (format "illegal cycle in type declarations, unresolved types were: ~a"
                               (map type-binding-id 
                                    (filter (lambda (binding)
                                              (false? (unbox (type-binding-ty binding))))
                                            new-te)))))))
        
        (define (check-repeat-args decs)
          (map (match-lambda
                 [(tydec ty-id (record-of tyfields))
                  (when (contains-dupes? (map tyfield-id tyfields))
                    (error 
                     (format "semantic error: record declaration ~a contains multiple fields with the same identifier ~a"
                             ty-id (contains-dupes? (map tyfield-id tyfields)))))]
                 [else #f])
               decs))
        
        (define (check-repeat-type-decs decs)
          (when (contains-dupes? (map tydec-type-id decs))
            (error (format "semantic error: multiple type declarations for same identifier ~a in same block"
                           (contains-dupes? (map tydec-type-id decs))))))]
       
       (check-repeat-args decs)
       (check-repeat-type-decs decs)
       (fix-decs! (map type-binding-ty new-bindings) (map tydec-ty decs))
       
       (type-of-env exp new-te ve))]
    
    
    [(funcall fun-id caller-args)
     (let* [(f (var-lookup (id-name fun-id) ve))
            (fundef-args (t-fun-args f))]
       ;TODO make sure the funcall and the fundef have the same number of arguments
       (if (andmap (λ (fundef-arg caller-arg) (assignable-to? (unbox fundef-arg) (type-of-env caller-arg te ve)))
                   (t-fun-args f)
                   caller-args)
           (unbox (t-fun-result f))
           (error (format "type error: mismatched type applied to function argument, expected ~a, found ~a"
                          (t-fun-args f) caller-args))))]
     
    ))
                               
(define (contains-dupes? a-list (accum-set (set)))
  (match a-list
    [(list) #f]
    [(cons hd tl) (if (set-member? accum-set hd)
                      hd
                      (contains-dupes? tl (set-add accum-set hd)))]))

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
(check-error (type-of (parse-string "if 115 then 6")) "type error: then and else branches of if statement must have same type")
(check-expect (type-of (parse-string "if 44 then ()")) (t-void))
(check-expect (type-of (parse-string "if 4 then 7 else 6"))
              (t-int))
(check-error (type-of (parse-string "if 4 then \"hamburger\" else 12")) "type error: then and else branches of if statement must have same type")
(check-error (type-of (parse-string "while 1 do (27; \"spain\")")) "type error: while statement body must return no value")

(check-expect (type-of (parse-string "let type a = int in 4 end"))
              (t-int))
(check-expect (type-of (parse-string "let type f = int type g = int in 5 end"))
              (t-int))
(check-expect (type-of (parse-string "let var y := 4 in 29 end"))
              (t-int))
(check-expect (type-of (parse-string "let var u : int := 13 in 100 end"))
              (t-int))

; array creation/access tests
(check-error (type-of (parse-string "let type sa = array of int var a := sa[10] of 0 in a[\"zoomba\"] end")) "type error: attempted to access array #(struct:id a) with non-integer index")
(check-error (type-of (parse-string "let type intarray = array of int var x := intarray[4] of \"pizza\" in end")) "type error: type mismatch; initial value #(struct:t-string) must match array type #(struct:t-array #&#(struct:t-int))")
(check-error (type-of (parse-string "let var x := int[10] of 338 in end")) "type error: type of array must be declared as an array, instead found #(struct:t-int)")
(check-expect (type-of (parse-string "let type intarray = array of int var y := intarray[26] of 0 in y end")) (t-array (box (t-int))))
(check-expect (type-of (parse-string "let type intarray = array of int var y := intarray[26] of 0 in y[3] end")) (t-int))

(check-expect (type-of (parse-string "let type point = {x : int, y:int} type pointarray = array of point var y := pointarray[50] of nil in y end")) (t-array (box (t-record (list (field 'x (box (t-int))) (field 'y (box (t-int))))))))

(check-error (type-of (parse-string "a[4]")) "unbound identifier a in environment ()")

(check-expect (type-of (parse-string "let type intarray = array of int var ab := intarray[10] of 5 in ab[24] end")) (t-int))
(check-error (type-of (parse-string "let type intarray = array of int var z := intarray[10] of 5 in z[\"pizza\"] end")) "type error: attempted to access array #(struct:id z) with non-integer index")
(check-error (type-of (parse-string "let type intarray = array of int var aa := intarray[10] of 5 in aa[nil] end")) "type error: attempted to access array #(struct:id aa) with non-integer index")
(check-expect (type-of (parse-string "let type a = array of int var x : a := a[7] of 1 in x end"))
              (t-array (box (t-int))))

(check-error (type-of (parse-string "let type a = array of int type b = array of int var x : a := b[10] of 0 in end")) "type error: type mismatch, found type a; expected b") ;
(check-error (type-of (parse-string "let type a = array of int var a : int := 0 in a[10] end")) "type error: illegal access of non-array variable #(struct:id a)")
(check-expect (type-of (parse-string "let type a = array of int type b = array of a var x := b[10] of a[10] of 9 in x[3][3] end"))
              (t-int))

; record creation/access tests

(check-expect (type-of (parse-string "let type foo = {} var x : foo := foo{} in x end")) (t-record empty))
(check-expect (type-of (parse-string "let type wazza = {x : int} var w : wazza := wazza{x=5} in w end")) (t-record (list (field 'x (box (t-int))))))
(check-expect (type-of (parse-string "let type pizza = {x : int, y : int} var z := pizza{x=3,y=-39} in z end")) (t-record (list (field 'x (box (t-int))) (field 'y (box (t-int))))))
(check-error (type-of (parse-string "let type oatmeal = {x : int} var m : oatmeal := oatmeal{x=\"i hate oatmeal\"} in m end")) "type error: type mismatch; field x was given value of type #(struct:t-string); expected #&#(struct:t-int)")
(check-error (type-of (parse-string "let type soda = {x : int} var y : soda := soda{p=3} in y end")) "type mismatch; no value specified for field #(struct:field x #&#(struct:t-int)) in soda")
(check-error (type-of (parse-string "let type bagels = {x : int, y : blarg} in end")) "unbound type blarg")
(check-error (type-of (parse-string "let type sandwich = {x : string} var turkey := sandwich{x = \"tomato\", y = \"pickles\"} in turkey end")) "type error: type mismatch; wrong number of fields (#(struct:fieldval x #(struct:string-literal tomato)) #(struct:fieldval y #(struct:string-literal pickles))) specified for creation of record #(struct:t-record (#(struct:field x #&#(struct:t-string))))")
(check-error (type-of (parse-string "let type greem = {x : int} var z : greem := greem{x=12,m=22} in z end")) "type error: type mismatch; wrong number of fields (#(struct:fieldval x #(struct:int-literal 12)) #(struct:fieldval m #(struct:int-literal 22))) specified for creation of record #(struct:t-record (#(struct:field x #&#(struct:t-int))))")
(check-expect (type-of (parse-string "let type pt = {x : int, y: int} in let type line = { a : pt, b : pt} in line{a=pt{x=1,y=44},b=pt{x=98,y=6000000}} end end")) (t-record (list (field 'a (box (t-record (list (field 'x (box (t-int))) (field 'y (box (t-int))))))) (field 'b (box (t-record (list (field 'x (box (t-int))) (field 'y (box (t-int))))))))))
(check-expect (type-of (parse-string "let type a = {x:a,z:int} var y := a{x=a{x=a{x=nil,z=3},z=3}, z=3} in y.x.x.z end"))
              (t-int))

; combo array/record access tests
(check-expect (type-of (parse-string "let type a = array of b type b = {x:a,y:int} var z := a[10] of b{x=a[15] of nil,y=3} in z[4].x[6].y end"))
              (t-int))

(check-expect (type-of (parse-string "(34; 27)")) (t-int))

; let-statement tests

(check-error (type-of (parse-string "let var w := () in end")) "type error: cannot assign void type to variable w")
(check-error (type-of (parse-string "let var w : string := 22 in 1 end")) "type error: type mismatch, found type string; expected #(struct:t-int)")
(check-error (type-of (parse-string "let type a = int in let var x : a := \"green\" in 37 end end")) "type error: type mismatch, found type a; expected #(struct:t-string)")
(check-expect (type-of (parse-string "let var m : int := 4+4 in m end"))
              (t-int))
(check-expect (type-of (parse-string "let var n := 5*3+2-12*66-304440403 in \"waaaa\" end"))
              (t-string))
(check-error (type-of (parse-string "let var z := nil in end")) "type error: variable z has value nil but no type")
(check-error (type-of (parse-string "let var z := nil in zz end")) "type error: variable z has value nil but no type")
(check-error (type-of (parse-string "let var zz : int := nil in end")) "type error: type mismatch, found type int; expected #(struct:t-nil)")
(check-expect (type-of (parse-string "let type a = int var x : a := 2 in 154 end")) (t-int))
(check-expect (type-of (parse-string "let type a = int type b = a in let var nobbish : b := 48 in 23 end end")) (t-int))
(check-expect (type-of (parse-string "let var x := 7   var y := x in y  end"))
              (t-int))
(check-expect (type-of (parse-string "let var x := 7 in let var y := x in y end end"))
              (t-int))



; fundec/funcall tests
(check-expect (type-of (parse-string "let function f() : int = 25 in f end")) (t-fun empty (box (t-int))))
(check-expect (type-of (parse-string "let function f(x : int) : int = 25 in f(12) end")) (t-int))
(check-expect (type-of (parse-string "let function f(x : int) = (25;()) in f end")) (t-fun (list (box (t-int))) (t-void)))

(check-expect (type-of (parse-string "let function g(x : int) : int = 12 in g end")) (t-fun (list (box (t-int))) (box (t-int))))
(check-expect (type-of (parse-string "let type fun = int -> int function f(x : fun) : fun = let function g(x : int) : int = 7 in g end in f end")) (t-fun (list (box (t-fun (list (box (t-int))) (box (t-int))))) (box (t-fun (list (box (t-int))) (box (t-int))))))

(check-expect (type-of (parse-string "let type fun = int -> int function g(x : int) : int = 7 in let function f(x : fun) : fun = g in f end end")) (t-fun (list (box (t-fun (list (box (t-int))) (box (t-int))))) (box (t-fun (list (box (t-int))) (box (t-int))))))

(check-expect (type-of (parse-string "let type fun = int -> int function g(x : int) : int = 7 function f(x : fun ) : fun = g in f end")) (t-fun (list (box (t-fun (list (box (t-int))) (box (t-int))))) (box (t-fun (list (box (t-int))) (box (t-int))))))

(check-expect (type-of (parse-string "let function f(x:int):int = 1+f(x) in f end"))
              (t-fun (list (box (t-int))) (box (t-int))))
(check-expect (type-of (parse-string "let function f(x:int):int = g(x)+3 function g(x:int):int = if g(x) then f(x)+4 else 26 in g(5) end")) (t-int))

; misc tests
(check-expect (type-of (parse-string "if 1 then nil else nil")) (t-nil))
(check-error (type-of (parse-string "let var x := if 1 then nil else nil in end")) "type error: variable x has value nil but no type")

(check-expect (type-of (parse-string "let type a = {x:int} in if 1 then nil else a{x=1} end"))
              (t-record (list (field 'x (box (t-int))))))
(check-expect (type-of (parse-string "let type a = {x:int} in if 1 then a{x=1} else nil end"))
              (t-record (list (field 'x (box (t-int))))))

(check-error (type-of (parse-string "let type a = {x:int, x:int} in end")) "semantic error: record declaration a contains multiple fields with the same identifier x")
(check-error (type-of (parse-string "let function f(x:int,x:int):int = x in end")) "semantic error: definition of function f contains multiple arguments with the same identifier x")

(check-error (type-of (parse-string "let type a = int type a = string in end")) "semantic error: multiple type declarations for same identifier a in same block")
(check-error (type-of (parse-string "let function f():int=4 function f():int=7 in end")) "semantic error: multiple function declarations for same identifier f in same block")
(check-expect (type-of (parse-string "let type int = string var a:int := \"test\" in a end"))
              (t-string)) ; shadowing original types!
(check-expect (type-of (parse-string "let type a = int in let type int = {x:a,y:a} in int{x=1,y=2} end end"))
              (t-record (list (field 'x (box (t-int)))
                              (field 'y (box (t-int))))))


; recursive types tests
(check-expect (type-of (parse-string "let type intlist = {hd:int, tl:intlist} var x := intlist{hd=1, tl=intlist{hd=2, tl=intlist{hd=3, tl=nil}}} in x end"))
              (local [(define b (box #f))
                      (define r (t-record (list (field 'hd (box (t-int))) (field 'tl b))))]
                (set-box! b r)
                r))
(check-expect (type-of (parse-string "let type intlist = {hd:int, tl:intlist} var x := intlist{hd=1, tl=intlist{hd=2, tl=intlist{hd=3, tl=nil}}} in x end"))
              (local [(define b (box #f))
                      (define r (t-record (list (field 'hd (box (t-int))) (field 'tl b))))]
                (set-box! b r)
                r))
(check-expect (type-of (parse-string "let type tree = {key:int, children:treelist} type treelist = {hd:tree, tl:treelist} var x : tree := nil in x end"))
              (local [(define b1 (box #f))
                      (define t (t-record (list (field 'key (box (t-int)))
                                                (field 'children b1))))
                      (define b2 (box #f))
                      (define tl (t-record (list (field 'hd (box t))
                                                 (field 'tl b2))))]
                (set-box! b1 tl)
                (set-box! b2 tl)
                t))

(check-expect (type-of (parse-string "
let
  type tree = {val:int, body:treearr}
  type treearr = array of tree
  var x : tree := nil
in
  x
end
"))
              (let* [(b (box #f))
                     (tree (t-record (list (field 'val (box (t-int))) (field 'body b))))
                     (treearr (t-array (box tree)))]
                (set-box! b treearr)
                tree))

(check-expect (type-of (parse-string "let type a = b type b = c type c = d type d = int var x : a := 6 in x end")) (t-int))
(check-expect (type-of (parse-string "let type a = array of b type b = c type c = int var x := a[6] of 0 in x end"))
              (t-array (box (t-int))))


(check-error (type-of (parse-string "let type a = b type b = a in end")) "illegal cycle in type declarations, unresolved types were: (a b)")

(check-expect (type-of (parse-string "let type b = int -> intfun type intfun = b -> int in end")) (t-void))
(check-expect (type-of (parse-string "let type a = array of int type alist = {x:a,y:alist} in alist{x=a[10] of 3, y=nil} end"))
              (local [(define x (field 'x (box (t-array (box (t-int))))))            
                      (define b (box #f))
                      (define alist (t-record (list x (field 'y b))))]
                (set-box! b alist)
                alist))
(check-expect (type-of (parse-string "let type a = int -> int type arec = {x:a,y:arec} type fun = arec -> a in end")) (t-void)) 
(check-expect (type-of (parse-string "break")) (t-void))
#;(check-expect (type-of (parse-string "let type a = {x:b,y:c} type b = {x:a,y:c} type c = {x:a,y:b} var z := c{x=nil,y=nil} in z end"))
              (let* [(a->x (box #f))
                    (a->y (box #f))               
                    (b->x (box #f))
                    (b->y (box #f))
                    (c->x (box #f))
                    (c->y (box #f))
                    (ta (t-record (list (field 'x a->x)
                                        (field 'y a->y))))
                    (tb (t-record (list (field 'x b->x)
                                        (field 'y b->y))))
                    (tc (t-record (list (field 'x c->x)
                                        (field 'y c->y))))]
                (set-box! a->x tb)
                (set-box! a->y tc)
                (set-box! b->x ta)
                (set-box! b->y tc)
                (set-box! c->x ta)
                (set-box! c->y tb)
                tc))

(test)

; TODO more test cases
