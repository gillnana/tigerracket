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
(struct t-bool () #:transparent)
(struct t-void () ) ; not transparent
(struct t-nil () #:transparent)

(struct t-fun (args result) #:transparent) ; args is list of types
(struct t-array (elem-type) #:transparent)
(struct t-record (fields) #:transparent)
(struct field (name ty) #:transparent)

(define (type-lookup type-symbol type-env) 
  (cond 
    [(equal? type-symbol 'int) (t-int)]
    [(equal? type-symbol 'string) (t-string)]
    [else (or (ormap (lambda (binding) (if (equal? (type-binding-id binding) type-symbol)
                               (type-binding-ty binding)
                               false))
                     type-env)
              (error (format "unbound type ~a" type-symbol)))]))
    

(define (type-of expr)
  (type-of-env expr empty empty))

(define (type-of-env expr te ve) ; type-env and var-env
  (match expr
    [(int-literal a) (t-int)]
    [(string-literal a) (t-string)]
    [(nil) (t-nil)]
    
    [(array-creation (type-id type) size initval)
     (cond
       [(not (equal? (type-of-env size te ve) (t-int))) (error "type of array size not int")]
       [(not (equal? type (type-of-env initval te ve))) (error "type of array not same as initial value")]
       [else (t-array (type-lookup type te))])]
    
    [(binary-op (op sym) arg1 arg2)
     (cond [(symbol=? sym '=) (if (equal? (type-of-env arg1 te ve) (type-of-env arg2 te ve))
                                  (t-int)
                                  (error "arguments for equality comparison must be of same type"))]
           [(and (equal? (type-of-env arg1 te ve) (t-int))
                      (equal? (type-of-env arg2 te ve) (t-int)))
            (t-int)]
           [else (error (format "args for operator ~a must be integers" sym))])]
    
    [(unary-op (op '-) arg1)
     (if (not (equal? (type-of-env arg1 te ve) (t-int)))
         (error "arg to unary minus must be int")
         (t-int))]
    
    [(sequence (list)) (t-void)]
    [(sequence (list a ... b))
     (type-of-env b te ve)]
    
    [(if-statement c t (list))
     (cond [(not (equal? (type-of-env c te ve) (t-int))) (error "condition of if statement must have boolean value")]
           [(not (equal? (type-of-env t te ve) (t-void))) (error "then branch of an if statement must have no value")]
           [else (t-void)])]
    [(if-statement c t e)
     (let ([type-of-t (type-of-env t te ve)]) 
       (cond [(not (equal? (type-of-env c te ve) (t-int))) (error "condition of if statement must have boolean value")]
             [(not (equal? type-of-t (type-of-env e te ve))) (error "then and else branches of if statement must have same type")]
             [else type-of-t]))]
              
    ;[(record-creation (type-id type) fields)
    ;(if (check-record-fields fields (type-lookup type te))
    ))



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

(test)