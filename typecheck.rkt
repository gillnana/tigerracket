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
    [(binary-op (op '=) arg1 arg2)
     (if (equal? (type-of-env arg1 te ve)
                 (type-of-env arg2 te ve))
         (t-bool)
         (error ("arguments for equality comparison must be of the same type")))]
    [(binary-op (op (and operator (or '+ '- '* '/))) arg1 arg2)
     (cond
       [(not (and (equal? (type-of-env arg1 te ve) (t-int))
                  (equal? (type-of-env arg2 te ve) (t-int))))
        (error (format "args to ~a must be ints!" operator))]
       [else (t-int)])]
    [(binary-op (op (and comparator (or '<> '< '> '<= '>=))) arg1 arg2)
     (cond
       [(not (and (equal? (type-of-env arg1 te ve) (t-int))
                  (equal? (type-of-env arg2 te ve) (t-int))))
        (error (format "args to ~a must be ints!" comparator))]
       [else (t-bool)])]
    [(binary-op (op (and operator (or '& '\|))) arg1 arg2)
     (cond
       [(not (and (equal? (type-of-env arg1 te ve) (t-bool))
                  (equal? (type-of-env arg2 te ve) (t-bool))))
        (error (format "args to ~a must be ints!" operator))]
       [else (t-bool)])]
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
              (t-bool))
(check-expect (type-of (parse-string "4=7")) 
              (t-bool))
(check-expect (type-of (parse-string "\"zoomba\"=\"pizza\"")) 
              (t-bool))
(test)