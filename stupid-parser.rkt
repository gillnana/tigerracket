#lang racket
(require parser-tools/yacc)
(require parser-tools/lex)

(define-empty-tokens ops (+ - * /))
(define-empty-tokens parens (open close))
(define-tokens nums (num))

(define parse
  (parser
   
   (tokens ops parens nums)
   
   (grammar
    (numexp ((num))))
   
   (error (λ () (error "OH NO")))
   
   ))