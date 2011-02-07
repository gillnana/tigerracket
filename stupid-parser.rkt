#lang racket
(require parser-tools/yacc)
(require parser-tools/lex)

(define-empty-tokens ops (plus minus times divide))
(define-empty-tokens parens (open close))
(define-tokens nums (num))
(define-empty-tokens ending (EOF))


(define lex
  (lexer
   [whitespace (lex input-port)]
   [(repetition 1 +inf.0 (char-range #\0 #\9)) (token-num (string->number lexeme))]
   ["+" (token-plus)]
   [(eof) (token-EOF)]
   ))

(define parse
  (parser
   
   (tokens ops parens nums ending)
   
   (grammar
    (expr [(numexp) $1]
          [(sumexp) $1])
    (sumexp [(num plus num) (list $1 $3)])
    (numexp [(num) $1])
    
    
    )
   
   
   (start expr)
   (end EOF)
   (error (λ (valid? token value) 
            (error 
             (format "parse error at ~a: with value:~a was token valid?:~a"
                     token 
                     value 
                     valid?))))
   
   ))