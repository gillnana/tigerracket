#lang racket
(require parser-tools/yacc)
(require parser-tools/lex)

; i added this comment
(require test-engine/racket-tests)

(define-empty-tokens ops (plus minus times divide))
(define-empty-tokens parens (open close))
(define-tokens nums (num))
(define-empty-tokens ending (eof))


(define lex
  (lexer
   [whitespace (lex input-port)]
   [(repetition 1 +inf.0 (char-range #\0 #\9)) (token-num (string->number lexeme))]
   ["+" (token-plus)]
   ["*" (token-times)]
   [(eof) (token-eof)]
   ))

(struct sum (a b) #:transparent)
(struct prod (a b) #:transparent)

(define parse
  (parser
   
   (tokens ops parens nums ending)
   
   
   (grammar
    
    (expr [(sum) $1]
          [(product) $1]
          [(num) $1])
    (sum [(expr plus expr) (sum $1 $3)])
    (product [(expr times expr) (prod $1 $3)])
    
    )
   
   (precs (left times) (left plus))
   
   (start expr)
   (end eof)
   (error (λ (valid? token value) 
            (error 
             (format "parse error at ~a: with value:~a was token valid?:~a"
                     token 
                     value 
                     valid?))))
   
   ))

(define (run)
  (parse (λ () (lex (current-input-port)))))

(define (parse-string str)
  (let [(port (open-input-string str))]
    (parse (λ () (lex port)))))

(check-expect (parse-string "4 * 3 + 2") (sum (prod 4 3) 2))
(test)
