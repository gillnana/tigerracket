#lang racket
(require parser-tools/lex)

(define-empty-tokens booleans [true false])
(define-tokens names [id])

(define (lex in)
  ((lexer
    [whitespace (lex in)]
    ["true" (token-true)]
    ["false" (token-false)]
    ;["\n" (eof)]
    [(repetition 1 +inf.0 (char-complement whitespace)) (token-id lexeme)]
    ) in))

(define (vowels in)
  ((lexer

    [(union "a" "e" "i" "o" "u") 'vowel]
    ["y" 'y]
    [(char-complement (union "a" "e" "i" "o" "u" "y")) lexeme]
    
    ) in))