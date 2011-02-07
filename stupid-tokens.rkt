#lang racket
(require parser-tools/lex)

(define-empty-tokens stupid [a ss b])

(define (lex in)
  ((lexer
    [whitespace (lex in)]
    ["a" (token-a)]
    ["b" (token-b)]) in))
