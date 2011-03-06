#lang racket

(require "parser.rkt")
(require test-engine/racket-tests)

; daggify ast -> ast
; returns a new ast with shared nodes if they appear in a simple expression
(define (daggify ast)
  (match ast
    [(binary- ;TODO make some changes