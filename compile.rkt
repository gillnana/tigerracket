
#lang racket

(require "codegen.rkt")
(require "canonicalize.rkt")
(require "intermediate.rkt")
(require "parser.rkt")
(require "semantic-loop.rkt")
(require "typecheck.rkt")



(ln ".globl main")
(ln ".text")

(gen-code 
 (gen-prog
  (canonicalize
   (wrapstdlib 
    
    (let [(ast (wrapstdlib (parse-stdin)))]
      (type-of ast)
      (verify-loop-semantics ast)
      ast)
    
   )
  )
 )
)