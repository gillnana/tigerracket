#lang racket

(require "codegen.rkt")
(require "intermediate.rkt")
(require "parser.rkt")
(require "typecheck.rkt")


(ln ".globl main")
(ln ".text")

(gen-code 
 (gen-prog 
  (wrapstdlib 
   
   (let [(ast (wrapstdlib (parse-stdin)))]
     ;(type-of ast) ; TODO: fix the wtf errors
     ast)
      
   
  )
 )
)