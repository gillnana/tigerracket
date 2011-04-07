#lang racket

#;(require rnrs/io/ports-6)

(define cur-prog (open-output-string))

#;(define (gen-code ir)
  (match ir
    [(move-ins src dest
      
      )]
  ))
    
(define (ln text)
  (display (string-append text "\n") cur-prog))
  

#;(define (stack-allocate item)
  )

(check-expect (begin (ln "wossar") (ln "foop") (get-output-string cur-prog)) "wossar\nfoop\n")