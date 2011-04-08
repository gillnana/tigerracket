#lang racket

#;(require rnrs/io/ports-6)
(require test-engine/racket-tests)
(require "intermediate.rkt")

(define cur-prog (open-output-string))

(define (gen-code ir temps)
  (match ir
    [(fxn-block label inslist)
     (let [(ts (remove-duplicates (apply append (map get-locs inslist))))]
       (begin
         (ln (labelize label))
         (ln (stack-setup ts))
         (gen-code inslist ts)
         (ln (stack-teardown ts))
         ))]
    
    #;[(move-ins src dest)
       ()
       ]
    
    ))
  
(define (index-of item ls (count 0))
  (if (empty? ls)
      (error "item not in list") ; TODO fix errmesg
      (if (equal? item (first ls))
          count
          (index-of item (rest ls) (+ count 1))
          )
      ))

(define (get-offset temp temp-list)
  (* 4 (+ 1 (index-of temp temp-list))))

(define (stack-setup temps)
  (push "ra")
  (ln (string-append "sub $sp, $sp, " (number->string (* 4 (length temps)))))
  )

(define (stack-teardown temps)
  (pop "ra")
  (ln (string-append "add $sp, $sp, " (number->string (* 4 (length temps)))))
  )

; annoying function that extracts a list of locations from an instruction
(define (get-locs ins)
  (match ins
    [(move-ins src dest)
     (map get-loc (filter location? (list src dest)))
     ]
    [(lim-ins imm dest)
     (map get-loc (filter location? (list imm dest)))
     ]
    [(binary-ins op src1 src2 dest)
     (map get-loc (filter location? (list op src1 src2 dest)))
     ]
    [(unary-ins op src dest)
     (map get-loc (filter location? (list op src dest)))
     ]
    [(uncond-jump-ins dest)
     (map get-loc (filter location? (list dest)))
     ]
     [(cond-jump-ins src dest)
      (filter location? (list src dest))
      ]
     [(cond-jump-relop-ins op src1 src2 dest)
      (filter location? (list src dest))
      ]
     [(push-ins src)
      (filter location? (list src dest))
      ]
    ;todo some instructions remain
  ))

(define (get-loc loc) loc)

  
(define (push name)
  (string-append "sub $sp, $sp, 4"
                 "\n"
                 "sw $" name ", 4($sp)"
                 ))

(define (pop name)
  (string-append "lw $" name ", 4($sp)"
                 "\n"
                 "add $sp, $sp, 4"
                 ))

(define (appendln . lines)
  (if (empty? lines)
      ""
      (apply string-append
             (cons (first lines)
                   (map (λ (line)
                          (string-append "\n" line))
                        (rest lines))))))

(define (ln text)
  (display (string-append text "\n") cur-prog))

(define (labelize label)
  (string-append (symbol->string label) ":"))
  

#;(define (stack-allocate item)
  ;first put the item in t0 - how?
  ; do that
  ;then move the contents of t0 onto the stack
  (ln "sub 4, $sp")
  (ln "sw $t0, 4($sp)")
  
  )

(check-expect (begin (ln "wossar") (ln "foop") (get-output-string cur-prog)) "wossar\nfoop\n")