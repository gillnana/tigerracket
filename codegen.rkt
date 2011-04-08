#lang racket

#;(require rnrs/io/ports-6)
(require test-engine/racket-tests)
(require "intermediate.rkt")
(require "parser.rkt")

(provide (all-defined-out))

; TODO: global stateful variables are evil and we should fix this
; possibly using with-output-to
; or something 
; -dpercy
;(define cur-prog (open-output-string))

(define (gen-code ir (temps empty))
  (match ir
    [(fxn-block label inslist)
     (let [(ts (remove-duplicates (apply append (map get-locs inslist))))]
       (begin
         (ln (labelize label))
         (ln (stack-setup ts))
         (map (λ (ins) (gen-code ins ts)) inslist)
         (ln (stack-teardown ts))
         (ln "jr $ra")
         ))]
    ; TODO: some of these cases may be overly specific; we need to actually do instruction selection
    [(lim-ins imm dest)
     (when (not (eq? dest 'ans)) ; hacky! prevents lim into the ans, but instruction selection should actually handle this...
       (begin
         ; hacky! we have to do real instruction selection instead of this super-specific bullshit I made!  --dpercy
         (ln (if (number? imm) "li" "la") " $t0, " imm)
         (ln "sw $t0, " (get-offset dest temps) "($sp)")
         )
       )
     ]
    
    [(funcall-ins labloc args dest)
     (begin
       (when (> (length args) 4) 
         (error "TODO more than 4 arguments to function not yet supported"))
       ; load args
       (map (λ (arg num)
              (ln "lw $a" num ", " (get-offset arg temps) "($sp)")
              )
            args
            (build-list (length args) values))
       ; jump!
       (ln "lw $t0, " (get-offset labloc temps) "($sp)")
       (ln "jalr $t0")
       ; retrieve return val
       (when (not (eq? dest 'ans))
         (ln "sw $ra, " (get-offset dest temps) "($sp)")
         )
       )]
     
    #;[(move-ins src dest)
     (ln "move " (get-offset dest temps) "($sp), " (get-offset src temps) "($sp)")
     ]
    
    
    
    ))
  
(define (index-of item ls (count 0))
  (if (empty? ls)
      (error (format "item ~a not in list ~a" item ls))
      (if (equal? item (first ls))
          count
          (index-of item (rest ls) (+ count 1))
          )
      ))


(define (get-offset temp temp-list)
  (* 4 (+ 1 (index-of temp temp-list))))

(define (stack-setup temps)
  (appendln
   (push "ra")
   (string-append "sub $sp, $sp, " (number->string (* 4 (length temps))))
   ))

(define (stack-teardown temps)
  (appendln
   (string-append "add $sp, $sp, " (number->string (* 4 (length temps))))
   (pop "ra")
   ))

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
      (filter location? (list src1 src2 dest))
      ]
     [(push-ins src)
      (filter location? (list src))
      ]
    [(funcall-ins labloc params dest)
     (filter location? (list* labloc dest params))
     ]
    ;todo some instructions remain
  ))

(define (get-loc loc) loc) ;silly, but was easier than deleting all those calls to get-loc. 

  
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
  (apply string-append (add-between lines "\n")))


; ln calls display on each of its arguments,
; then displays a new line
(define (ln . textlist)
  (map (λ (s) (display s)) textlist)
  (displayln ""))

(define (labelize label)
  (string-append (symbol->string label) ":"))
  

#;(define (stack-allocate item)
  ;first put the item in t0 - how?
  ; do that
  ;then move the contents of t0 onto the stack
  (ln "sub 4, $sp")
  (ln "sw $t0, 4($sp)")
  
  )

(check-expect (begin (ln "wossar") (ln "foop") (get-output-string)) "wossar\nfoop\n")