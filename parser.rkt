#lang racket
(require parser-tools/yacc)
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require test-engine/racket-tests)

(test-silence true)

(struct stdlibfxn (fxn type) #:transparent)

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;  Lexer   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-tokens tiger-tokens 
  (id
   int
   string
   ; comment ; this should not be a token; comments are like whitespace
   ))
(define-empty-tokens tiger-empty-tokens
  (type
   array
   var
   function
   if
   then
   else
   while
   do
   for
   to
   let
   in
   end
   of
   break
   nil
   unit
   
   plus
   minus
   times
   divide
   
   equals
   not-equals
   less-than
   greater-than
   less-or-equal
   greater-or-equal
   
   and
   or

   open-paren
   close-paren
   open-brace
   close-brace
   open-bracket
   close-bracket
   
   dot
   comma
   semicolon
   colon
   arrow
   
   assign
   
   invalid ; for lexical errors
   eof ; special end of file token
   ))


(define lex
  (lexer
   
   [whitespace (lex input-port)]
   [(eof) (token-eof)]
   ;["EOF" (token-eof)]
   
   ; keywords
   ["type" (token-type)]
   ["array" (token-array)]
   ["var" (token-var)]
   ["function" (token-function)]
   ["if" (token-if)]
   ["then" (token-then)]
   ["else" (token-else)]
   ["while" (token-while)]
   ["do" (token-do)]
   ["for" (token-for)]
   ["to" (token-to)]
   ["let" (token-let)]
   ["in" (token-in)]
   ["end" (token-end)]
   ["of" (token-of)]
   ["break" (token-break)]
   ["nil" (token-nil)]
   ["unit" (token-unit)]
   
   ; reserved words (like keywords, but no meaning)
   ["and" (token-invalid)]
   ["or" (token-invalid)]
   ;["not" (token-invalid)] ;uh this is a call in the stdlib.  this causes it to not parse.
   ["goto" (token-invalid)]
   
   ; arithmetic 
   ["+" (token-plus)]
   ["-" (token-minus)]
   ["*" (token-times)]
   ["/" (token-divide)]
   
   ; comparators
   ["=" (token-equals)]
   ["<>" (token-not-equals)]
   ["<" (token-less-than)]
   [">" (token-greater-than)]
   ["<=" (token-less-or-equal)]
   [">=" (token-greater-or-equal)]
   
   ; logical operators
   ; TODO: what about NOT?
   ["&" (token-and)]
   ["|" (token-or)]
   
   ; parens and things
   ["(" (token-open-paren)]
   [")" (token-close-paren)]
   ["{" (token-open-brace)]
   ["}" (token-close-brace)]
   ["[" (token-open-bracket)]
   ["]" (token-close-bracket)]
   
   ; other punctuation
   ["." (token-dot)]
   ["," (token-comma)]
   [";" (token-semicolon)]
   [":" (token-colon)]
   ["->" (token-arrow)]
   
   ; assignment
   [":=" (token-assign)]
   
   ;TODO fix 4a
   ; identifiers
   [(concatenation alphabetic 
                   (repetition 0 +inf.0 
                               (union alphabetic 
                                      (char-range #\0 #\9)
                                      "_")))
    (token-id (string->symbol lexeme))]
   
   ; numbers (integers)
   [(repetition 1 +inf.0 (char-range #\0 #\9))
    (token-int (string->number lexeme))]
   
   ; strings
   ["\"" (token-string (string-lex input-port))]
   
   ; comments are the same as whitespace
   [(:: "/*" (complement (:: any-string "*/" any-string)) "*/")
    (lex input-port)]
    
     
    ))

; call this after eating the open double-quote character
; it will eat up to and including the close double-quote
; and return a list of string to concatenate together
(define (string-lex input-port)
  (define (prepend str)
    (string-append str (string-lex input-port)))
  ((lexer
    [(repetition 1 +inf.0 (union alphabetic
                                 whitespace ; excludes newlines
                                 (intersection punctuation 
                                               (complement "\\")
                                               (complement "\""))
                                 ))
     (prepend lexeme)]
    ["\\\\" (prepend "\\")]
    ["\\n" (prepend "\n")]
    ["\\t" (prepend "\t")]
    ["\\\"" (prepend "\"")]
    ["\"" ""]
    [(eof) (error "eof in string")])
   input-port))
    
    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;  Parser   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; literals
; value is a scheme integer
(struct int-literal (value) #:transparent)
; value is a scheme string
(struct string-literal (value) #:transparent)
(struct nil () #:transparent)

; array and struct creation
; type-id is a type-id struct
; initval is an expression
(struct array-creation (type-id size initval) #:transparent)
; type-id is a type-id struct
; fieldvals is a list of fieldval
(struct record-creation (type-id fieldvals) #:transparent)
; name is a symbol
; val is an expression
(struct fieldval (name val) #:transparent)

; lvalues' intermediate representation
; this is used within the parser to parse lvalues right-recursively
; it is never returned by the parser at the top level
(struct lvalue-record-access (id) #:transparent)
(struct lvalue-array-access (index) #:transparent)


; lvalues - true representation
; id is always a variable
; an id is not an expression
; an lvalue of an id and no suffixes is, however
; see struct lvalue
(struct id (name) #:transparent)
; rec-id is an lvalue
(struct record-access (rec-id field-id offset) #:transparent #:mutable)
; id is an lvalue
(struct array-access (id index) #:transparent)


; declarations
(struct tydec (type-id ty) #:transparent) ; type declaration
(struct vardec (id type-id val) #:transparent) ; type-id can be #f
(struct fundec (id tyfields type-id body) #:transparent) ; type-id can be #f
(struct funcall (fun-id args) #:transparent)

; types
(struct type-id (name) #:transparent)
(struct function-type (dom rng) #:transparent) ; domain and range
(struct record-of (tyfields) #:transparent) ; record type contains list of tyfield
(struct tyfield (id type-id) #:transparent)
(struct array-of (type) #:transparent) ; array type

; assignment
(struct assignment (lvalue val) #:transparent)

; program structure
(struct if-statement (cond then else) #:transparent) ; else is optional
(struct while-statement (cond body) #:transparent)
(struct for-statement (var start end body) #:transparent)
;(struct sequence (expseq) #:transparent)
(struct expseq (exps) #:transparent)

; body is an expression
(struct let-vars (bindings body) #:transparent)
(struct let-types (bindings body) #:transparent)
(struct let-funs (bindings body) #:transparent)


(struct op (op) #:transparent)
(struct binary-op (op arg1 arg2) #:transparent)
(struct unary-op (op arg1) #:transparent)
(struct break () #:transparent)


(define parse
  (parser
   
   (tokens tiger-tokens tiger-empty-tokens)
   (grammar
    (decs [(dec decs) (cons $1 $2)]
          [() empty])
    (dec [(tydec) $1]
         [(vardec) $1]
         [(fundec) $1])
    (tydec [(type id equals ty) (tydec $2 $4)])
    (ty [(id) (type-id $1)]
        ;[(unit) (type-id 'unit)] ;TODO how to handle unit?
        [(open-brace tyfields close-brace) (record-of $2)]
        [(array of id) (array-of (type-id $3))]
        [(ty arrow ty) (function-type (list $1) $3)]
        [(open-paren tylist close-paren arrow ty) (function-type $2 $5)]
        )
    (tylist [() empty]
            [(ty) (list $1)]
            [(ty comma tylist) (cons $1 $3)])
    (tyfields [() empty]
              [(id colon id) (cons (tyfield $1 (type-id $3)) empty)]
              [(id colon id comma tyfields) (cons (tyfield $1 (type-id $3)) $5)])
    (vardec [(var id assign exp) (vardec $2 #f $4)]
            [(var id colon id assign exp) (vardec $2 $4 $6)])
    (fundec [(function id open-paren tyfields close-paren equals exp) (fundec $2 $4 #f $7)]
            [(function id open-paren tyfields close-paren colon id equals exp) (fundec $2 $4 $7 $9)])
    (exp [(literal) $1]
         [(lvalue) $1]
         [(funcall) $1]
         [(arithmetic) $1]
         [(structures) $1]
         [(assignment) $1]
         [(control) $1]
         )
    
    
    (lvalue [(id lvalue-rest) 
             (foldl (lambda (lval-suf sub-lval)
                      ; transform-lvalue into left recursive representation
                      ; lval-suf is the first suffix in the suffix list
                      ; sub-lval is the new lvalue constructed so far
                      (match lval-suf
                        [(lvalue-record-access field-name) (record-access sub-lval field-name #f)]
                        [(lvalue-array-access index) (array-access sub-lval index)]))
                    (id $1)
                    $2)])
    (lvalue-rest [() empty]
                 [(dot id lvalue-rest) (cons (lvalue-record-access $2) $3)]
                 [(open-bracket exp close-bracket lvalue-rest) (cons (lvalue-array-access $2) $4)])
    
    #;(lvalue [(id lvalue-rest)
             ($2 (id $1))])
    
    #;(lvalue-rest [() values]
                 [(dot id lvalue-rest)
                  (lambda (x) ($3 (record-access x $2)))]
                 [(open-bracket exp close-bracket lvalue-rest)
                  (lambda (x) ($4 (array-access x $2)))])
    
    (literal [(int) (int-literal $1)]
             [(string) (string-literal $1)]
             [(nil) (nil)])
    
    (funcall [(exp open-paren funcall-args close-paren) (funcall $1 $3)])
    (funcall-args [() empty]
                  [(exp) (cons $1 empty)]
                  [(exp comma funcall-args) (cons $1 $3)])
    
    (arithmetic [(exp plus exp) (binary-op (op '+) $1 $3)]
                [(exp minus exp) #;(prec plus) (binary-op (op '-) $1 $3)]
                [(exp times exp) (binary-op (op '*) $1 $3)]
                [(exp divide exp) (binary-op (op '/) $1 $3)]
                [(exp equals exp) (binary-op (op '=) $1 $3)]
                [(exp not-equals exp) (binary-op (op '<>) $1 $3)]
                [(exp less-than exp) (binary-op (op '<) $1 $3)]
                [(exp less-or-equal exp) (binary-op (op '<=) $1 $3)]
                [(exp greater-than exp) (binary-op (op '>) $1 $3)]
                [(exp greater-or-equal exp) (binary-op (op '>=) $1 $3)]
                [(exp and exp) (binary-op (op '&) $1 $3)]
                [(exp or exp) (binary-op (op 'or) $1 $3)]
                [(minus exp) (prec open-paren) (unary-op (op '-) $2)]
                )
    
    (structures [(record-creation) $1]
                [(array-creation) $1])
    (record-creation [(id open-brace record-creation-args close-brace) (record-creation (type-id $1) $3)])
    (record-creation-args [() empty] 
                          [(id equals exp) (cons (fieldval $1 $3) empty)]
                          [(id equals exp comma record-creation-args) (cons (fieldval $1 $3) $5)])
    (array-creation [(id open-bracket exp close-bracket of exp) (array-creation (type-id $1) $3 $6)])
    
    (assignment [(lvalue assign exp) (assignment $1 $3)]) 
;    
    (control [(if-nonterminal) $1]
             [(while-nonterminal) $1]
             [(for-nonterminal) $1]
             [(break) (break)]
             [(let-nonterminal) $1]
             [(sequencing) $1])
    
    (if-nonterminal [(if exp then exp else exp) (if-statement $2 $4 $6)]
                    [(if exp then exp) (if-statement $2 $4 (expseq empty))]
                    
                    )
    
    (while-nonterminal [(while exp do exp) (while-statement $2 $4)])
    (let-nonterminal [(let decs in expseq end) 
                      ; transform-let into 3 different types of lets
                      (foldr (lambda (dec new-let) 
                               (cond 
                                 [(and (tydec? dec)
                                       (let-types? new-let))   (let-types (cons dec (let-types-bindings new-let))
                                                                          (let-types-body new-let))]
                                 [(and (fundec? dec)
                                       (let-funs? new-let))   (let-funs (cons dec (let-funs-bindings new-let))
                                                                        (let-funs-body new-let))]
                                 [(and (vardec? dec)
                                       (let-vars? new-let))   (let-vars (cons dec (let-vars-bindings new-let))
                                                                        (let-vars-body new-let))]
                                 [(tydec? dec)   (let-types (list dec)
                                                            new-let)]
                                 [(fundec? dec)  (let-funs (list dec)
                                                           new-let)]
                                 [(vardec? dec)  (let-vars (list dec)
                                                           new-let)]))
                             (expseq $4)
                             $2)
                      
                    ;  (let-statement $2 (expseq $4))
                      
                      
                      ])
    (for-nonterminal [(for id assign exp to exp do exp) (for-statement $2 $4 $6 $8)])
    (expseq [() empty]
            [(exp) (cons $1 empty)]
            [(exp semicolon expseq) (cons $1 $3)]
            ; TODO: LISTEN TO ROAN MORE OFTEN
            )
    (sequencing [(open-paren expseq close-paren) (expseq $2)])
    )
   
   ; lower on this list means binds tighter
   (precs (nonassoc do)
          (nonassoc assign)
          (right then else)
          (nonassoc of)
          (right arrow)
          
          (left or)
          (left and)
          (nonassoc equals not-equals greater-or-equal less-or-equal greater-than less-than)
          (left plus minus)
          (left divide times)
          #;(left minus)
          ; parens correspond to function application 
          (nonassoc open-paren open-bracket close-paren close-bracket open-brace close-brace)
           ;   (nonassoc comma semicolon colon dot)
         ; ()
          
;          (nonassoc id) ;this removed 1 shift-reduce conflict
;          (nonassoc var type array function break if while for to let in end)
          )
          
   (start exp)
   (end eof)
   (error (λ (valid? token value) 
            (error 
             (format "parse error at ~a: with value:~a was token valid?:~a"
                     token 
                     value 
                     valid?))))
   
   ))


(define (wrapstdlib ast)
  (let-funs
   (list
    (fundec 'print (list (tyfield 's (type-id 'string))) #f (stdlibfxn 'print 'void))
    ;(fundec 'flush (list) #f (stdlibfxn 'flush 'void))
    (fundec 'getchar (list) 'string (stdlibfxn 'getchar 'str))
    (fundec 'ord (list (tyfield 's (type-id 'string))) 'int (stdlibfxn 'ord 'int))
    (fundec 'chr (list (tyfield 'i (type-id 'int))) 'string (stdlibfxn 'chr 'str))
    (fundec 'size (list (tyfield 's (type-id 'string))) 'int (stdlibfxn 'size 'int))
    (fundec 'substring (list (tyfield 's (type-id 'string))
                             (tyfield 'first (type-id 'int))
                             (tyfield 'n (type-id 'int))) 'string (stdlibfxn 'substring 'str))
    (fundec 'concat (list (tyfield 's1 (type-id 'string)) (tyfield 's2 (type-id 'string))) 'string (stdlibfxn 'concat 'str))
    (fundec 'not (list (tyfield 'i (type-id 'int))) 'int (stdlibfxn 'not 'int))
    (fundec 'exit (list (tyfield 'i (type-id 'int))) #f (stdlibfxn 'exit 'void))
    (fundec 'print_int (list (tyfield 'i (type-id 'int))) #f (stdlibfxn 'print_int 'void))
    )
   (expseq (list ast))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;  Helpers and tests  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (parse-stdin)
  (parse (λ () (lex (current-input-port)))))

(define (parse-string str)
  (let [(port (open-input-string str))]
    (parse (λ () (lex port)))))

(define (lex-string str)
  (let [(port (open-input-string str))]
    (λ () (lex port))))
(define (parse-file file)
  (let [(port (open-input-file file))]
    (parse (λ () (lex port)))))


(check-expect (parse-string "4") (int-literal 4))
(check-expect (parse-string "\"zoomba\"") (string-literal "zoomba"))
(check-expect (parse-string "\"he said \\\"hi\\\", right?\"") (string-literal "he said \"hi\", right?"))
(check-expect (parse-string "nil") (nil))
(check-expect (parse-string "some_identifier") (id 'some_identifier))

;;dangling else testing
(check-expect (parse-string "if 4 then 4")
              (if-statement (int-literal 4) (int-literal 4) (expseq empty)))
(check-expect (parse-string "if 5 then 5 else 5")
              (if-statement (int-literal 5) (int-literal 5) (int-literal 5)))
(check-expect (parse-string "if if 1 then 1 then if 2 then 2 else if 3 then 3")
              (if-statement (if-statement (int-literal 1) (int-literal 1) (expseq empty)) (if-statement (int-literal 2) (int-literal 2) (if-statement (int-literal 3) (int-literal 3) (expseq empty))) (expseq empty) ))
(check-expect (parse-string "if 4 then if 5 then 5 else 5")
              (if-statement (int-literal 4) (if-statement (int-literal 5) (int-literal 5) (int-literal 5)) (expseq empty)))
(check-expect (parse-string "if 4 then (if 5 then 5) else 4")
              (if-statement (int-literal 4) (expseq (list (if-statement (int-literal 5) (int-literal 5) (expseq empty)))) (int-literal 4)))

;dangling do testing
(check-expect (parse-string "while 4 do 4")
              (while-statement (int-literal 4) (int-literal 4)))
(check-expect (parse-string "while 5 do while 6 do 6")
              (while-statement (int-literal 5) (while-statement (int-literal 6) (int-literal 6))))
(check-expect (parse-string "while 5 do for pig := 7 to 7 do 7")
              (while-statement (int-literal 5) (for-statement 'pig (int-literal 7) (int-literal 7) (int-literal 7))))
(check-expect (parse-string "for pig := 8 to 8 do while 6 do 6")
              (for-statement 'pig (int-literal 8) (int-literal 8) (while-statement (int-literal 6) (int-literal 6))))
(check-expect (parse-string "for apple := 10 to 10 do for mike := 20 to 20 do 20")
              (for-statement 'apple (int-literal 10) (int-literal 10) (for-statement 'mike (int-literal 20) (int-literal 20) (int-literal 20))))

(check-expect (parse-string "for apple := 36 to for mike := 11 to 11 do 11 do 36")
              (for-statement 'apple (int-literal 36) (for-statement 'mike (int-literal 11) (int-literal 11) (int-literal 11)) (int-literal 36)))

#;(check-expect (parse-string "a.b.c.d.zoomba[pizza].lorg[a.b]")
              (array-access
               (record-access (array-access (record-access (record-access (record-access (record-access (id 'a) 'b) 'c) 'd) 'zoomba) (id 'pizza)) 'lorg)
               (record-access (id 'a) 'b)))
#;(check-expect (parse-string "a.b.c.d.zoomba[pizza].lorg[a.b] := 7")
              (assignment
               (array-access
                (record-access (array-access (record-access (record-access (record-access (record-access (id 'a) 'b) 'c) 'd) 'zoomba) (id 'pizza)) 'lorg)
                (record-access (id 'a) 'b))
               (int-literal 7)))

;lvalue testing including array accesses and declarations
#;(check-expect (parse-string "drugs.f")
              (record-access (id 'drugs) 'f))
(check-expect (parse-string "bears[philip] of 7")
              (array-creation (type-id 'bears) (id 'philip) (int-literal 7)))
(check-expect (parse-string "int[philip] of 7")
              (array-creation (type-id 'int) (id 'philip) (int-literal 7)))
(check-expect (parse-string "a[b]")
              (array-access (id 'a) (id 'b)))

;; let in sequence
(check-expect (parse-string "let in 1 end") (expseq (list (int-literal 1))))
(check-expect (parse-string "let in 1; 2 end") (expseq (list (int-literal 1) (int-literal 2))))
(check-expect (parse-string "let in (1; 2) end") (expseq (list (expseq (list (int-literal 1) (int-literal 2))))))

;precedence testing
(check-expect (parse-string "4/5*6")
              (binary-op (op '*) (binary-op (op '/) (int-literal 4) (int-literal 5)) (int-literal 6)))
(check-expect (parse-string "4*5/6")
              (binary-op (op '/) (binary-op (op '*) (int-literal 4) (int-literal 5)) (int-literal 6)))
(check-expect (parse-string "1+2*3")
              (binary-op (op '+) (int-literal 1) (binary-op (op '*) (int-literal 2) (int-literal 3))))
(check-expect (parse-string "1*4+5")
              (binary-op (op '+) (binary-op (op '*) (int-literal 1) (int-literal 4)) (int-literal 5)))

;type declaration tests
(check-expect (parse-string "let type a = int in end")
              (let-types (list (tydec 'a (type-id 'int))) (expseq empty)))
(check-expect (parse-string "let type b = array of charlie in end")
              (let-types (list (tydec 'b (array-of (type-id 'charlie)))) (expseq empty)))
(check-expect (parse-string "let type c = {} in end")
              (let-types (list (tydec 'c (record-of empty))) (expseq empty)))
(check-expect (parse-string "let type d = { beer : int } in end")
              (let-types (list (tydec 'd (record-of (list (tyfield 'beer (type-id 'int)))))) (expseq empty)))
(check-expect (parse-string "let type e = { chocolate : int, mufflepuff : stormclouds, wozzar : string} in end")
              (let-types (list (tydec 'e (record-of (list (tyfield 'chocolate (type-id 'int))
                                                          (tyfield 'mufflepuff (type-id 'stormclouds))
                                                          (tyfield 'wozzar (type-id 'string)))))) 
                         (expseq empty)))

; breaking up let-statement tests
(check-expect (parse-string "let type a = horse type b = radish in end")
              (let-types (list (tydec 'a (type-id 'horse))
                               (tydec 'b (type-id 'radish)))
                         (expseq empty)))
(check-expect (parse-string "let var x := 12 var y := 47 in end")
              (let-vars (list (vardec 'x false (int-literal 12))
                               (vardec 'y false (int-literal 47)))
                         (expseq empty)))
(check-expect (parse-string "let function f() : int = 1 function g() : string = \"zebra\" in end")
              (let-funs (list (fundec 'f empty 'int (int-literal 1))
                               (fundec 'g empty 'string (string-literal "zebra")))
                         (expseq empty)))


; function types
(check-expect (parse-string "let type f = {} -> {} in end")
              (let-types (list 
                          (tydec 'f (function-type (list (record-of empty))
                                                       (record-of empty))))
                         (expseq empty)))
(check-expect (parse-string "let type f = int -> int -> int in end")
              (let-types (list 
                          (tydec 'f (function-type (list (type-id 'int))
                                                   (function-type (list (type-id 'int))
                                                                  (type-id 'int)))))
                         (expseq empty)))
(check-expect (parse-string "let type f = ( int, int ) -> int  in end")
              (let-types (list (tydec 'f (function-type (list (type-id 'int) (type-id 'int)) (type-id 'int)))) (expseq empty)))
(check-expect (parse-string "let type f = ( int -> int ) -> int  in end")
              (let-types (list 
                          (tydec 'f (function-type (list (function-type (list (type-id 'int)) (type-id 'int)))
                                                   (type-id 'int)))) 
                         (expseq empty)))
(check-expect (parse-string "7 /*****asdf***/ + /**omgwtfbbg*/ 2")
              (binary-op (op '+) (int-literal 7) (int-literal 2)))

; file io
(check-expect (parse-file "./tests/four.tig")
              (int-literal 4))
;; just check that it parses, don't examine the tree
(check-expect (begin (parse-file "./tests/queens.tig") 'great)
              'great)

;; canonicalization tests
;no
;(check-expect (begin (canonicalize (parse-file "./tests/queens.tig")) (call/cc (λ (k) {k (k "pizza")}))) "pizza")
(test)