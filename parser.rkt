#lang racket
(require parser-tools/yacc)
(require parser-tools/lex)

(require (prefix-in : parser-tools/lex-sre))

(require test-engine/racket-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;  Lexer   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-tokens tiger-tokens 
  (id
   int
   string
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
   
   ; reserved words (like keywords, but no meaning)
   ["and" (token-invalid)]
   ["or" (token-invalid)]
   ["not" (token-invalid)]
   ["goto" (token-invalid)]
   
   ; arithmetic 
   ; TODO: precedence
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
   
   ; assignment
   [":=" (token-assign)]
   
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
   [(:: "\"" (complement (:: any-string "\"" any-string)) "\"")
    (token-string
     (let [(l (string-length lexeme))]
       (substring lexeme 1 (- l 1))))]
     
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;  Parser   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct tydec (type-id ty) #:transparent) ; type declaration
(struct type-id (name) #:transparent)
(struct id (name) #:transparent)
(struct record-of (tyfields) #:transparent) ; record type contains list of tyfield
(struct array-of (type) #:transparent) ; array type
(struct tyfield (id type-id) #:transparent)
(struct vardec (id type-id val) #:transparent) ; type-id can be #f
(struct fundec (id tyfields type-id body) #:transparent) ; type-id can be #f
(struct lvalue (id suffixes) #:transparent)
(struct lvalue-record-access (id) #:transparent)
(struct lvalue-array-access (index) #:transparent)
(struct funcall (fun-id args) #:transparent)
(struct array-creation (type-id size initval) #:transparent)
(struct record-creation (type-id fieldvals) #:transparent)
(struct fieldval (name val) #:transparent)
(struct assignment (lvalue val) #:transparent)
(struct if-statement (cond then else) #:transparent) ; else is optional
(struct while-statement (cond body) #:transparent)
(struct for-statement (var start end body) #:transparent)
; TODO: careful of sequence vs list-of exp
(struct let-statement (bindings expseq) #:transparent)
(struct sequence (exps) #:transparent)

(struct nil () #:transparent)
(struct op (op) #:transparent)
(struct binary-op (op arg1 arg2) #:transparent)
(struct unary-op (op arg1) #:transparent)
(struct break () #:transparent)

(define parse
  (parser
   
   (tokens tiger-tokens tiger-empty-tokens)
   (grammar
;    (decs [(dec decs) (cons $1 $2)]
;          [() empty])
;    (dec [(tydec) $1]
;         [(vardec) $1]
;         [(fundec) $1])
;    (tydec [(type id equals ty) (tydec $2 $4)])
;    (ty [(id) (type-id $1)]
;        [(open-brace tyfields close-brace) $2]
;        [(array of id) (array-of $3)])
;    (tyfields [() empty]
;              [(id colon id) (cons (tyfield $1 $3) empty)]
;              [(id colon id comma tyfields) (cons (tyfield $1 $3) $5)])
;    (vardec [(var id assign exp) (vardec $2 #f $4)]
;            [(var id colon id assign exp) (vardec $2 $4 $6)])
;    (fundec [(function id open-paren tyfields close-paren equals exp) (fundec $2 $4 #f $7)]
;            [(function id open-paren tyfields close-paren colon id equals exp) (fundec $2 $4 $7 $9)])
    (exp [(literal) $1]
;         [(lvalue) $1]
;         [(funcall) $1]
          [(arithmetic) $1]
;         [(structures) $1]
;         [(assignment) $1]
;         [(control) $1]
         )
    
    
;    (lvalue [(id lvalue-rest) (lvalue (id $1) $2)])
;    (lvalue-rest [() empty]
;                 [(dot id lvalue-rest) (cons (lvalue-record-access $2) $3)]
;                 [(open-bracket exp close-bracket lvalue-rest) (cons (lvalue-array-access $2) $4)])
    
    (literal [(int) $1]
             [(string) $1]
             [(nil) (nil)])
    
;    (funcall [(exp open-paren funcall-args close-paren) (funcall $1 $3)])
;    (funcall-args [() empty]
;                  [(exp) (cons $1 empty)]
;                  [(exp comma funcall-args) (cons $1 $3)])
;    
    (arithmetic [(exp plus exp) (binary-op (op '+) $1 $3)]
                [(exp minus exp) (prec plus) (binary-op (op '-) $1 $3)]
                [(exp times exp) (binary-op (op '*) $1 $3)]
                [(exp divide exp) (binary-op (op '/) $1 $3)]
                [(exp equals exp) (binary-op (op '=) $1 $3)]
                [(exp not-equals exp) (binary-op (op '<>) $1 $3)]
                [(exp less-than exp) (binary-op (op '<) $1 $3)]
                [(exp less-or-equal exp) (binary-op (op '<=) $1 $3)]
                [(exp greater-than exp) (binary-op (op '>) $1 $3)]
                [(exp greater-or-equal exp) (binary-op (op '>=) $1 $3)]
                [(exp and exp) (binary-op (op '&) $1 $3)]
                [(exp or exp) (binary-op (op '\|) $1 $3)]
                [(minus exp) (unary-op (op '-) $2)]
                )
    
;    (structures [(record-creation) $1]
;                [(array-creation) $1])
;    (record-creation [(id open-brace record-creation-args close-brace) (record-creation (type-id $1) $3)])
;    (record-creation-args [() empty] 
;                          [(id equals exp) (cons (fieldval $1 $3) empty)]
;                          [(id equals exp comma record-creation-args) (cons (fieldval $1 $3) $5)])
;    ;TODO array-creation generates precedence problems because this also looks like an array access
;    (array-creation [(id open-bracket exp close-bracket of exp) (array-creation $1 $3 $6)])
;    
;    (assignment [(lvalue assign exp) (assignment $1 $3)]) 
;    
;    (control [(if-nonterminal) $1]
;             [(while-nonterminal) $1]
;             [(for-nonterminal) $1]
;             [(break) (break)]
;             [(let-nonterminal) $1]
;             [(sequencing) $1])
;    
;    (if-nonterminal [(if exp then exp) (if-statement $2 $4 empty)]
;                    [(if exp then exp else exp) (if-statement $2 $4 $6)])
;    (while-nonterminal [(while exp do exp) (while-statement $2 $4)])
;    (let-nonterminal [(let decs in expseq end) (let-statement $2 $4)])
;    (for-nonterminal [(for id assign exp to exp do exp) (for-statement $2 $4 $6 $8)])
;    (expseq [() empty]
;            [(exp) (cons $1 empty)]
;            [(exp semicolon expseq) (cons $1 $3)])
;    (sequencing [(open-paren expseq close-paren) (sequence $2)])
    )
   
   (precs (left or)
          (left and)
          (left plus minus)
          (left divide times)
;          (right then else)
;          (nonassoc assign)
;          
;          ; as a rule, i don't understand how the follow precedences remove shift-reductions or if these are all correct.
;          
          (nonassoc equals not-equals greater-or-equal less-or-equal greater-than less-than)
;          (nonassoc of) ;this removed 12 shift-reduce conflicts.
;          (nonassoc open-paren) ;this removed 4 shift-reduce conflicts
;          (nonassoc open-bracket) ;this removed 1 shift-reduce conflict
;          (nonassoc do) ;this removed 26 shift-reduce conflicts
;          (nonassoc id) ;this removed 1 shift-reduce conflict
;
;          (nonassoc close-paren close-bracket open-brace close-brace)
;          (nonassoc var type array function break if while for to let in end)
;          (nonassoc invalid dot comma semicolon colon)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;  Helpers and tests  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (run)
  (parse (λ () (lex (current-input-port)))))

(define (parse-string str)
  (let [(port (open-input-string str))]
    (parse (λ () (lex port)))))

;(check-expect (parse-string "4") 4)
;(check-expect (parse-string "\"zoomba\"") "zoomba")
;(check-expect (parse-string "nil") (nil))
;(check-expect (parse-string "some_identifier") (lvalue (id 'some_identifier) empty))
;
;;dangling else testing
;(check-expect (parse-string "if 4 then 4")
;              (if-statement 4 4 empty))
;(check-expect (parse-string "if 5 then 5 else 5")
;              (if-statement 5 5 5))
;(check-expect (parse-string "if if 1 then 1 then if 2 then 2 else if 3 then 3")
;              (if-statement (if-statement 1 1 empty) (if-statement 2 2 (if-statement 3 3 empty)) empty))
;(check-expect (parse-string "if 4 then if 5 then 5 else 5")
;              (if-statement 4 (if-statement 5 5 5) empty))
;(check-expect (parse-string "if 4 then (if 5 then 5) else 4")
;              (if-statement 4 (sequence (list (if-statement 5 5 empty))) 4))
;
;;dangling do testing
;(check-expect (parse-string "while 4 do 4")
;              (while-statement 4 4))
;(check-expect (parse-string "while 5 do while 6 do 6")
;              (while-statement 5 (while-statement 6 6)))
;(check-expect (parse-string "while 5 do for pig := 7 to 7 do 7")
;              (while-statement 5 (for-statement 'pig 7 7 7)))
;(check-expect (parse-string "for pig := 8 to 8 do while 6 do 6")
;              (for-statement 'pig 8 8 (while-statement 6 6)))
;(check-expect (parse-string "for apple := 1 to 10 do for mike := 11 to 20 do 36")
;              (for-statement 'apple 1 10 (for-statement 'mike 11 20 36)))
;
;(check-expect (parse-string "for apple := 1 to for mike := 11 to 20 do 36 do 21")
;              (for-statement 'apple 1 (for-statement 'mike 11 20 36) 21))
;
;;lvalue testing including array accesses and declarations
;(check-expect (parse-string "drugs.f")
;              (lvalue (id 'drugs) (list (lvalue-record-access 'f))))
;(check-expect (parse-string "bears[philip] of 7")
;              (array-creation 'bears (lvalue (id 'philip) empty) 7))
;(check-expect (parse-string "a[b]")
;              (lvalue (id 'a) (list (lvalue-array-access (lvalue (id 'b) empty)))))

;; let in sequence
;(check-expect (parse-string "let in 1 end") (let-statement empty (list 1)))
;(check-expect (parse-string "let in 1; 2 end") (let-statement empty (list 1 2)))
;(check-expect (parse-string "let in (1; 2) end") (let-statement empty (list (sequence (list 1 2)))))

;precedence testing
(check-expect (parse-string "4/5*6")
              (binary-op (op '*) (binary-op (op '/) 4 5) 6))
(check-expect (parse-string "4*5/6")
              (binary-op (op '/) (binary-op (op '*) 4 5) 6))
(check-expect (parse-string "1+2*3")
              (binary-op (op '+) 1 (binary-op (op '*) 2 3)))
(check-expect (parse-string "1*4+5")
              (binary-op (op '+) (binary-op (op '*) 1 4) 5))

(test)