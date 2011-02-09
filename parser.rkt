#lang racket
(require parser-tools/yacc)
(require parser-tools/lex)

(require (prefix-in : parser-tools/lex-sre))

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
   if-token
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

(struct tydec (type-id ty)) ; type declaration
(struct type-id (name))
(struct id (name))
(struct record-of (tyfields)) ; record type contains list of tyfield
(struct array-of (type)) ; array type
(struct tyfield (id type-id))
(struct vardec (id type-id val)) ; type-id can be #f
(struct fundec (id tyfields type-id body)) ; type-id can be #f
(struct record-access (lvalue id))
(struct array-access (lvalue index))
(struct funcall (fun-id args))
(struct array-creation (type-id size initval))
(struct record-creation (type-id fieldvals))
(struct fieldval (name val))
(struct assignment (lvalue val))
(struct if-statement (cond then else)) ; else is optional
(struct while-statement (cond body))
(struct for-statement (var start end body))
; TODO: careful of sequence vs list-of exp
(struct let-statement (bindings expseq))
(struct sequence (exps))


;TODO: i added some more that i think we need.  do we need these?
(struct nil ())
(struct arithmetic-op (op arg1 arg2))
(struct arithmetic-negation (arg))
(struct break ())

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
        [(open-brace tyfields close-brace) $2]
        [(array of id) (array-of $3)])
    (tyfields [() empty]
              [(id colon id) (cons (tyfield $1 $3) empty)]
              [(id colon id comma tyfields) (cons (tyfield $1 $3) $5)])
    (vardec [(var id assign exp) (vardec $2 #f $4)]
            [(var id colon id assign exp) (vardec $2 $4 $6)])
    (fundec [(function id open-paren tyfields close-paren equals exp) (fundec $2 $4 #f $7)]
            [(function id open-paren tyfields close-paren colon id equals exp) (fundec $2 $4 $7 $9)])
    (exp [(literal) $1]
         [(lvalue) $1]
         [(funcall) $1]
         ;[(arithmetic) $1])
         [(structures) $1]
         [(assignment) $1]
         [(control) $1])
    
    (lvalue [(id) (id $1)]
            [(lvalue dot id) (record-access $1 $3)]
            [(lvalue open-bracket exp close-bracket) (array-access $1 $3)])       
    (literal [(int) $1]
             [(string) $1]
             [(nil) (nil)])
    
    (funcall [(exp open-paren funcall-args close-paren) (funcall $1 $3)])
    (funcall-args [() empty]
                  [(exp) (cons $1 empty)]
                  [(exp comma funcall-args) (cons $1 $3)])
    ;arithmetic
    
    (structures [(record-creation) $1]
                [(array-creation) $1])
    (record-creation [(id open-brace record-creation-args close-brace) (record-creation (type-id $1) $3)])
    (record-creation-args [() empty] 
                          [(id equals exp) (cons (fieldval $1 $3) empty)]
                          [(id equals exp comma record-creation-args) (cons (fieldval $1 $3) $5)])
    ;TODO array-creation generates precedence problems because this also looks like an array access
    (array-creation [(id open-bracket exp close-bracket of exp) (array-creation $1 $3 $6)])
    
    (assignment [(lvalue assign exp) (assignment $1 $3)]) 
    
    (control [(if-nonterminal) $1]
             [(while-nonterminal) $1]
             [(for-nonterminal) $1]
             [(break) (break)]
             [(let-nonterminal) $1]
             [(sequencing) $1])
    
    (if-nonterminal [(if exp then exp) (if-statement $2 $4 empty)]
                    [(if exp then exp else exp) (if-statement $2 $4 $6)])
    (while-nonterminal [(while exp do exp) (while-statement $2 $4)])
    (let-nonterminal [(let decs in expseq end) (let-statement $2 $4)])
    (for-nonterminal [(for id assign exp to exp do exp) (for-statement $2 $4 $6 $8)])
    (expseq [() empty]
            [(exp) (cons $1 empty)]
            [(exp semicolon expseq) (cons $1 $3)])
    (sequencing [(open-paren expseq close-paren) (sequence $2)]))
   
   
   (start exp)
   (end eof)
   (error (λ (valid? token value) 
            (error 
             (format "parse error at ~a: with value:~a was token valid?:~a"
                     token 
                     value 
                     valid?))))
   
   ))

(define (run)
  (parse (λ () (lex (current-input-port)))))