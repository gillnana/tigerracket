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
   funtion
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
   
   ; keywords
   ["type" (token-type)]
   ["array" (token-array)]
   ["var" (token-var)]
   ["funtion" (token-funtion)]
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
    (token-string lexeme)]
     
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

(define parse
  (parser
   
   
    
   (tokens tiger-tokens tiger-empty-tokens)
   
   
   (grammar
;    (decs [(dec decs)    ]
;          [() ])
    (exp [(literal) $1])
     
    (literal [(int) $1]
             [(string) $1]
             [(id) (id $1)])
    
    
    )
   
   ;    (exp [(numexp) $1]
;         [(sumexp) $1])
;    (sumexp [(expr plus expr) (list $1 '+ $3)])
;    (numexp [(num) $1])
   
   
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