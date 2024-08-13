#lang racket/base

(require brag/support)

(provide basic-lexer)

(define-lex-abbrev digits (:+ (char-set "0123456789")))

;TODO43 - filenames vs identifiers
(define-lex-abbrev filename (:seq (:= 1 (:or "_" alphabetic)) (:+ (:or digits alphabetic (char-set "-_/")))))

(define-lex-abbrev module-identifier (:seq (:= 1 alphabetic)
                                           (:* (:or digits alphabetic (char-set "-_/")))))

(define-lex-abbrev identifier (:seq (:= 1 (:or "_" alphabetic))
                                    (:* (:or digits alphabetic (char-set "-_:")))))

;(define-lex-abbrev dotty-ident (:seq (:= 1 identifier) (:+ (:seq "." identifier))))

(define basic-lexer
  (lexer-srcloc
   ;    ["\n" (token 'NEWLINE lexeme)]
   [(:or "->" "<-" "<<" ">>" "<" ">"
         "==" "!=" ":=" "++"
         "for/list"  "module" "private" "import" "export" "as" "return" "strf"
         "strvars" "expandvars" "base64encode" "base64decode" "using" "overlays" "abstracts" "assert"
         "lowercase" "uppercase" "replace"
         "type" "in" "pprint" "env" "true" "false"
         "imm:" ) (token lexeme lexeme)]
   [(:= 1 (char-set "+-/*.[](){}=:,|")) lexeme]
   [digits (token 'INTEGER (string->number lexeme))]
   [whitespace (token lexeme #:skip? #t)]
   [";" (token lexeme #:skip? #t)]
   [identifier (token 'IDENTIFIER (string->symbol lexeme)) ]
   [module-identifier (token 'MODULE-IDENTIFIER (string->symbol lexeme)) ]
   [filename (token 'FILENAME (string->symbol lexeme)) ]
   ;   [dotty-ident (token 'DOTTY-IDENT (string->symbol lexeme)) ]
   [(from/stop-before "#" "\n") (token lexeme #:skip? #t)]
   ;    [(:or "print" "goto" "end"
   ;  "+" ":" ";") (token lexeme lexeme)]
   ;    [(:or (:seq (:? digits) "." digits)
   ;  (:seq digits "."))
   ; (token 'DECIMAL (string->number lexeme))]
   [(:or (from/to "\"" "\"") (from/to "'" "'"))
    (token 'STRING
           (substring lexeme
                      1 (sub1 (string-length lexeme))))]
   ))