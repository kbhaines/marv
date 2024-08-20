#lang racket/base

(require racket/string)
(require racket/format)
(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     racket/pretty
                     marv/core/globals))

(require racket/pretty)
(require racket/contract)

(require marv/alpha/support)
(require marv/core/values)
(require marv/core/globals)
(require marv/log)

; TODO - swap prefix usage to m- on the provided (define a macro?)

(define-for-syntax (src-location s) (format "~a:~a" (syntax-source s) (syntax-line s)))

(begin-for-syntax

  (define (m-marv-spec stx)
    (syntax-parse stx
      [(_ MODULE ...)
       #'(begin
           (require marv/alpha/support)
           (require racket/hash)
           (require racket/pretty)
           MODULE ...
           )]
      [_ (raise "nowt")]))

  (define (m-outer-decl stx)
    (syntax-parse stx
      [(_ OUTER) (syntax/loc stx OUTER)]
      [_ (raise "nowt-outer-decl")]))

  (define (m-module-export stx)

    (define-splicing-syntax-class export-spec
      #:description "export specification"
      #:attributes (spec)
      (pattern (~seq src-name:id "as" export-alias)
        #:attr spec #'(rename-out [src-name export-alias]))
      (pattern (~seq src-name:id)
        #:attr spec #'src-name))

    (syntax-parse stx
      [(_ spec:export-spec ...) (syntax/loc stx (provide spec.spec ...))]
      [_ (raise "nowt-module-export")]))

  (define-splicing-syntax-class named-argument
    #:attributes (name value)
    #:literals (expression)
    (pattern (~seq name:id "=" value)))

  (define (m-marv-module stx)

    (syntax-parse stx #:datum-literals (statement module-return)
      [(_ (~optional (~and private? "private"))
          mod-id:expr (~seq "(" IDS:id ... NAMED-ARGUMENTS:named-argument ... ")")
          (statement STMT) ...
          (~optional RETURN #:defaults ([RETURN #'(hash)])))
       #:with MAYBE-PRIVATE (if (attribute private?) #'(void) #'(provide mod-id))
       (syntax/loc stx
         (begin
           (define (mod-id IDS ... [keywp (hash)])
             (define NAMED-ARGUMENTS.name
               (hash-ref keywp 'NAMED-ARGUMENTS.name NAMED-ARGUMENTS.value)) ...
             (define resid-prefix (get-resource-prefix))
             (log-marv-debug "** Invoking module: ~a=~a(~a)" resid-prefix 'mod-id keywp)
             STMT ...
             (define returns RETURN)
             (log-marv-debug "** module invocation completed for ~a.~a" resid-prefix 'mod-id)
             (log-marv-debug "-> resources: ~a" (ordered-resource-ids))
             (log-marv-debug "-> returns: ~a" returns)
             returns)
           MAYBE-PRIVATE))]
      [_ (displayln stx)(raise "invalid module spec m-marv-module")]))

  (define (m-module-parameter stx)
    (syntax-parse stx
      [(_ PARAMETER "=" EXPR) (syntax/loc stx (define PARAMETER (get-param 'PARAMETER EXPR)))]
      [(_ PARAMETER) (syntax/loc stx (define PARAMETER (get-param 'PARAMETER)))]))

  (define (m-module-return stx)
    (syntax-parse stx
      [(_ RETURNS ...) (syntax/loc stx (make-immutable-hasheq (list RETURNS ...)))]
      [_ (raise "m-module-return f*")]))

  (define (m-return-parameter stx)
    (syntax-parse stx
      [(_ NAME VALUE) (syntax/loc stx (cons 'NAME VALUE))]))

  (define (m-module-import stx)
    (syntax-parse stx
      ; this fix comes courtesy of replies to this:
      ; https://stackoverflow.com/questions/77621776/hard-coded-require-from-a-racket-macro-doesnt-bind-provided-identifiers
      [(_ MOD-ID:id)
       (define mpath (format "marv/~a.mrv" (syntax-e #'MOD-ID)))
       (datum->syntax stx `(require (lib ,mpath)))]

      [(_ MOD-ID:id "as" ALIAS)
       (define mpath (format "marv/~a.mrv" (syntax-e #'MOD-ID)))
       (define alias (format-id #f "~a:" (syntax-e #'ALIAS)))
       (datum->syntax stx `(require (prefix-in ,alias (lib ,mpath)))) ]

      [(_ FILENAME:string) (syntax/loc stx (require FILENAME)) ]
      [(_ FILENAME:string "as" ALIAS)
       (define alias (format-id #f "~a:" (syntax-e #'ALIAS)))
       (define filename (syntax-e #'FILENAME))
       (datum->syntax stx `(require (prefix-in ,alias ,filename))) ]
      [_ (raise "m-import")]))

  (define (m-statement stx)
    (syntax-parse stx
      [(_ STMT) (syntax/loc stx STMT)]
      [_ (raise "nowt-stmt")]))

  (define (m-decl stx)
    (syntax-parse stx
      [(_ DECL) (syntax/loc stx DECL)]
      [_ (raise "nowt-decl")]))

  (define (m-var-decl stx)
    (syntax-parse stx
      [(_ id:expr EXPR)
       (syntax/loc stx
         (define id (with-resource-prefix 'id (lambda()EXPR))))]
      [_ (raise "nowt-var-decl")]))

  (define (m-config-func-decl stx)
    (syntax-parse stx
      [(_ id:expr param ... CONF-OBJ)
       (syntax/loc stx
         (define (id param ...) CONF-OBJ))]
      [_ (raise "config-func-decl")]))

  (define (m-func-call stx)
    (syntax-parse stx
      #:literals (expression)
      [(_ func "(" (expression exprs) ... NAMED-PARAMETERS:named-argument ... ")")
       (syntax/loc stx
         (apply func (expression exprs) ...
                (make-immutable-hash
                 (list (cons 'NAMED-PARAMETERS.name NAMED-PARAMETERS.value) ...))))]
      ; TODO45 - possibly not needed, but might be optimal
      [(_ func "(" (expression exprs) ... ")")
       (syntax/loc stx
         (apply func (expression exprs) ...))]
      [_ (raise "func-call")]))

  (define (m-func-ident stx)
    (syntax-parse stx
      [(_ ident)
       (define id-parts (split-symbol (syntax-e #'ident)))
       (define root (format-id stx "~a" (car id-parts)))
       (define rst (datum->syntax stx (cdr id-parts)))
       (with-syntax ([root root]
                     [rst rst])
         (syntax/loc stx (find-function root 'root 'rst)))]
      [_ (raise "func-ident")]))

  (define (m-func-decl stx)
    (syntax-parse stx
      [(_ id:expr IDS:id ... NAMED-ARGUMENTS:named-argument ... BODY)
       (syntax/loc stx
         (define (id IDS ... [ keywp (hash)])
           (define NAMED-ARGUMENTS.name
             (hash-ref keywp 'NAMED-ARGUMENTS.name NAMED-ARGUMENTS.value)) ...
           BODY))]
      [_ (raise "func-decl")]))

  (define-splicing-syntax-class type-body
    #:description "body declaration"
    #:literals (func-decl expression)
    (pattern (func-decl func-id:id param-id ... (expression confex))))

  (define (m-type-decl stx)
    (syntax-parse stx
      #:datum-literals (type-parameters type-wild type-id)
      [(_ (type-id tid:expr) body:type-body ... (type-wild wildcard) ...)
       (with-syntax ([srcloc (src-location stx)])
         (syntax/loc stx
           (define tid
             (let* ([ body.func-id (lambda(body.param-id ...) body.confex) ] ...)
               (make-immutable-hasheq
                (append
                 (hash->list wildcard) ...
                 (list
                  (cons 'body.func-id body.func-id) ...)))))))]
      [(_ (type-id tid) (type-parameters (type-id template-id) params ...))
       (syntax/loc stx (define tid (template-id params ... )))]
      ))

  (define (m-type-template stx)
    (syntax-parse stx
      #:datum-literals (type-parameters type-id type-wild)
      [(_ (type-parameters (type-id template-id) params ...) body:type-body ... (type-wild wildcard) ...)
       (syntax/loc stx
         (define (template-id params ... [allow-missing? #f])
           (log-marv-debug "type-template ~a" 'template-id)
           (define (body.func-id body.param-id ...) body.confex) ...
           (make-immutable-hasheq
            (append
             (hash->list wildcard) ...
             (list
              (cons 'body.func-id body.func-id) ...)))))]
      ))

  (define (m-generic-placeholder stx)stx)

  (define (m-built-in stx)
    (syntax-parse stx
      [(_ "lowercase" expr ) (syntax/loc stx (resolve-terms string-downcase expr))]
      [(_ "uppercase" expr ) (syntax/loc stx (resolve-terms string-downcase expr))]
      [(_ "replace" expr from to ) (syntax/loc stx (resolve-terms string-replace expr (regexp from) to))]
      [(_ BUILTIN) (syntax/loc stx BUILTIN)]
      [_ (raise "nowt-builtin")]))

  (define (m-env-read stx)
    (syntax-parse stx
      [(_ env-var:string) (syntax/loc stx (getenv-or-raise env-var))]
      [_ (raise "m-env-read")]))

  (define (m-assertion stx)

    (define (assert inv-op fn expr1 expr2)
      (with-syntax
          ([locn (src-location stx)]
           [fn (datum->syntax stx fn)]
           [op inv-op]
           [expr1 expr1]
           [expr2 expr2])
        (syntax/loc stx
          (or (fn expr1 expr2)
              (raise/src
               locn
               (~a "assertion failure: '" expr1 "' " op " '" expr2 "'"))))))

    (syntax-parse stx
      [(_ expr1 "==" expr2) (assert "does not equal" equal? #'expr1 #'expr2)]
      [(_ expr1 "!=" expr2) (assert "equals" (compose1 not equal?) #'expr1 #'expr2)]
      [_ (raise "m-assertion")]))

  (define (m-strf stx)
    (syntax-parse stx
      [(_ str:expr expr ... ) (syntax/loc stx (resolve-terms format str expr ...))]
      [_ (raise "m-strf")]))

  (define (m-urivars stx)
    (syntax-parse stx
      [(_ str:expr) (syntax/loc stx (uri-vars str))]
      [_ (raise "m-urivars")]))

  (define (m-uritemplate stx)
    (syntax-parse stx
      [(_ str:expr CFG) (syntax/loc stx (uri-template str CFG))]
      [_ (raise "m-uritemplate")]))

  (define (m-base64encode stx)
    (syntax-parse stx
      [(_ ex:expr) (syntax/loc stx (b64enc ex))]
      [_ (raise "m-base64encode")]))

  (define (m-base64decode stx)
    (syntax-parse stx
      [(_ ex:expr) (syntax/loc stx (b64dec ex))]
      [_ (raise "m-base64decode")]))

  (define (m-pprint stx)
    (syntax-parse stx
      [(_ exp:expr) (syntax/loc stx (pretty-print exp))]))

  (define (m-indexed-identifier stx)
    (syntax-parse stx
      [(_ ident:id "[" expr "]" ) (syntax/loc stx (resolve-terms list-ref ident expr))]
      [(_ ident) (syntax/loc stx ident)]
      ))

  (define (m-expression stx)
    (syntax-parse stx
      #:literals (expression)
      [(_ term1 "|" term2)
       (syntax/loc stx
         (with-handlers
             ([exn:fail? (lambda(_) term2)]) term1))]

      ; TODO45 - add type checking
      [(_ term:string) (syntax/loc stx term)]
      ; TODO45 - for compile speed, add some concrete type checks
      ; e.g. string, to avoid calling check-for-ref
      [(_ term) (syntax/loc stx (check-for-ref term))]
      ))

  (define (m-root-term stx)
    (syntax-parse stx
      #:literals (expression)
      [(_ func-exp "(" (expression exprs) ... ")")
       (syntax/loc stx (apply func-exp (list (expression exprs) ...)))]

      ; [(_ map-expr "." ident:id)
      ;  (syntax/loc stx (resolve-terms dot-op map-expr 'ident))]

      [(_ list-exp "[" expr "]")
       (syntax/loc stx (resolve-terms list-ref list-exp expr ))]
      [(_ id:id) #'id]
      [(dot-apply e) #'e]
      ))

  (define (m-dot-apply stx)
    (syntax-parse stx
      [(_ map-expr "." ident:id)
       (syntax/loc stx (resolve-terms dot-op map-expr 'ident))]))

  (define (m-boolean-expression stx)
    (syntax-parse stx
      [(_ "true") (syntax/loc stx #t)]
      [(_ "false") (syntax/loc stx #f)]
      [(_ expr1 "==" expr2 ) (syntax/loc stx (resolve-terms equal? expr1 expr2))]
      [(_ expr1 "!=" expr2 ) (syntax/loc stx (resolve-terms (compose1 not equal?) expr1 expr2))]
      ))

  (define (m-string-expression stx)
    (syntax-parse stx
      [(_ str1 "++" str2) (syntax/loc stx (resolve-terms string-append str1 str2))]
      [(_ str) (syntax/loc stx str)]
      [_ (raise "string-expr")]
      ))

  (define (m-num-expression stx)
    (syntax-parse stx
      [(_ term) (syntax/loc stx term)]
      [(_ term1 "+" term2) (syntax/loc stx (resolve-terms + term1 term2))]
      [(_ term1 "-" term2) (syntax/loc stx (resolve-terms - term1 term2))]
      ))

  (define (m-num-term stx)
    (syntax-parse stx
      [(_ term) (syntax/loc stx term)]
      [(_ term1 "*" term2) (syntax/loc stx (resolve-terms * term1 term2))]
      [(_ term1 "/" term2) (syntax/loc stx (resolve-terms / term1 term2))]
      ))

  (define (m-list-expression stx)
    (syntax-parse stx
      [(_ "[" expr ... "]") #'(resolve-terms list expr ...)]
      [(_ expr) #'expr]))

  (define (m-map-expression stx)

    (define (check-terms stx test1? test2? op term1 term2)
      (with-syntax
          ([locn (src-location stx)]
           [test1? test1?] [test2? test2?] [op op] [term1 term1] [term2 term2])
        (syntax/loc stx
          (begin
            ; TODO45- reinstate type checking
            ; (check-operator-types locn test1? test2? term1 term2)
            (resolve-terms op term1 term2)))))

    (syntax-parse stx
      [(_ term) (syntax/loc stx term)]
      [(_ term1 "<-" term2) (check-terms stx #'hash? #'hash? #'config-overlay #'term2 #'term1)]
      [(_ term1 "->" term2) (check-terms stx #'hash? #'hash? #'config-overlay #'term1 #'term2)]
      [(_ term1 "<<" term2) (check-terms stx #'hash? #'(listof symbol?) #'config-reduce #'term1 #'term2)]
      ))

  (define (m-dot-expression stx)
    (syntax-parse stx
      ; [(_ map-expr ident:id) (syntax/loc stx (hash-ref map-expr 'ident))]
      [(_ map-expr ident:id) (syntax/loc stx (resolve-terms dot-op map-expr 'ident))]
      [(_ map-expr ident:id "[" idx "]")
       (syntax/loc stx (resolve-terms
                        list-ref
                        (resolve-terms dot-op map-expr 'ident) idx))]
      [(_ map-expr ident:id params ...) (syntax/loc stx ((hash-ref map-expr 'ident) params ...))]))

  (define (m-map-spec stx)

    (define (this-name stx) (format-id #f "this_~a" (syntax-e stx)))
    (define (xid x)x)

    (define-splicing-syntax-class attr-decl
      #:description "attribute declaration"
      #:attributes (name tname expr)
      (pattern (~seq name:id expr)
        #:attr tname (this-name #'name))
      (pattern (~seq aname:string expr)
        #:attr name (format-id #f "~a" (syntax-e #'aname))
        #:attr tname (this-name #'aname))
      (pattern (~seq name:id "imm:" iexpr)
        #:attr tname (this-name #'name)
        #:attr expr #'(ival iexpr)))

    (syntax-parse stx
      [(_ attr:attr-decl ...)
       ; TODO45 - should resolve-terms wrap make-imm-hash and 'list'?
       ; TODO45 - compile speed; the internal lambda might be causing slow-down
       #'(make-immutable-hasheq
          (list (cons 'attr.name (resolve-terms (lambda(x)x) attr.expr)) ...)) ]
      [_ (displayln stx)(raise "m-map-spec")]))

  (define (m-alist stx)
    (syntax-parse stx
      [(_ EXPR ...)
       (syntax/loc stx (list EXPR ...))]
      [_ (raise "m-alist")]))

  (define (m-attribute-name stx)
    (syntax-parse stx
      [(_ sname:string)
       (define id (format-id #f "~a" (syntax-e #'sname)))
       (with-syntax ([name id]) (syntax/loc stx 'name))]
      [(_ name:id) (syntax/loc stx 'name)]
      [_ (raise "m-attribute-name")]))

  (define (m-attr-list stx)
    (syntax-parse stx
      [(_ ATTR ...)
       (syntax/loc stx (list ATTR ...))]
      [_ (raise "m-attr-list")]))

  (define (m-config-expr stx)
    (syntax-parse stx
      [(_ CFEXPR) #'CFEXPR]
      [_ (raise "m-config-expr")]))

  (define (m-keyword stx)
    (syntax-parse stx
      [(_ keyword) (syntax/loc stx keyword)]))

  (define (m-res-decl stx)
    (syntax-parse stx
      [(_ name:id ((~literal type-id) tid:id) cfg)
       #`(define name
           (with-src-handlers #,(src-location stx)  "valid type" 'tid
             (lambda()
               (with-resource-prefix 'name
                 (lambda() (def-res tid cfg))))))]
      ))

  (define (m-named-parameter stx)
    (syntax-parse stx
      ; NB string has to be first!
      [(_ param-name:string EXPR) (syntax/loc stx (cons (string->symbol param-name) EXPR))]
      [(_ param-name:expr EXPR) (syntax/loc stx (cons 'param-name EXPR))]
      ))
  )

(define-syntax marv-spec m-marv-spec)
(define-syntax outer-decl m-outer-decl)
(define-syntax marv-module m-marv-module)
(define-syntax module-parameter m-module-parameter)
(define-syntax module-return m-module-return)
(define-syntax return-parameter m-return-parameter)
(define-syntax module-import m-module-import)
(define-syntax module-export m-module-export)
(define-syntax statement m-statement)
(define-syntax decl m-decl)
(define-syntax var-decl m-var-decl)
(define-syntax config-func-decl m-config-func-decl)
(define-syntax func-decl m-func-decl)
(define-syntax type-decl m-type-decl)
(define-syntax type-template m-type-template)
(define-syntax res-decl m-res-decl)
(define-syntax named-parameter m-named-parameter)
(define-syntax func-call m-func-call)
(define-syntax func-ident m-func-ident)
(define-syntax indexed-identifier m-indexed-identifier)
(define-syntax expression m-expression)
(define-syntax root-term m-root-term)
(define-syntax dot-apply m-dot-apply)
(define-syntax boolean-expression m-boolean-expression)
(define-syntax string-expression m-string-expression)
(define-syntax list-expression m-list-expression)
(define-syntax map-expression m-map-expression)
(define-syntax num-expression m-num-expression)
(define-syntax num-term m-num-term)
(define-syntax dot-expression m-dot-expression)
(define-syntax map-spec m-map-spec)
(define-syntax alist m-alist)
(define-syntax attr-list m-attr-list)
(define-syntax attribute-name m-attribute-name)
(define-syntax keyword m-keyword)
(define-syntax built-in m-built-in)
(define-syntax assertion m-assertion)
(define-syntax env-read m-env-read)
(define-syntax strf m-strf)
(define-syntax urivars m-urivars)
(define-syntax uritemplate m-uritemplate)
(define-syntax base64encode m-base64encode)
(define-syntax base64decode m-base64decode)
(define-syntax pprint m-pprint)
(define-syntax config-expr m-config-expr)

(define-syntax api-id m-generic-placeholder)
(define-syntax transformer-id m-generic-placeholder)
(define-syntax type-id m-generic-placeholder)
(define-syntax func-id m-generic-placeholder)
(define-syntax type-parameters m-generic-placeholder)
(define-syntax type-wild m-generic-placeholder)

(provide marv-spec outer-decl marv-module module-parameter decl var-decl res-decl
         named-parameter module-return return-parameter
         module-import
         module-export
         api-id transformer-id type-id
         func-call func-ident config-func-decl func-decl type-decl type-template
         indexed-identifier
         expression root-term dot-apply boolean-expression string-expression num-expression num-term
         list-expression map-expression dot-expression statement map-spec alist attr-list attribute-name
         config-expr
         keyword built-in assertion env-read pprint strf urivars uritemplate  base64encode base64decode)
