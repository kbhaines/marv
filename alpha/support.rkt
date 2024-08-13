#lang racket/base

(require racket/hash)
(require racket/list)
(require racket/format)
(require racket/match)
(require racket/set)
(require marv/log)
(require marv/core/values)
(require marv/utils/hash)
(require marv/utils/uri)
(require marv/core/globals)
(require marv/core/drivers)
(require marv/utils/base64)
(require marv/core/resources)
(require marv/core/modules)


(require (prefix-in core: marv/core/resources))
(require (prefix-in drv: marv/core/drivers))

(provide getenv-or-raise
         def-res
         with-src-handlers
         send-to-driver
         b64enc b64dec
         handle-override
         config-overlay config-reduce
         dot-op
         check-for-ref
         resolve-terms
         add-dep get-deps
         with-module-ctx
         with-resource-prefix
         get-resource-prefix
         get-resources
         ordered-resource-ids
         uri-vars
         uri-template
         find-function
         get-param
         src-location
         raise/src
         check-operator-types)

(define (error:excn msg)
  (raise (format "ERROR at ~a:~a :  ~a" 1 2 msg))) ;(syntax-source stx) (syntax-line stx)))

(define (init-resources) (list null (hash)))
(define RESOURCES (make-parameter (init-resources)))
(define (add-resource id res)
  (define idx (car (RESOURCES)))
  (define hs (cadr (RESOURCES)))
  (when (hash-has-key? hs id) (error:excn (format "~a is already defined" id)))
  (RESOURCES (list (cons id idx) (hash-set hs id res)))
  res)
(define (ordered-resource-ids) (reverse (car (RESOURCES))))
(define (get-resources) (cadr (RESOURCES)))

(define MODULE-PREFIX (make-parameter 'main))

(define init-var-deps set)
(define VAR-DEPS (make-parameter (init-var-deps)))
(define (add-dep d) (VAR-DEPS (set-add (VAR-DEPS) d)))
(define (get-deps) (set->list(VAR-DEPS)))

(define (with-module-ctx params proc)
  (log-marv-debug "Switching into module-context: ~a" (MODULE-PREFIX))
  (parameterize ([PARAMS params])
    (proc)))

(define (with-resource-prefix id proc)
  (define new-prefix (join-symbols (list (MODULE-PREFIX) id)))
  (log-marv-debug "Setting resource prefix: ~a" new-prefix)
  (parameterize
      ([MODULE-PREFIX new-prefix]
       [VAR-DEPS (init-var-deps)])
    (define r (proc))
    (log-marv-debug "~a depends on ~a" id (get-deps))
    r))

(define (get-resource-prefix) (MODULE-PREFIX))

(define PARAMS (make-parameter (hash)))
(define (get-param p [def (lambda()
                            (error:excn (format "Parameter '~a' has not been assigned" p)))])
  (hash-ref (PARAMS) p def))

(define (def-res type-id cfg)
  (define gid (get-resource-prefix))
  (log-marv-debug "Defining resource: ~a" gid)
  (add-resource gid (mk-resource gid type-id (get-deps) cfg)))

(define (with-src-handlers src-locn expected given thunk)
  (define (handle-exn e)
    (raise-argument-error
     'type
     (format "~a at ~a~nActual exception:~n~a~n)" expected src-locn e) given))
  (with-handlers ([exn? handle-exn]) (thunk)))

(define (config-overlay top bottom) (hash-union top bottom #:combine (lambda (t _) t)))

(define (config-reduce cfg attrs) (hash-take cfg attrs))

(define (handle-override base op verb confex)
  (case op
    ['equals confex]
    ['overlay (config-overlay confex (base verb))]))

(define (dot-op tgt attr)
  (log-marv-debug "dot-op: ~a . ~a" tgt attr)
  (define r
    (cond
      [(resource? tgt) (ref (resource-gid tgt) attr)]
      [(hash? tgt) (hash-ref tgt attr)]
      [else (raise "unsupported ref type")]))
  (when (ref? r) (add-dep r))
  r)

(define (check-for-ref term)
  (log-marv-debug "-> checking ref: ~a" term)
  (cond
    [(ref? term) (add-dep term)]
    [(deferred? term) (map add-dep (deferred-deps term))])
  term)

(define (resolve-terms op . terms)

  (define (try-resolve e)

    ; TODO45 - resolve-ref here vs resources.rkt version
    (define (resolve-ref r)
      (log-marv-debug "-> attempting to resolve: ~a" r)
      (unpack-value
       (hash-nref
        (resource-config (hash-ref (get-resources) (ref-gid r)))
        (id->list (ref-path r))
        r)))

    (cond
      [(ref? e) (add-dep e) (resolve-ref e)]
      [else e]))

  (log-marv-debug "Resolving terms: ~a ~a" op terms)
  (define resolved-terms (map try-resolve terms))
  (log-marv-debug "-> terms: ~a" resolved-terms)

  (cond [(memf (lambda(x) (or (ref? x) (deferred? x))) resolved-terms)
         (log-marv-debug "-> couldn't resolve, deferred")
         (define all-deferred-deps
           (for/fold
            ([acc (get-deps)])
            ([rt resolved-terms]
             #:when (deferred? rt))
             (set-union acc (deferred-deps rt))))
         (apply deferred op all-deferred-deps resolved-terms)]
        [else
         (log-marv-debug "-> resolved, invoking operation: ~a" op)
         (define r (apply op resolved-terms))
         (log-marv-debug "<- finished resolving: ~a" r)
         r ]
        ))

(define (uri-vars str) (uri-vars str))
(define (uri-template str cfg) (expand-uri str cfg))

(define (find-function root fst rst)
  (log-marv-debug "find-function: ~a.~a" fst rst)
  (define func
    (match/values
     (values root rst)
     [((? procedure? proc) (? null? _) )
      (log-marv-debug "  ->func: ~a" proc)proc]
     [((? procedure? proc) (list r))
      (log-marv-debug "  ->func ~a in type ~a" r proc)
      (proc r)]
     [((? hash? hsh) lst)
      (log-marv-debug "  ->func in hash")
      (hash-nref hsh lst)]
     [(_ _) (raise "unsupported function reference")]))
  (if (procedure? func) func (raise (~a "expected a function, got: " func))))

(define (src-location s) (format "~a:~a" (syntax-source s) (syntax-line s)))

(define (raise/src locn msg) (raise (~a "Error:" msg " at " locn) ))

(define (check-operator-types locn test1? test2? term1 term2)
  (unless (and (test1? term1) (test2? term2))
    (raise (~a "terms not valid at:" locn "~n got " term1 "~n and " term2))))