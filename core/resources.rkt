
#lang racket/base

(require racket/contract)
(require racket/pretty)
(require racket/string)
(require marv/core/config)
(require marv/log)
(require marv/utils/hash)
(require marv/core/values)

(provide hash->attribute
         flat-attributes
         res-id/c prefix-id list->id id->list
         resource-call
         resource-type-fn
         resource-origin
         resource/c
         origin/c
         resource-set/c
         config-resolve
         resource? resource-gid resource-type resource-deps resource-config
         mk-resource update-resource-config
         (struct-out attribute))

(struct attribute (name value) #:prefab)

; Don't provide 'resource' to clients; we want them to use mk-resource.
(struct resource (gid type deps config) #:prefab)

(define (mk-resource gid type deps config)
  (resource gid type deps (type-call type 'identity config)))

(define (type-call type verb . args)
  (apply (hash-ref type verb) args))

(define (update-resource-config r new-config)
  (resource (resource-gid r) (resource-type r) (resource-deps r) new-config))

(define res-id/c symbol?)

(define origin/c hash?)

(define/contract (resource-type-fn res)
  (resource? . -> . any/c)
  (lambda(verb . args)(apply type-call (resource-type res) verb args)))

(define/contract (resource-call verb res)
  (symbol? resource? . -> . any/c)
  (type-call (resource-type res) verb (resource-config res)))

(define/contract (resource-origin res)
  (resource? . -> . origin/c)
  (resource-call 'origin res))

(define/contract (prefix-id prf id)
  (res-id/c res-id/c . -> . res-id/c )
  (string->symbol (format "~a.~a" prf id)))

(define/contract (id->list id)
  (res-id/c . -> . (listof symbol?))
  (map string->symbol (string-split (symbol->string id) ".")))

(define/contract (list->id lst)
  ((listof symbol?) . -> . res-id/c)
  (string->symbol (string-join (map symbol->string lst) ".")))

(define type? hash?)

(define resource/c (struct/c resource symbol? type? list? config/c))

(define resource-set/c (hash/c res-id/c resource/c))

(define/contract (hash->attribute h)
  (hash? . -> . (listof attribute?))

  (define (mk-attr k v)
    (cond [(hash? v) (attribute k (hash-map v mk-attr))]
          [else (attribute k v)]))

  (hash-map h mk-attr))

(define/contract (flat-attributes attrs)
  ((listof attribute?) . -> . (listof any/c))

  (define (flat a acc)
    (cond [((listof attribute?) (attribute-value a))
           (foldl flat acc (attribute-value a) )]
          [else (cons a acc)]))

  (foldl flat '() attrs))


; used to finalise all deferred computations and references in the
; given 'cfg', making the values concrete so they can be sent to an API (for example).
; This resolver is NOT allowed to fail, as opposed to the resolver in
; support.rkt

(define (config-resolve cfg get-by-gid)

  (define (process _ v)
    (when (and (value? v) (deferred? (unpack-value v))) (raise "invariant violation; value wraps deferred"))
    (if (deferred? v)
        (resolve-deferred v)
        (resolve-ref v)))

  (define (resolve-deferred d)

    (log-marv-debug "resolve-deferred: ~a" d)

    (define (handle-term t)
      ; We can still have values packed inside deferred terms
      (define tr (resolve-ref (unpack-value t)))
      (if (deferred? tr) (resolve-deferred tr) tr))

    (define resolved (map handle-term (deferred-terms d)))
    (when (memf deferred? resolved) (raise "unabled to compute deferred term"))
    (if (deferred-op d) (apply (deferred-op d) resolved) (car resolved)))

  (define (resolve-ref v)
    (define uv (unpack-value v))
    (if (ref? uv)
        (hash-nref
         (get-by-gid (ref-gid uv))
         (id->list (ref-path uv))
         (lambda()(raise "resolve-ref: unable to resolve")))
        uv))

  (hash-apply cfg process))
