
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

; TODO45 - resource-type stuff/names and consolidate (vs lifecycle)
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

; TODO45
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

(define (resolve-ref r get-by-gid)
  ; TODO45 - refs can be values (which are only immutables, atm)
  (with-value r
    (lambda(v)
      (if (ref? v)
          (hash-nref
           (get-by-gid (ref-gid v))
           (id->list (ref-path v))
           (lambda()(raise "arg")))
          v))))

; This resolver is NOT allowed to fail, as opposed to the resolver in
; support.rkt
(define (resolve-deferred d get-by-gid)

  (log-marv-debug "resolve-deferred: ~a" d)

  (define (handle-term t)
    (define tr (resolve-ref t get-by-gid))
    (if (deferred? tr) (resolve-deferred tr get-by-gid) tr))

  ; (displayln (format "===> DTs: ~a" (pretty-format (deferred-terms d))))
  (define resolved (map handle-term (deferred-terms d)))
  (when (memf deferred? resolved) (raise "aiiiieee"))
  (if (deferred-op d) (apply (deferred-op d) resolved) (car resolved)))

(define (config-resolve cfg get-by-gid)
  (define (process _ v)
    ; (displayln (format "===> LOOKING at (~a) ~a " (deferred? v) (pretty-format v)))
    ; TODO45 - can be values (which are only immutables, atm)
    (if (deferred? (unpack-value v))
        (update-val v (lambda(vv)(resolve-deferred vv get-by-gid)))
        (update-val v (lambda(vv)(resolve-ref vv get-by-gid)))))
  (hash-apply cfg process))
