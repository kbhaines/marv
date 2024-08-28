#lang racket/base

(require racket/contract)
(require racket/match)
(require racket/pretty)
(require racket/set)
(require racket/list)
(require racket/hash)
(require racket/string)
(require racket/path)

(require marv/core/drivers)
(require marv/core/resources)
(require marv/core/modules)
(require marv/core/values)
(require marv/core/config)
(require marv/utils/hash)
(require marv/log)

(provide init-module get-module
         resource-keys
         resource-ref
         module-ref
         get-resource-params
         validate-params
         resource-refs
         unpack-value
         update-val
         unwrap-values
         (struct-out ref)
         (struct-out value)
         ival ival?  iref?
         ;iref
         match-resource-attr?
         (struct-out params))

(struct params (required accepted))

; TODO - using parameters for this isn't necessary
(define RESOURCES (make-parameter (lambda take-any-params (hash))))

(define/contract (init-module f)
  (string? . -> . void?)
  (define rel-mod (find-relative-path (current-directory) f) )
  (RESOURCES (dynamic-require rel-mod 'main))
  (void))

(require marv/alpha/support)
(define/contract (get-module params purge?)
  ((hash/c symbol? string?) boolean? . -> . resource-set/c)
  ; (validate-params params)
  ; TODO - 'main?
  (define returns ((RESOURCES) params))
  (define resources (get-resources))
  (if purge? (hash) resources))

(define (make-keyword-params params)
  ; TODO: test for check that #t (sorting) works
  (hash-map params (lambda(k v) (cons (string->keyword k) v)) #t))

(define/contract (validate-params input-params)
  ((hash/c string? string?) . -> . void?)
  (define invalids (set-subtract (hash-keys input-params)
                                 (params-accepted (get-resource-params))))
  (when (not (empty? invalids))
    (raise (format "Invalid parameters: ~a\n(Valid are: ~a)"
                   invalids (params-accepted (get-resource-params)))))
  (define missing (set-subtract (params-required (get-resource-params))
                                (hash-keys input-params)))
  (when (not (empty? missing))
    (raise (format "Missing required parameters ~a" missing))))

(define (get-resource-params)
  (define-values (required accepted) (procedure-keywords (RESOURCES)))
  (params (map keyword->string required) (map keyword->string accepted))
  )

; TODO - tighten the contracts
(define/contract (resource-keys rs)
  (any/c . -> . (listof any/c))
  (hash-keys rs))

(define/contract (resource-ref mod k)
  (resource-set/c res-id/c . -> . resource/c)
  (hash-ref mod k))

; module-ref returns the resource, or the driver-config if a driver
(define/contract (module-ref mod k)
  (any/c any/c . -> . any/c)
  (define ref-spec (map string->symbol (string-split (symbol->string k) ".")))
  (hash-nref mod ref-spec))

(define/contract (resource-refs res)
  (resource/c . -> . (listof (cons/c any/c ref?)))
  (filter (compose1 ref? cdr)
          (map (lambda (kv) (cons (car kv) (unpack-value(cdr kv))))
               (hash->flatlist (resource-config res)))))

(define (match-resource-attr? res matchf) (hash-match? res matchf))

(define/contract (unwrap-values res)
  (config/c . -> . config/c)
  (define (unwrap _ v)  (unpack-value v))
  (hash-apply res unwrap))