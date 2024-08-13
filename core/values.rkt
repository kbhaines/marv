#lang racket/base

(require racket/string)
(require racket/match)
(require racket/contract)

(provide (struct-out value)
         (struct-out ref)
         deferred
         deferred-op deferred-terms deferred-deps
         deferred?
         unpack-value
         update-val
         ival ival?
         ;  iref
         iref? vref?
         ref-split
         ref->id)

(struct value (val flags) #:prefab)

(define (ival v)
  (cond [(hash? v)
         (hash-map/copy v (lambda (k v) (values k (ival v))))]
        [(list? v) (map ival v)]
        [else (value v '(immutable))]))

(define (ival? v) (and (value? v) (member 'immutable (value-flags v)) #t))

; TODO - is there a better way of encapsulating in the struct def?
(define (unpack-value v) (if (value? v) (value-val v) v))

; TODO - some unit tests here.
; TODO - needs to be used more often (or replaced with suitable mechanism)
(define (update-val v fn)
  (if (value? v)
      (value (fn (value-val v)) (value-flags v))
      (fn v)))

(struct ref (gid path)  #:prefab)

(struct _deferred (op deps terms) #:prefab)
(define (deferred op deps . terms) (_deferred op deps terms))
(define deferred? _deferred?)
(define deferred-op _deferred-op)
(define deferred-deps _deferred-deps)
(define deferred-terms _deferred-terms)

; (define (iref r) (ival (ref r)))
(define (iref? v) (and (ival? v) (ref? (unpack-value v))))


(define (vref? v) (ref? (unpack-value v)))

(define/contract (ref-split r)
  (vref? . -> . (values symbol? symbol?))
  (match (map string->symbol (string-split (symbol->string (ref-path (unpack-value r))) "/"))
    [(list id attrs) (values id attrs)]
    [else (raise (format "Bad reference format:~a" r))]))

(define/contract (ref->id ref)
  (vref? . -> . symbol?)
  (define-values (id _) (ref-split (unpack-value ref)))
  id)
