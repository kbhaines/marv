#lang racket/base

(require rackunit)
(require marv/core/resources)
(require marv/core/values)

(require/expose marv/core/resources (deferred))

(define restype (hash 'identity (lambda(cfg)cfg)))

(define b1 (mk-resource 'main.bucket1 restype '() (hash 'name "buck1")))

(define deferred2 (deferred string-append '() "buck2" (ref 'main.bucket1 'name)))
(define b2 (mk-resource 'main.bucket2 restype '() (hash 'name deferred2)))

(define deferred3
  (deferred string-append '()
    (deferred string-append '() (ref 'main.bucket1 'name) (ref 'main.bucket2 'name))
    "-buck3"))
(define b3 (mk-resource 'main.bucket3 restype '() (hash 'name deferred3)))

(define b4 (mk-resource 'main.bucket4 restype '() (hash 'name "test" 'labels (hash 'label1 deferred3))))

(define all (hash 'main.bucket1 b1 'main.bucket2 b2 'main.bucket3 b3 'main.bucket4 b4))
(define (gbid id) (resource-config (hash-ref all id)))

(check-equal? (config-resolve (resource-config b1) gbid) (hash 'name "buck1"))
(check-equal? (config-resolve (resource-config b2) gbid) (hash 'name "buck2buck1"))
(check-equal? (config-resolve (resource-config b3) gbid) (hash 'name "buck1buck2buck1-buck3"))
(check-equal? (config-resolve (resource-config b4) gbid)
              (hash 'name "test"
                    'labels (hash 'label1 "buck1buck2buck1-buck3")))