#lang racket/base

(require rackunit)
(require marv/core/resources)
(require marv/core/values)

(require/expose marv/core/resources (deferred))

(define b1 (mk-resource 'main.bucket1 (hash) '() (hash 'name "buck1")))

(define deferred2 (deferred string-append '() "buck2" (ref 'main.bucket1 'name)))

(define b2 (mk-resource 'main.bucket2 (hash) '() (hash 'name deferred2)))

(define deferred3
  (deferred string-append '()
    (deferred string-append '() (ref 'main.bucket1 'name) (ref 'main.bucket2 'name))
    "-buck3"))
(define b3 (mk-resource 'main.bucket3 (hash) '() (hash 'name deferred3)))
(define b4 (mk-resource 'main.bucket4 (hash) '() (hash 'name "test" 'labels (hash 'label1 deferred3))))

(define all (hash 'main.bucket1 b1 'main.bucket2 b2 'main.bucket3 b3 'main.bucket4 b4))

(define (gbid id) (resource-config (hash-ref all id)))

(config-resolve (resource-config b1) gbid)
(config-resolve (resource-config b2) gbid)
(config-resolve (resource-config b3) gbid)
(config-resolve (resource-config b4) gbid)

;given: '#hash((labels . #hasheq((b1ent . (#s(_deferred #<procedure:string-append> (#s(ref main.bucket1.bucket storageClass) #s(ref main.bucket1.bucket name)) (("b1_" #s(_deferred #<procedure:.../alpha/expander.rkt:370:48> (#s(ref main.bucket1.bucket name) #s(r...
; TODO45 - might need a test for a top-level deferred, *if* we're going to allow that!