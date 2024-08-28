#lang racket/base

(require graph)
(require racket/contract)
(require racket/list)
(require racket/set)
(require marv/core/resources)
(require marv/core/resource-def)
(require marv/core/values)

(provide resources-dag-topo resources-dag resource-edges)

(define root '$root)

(define/contract (resources-dag-topo modl)
  (resource-set/c . -> . (listof res-id/c))
  (tsort (resources-dag modl)))

; TODO - don't need a full module?
(define/contract (resources-dag modl)
  (resource-set/c . -> . graph?)
  (define edges (append-map (lambda (k) (resource-edges k (resource-ref modl k))) (resource-keys modl)))
  (define graph (validated-graph edges modl))
  (remove-vertex! graph root)
  graph)

(define (validated-graph edges modl)
  (define graph (directed-graph edges))
  (define res-keys (set-add (list->set (resource-keys modl)) root))
  ; TODO - not right place to check/raise this?
  (when (not (dag? graph)) (raise "References are circular"))
  (define fails
    (filter-map
     (lambda (edge) (if (set-member? res-keys (first edge)) #f edge))
     (get-edges graph)))
  (if (empty? fails) graph
      (raise (format "Reference failures: ~a" fails))))

(define (resource-edges id res)
  (append (list(list root id))
          (map (lambda(dep) (list (ref-gid dep) id))
               (resource-deps res))))