#lang racket/base

(require racket/set)
(require racket/dict)
(require racket/hash)
(require racket/match)
(require racket/contract)

(require marv/core/values)
(require marv/core/state)
(require marv/core/diff)
(require marv/core/config)
(require marv/core/resource-def)
(require marv/core/resources)
(require marv/core/graph)
(require marv/utils/hash)
(require marv/core/drivers)
(require marv/drivers/types)
(require marv/log)

(provide import-resources
         plan-changes
         apply-changes
         output-plan
         (struct-out plan))

(struct plan (resources desired-state ordered-operations) #:prefab)

(struct op-base (reason) #:transparent)
(struct op-create(op-base) #:transparent)
(struct op-replace(op-base diff) #:transparent)
(struct op-update(op-base diff) #:transparent)
(struct op-delete(op-base) #:transparent)

(define (plan-changes mod (refresh? #t))
  (get-plan-for mod refresh?))

(define (resource-origin res)
  ((resource-type-fn res) 'origin (resource-config res)))

(define/contract (get-plan-for rsset refresh?)
  (resource-set/c boolean? . -> . plan?)
  (define resource-keyset (list->set (resource-keys rsset)))
  ; TODO - pass in state, also define hash(id->state) contract types
  (define state-keyset (list->set (state-keys)))

  (define (filter-unstable-origins set-ids)
    (define (stable? id)
      (log-marv-debug "  unstable check: ~a <-> ~a" (resource-origin (resource-ref rsset id)) (state-ref-origin id))
      (equal? (resource-origin (resource-ref rsset id)) (state-ref-origin id)))
    (filter stable? (set->list set-ids)))

  (define to-create (set-subtract resource-keyset state-keyset))
  (define to-delete (set-subtract state-keyset resource-keyset))
  (define to-refresh (set-subtract resource-keyset to-delete to-create))

  (log-marv-debug "stable-origins: ~a -> ~a" to-refresh (filter-unstable-origins to-refresh))
  (when refresh? (refresh-resources rsset (filter-unstable-origins to-refresh)))

  (define current-dfs (mk-state-dfh (state-get-state-set)))
  (define new-dfs (merge-dfs current-dfs (mk-resource-dfh rsset)))
  (define ordered-rks (resources-dag-topo rsset))
  (define operations
    (foldl
     (lambda (id acc-ops)
       ; TODO - pass in state, also define hash(id->state) contract types
       (hash-set acc-ops id (operation id current-dfs new-dfs acc-ops))) (hash) ordered-rks))

  (define ordered-ops
    (append
     (map (lambda (rk) (cons rk (op-delete "Removed from definition")))
          (sort (set->list to-delete) > #:key state-ref-serial))
     (map (lambda (rk) (cons rk (hash-ref operations rk)))
          ordered-rks)))

  (plan rsset new-dfs ordered-ops))

; TODO - fix import
(define (import-resources mod ids)
  (raise "unimplemented"))
; (define (import-one id)
;    (displayln (format "importing ~a" id))
;    (define res
;      (unwrap-values
;       (deref-resource
;        (rmodule (rmodule-drivers mod) (mk-id->state)) (resource-ref mod id))))
;    (define driver-id (resource-driver-id res))
;    (define config ((current-driver) driver-id 'read (resource-config res)))
;    (state-set-ref id (resource driver-id config)))

; (map (lambda(id) (null id)) ids))

(define (get-driver-id type-fn res)
  (string->symbol (hash-ref (type-fn 'origin (resource-config res)) 'driver )))

(define (refresh-resources rsset ids)
  (define (refresh k)
    (displayln (format "Refreshing ~a" k))

    (define res (resource-ref rsset k))
    (define type-fn (resource-type-fn res))
    (define driver-id (string->symbol(state-entry-driver (state-ref k))))
    (define cmd (type-fn 'read (state-ref-config k)))
    (define reply-cfg (send-to-driver driver-id cmd))
    (define state-cfg (type-fn 'post-read (state-ref-config k) reply-cfg))
    (state-set-ref k (state-ref-origin k) (state-ref-destructor k) state-cfg))
  (for ([i ids]) (refresh i)))

(define/contract (send-driver-cmd driver-id cmd)
  (driver-id/c driver-cmd/c . -> . driver-resp/c)
  (define resp-cfg (send-to-driver driver-id cmd))
  resp-cfg)

(define/contract (deref-config cfg)
  (config/c . -> . config/c)
  (config-resolve cfg state-ref-config))

(define (apply-changes mod (refresh? #t))

  (define (create id)
    (display (format "CREATING ~a" id))
    (flush-output)
    (define res (deref-resource id))
    (define type-fn (resource-type-fn res))
    (define res-cfg (resource-config res))
    (define driver-id (get-driver-id type-fn res))
    (define cmd (type-fn 'create res-cfg))
    (define reply-cfg (send-driver-cmd driver-id cmd))
    (log-marv-debug "  creation reply: ~a" reply-cfg)

    (define origin (type-fn 'origin (resource-config res)))
    (log-marv-debug "  origin: ~a" origin)

    (define state-cfg (type-fn 'post-create res-cfg reply-cfg))
    (define delete-cmd (type-fn 'delete reply-cfg))
    (state-set-ref id origin delete-cmd state-cfg))

  (define (delete id)
    (display (format "DELETING ~a" id))
    (flush-output)
    (define delete-cmd (state-ref-destructor id))
    (define driver-id (string->symbol (state-entry-driver (state-ref id))))
    (send-to-driver driver-id delete-cmd)
    (state-delete id))

  (define (update id)
    (display (format "UPDATING ~a" id))
    (flush-output)
    (define res (deref-resource id))
    (define type-fn (resource-type-fn res))
    (define res-cfg (resource-config res))
    (define driver-id (get-driver-id type-fn res))
    (define cmd (type-fn 'update res-cfg))
    (define reply-cfg (send-driver-cmd driver-id cmd))
    (define state-cfg (type-fn 'post-update res-cfg reply-cfg))
    (state-set-ref id (state-ref-origin id) (state-ref-destructor id) state-cfg))

  (define (deref-resource id)
    (define res (hash-ref mod id))
    (update-resource-config res (unwrap-values (deref-config (resource-config res)))))

  (define plan (get-plan-for mod refresh?))

  (define (delete-replacements res-op)
    (match res-op
      [(cons id (op-replace _ _)) (delete id)]
      [_ void]))

  (define (process-op res-op)
    (match res-op
      [(cons _ #f) void]
      [(cons id (op-create _)) (create id)]
      [(cons id (op-replace _ _)) (create id)]
      [(cons id (op-update _ _)) (update id)]
      [(cons id (op-delete _)) (delete id)]
      [_ (raise (format "Illegal op: ~a" res-op))]
      ))

  (for-each delete-replacements (reverse(plan-ordered-operations plan)))
  (for-each process-op (plan-ordered-operations plan)))

(define (has-immutable-ref-to-replaced-resource? new acc-ops)
  (define (match? _ v)
    (and (iref? v) (op-replace? (hash-ref acc-ops (ref-gid (unpack-value v))))))
  (match-resource-attr? new match?))

(define (has-immutable-ref-to-updated-attr? new acc-ops) (ref-updated-attr? iref? new acc-ops))
(define (has-ref-to-updated-attr? new acc-ops)(ref-updated-attr? ref? new acc-ops))

(define (ref-updated-attr? test-fn? new acc-ops)
  (define (is-ref-a-diff? ref)
    (diff? (get-ref-diff ref acc-ops)))
  (define (match? _ v)
    (and (test-fn? v) (is-ref-a-diff? v)))
  (match-resource-attr? new match?))

(define (get-ref-diff ref acc-ops)
  ; (define-values (id attr) (ref-split ref))
  (define uref (unpack-value ref))
  (define id (ref-gid uref))
  (define attr (ref-path uref))
  (match (hash-ref acc-ops id)
    [(op-update _ diff) (hash-ref diff attr #f)]
    [(op-replace _ diff) (hash-ref diff attr #f)]
    [_ #f]))

(define (diff-resources acc-ops old-cfg new-cfg)
  (define munged (deref-config new-cfg))
  (log-marv-debug "deref'd resource: ~a" munged)
  (define (flathash h) (make-immutable-hasheq (hash->flatlist h)))
  (define diff (make-diff (hash-merge (flathash munged)
                                      (flathash old-cfg)) (flathash old-cfg)))
  (cond
    [(has-immutable-diff? diff) (op-replace "immutable diff" diff)]
    [(has-immutable-ref-to-replaced-resource? new-cfg acc-ops)
     (op-replace "immutable ref to replaced resource" diff)]
    [(has-immutable-ref-to-updated-attr? new-cfg acc-ops)
     (op-replace "immutable ref to updated attribute" diff)]
    [(has-ref-to-updated-attr? new-cfg acc-ops) (op-update "ref to updated attribute" diff)]
    [(has-diff? diff) (op-update "updated attribute" diff)]
    [else #f]))

(define (operation id current-dfs new-dfs acc-ops)
  (log-marv-debug "Checking diffs for ~a" id)
  (define in-current-state? (hash-has-key? current-dfs id))
  (define stable-origin?
    (and in-current-state?
         (equal? (df-origin (hash-ref current-dfs id))
                 (df-origin (hash-ref new-dfs id)))))
  (cond [(and in-current-state? (not stable-origin?))
         (op-replace "origin changed" (hash))]
        [in-current-state?
         (define current-cfg (df-config (hash-ref current-dfs id)))
         (define new-cfg (df-config (hash-ref new-dfs id)))
         (log-marv-debug " current-state: ~a" current-cfg)
         (log-marv-debug " new-state: ~a" new-cfg)
         (define diff (diff-resources acc-ops current-cfg new-cfg))
         (log-marv-debug " diff: ~a" diff)
         diff]
        [else (op-create "New resource!")]))

; Functions for helping with the diff'ing process. df=diff, dfh=diff hash.
; For diff purposes, we need origin and configuration, the 'df' construct is a
; pair where df = (cons origin config)

(define dfh/c (hash/c res-id/c pair?))
(define empty-df (cons (hash) (hash)))
(define df cons)
(define (state-entry->df s) (df (state-entry-origin s) (state-entry-config s)))
(define (resource->df r) (df (resource-origin r) (resource-config r)))
(define (df-origin r) (car r))
(define (df-config r) (cdr r))
(define (mk-state-dfh state) (hash-map/copy state (lambda(k v) (values k (state-entry->df v)))))
(define (mk-resource-dfh rs) (hash-map/copy rs (lambda(k v) (values k (resource->df v)))))

(define/contract (merge-dfs current-state new-resources)
  (dfh/c dfh/c . -> . dfh/c)

  (define/contract (merge old new)
    (pair? pair? . -> . pair?)

    (define (combine-hash s r) (hash-union s r #:combine merge-em))

    ; TODO - not sure if this is desired, e.g. can't empty a set of labels
    (define (merge-em s r)
      (cond [(and (hash? r) (hash? s)) (combine-hash s r)]
            [else r]))

    (define oldcfg (df-config old))
    (define newcfg (df-config new))

    (define new-conf (if (equal? empty-df oldcfg) newcfg (combine-hash oldcfg newcfg)))
    (df (df-origin new) new-conf))

  (make-immutable-hash
   (map
    (lambda(rk)
      (cons rk (merge
                (hash-ref current-state rk empty-df)
                (hash-ref new-resources rk))))
    (hash-keys new-resources))))



(define (output-plan plan)
  (define (report-op rid op)

    (define (fmt-diff d)
      (match d
        [(changed new old) (format "~a -> ~a" (unpack-value old) (unpack-value new))]
        [(added new) (format "~a (added)" (unpack-value new))]
        [(removed old) (format "~a (removed)" (unpack-value old))]))

    (define (report-diff d)
      (define diffs (filter (lambda (p) (diff? (cdr p))) (hash->flatlist d)))
      (dict-map diffs
                (lambda (k v)
                  (displayln (format "  ~a: ~a" k (fmt-diff v))))))

    (define (header id msg) (displayln (format "~a -> ~a" id msg)))

    (match op
      [(op-create reason) (header rid reason )]
      [(op-delete reason) (header rid reason)]
      [(op-update reason diff) (header rid reason)
                               (report-diff  diff)]
      [(op-replace reason diff) (header rid (format "REPLACE: ~a" reason))
                                (report-diff diff)]
      [_ void]
      ))

  (for-each
   (lambda (op) (report-op (car op) (cdr op)))
   (plan-ordered-operations plan)))
