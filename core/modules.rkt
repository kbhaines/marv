
#lang racket/base

(require racket/contract)
(require racket/string)
(require marv/core/config)

(provide (struct-out mmodule))


(struct mmodule (gid res-fn) #:prefab)
