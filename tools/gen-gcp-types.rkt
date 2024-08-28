#lang racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/list)
(require marv/drivers/gcp/discovery)
(require racket/match)
(require marv/core/globals)

(define compute  (load-discovery "gcp" "compute:beta"))
(define iam  (load-discovery "gcp" "iam:v1"))
(define storage  (load-discovery "gcp" "storage:v1"))
(define secretmanager  (load-discovery "gcp" "secretmanager:v1"))
(define sql  (load-discovery "gcp" "sqladmin:v1"))

; (get-discovery-resources secretmanager)
; (api-by-resource-path secretmanager "/resources/projects/resources/secrets" "create")

(define (gen-all)

  (define (gen-auto-file disc api-name create read update delete)
    (define (schemas) (gen-schemas disc))
    (define (gen) (gen-types disc api-name create read update delete))
    (define (shim) (gen-shim disc api-name))
    (with-output-to-file #:exists 'replace (format "types/gcp/_auto/~a-schema.mrv" api-name) schemas)
    (with-output-to-file #:exists 'replace (format "types/gcp/_auto/~a.mrv" api-name) gen)
    (with-output-to-file #:exists 'replace (format "types/gcp/~a-shim.mrv" api-name) shim))

  (gen-auto-file storage "storage" "insert" "get" "patch" "delete")
  (gen-auto-file compute "compute" "insert" "get" "patch" "delete")
  (gen-auto-file iam "iam" "create" "get" "patch" "delete")
  (gen-auto-file secretmanager "secretmanager" "create" "get" "patch" "delete")
  (gen-auto-file sql "sql" "insert" "get" "patch" "delete")
  )

(define (gen-schemas disc)
  (displayln autogen-header)
  (for ([type (disc-schemas disc)])
    (define schema (get-disc-schema disc type))
    (define properties (hash-keys (hash-ref schema 'properties)))
    (displayln (format schema-template type type  (symbols->string properties ","))))
  (displayln "schemas={")
  (define schemas
    (map
     (lambda(type) (format "  ~a=~a" type type))
     (disc-schemas disc)))
  (displayln (string-join schemas ",\n"))
  (displayln "}\nexport schemas"))

(define res-spec-verbs
  (hash
   "/resources/projects/resources/secrets/resources/versions"
   '("/resources/projects/resources/secrets:addVersion"
     ":access"
     "/resources/projects/resources/secrets:addVersion"
     ":destroy")
   ))

(define (gen-types disc api-id api-create api-read api-update api-delete)
  (define types (get-discovery-resources disc))
  (displayln autogen-header)
  (displayln (format types-header api-id api-id))
  (for ([res-path (in-list types)])
    ; TODO- filter out empty apis

    (define (parse-verb->api v)
      (match (string-split v ":")
        [(list path method) (api-by-resource-path disc path method)]
        [(list method) (api-by-resource-path disc res-path method)]))

    (define (api-spec verb)
      (define api (parse-verb->api verb))
      (define (request-clause)
        (let ((request-type (api-request-type api)))
          (if (string? request-type) (format "request-body=schemas.~a(cfg)|" request-type) "" )))

      (define spec
        (cond
          [(disc-api? api)
           (format "api-id=API-ID|~aresponse-type=\"~a.schemas.~a\"|method=\"~a\"|url=\"~a\"|required=[\"~a\"]"
                   (request-clause) api-id (api-response-type api) (api-http-method api)
                   (api-resource-url-base api)
                   (map symbol->string (api-required-params api)))]
          [else ""]))
      (string-replace spec "|" ",\n   "))

    (define type (substring (string-replace res-path "/resources/" "_") 1))
    (define-values (create read update delete)
      (apply values (hash-ref res-spec-verbs res-path (list api-create api-read api-update api-delete))))
    (define t (hash 'type type 'res-path res-path 'api api-id
                    'create (api-spec create)
                    'read (api-spec read)
                    'update (api-spec update)
                    'delete (api-spec delete)))
    (displayln (hash-format-string t type-template))))

(define (hash-format-string hs str)
  (define var-regex #rx"\\{\\{[^\\}]+\\}\\}")
  (define (replace-for str)
    (format "~a" (hash-ref hs (string->symbol (substring str 2 (- (string-length str) 2))))))
  (regexp-replace* var-regex str replace-for))

(define (singularise str)
  (cond [(string-suffix? str "ies") (string-append (string-trim str "ies") "y")]
        [else (string-trim #:left? #f str "s")]))

(define (gen-shim disc api-name)
  (define types (get-discovery-resources disc))
  (displayln (format shim-header api-name))
  (for ([t (in-list types)])
    (define auto-type (substring (string-replace t "/resources/" "_") 1))
    (define sgl (singularise (last (string-split auto-type "_"))))
    (define tpl (hash 'type sgl 'auto-type auto-type ))
    (displayln (hash-format-string tpl shim-template))))

(define shim-header #<<EOF
#lang marv

# WARNING: This shim file is incomplete. You need to define an 'API' type
# relevant to the API being imported, and then remove this warning.

import types/gcp/_auto/~a as _auto

# Modify this example per API

type API<T> = {
 post-create(original, state) = state <- { project=original.project  region=original.region }
 post-read(o, cfg) = post-create(o, cfg)
 post-update(original, cfg)=post-create(original, cfg)
 delete(state) = T.delete(state) <- { config={name=state.name} }
 identity(cfg) = cfg
 * = T.*
}
EOF
  )

(define shim-template #<<EOF

type {{type}}=API<_auto:{{auto-type}}>
export {{type}}
EOF
  )

(define autogen-header #<<EOF
#lang marv
## AUTO-GENERATED FILE - DO NOT EDIT!

EOF
  )

(define types-header #<<EOF
import types/gcp/_auto/~a-schema
API-ID="~a"
EOF
  )

(define type-template #<<EOF
# {{res-path}}
type {{type}} = {
 origin(cfg)= {
  driver="gcp",
  type="gcp:{{api}}:{{res-path}}"
 }
 identity(cfg) = cfg
 create(cfg)={
  config=cfg,
  api={
   {{create}}
  }
 }
 read(cfg)={
  config=cfg,
  api={
   {{read}}
  }
 }
 update(cfg)={
  config=cfg,
  api={
   {{update}}
  }
 }
 delete(cfg)={
  config=cfg,
  api={
   {{delete}}
  }
 }
}
export {{type}}

EOF
  )

(define schema-template #<<EOF

# Schema for ~a
~a(c)=c << [~a]
EOF
  )
