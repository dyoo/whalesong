#lang racket/base
(provide resource->url)

(require "structs.rkt"
         net/url)


;; resource->url: resource -> string
(define (resource->url r)
  (url->string (path->url (resource-path r))))
