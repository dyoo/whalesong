#lang racket/base
(provide resource->url)

(require "structs.rkt"
         net/url)



(define (resource->url r)
  (path->url (resource-path r)))
