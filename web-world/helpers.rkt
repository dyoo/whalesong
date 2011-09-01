#lang s-exp "../lang/base.rkt"

(provide fresh-id)


;; fresh-id: -> string
;; Returns a freshly generated id.
(define (fresh-id)
  (symbol->string (gensym 'fresh-web-world-id)))
