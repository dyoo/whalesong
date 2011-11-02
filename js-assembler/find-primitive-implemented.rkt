#lang racket/base

(require racket/runtime-path
         racket/list
         (for-syntax racket/base))

;; Provides a list of symbols of the function implemented primitively.  Knowing
;; this allows us to do certain procedure applications more efficiently without
;; touching the stack so much.
(provide primitive-ids)

(define a-regexp 
  #px"installPrimitiveProcedure\\s*\\(\\s*['\"]([^'\"]+)['\"]")

(define-runtime-path baselib-primitives.js
  (build-path "runtime-src" "baselib-primitives.js"))

(define ip (open-input-file baselib-primitives.js))

(define primitive-ids
  (let loop ()
    (let ([a-match (regexp-match a-regexp ip)])
      (cond
        [a-match => (lambda (a-match)
                      (cons (string->symbol (bytes->string/utf-8 (second a-match)))
                            (loop)))]
        [else empty]))))