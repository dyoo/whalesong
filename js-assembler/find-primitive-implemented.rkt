#lang racket/base

(require racket/runtime-path
         racket/list
         (for-syntax racket/base)
         "../compiler/arity-structs.rkt")

;; Provides a list of symbols of the function implemented primitively.  Knowing
;; this allows us to do certain procedure applications more efficiently without
;; touching the stack so much.
(provide primitive-ids)

(define a-regexp 
  #px"installPrimitiveProcedure\\s*\\(\\s*['\"]([^'\"]+)['\"]\\s*,\\s*([^\n]+)\n")

(define-runtime-path baselib-primitives.js
  (build-path "runtime-src" "baselib-primitives.js"))

(define ip (open-input-file baselib-primitives.js))

(define (parse-arity-string s)
  (define arity
    (let loop ([s s])
      (let ([s (regexp-replace #px",\\s+$" s "")])
        (cond
          [(regexp-match #px"^(\\d+)" s)
           =>
           (lambda (m) (string->number (second m)))]
          [(regexp-match #px"^makeList\\((.+)\\)" s)
           =>
           (lambda (m) 
             (map string->number (regexp-split #px"\\s*,\\s*" (second m))))]
          [(regexp-match #px"^baselib.arity.makeArityAtLeast\\((\\d+)\\)" s)
           =>
           (lambda (m) 
             (ArityAtLeast (string->number (second m))))]
          [else
           (error 'parse-arity-string "How to parse? ~e" s)]))))
  arity)
  
(define primitive-ids
  (let loop ()
    (let ([a-match (regexp-match a-regexp ip)])
      (cond
        [a-match => (lambda (a-match)
                      (define name (second a-match))
                      (define arity-string (bytes->string/utf-8 (third a-match)))
                      (define arity (parse-arity-string arity-string))
                      (cons (cons (string->symbol (bytes->string/utf-8 name)) arity)
                            (loop)))]
        [else empty]))))