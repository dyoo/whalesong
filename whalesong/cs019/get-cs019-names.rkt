#lang racket/base

;; Grabs all the names exported by the real cs019 language, so we can
;; compare and see what names are missing from our implementation.
(require racket/set)

(provide cs019-names
         whalesong-cs019-names
         missing-cs019-names)


(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))


(require (prefix-in cs019: (planet cs019/cs019/cs019)))
(define cs019-names 
  (for/set ([name (namespace-mapped-symbols ns)]
             #:when (regexp-match #rx"^cs019:" (symbol->string name)))
    (string->symbol
     (substring (symbol->string name) (string-length "cs019:")))))


(require (prefix-in whalesong-cs019: "cs019.rkt"))
(define whalesong-cs019-names 
  (for/set ([name (namespace-mapped-symbols ns)]
             #:when (regexp-match #rx"^whalesong-cs019:" (symbol->string name)))
    (string->symbol
     (substring (symbol->string name) (string-length "whalesong-cs019:")))))


(define missing-cs019-names
  (set-subtract cs019-names whalesong-cs019-names))