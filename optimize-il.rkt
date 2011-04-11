#lang typed/racket/base
(require "il-structs.rkt")

(provide optimize-il)

;; perform optimizations on the intermediate language.
;;



(: optimize-il ((Listof Statement) -> (Listof Statement)))
(define (optimize-il statements)
  statements)
