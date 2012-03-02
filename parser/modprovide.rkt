#lang racket/base
(require racket/match
         "../compiler/expression-structs.rkt")

(provide get-provided-names)

;; get-provided-names: bytecode -> (listof ModuleProvide)
(define (get-provided-names bytecode)
  (match bytecode
    [(struct Top [_ (struct Module (name path prefix requires provides code))])
     provides]
    [else
     '()]))
