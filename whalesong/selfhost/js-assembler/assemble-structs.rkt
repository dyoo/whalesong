#lang whalesong (require "../selfhost-lang.rkt")
; #lang typed/racket/base

(provide (all-defined-out))


(require "../compiler/il-structs.rkt")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assembly

(define-struct: BasicBlock ([name : Symbol] 
                            [stmts : (Listof UnlabeledStatement)]) 
  #:transparent)



;; Represents a hashtable from symbols to basic blocks
(define-type Blockht (HashTable Symbol BasicBlock))
