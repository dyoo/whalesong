#lang typed/racket/base

(provide (all-defined-out))


(require "il-structs.rkt")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assembly

(define-struct: BasicBlock ([name : Symbol] 
                            [stmts : (Listof UnlabeledStatement)]) 
  #:transparent)
