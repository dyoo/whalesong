#lang typed/racket/base

(require "assemble-structs.rkt"
         "../compiler/il-structs.rkt"
         racket/list)

(require/typed "../logger.rkt" [log-debug (String -> Void)])


(provide optimize-basic-blocks)


(define-type Blockht (HashTable Symbol BasicBlock))


(: optimize-basic-blocks ((Listof BasicBlock) -> (Listof BasicBlock)))
(define (optimize-basic-blocks blocks)
  (let: ([blockht : Blockht (make-hasheq)])
    (for-each (lambda: ([b : BasicBlock])
                       (hash-set! blockht (BasicBlock-name b) b))
              blocks)

    (map (lambda: ([b : BasicBlock])
           (optimize-block b blockht))
         blocks)))




(: optimize-block (BasicBlock Blockht -> BasicBlock))
;; Simple optimization: optimize away single-statement goto blocks with their
;; immediate contents.
(define (optimize-block b blocks)
  (let ([stmts (BasicBlock-stmts b)])
    (cond
     [(= (length stmts) 1)
      (let ([first-stmt (first stmts)])
        (cond
         [(GotoStatement? first-stmt)
          (let ([target (GotoStatement-target first-stmt)])
            (cond
             [(Label? target)
              (log-debug (format "inlining basic block ~a" (BasicBlock-name b)))
              (optimize-block (make-BasicBlock (BasicBlock-name b)
                                                (BasicBlock-stmts
                                                 (hash-ref blocks (Label-name target))))
                               blocks)]
             [else
              b]))]
         [else
          b]))]
     [else
      b])))
