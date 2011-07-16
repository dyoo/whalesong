#lang typed/racket/base

(require "assemble-structs.rkt"
         "assemble-helpers.rkt"
         "assemble-open-coded.rkt"
         "assemble-expression.rkt"
         "assemble-perform-statement.rkt"
         "collect-jump-targets.rkt"
         "../compiler/il-structs.rkt"
         "../compiler/lexical-structs.rkt"
         "../compiler/expression-structs.rkt"
         "../helpers.rkt"
         racket/string
         racket/list)


(provide fracture)




;; fracture: (listof stmt) -> (listof basic-block)
(: fracture ((Listof Statement) -> (Listof BasicBlock)))
(define (fracture stmts)
  (let*: ([first-block-label : Symbol (if (and (not (empty? stmts))
                                               (symbol? (first stmts)))
                                          (first stmts)
                                          (make-label 'start))]
          [stmts : (Listof Statement) (if (and (not (empty? stmts))
                                               (symbol? (first stmts)))
                                          (rest stmts)
                                          stmts)]
          [jump-targets : (Listof Symbol)
                        (cons first-block-label (collect-general-jump-targets stmts))])
         (let: loop : (Listof BasicBlock)
               ([name : Symbol first-block-label]
                [acc : (Listof UnlabeledStatement) '()]
                [basic-blocks  : (Listof BasicBlock) '()]
                [stmts : (Listof Statement) stmts]
                [last-stmt-goto? : Boolean #f])
               (cond
                 [(null? stmts)
                  (reverse (cons (make-BasicBlock name (reverse acc))
                                 basic-blocks))]
                 [else
                  (let: ([first-stmt : Statement (car stmts)])
                        (: do-on-label (Symbol -> (Listof BasicBlock)))
                        (define (do-on-label label-name)
                          (cond
                            [(member label-name jump-targets)
                             (loop label-name
                                   '()
                                   (cons (make-BasicBlock 
                                          name  
                                          (if last-stmt-goto? 
                                              (reverse acc)
                                              (reverse (append `(,(make-GotoStatement (make-Label label-name)))
                                                               acc))))
                                         basic-blocks)
                                   (cdr stmts)
                                   last-stmt-goto?)]
                            [else
                             (loop name
                                   acc
                                   basic-blocks
                                   (cdr stmts)
                                   last-stmt-goto?)]))
                        (cond
                          [(symbol? first-stmt)
                           (do-on-label first-stmt)]
                          [(LinkedLabel? first-stmt)
                           (do-on-label (LinkedLabel-label first-stmt))]
                          [else
                           (loop name
                                 (cons first-stmt acc)
                                 basic-blocks
                                 (cdr stmts)
                                 (GotoStatement? (car stmts)))]))]))))
