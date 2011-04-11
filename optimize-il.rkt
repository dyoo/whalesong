#lang typed/racket/base
(require "il-structs.rkt"
         "lexical-structs.rkt"
         racket/list)

(provide optimize-il)

;; perform optimizations on the intermediate language.
;;



(: optimize-il ((Listof Statement) -> (Listof Statement)))
(define (optimize-il statements)
  ;; For now, replace pairs of PushEnvironment / AssignImmediate(0, ...)
  ;; We should do some more optimizations here, like peephole...
  (let loop ([statements statements])
    (cond
      [(empty? statements)
       empty]
      [else
       (let ([first-stmt (first statements)])
         (: default (-> (Listof Statement)))
         (define (default)
           (cons first-stmt
                 (loop (rest statements))))
         (cond
           [(empty? (rest statements))
            (default)]
           [else
            (let ([second-stmt (second statements)])
              (cond
                [(and (PushEnvironment? first-stmt)
                      (equal? first-stmt (make-PushEnvironment 1 #f))
                      (AssignImmediateStatement? second-stmt))
                 (let ([target (AssignImmediateStatement-target second-stmt)])
                   (cond
                     [(equal? target (make-EnvLexicalReference 0 #f))
                      (cons (make-PushImmediateOntoEnvironment 
                             (adjust-oparg-depth 
                              (AssignImmediateStatement-value second-stmt) -1)
                             #f)
                            (loop (rest (rest statements))))]
                     [else
                      (default)]))]
                [else
                 (default)]))]))])))
       



(: adjust-oparg-depth (OpArg Integer -> OpArg))
(define (adjust-oparg-depth oparg n)
  (cond
    [(Const? oparg) oparg]
    [(Label? oparg) oparg]
    [(Reg? oparg) oparg]
    [(EnvLexicalReference? oparg)
     (make-EnvLexicalReference (ensure-natural (+ n (EnvLexicalReference-depth oparg)))
                               (EnvLexicalReference-unbox? oparg))]
    [(EnvPrefixReference? oparg)
     (make-EnvPrefixReference (ensure-natural (+ n (EnvPrefixReference-depth oparg)))
                              (EnvPrefixReference-pos oparg))]
    [(EnvWholePrefixReference? oparg)
     (make-EnvWholePrefixReference (ensure-natural (+ n (EnvWholePrefixReference-depth oparg))))]
    [(SubtractArg? oparg)
     (make-SubtractArg (adjust-oparg-depth (SubtractArg-lhs oparg) n)
                       (adjust-oparg-depth (SubtractArg-rhs oparg) n))]))


(define-predicate natural? Natural)
(define (ensure-natural x)
  (if (natural? x)
      x
      (error 'ensure-natural)))