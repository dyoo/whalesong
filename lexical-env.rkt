#lang typed/racket/base

(require racket/list
         "typed-structs.rkt")
(provide find-variable extend-lexical-environment)


;; find-variable: symbol compile-time-environment -> lexical-address
;; Find where the variable should be located.
(: find-variable (Symbol CompileTimeEnvironment -> LexicalAddress))
(define (find-variable name cenv)
  (: find-pos (Symbol (Listof Symbol) -> Natural))
  (define (find-pos sym los)
    (cond
      [(eq? sym (car los))
       0]
      [else
       (add1 (find-pos sym (cdr los)))]))
  (let: loop : LexicalAddress ([cenv : CompileTimeEnvironment cenv]
                               [depth : Natural 0])
        (cond [(empty? cenv)
               (error 'find-variable "Unable to find ~s in the environment" name)]
              [(Prefix? (first cenv))
               (cond [(member name (Prefix-names (first cenv)))
                      (make-PrefixAddress depth (find-pos name (Prefix-names (first cenv))) name)]
                     [else
                      (loop (rest cenv) (add1 depth))])]
              [(member name (first cenv))
               (make-LocalAddress depth (find-pos name (first cenv)))]
              [else
               (loop (rest cenv) (add1 depth))])))



;; extend-lexical-environment: lexical-environment (listof symbol) -> lexical-envrionment
(: extend-lexical-environment (CompileTimeEnvironment (Listof Symbol) -> CompileTimeEnvironment))
(define (extend-lexical-environment cenv names)
  (cons names cenv))