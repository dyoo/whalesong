#lang typed/racket/base

(require racket/list
         "il-structs.rkt"
         "lexical-structs.rkt"
         "sets.rkt")
(provide find-variable 
         extend-lexical-environment
         lexical-environment-pop-depth
         collect-lexical-references
         lexical-references->compile-time-environment)


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



(: extend-lexical-environment (CompileTimeEnvironment (U (Listof Symbol) Prefix) -> CompileTimeEnvironment))
;; Extends the lexical environment with procedure bindings.
(define (extend-lexical-environment cenv names)
  (cond [(Prefix? names)
         (cons names cenv)]
        [(list? names)
         (cons names cenv)]))




(: lexical-environment-pop-depth (CompileTimeEnvironment -> Natural))
;; Computes how many environments we need to pop till we clear the procedure arguments.
(define (lexical-environment-pop-depth cenv)
  (cond [(empty? cenv)
         (error 'lexical-environment-pop-depth "Empty environment")]
        [(Prefix? (first cenv))
         1]
        [(list? (first cenv))
         1]))




(: collect-lexical-references ((Listof LexicalAddress) 
                               -> 
                               (Listof (U EnvLexicalReference EnvWholePrefixReference))))
;; Given a list of lexical addresses, computes a set of unique references.
;; Multiple lexical addresses to a single prefix should be treated identically.
(define (collect-lexical-references addresses)
  (let: ([prefix-references : (Setof EnvWholePrefixReference) (new-set)]
         [lexical-references : (Setof EnvLexicalReference) (new-set)])
        (let: loop : (Listof (U EnvLexicalReference EnvWholePrefixReference)) 
              ([addresses : (Listof LexicalAddress) addresses])
              (cond 
                [(empty? addresses)
                 (append (set->list prefix-references) (set->list lexical-references))]
                [else
                 (let ([addr (first addresses)])
                   (cond
                     [(LocalAddress? addr)
                      (set-insert! lexical-references
                                   (make-EnvLexicalReference (LocalAddress-depth addr)
                                                             (LocalAddress-pos addr)))
                      (loop (rest addresses))]
                     [(PrefixAddress? addr)
                      (set-insert! prefix-references
                                   (make-EnvWholePrefixReference (PrefixAddress-depth addr)))
                      (loop (rest addresses))]))]))))


(: lexical-references->compile-time-environment ((Listof (U EnvLexicalReference EnvWholePrefixReference))
                                                 CompileTimeEnvironment
                                                 -> CompileTimeEnvironment))
(define (lexical-references->compile-time-environment refs cenv)
  cenv
  #;(cond
    [(empty? refs)
     cenv]
    [else
     (let ([a-ref (first refs)])
       (cond
         [(EnvLexicalReference? a-ref)
          ...]))]))

