#lang typed/racket/base

(require racket/list
         "il-structs.rkt"
         "lexical-structs.rkt"
         "sets.rkt")
(provide find-variable 
         extend-lexical-environment
         extend-lexical-environment/names
         extend-lexical-environment/boxed-names
         extend-lexical-environment/placeholders
         collect-lexical-references
         lexical-references->compile-time-environment
         place-prefix-mask)


;; find-variable: symbol compile-time-environment -> lexical-address
;; Find where the variable should be located.
(: find-variable (Symbol CompileTimeEnvironment -> LexicalAddress))
(define (find-variable name cenv)
  (: find-pos (Symbol (Listof (U Symbol False)) -> Natural))
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
              [else
               (let: ([elt : CompileTimeEnvironmentEntry (first cenv)])
                 (cond
                   [(Prefix? elt)
                    (cond [(member name (Prefix-names elt))
                           (make-PrefixAddress depth (find-pos name (Prefix-names elt)) name)]
                          [else
                           (loop (rest cenv) (add1 depth))])]
                   
                   [(symbol? elt)
                    (cond
                      [(eq? elt name)
                       (make-LocalAddress depth #f)]
                      [else
                       (loop (rest cenv) (add1 depth))])]

                   [(box? elt)
                    (cond
                      [(eq? (unbox elt) name)
                       (make-LocalAddress depth #t)]
                      [else
                       (loop (rest cenv) (add1 depth))])]
                   
                   [(eq? elt #f)
                    (loop (rest cenv) (add1 depth))]))])))


(: list-index (All (A) A (Listof A) -> (U #f Natural)))
(define (list-index x l)
  (let: loop : (U #f Natural) ([i : Natural 0]
                               [l : (Listof A) l])
        (cond
          [(empty? l)
           #f]
          [(eq? x (first l))
           i]
          [else
           (loop (add1 i) (rest l))])))


(: extend-lexical-environment 
   (CompileTimeEnvironment CompileTimeEnvironmentEntry -> CompileTimeEnvironment))
;; Extends the lexical environment with procedure bindings.
(define (extend-lexical-environment cenv extension)
  (cons extension cenv))



(: extend-lexical-environment/names (CompileTimeEnvironment (Listof Symbol) -> CompileTimeEnvironment))
(define (extend-lexical-environment/names cenv names)
  (append names cenv))


(: extend-lexical-environment/boxed-names (CompileTimeEnvironment (Listof Symbol) -> CompileTimeEnvironment))
(define (extend-lexical-environment/boxed-names cenv names)
  (append (map (inst box Symbol) names) cenv))


(: extend-lexical-environment/placeholders
   (CompileTimeEnvironment Natural -> CompileTimeEnvironment))
;; Add placeholders to the lexical environment (This represents what happens during procedure application.)
(define (extend-lexical-environment/placeholders cenv n)
  (append (build-list n (lambda: ([i : Natural]) #f))
          cenv))
  



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
                                                             (LocalAddress-unbox? addr)))
                      (loop (rest addresses))]
                     [(PrefixAddress? addr)
                      (set-insert! prefix-references
                                   (make-EnvWholePrefixReference (PrefixAddress-depth addr)))
                      (loop (rest addresses))]))]))))



(: lexical-references->compile-time-environment ((Listof EnvReference) CompileTimeEnvironment CompileTimeEnvironment
                                                                     (Listof Symbol)
                                                                       -> CompileTimeEnvironment))
;; Creates a lexical environment containing the closure's bindings.
(define (lexical-references->compile-time-environment refs cenv new-cenv symbols-to-keep)
  (let: loop : CompileTimeEnvironment ([refs : (Listof EnvReference) (reverse refs)]
                                       [new-cenv : CompileTimeEnvironment new-cenv])
        (cond
          [(empty? refs)
           new-cenv]
          [else
           (let: ([a-ref : EnvReference (first refs)])
                 (cond
                   [(EnvLexicalReference? a-ref)
                    (loop (rest refs)
                          (cons (list-ref cenv (EnvLexicalReference-depth a-ref))
                                new-cenv))]
                   [(EnvWholePrefixReference? a-ref)
                    (loop (rest refs)
                          (cons (place-prefix-mask 
                                 (ensure-Prefix (list-ref cenv (EnvWholePrefixReference-depth a-ref)))
                                 symbols-to-keep)
                                new-cenv))]))])))

(: ensure-Prefix (Any -> Prefix))
(define (ensure-Prefix x)
  (if (Prefix? x)
      x
      (error 'ensure-Prefix "~s" x)))



(: place-prefix-mask (Prefix (Listof Symbol) -> Prefix))
;; Masks elements of the prefix off.
(define (place-prefix-mask a-prefix symbols-to-keep)
  (make-Prefix
   (map (lambda: ([n : (U Symbol False)])
                 (cond [(symbol? n)
                        (if (member n symbols-to-keep)
                            n
                            #f)]
                       [else n]))
        (Prefix-names a-prefix))))