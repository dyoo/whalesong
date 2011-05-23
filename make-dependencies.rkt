#lang typed/racket/base

(require "get-dependencies.rkt"
         "lexical-structs.rkt"
         "bootstrapped-primitives.rkt"
         "parse-bytecode.rkt"
         "get-module-bytecode.rkt"
         racket/list)
(provide make/dependencies
         Source
         OnlyStatements
         only-bootstrapped-code)


(define-type Source (U OnlyStatements Any))



(: make/dependencies ((Listof Source)
                      #:should-follow? (Path -> Boolean)
                      #:before-first (-> Void)
                      #:before-element ((U Expression #f) (Listof Statement) -> Void)
                      #:on-element ((U Expression #f) (Listof Statement) -> Void)
                      #:after-element ((U Expression #f) (Listof Statement) -> Void)
                      ->
                      Void))
(define (make/dependencies sources
                           #:should-follow? should-follow?

                           #:before-first before-first

                           #:before-element before-element
                           #:on-element on-element
                           #:after-element after-element

                           #:after-last after-last)

  (define (follow-dependencies sources)
    (define visited (make-hash))

    (define (collect-new-dependencies ast sources)
      (cond
       [(eq? ast #f)
        sources]
       [else
        (let* ([dependent-module-names (get-dependencies ast)]
               [paths
                (map ModuleName-real-path
                     (filter (lambda (mp) (and (path? (ModuleName-real-path mp))
                                               (should-follow?
                                                (path? (ModuleName-real-path mp)))))
                             dependent-module-names))])
          (append paths sources))]))
    
    (let loop ([sources sources])
      (cond
       [(empty? sources)
        (after-last)]
       [(hash-has-key? visited (first sources))
        (loop (rest sources))]
       [else
        (hash-set! visited (first sources) #t)
        (let-values ([(ast stmts)
                      (get-ast-and-statements (first sources))])
          (before-element ast stmts)
          (on-element ast stmts)
          (after-element ast stmts)
          (loop (collect-new-dependencies ast (rest sources))))])))


  (before-first)
  (follow-dependencies sources should-follow?)
  (after-last))



(define-struct: OnlyStatements ([code : (Listof Statement)]))


(: only-bootstrapped-code : OnlyStatements)
(define only-bootstrapped-code (make-OnlyStatements (get-bootstrapping-code)))



(: get-ast-and-statements (Source -> (values (U False Expression)
                                             (Listof Statement))))
(define (get-ast-and-statements source-code)
  (cond
   [(OnlyStatements? source-code)
    (values #f (get-bootstrapping-code))]
   [else
    (let ([ast
           (cond
            [(path? source-code)
             (parse-bytecode source-code)]
            [else
             (let ([source-code-op (open-output-bytes)])
               (write source-code source-code-op)
               (parse-bytecode
                (open-input-bytes
                 (get-module-bytecode
                  (open-input-bytes
                   (get-output-bytes source-code-op))))))])])
      (values ast
              (compile ast 'val next-linkage/drop-multiple)))]))
