#lang typed/racket/base

(require "compiler.rkt"
         "il-structs.rkt"
         "get-dependencies.rkt"
         "lexical-structs.rkt"
         "bootstrapped-primitives.rkt"
         "compiler-structs.rkt"
         "expression-structs.rkt"
         racket/list)


(require/typed "parse-bytecode.rkt"
               [parse-bytecode (Any -> Expression)])

(require/typed "get-module-bytecode.rkt"
               [get-module-bytecode ((U String Path Input-Port) -> Bytes)])


(provide make/dependencies
         Source
         OnlyStatements
         only-bootstrapped-code)


(define-type Source (U OnlyStatements Any))



(: make/dependencies ((Listof Source)
                      (Path -> Boolean)
                      (-> Void)
                      ((U Expression #f) (Listof Statement) -> Void)
                      ((U Expression #f) (Listof Statement) -> Void)
                      ((U Expression #f) (Listof Statement) -> Void)
                      (-> Void)
                      -> Void))
(define (make/dependencies sources
                           should-follow?

                           before-first

                           before-element
                           on-element
                           after-element

                           after-last)
  (: follow-dependencies ((Listof Source) -> Void))
  (define (follow-dependencies sources)
    (define visited ((inst make-hash Any Boolean)))

    (: collect-new-dependencies
     ((U False Expression) (Listof Source) -> (Listof Source)))
    (define (collect-new-dependencies ast sources)
      (cond
       [(eq? ast #f)
        sources]
       [else
        (let* ([dependent-module-names (get-dependencies ast)]
               [paths
                (foldl (lambda: ([mp : ModuleName]
                                 [acc : (Listof Source)])
                         (let ([rp [ModuleName-real-path mp]])
                           
                           (cond [(and (path? rp)
                                       (should-follow? rp)
                                  (cons rp acc))]
                                 [else
                                  acc])))
                       '()
                       dependent-module-names)])
          (append paths sources))]))
    
    (let: loop : Void ([sources : (Listof Source) sources])
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
  (follow-dependencies sources)
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
