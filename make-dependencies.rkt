#lang typed/racket/base

(require "compiler.rkt"
         "il-structs.rkt"
         "get-dependencies.rkt"
         "lexical-structs.rkt"
         "bootstrapped-primitives.rkt"
         "compiler-structs.rkt"
         "expression-structs.rkt"
         "parameters.rkt"
         "sets.rkt"
         racket/list
         racket/match)


(require/typed "parse-bytecode.rkt"
               [parse-bytecode (Any -> Expression)])

(require/typed "get-module-bytecode.rkt"
               [get-module-bytecode ((U String Path Input-Port) -> Bytes)])


(provide (all-defined-out))



(define-type Source (U OnlyStatements Any))


(define-struct: Configuration ([should-follow? : (Path -> Boolean)]
                               [before-first : (-> Void)]
                               [before-module-statements : ((U Expression #f)
                                                            (Listof Statement)
                                                            -> Void)]
                               [on-module-statements : ((U Expression #f)
                                                        (Listof Statement)
                                                        -> Void)]
                               [after-module-statements : ((U Expression #f)
                                                           (Listof Statement)
                                                           -> Void)]
                               [after-last : (-> Void)])
  #:mutable)

(define debug-configuration (make-Configuration
                             (lambda (p) #t)
                             (lambda () (void))
                             (lambda (ast stmt)
                               (void))
                             (lambda (ast stmt)
                               (when (and ast (expression-module-path ast))
                                 (printf "debug build configuration: visiting ~s\n"
                                         (expression-module-path ast))))
                             (lambda (ast stmt)
                               (void))
                             (lambda () (void))))




(: make/dependencies ((Listof Source) Configuration -> Void))
(define (make/dependencies sources config)
  (parameterize ([current-seen-unimplemented-kernel-primitives
                  ((inst new-seteq Symbol))])

    (match config
      [(struct Configuration (should-follow?
                              before-first
                              before-module-statements
                              on-module-statements
                              after-module-statements
                              after-last))

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
               (before-module-statements ast stmts)
               (on-module-statements ast stmts)
               (after-module-statements ast stmts)
               (loop (collect-new-dependencies ast (rest sources))))])))

       (before-first)
       (follow-dependencies sources)
       (after-last)])))



(define-struct: OnlyStatements ([code : (Listof Statement)]))


(: only-bootstrapped-code : OnlyStatements)
(define only-bootstrapped-code (make-OnlyStatements (get-bootstrapping-code)))



