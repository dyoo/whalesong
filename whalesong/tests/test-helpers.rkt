#lang racket/base

(require (prefix-in racket: racket/base)
         racket/runtime-path
         "../compiler/compiler-structs.rkt"
         "../compiler/compiler.rkt"
         "../parser/parse-bytecode.rkt"
         "../get-module-bytecode.rkt"
         "../language-namespace.rkt")



(provide parse parse-module run-compiler)

(define-runtime-path kernel-language-path
  "../lang/kernel.rkt")


;; Use Racket's compiler, and then parse the resulting bytecode
;; to our own AST structures.
(define (parse stx)
  (parameterize ([current-namespace (lookup-language-namespace 
                                     `(file ,(path->string kernel-language-path))
                                     #;'racket/base)]
                 ;; We want to disable some optimizations for the moment.
                 ;; See: http://docs.racket-lang.org/drracket/module.html
                 [compile-context-preservation-enabled #t])

    (let ([bc (racket:compile stx)]
          [op (open-output-bytes)])
      (write bc op)
      (parse-bytecode 
       (open-input-bytes (get-output-bytes op))))))


(define (parse-module x)
  (parse-bytecode (open-input-bytes (get-module-bytecode x))))


(define (run-compiler code)
  (compile (parse code) 'val next-linkage/drop-multiple))