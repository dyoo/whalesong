#lang racket/base

(require (prefix-in racket: racket/base)
         racket/runtime-path
         "compiler-structs.rkt"
         "compiler.rkt"
         "parse-bytecode-5.1.1.rkt"
         "language-namespace.rkt")


(provide parse run-compiler)

(define-runtime-path kernel-language-path
  "lang/kernel.rkt")


;; Use Racket's compiler, and then parse the resulting bytecode
;; to our own AST structures.
(define (parse stx)
  (parameterize ([current-namespace (lookup-language-namespace 
                                     `(file ,(path->string kernel-language-path))
                                     #;'racket/base)])
    (let ([bc (racket:compile stx)]
          [op (open-output-bytes)])
      (write bc op)
      (parse-bytecode 
       (open-input-bytes (get-output-bytes op))))))


(define (run-compiler code)
  (compile (parse code) 'val next-linkage/drop-multiple))