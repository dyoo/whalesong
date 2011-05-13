#lang racket/base

(require (prefix-in racket: racket/base)
         "compiler-structs.rkt"
         "compiler.rkt"
         "parse-bytecode-5.1.1.rkt"
         "language-namespace.rkt")


(provide parse run-compiler)


;; Use Racket's compiler, and then parse the resulting bytecode
;; to our own AST structures.
(define (parse stx)
  (parameterize ([current-namespace (lookup-language-namespace 'racket/base)])
    (let ([bc (racket:compile stx)]
          [op (open-output-bytes)])
      (write bc op)
      (parse-bytecode 
       (open-input-bytes (get-output-bytes op))))))


(define (run-compiler code)
  (compile (parse code) 'val next-linkage/drop-multiple))