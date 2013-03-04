#lang s-exp syntax/module-reader
#:language (lambda (ip) `(file ,(path->string semantics-path)))
(require racket/runtime-path)
(define-runtime-path semantics-path "semantics.rkt")
