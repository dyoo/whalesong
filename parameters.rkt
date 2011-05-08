#lang typed/racket/base

(require "expression-structs.rkt")
(provide current-defined-name)

(: current-defined-name (Parameterof (U Symbol LamPositionalName)))
(define current-defined-name (make-parameter 'unknown))