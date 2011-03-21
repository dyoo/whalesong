#lang typed/racket/base

(provide current-defined-name)

(: current-defined-name (Parameterof (U Symbol False)))
(define current-defined-name (make-parameter #f))