#lang racket/base

(require "expression-structs.rkt"
         racket/path)

(provide current-defined-name
         current-module-path
         current-root-path)



;(: current-defined-name (Parameterof (U Symbol LamPositionalName)))
(define current-defined-name (make-parameter 'unknown))


;(: current-module-path (Parameterof (U False Path)))
(define current-module-path 
  (make-parameter (build-path (current-directory) "anonymous-module.rkt")))


;(: current-root-path (Parameterof Path))
(define current-root-path
  (make-parameter (normalize-path (current-directory))))
