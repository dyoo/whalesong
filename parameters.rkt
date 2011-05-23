#lang typed/racket/base

(require "expression-structs.rkt"
         racket/path)

(provide current-defined-name
         current-module-path
         current-root-path
         current-warn-unimplemented-kernel-primitive)



(: current-module-path (Parameterof (U False Path)))
(define current-module-path 
  (make-parameter (build-path (current-directory) "anonymous-module.rkt")))


(: current-root-path (Parameterof Path))
(define current-root-path
  (make-parameter (normalize-path (current-directory))))



(: current-warn-unimplemented-kernel-primitive (Parameterof (Symbol -> Void)))
(define current-warn-unimplemented-kernel-primitive
  (make-parameter
   (lambda: ([id : Symbol])
            (printf "WARNING: Primitive Kernel Value ~s has not been implemented\n"
                    id))))









;;; These parameters below will probably go away soon.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: current-defined-name (Parameterof (U Symbol LamPositionalName)))
(define current-defined-name (make-parameter 'unknown))

