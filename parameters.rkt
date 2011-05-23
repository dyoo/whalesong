#lang typed/racket/base

(require "expression-structs.rkt"
         "sets.rkt"
         racket/path)

(provide current-defined-name
         current-module-path
         current-root-path
         current-warn-unimplemented-kernel-primitive
         current-seen-unimplemented-kernel-primitives)



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





;;; Do not touch the following parameters: they're used internally by package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: current-seen-unimplemented-kernel-primitives (Parameterof (Setof Symbol)))
(define current-seen-unimplemented-kernel-primitives
  (make-parameter
   ((inst new-seteq Symbol))))





;;; These parameters below will probably go away soon.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: current-defined-name (Parameterof (U Symbol LamPositionalName)))
(define current-defined-name (make-parameter 'unknown))

