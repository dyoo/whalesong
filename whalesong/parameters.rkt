#lang typed/racket/base

(require "compiler/expression-structs.rkt"
         "compiler/lexical-structs.rkt"
         "compiler/arity-structs.rkt"
         "sets.rkt"
         racket/path
         racket/port)

(require/typed "logger.rkt"
               [log-warning (String -> Void)])



(provide current-defined-name
         current-module-path
         current-root-path
         current-warn-unimplemented-kernel-primitive
         current-seen-unimplemented-kernel-primitives


         current-primitive-identifier?
         
         current-compress-javascript?
         current-one-module-per-file?
         current-with-cache?
         current-with-legacy-ie-support?
         current-header-scripts         
         
         current-report-port
         current-timing-port
         )



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
            (log-warning
             (format "WARNING: Primitive Kernel Value ~s has not been implemented\n"
                     id)))))





(: current-primitive-identifier? (Parameterof (Symbol -> (U False Arity))))
(define current-primitive-identifier? (make-parameter (lambda: ([name : Symbol]) #f)))


(: current-compress-javascript? (Parameterof Boolean))
(define current-compress-javascript? (make-parameter #f))


;; Turn this one so that js-assembler/package generates a file per module, as
;; opposed to trying to bundle them all together.
(: current-one-module-per-file? (Parameterof Boolean))
(define current-one-module-per-file? (make-parameter #f))


;; Turns on caching of compiled programs, so that repeated compilations
;; will reuse existing work.
(: current-with-cache? (Parameterof Boolean))
(define current-with-cache? (make-parameter #f))


;; Turns on ie legacy support; includes excanvas and other helper libraries
;; to smooth out compatibility issues.
(: current-with-legacy-ie-support? (Parameterof Boolean))
(define current-with-legacy-ie-support? (make-parameter #t))


;; Keeps list of Javascript files to be included in the header.
(: current-header-scripts (Parameterof (Listof Path)))
(define current-header-scripts (make-parameter '()))


(: current-report-port (Parameterof Output-Port))
(define current-report-port (make-parameter (current-output-port)))


(: current-timing-port (Parameterof Output-Port))
(define current-timing-port (make-parameter (open-output-nowhere) ;(current-output-port)
                                            ))




;;; Do not touch the following parameters: they're used internally by package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: current-seen-unimplemented-kernel-primitives (Parameterof (Setof Symbol)))
(define current-seen-unimplemented-kernel-primitives
  (make-parameter
   ((inst new-seteq Symbol))))





;;; These parameters below will probably go away soon.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Workaround for what appears to be a bug in 5.3.1 pre-release
(: UNKNOWN Symbol)
(define UNKNOWN 'unknown)

(: current-defined-name (Parameterof (U Symbol LamPositionalName)))
(define current-defined-name (make-parameter UNKNOWN))
