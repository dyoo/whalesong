#lang s-exp "kernel.rkt"

(require "base.rkt"
         "traced-app.rkt")

;; Programs written in Whalesong will have tracing enabled by default.
;; If you don't want this, write in whalesong/base instead.

(provide (except-out (all-from-out "base.rkt")
                     #%app)
         (rename-out [traced-app #%app]))