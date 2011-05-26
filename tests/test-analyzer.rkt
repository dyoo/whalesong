#lang racket/base
(require "../make.rkt"
         "../make-structs.rkt")


;; For some reason, this is breaking.  Why?
(make (list (make-ModuleSource (build-path "make.rkt")))
      debug-configuration)