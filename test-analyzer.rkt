#lang racket/base
(require "make-dependencies.rkt"
         "make-structs.rkt")


;; For some reason, this is breaking.  Why?
(make/dependencies
 (list (build-path "make-dependencies.rkt"))
 debug-configuration)