#lang racket/base
(require "make.rkt"
         "make-structs.rkt")


;; For some reason, this is breaking.  Why?
(make/dependencies
 (list (build-path "make.rkt"))
 debug-configuration)