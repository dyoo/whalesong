#lang s-exp "kernel.rkt"

(provide (except-out (all-from-out "kernel.rkt"))
         (all-from-out "private/list.rkt"))

(require "private/list.rkt")


;; Kludge: This forces modbeg to be compiled and packaged.
(require racket/private/modbeg)