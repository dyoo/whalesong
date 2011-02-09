#lang racket/base

(require "cm.rkt"
         "structs.rkt"
         "assemble.rkt"
         racket/runtime-path
         racket/port)
;; Packager: produce single .js files to be included.

(define-runtime-path runtime.js "runtime.js")

;; package: s-expression output-port -> void
(define (package source-code op)
  (call-with-input-file* runtime.js
    (lambda (ip)
      (copy-port ip op)))
  (newline op)
  (fprintf op "var invoke = ")
  (assemble/write-invoke (statements (compile source-code
                                              '()
                                              'val
                                              'return))
                         op)
  (fprintf op ";\n"))



(define (test s-exp)
  (package s-exp (current-output-port)))


(test '(define (factorial n)
           (if (= n 0)
               1
               (* (factorial (- n 1))
                  n))))
#;(test '(define (gauss n)
         (if (= n 0)
             0
             (+ (gauss (- n 1))
                n))))

#;(test '(define (fib n)
           (if (< n 2)
               1
               (+ (fib (- n 1))
                  (fib (- n 2))))))