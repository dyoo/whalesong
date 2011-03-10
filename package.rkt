#lang racket/base

(require "compile.rkt"
         "assemble.rkt"
         "typed-parse.rkt"
         "il-structs.rkt"
         racket/runtime-path
         racket/port)

(provide package
         package-anonymous)

;; Packager: produce single .js files to be included.

(define-runtime-path runtime.js "runtime.js")

;; package: s-expression output-port -> void
(define (package source-code op)
  (call-with-input-file* runtime.js
    (lambda (ip)
      (copy-port ip op)))
  (newline op)
  (fprintf op "var invoke = ")
  (assemble/write-invoke (compile (parse source-code)
                                  'val
                                  'next)
                         op)
  (fprintf op ";\n"))


(define (package-anonymous source-code op)
  (fprintf op "(function() {\n")
  (package source-code op)
  (fprintf op " return invoke; })\n"))