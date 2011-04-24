#lang racket/base

(require "compiler.rkt"
         "assemble.rkt"
         "typed-parse.rkt"
         "il-structs.rkt"
         "bootstrapped-primitives.rkt"
         racket/runtime-path
         racket/port)

(provide package
         package-anonymous)

;; Packager: produce single .js files to be included.

;; package: s-expression output-port -> void
(define (package source-code op)  
  (fprintf op "var invoke = ")
  (assemble/write-invoke (append (get-bootstrapping-code)
				 (compile (parse source-code)
					  'val
					  next-linkage/drop-multiple))
                         op)
  (fprintf op ";\n"))


(define (package-anonymous source-code op)
  (fprintf op "(function() {\n")
  (package source-code op)
  (fprintf op " return invoke; })\n"))