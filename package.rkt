#lang racket/base

(require "compiler.rkt"
         "compiler-structs.rkt"
         "js-assembler/assemble.rkt"
	 "parse-bytecode-5.1.1.rkt"
	 "language-namespace.rkt"
         "il-structs.rkt"
         "bootstrapped-primitives.rkt"
         racket/runtime-path
         racket/port
	 (prefix-in racket: racket/base))

(provide package
         package-anonymous)

;; Packager: produce single .js files to be included.


(define-runtime-path kernel-language-path
  "lang/kernel.rkt")

;; Use Racket's compiler, and then parse the resulting bytecode
;; to our own AST structures.
(define (parse stx)
  (parameterize ([current-namespace (lookup-language-namespace 
                                     `(file ,(path->string kernel-language-path))
                                     #;'racket/base)]
                 ;; We want to disable some optimizations for the moment.
                 ;; See: http://docs.racket-lang.org/drracket/module.html
                 [compile-context-preservation-enabled #t])

    (let ([bc (racket:compile stx)]
          [op (open-output-bytes)])
      (write bc op)
      (parse-bytecode 
       (open-input-bytes (get-output-bytes op))))))




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