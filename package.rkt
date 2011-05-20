#lang racket/base

(require "compiler.rkt"
         "compiler-structs.rkt"
	 "parse-bytecode.rkt"
	 "language-namespace.rkt"
         "il-structs.rkt"
         "bootstrapped-primitives.rkt"
         "get-dependencies.rkt"
         "js-assembler/assemble.rkt"
         "js-assembler/get-runtime.rkt"
         "quote-cdata.rkt"
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





(define (package-standalone-html a-module-path op)
  ;; FIXME: write the runtime ...
  ;; Next, write the function to load in each module.
  (fprintf op #<<EOF
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
<head>
  <meta charset="utf-8"/>
  <title>Example</title>
</head>
<script>\n
EOF
           )
  (display (quote-as-cdata (get-runtime)) op)
  (let ([buffer (open-output-string)])
    (assemble/write-invoke (compile (parse-bytecode a-module-path)
                                    'val
                                    next-linkage/drop-multiple)
                           buffer)
    (write-string (quote-as-cdata (get-output-string buffer))
                  op))
  ;; FIXME: Finally, invoke the main module.
  
  (fprintf op #<<EOF
\n</script>
<body onload='invokeMainModule()'>
</body>
</html>
EOF
           ))