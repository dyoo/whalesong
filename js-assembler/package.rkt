#lang racket/base

(require "assemble.rkt"
         "get-runtime.rkt"
         "../compiler.rkt"
         "../compiler-structs.rkt"
	 "../parse-bytecode.rkt"
	 "../language-namespace.rkt"
         "../il-structs.rkt"
         "../bootstrapped-primitives.rkt"
         "../get-module-bytecode.rkt"
         "../get-dependencies.rkt"
         "../lexical-structs.rkt"
         "../quote-cdata.rkt"
         racket/runtime-path
         racket/port
         racket/list
	 (prefix-in racket: racket/base))

(provide package
         package-anonymous)

;; Packager: produce single .js files to be included to execute a
;; program.  Follows module dependencies.


(define-runtime-path kernel-language-path
  "lang/kernel.rkt")



(define (package-anonymous source-code
                           #:should-follow? should-follow?
                           #:output-port op)
  (fprintf op "(function() {\n")
  (package source-code
           #:should-follow? should-follow?
           #:output-port op)
  (fprintf op " return invoke; })\n"))




;; package: s-expression (path -> boolean) output-port -> void

;; Compile package for the given source program.  should-follow?
;; indicates whether we should continue following module paths.
(define (package source-code
                 #:should-follow? should-follow?
                 #:output-port op)  
  (let ([source-code-op (open-output-bytes)])
    (fprintf op "var invoke = (function(MACHINE, SUCCESS, FAIL, PARAMS) {")
    (follow-dependencies (cons bootstrap (list source-code))
                         should-follow?
                         op)
    
    (fprintf op "});\n")))




;; follow-dependencies
(define (follow-dependencies sources should-follow? op)
  (define visited (make-hash))

  (define (collect-new-dependencies ast sources)
    (cond
     [(eq? ast #f)
      sources]
     [else
      (let* ([dependent-module-names (get-dependencies ast)]
             [paths
              (map ModuleName-real-path
                   (filter (lambda (mp) (and (path? (ModuleName-real-path mp))
                                             (should-follow?
                                              (path? (ModuleName-real-path mp)))))
                           dependent-module-names))])
        (append paths sources))]))
    
  (let loop ([sources sources])
    (cond
     [(empty? sources)
      (fprintf op "SUCCESS();")
      (void)]
     [(hash-has-key? visited (first sources))
      (loop (rest sources))]
     [else
      (hash-set! visited (first sources) #t)
      (let-values ([(ast stmts) (get-ast-and-statements (first sources))])
        (assemble/write-invoke stmts op)
        (fprintf op "(MACHINE, function() { ")
        (loop (collect-new-dependencies ast (rest sources)))
        (fprintf op " }, FAIL, PARAMS);"))])))



(define-struct Bootstrap (code))
(define bootstrap (make-Bootstrap (get-bootstrapping-code)))




;; get-ast-and-statements: X -> (values (U Expression #f) (Listof Statement))
(define (get-ast-and-statements source-code)
  (cond
   [(Bootstrap? source-code)
    (values #f (get-bootstrapping-code))]
   [else
    (let ([ast
           (cond
            [(path? source-code)
             (parse-bytecode source-code)]
            [else
             (let ([source-code-op (open-output-bytes)])
               (write source-code source-code-op)
               (parse-bytecode
                (open-input-bytes
                 (get-module-bytecode
                  (open-input-bytes
                   (get-output-bytes source-code-op))))))])])
      (values ast
              (compile ast 'val next-linkage/drop-multiple)))]))



;; (define (package-standalone-html a-module-path op)
;;   ;; FIXME: write the runtime ...
;;   ;; Next, write the function to load in each module.
;;   (fprintf op #<<EOF
;; <!DOCTYPE html>
;; <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
;; <head>
;;   <meta charset="utf-8"/>
;;   <title>Example</title>
;; </head>
;; <script>\n
;; EOF
;;            )
;;   (display (quote-as-cdata (get-runtime)) op)
;;   (let ([buffer (open-output-string)])
;;     (assemble/write-invoke (compile (parse-bytecode a-module-path)
;;                                     'val
;;                                     next-linkage/drop-multiple)
;;                            buffer)
;;     (write-string (quote-as-cdata (get-output-string buffer))
;;                   op))
;;   ;; FIXME: Finally, invoke the main module.
  
;;   (fprintf op #<<EOF
;; \n</script>
;; <body onload='invokeMainModule()'>
;; </body>
;; </html>
;; EOF
;;            ))