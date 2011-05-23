#lang racket/base

(require "assemble.rkt"
         "../make-dependencies.rkt"
         "../make-structs.rkt"
	 (prefix-in racket: racket/base))

(provide package
         package-anonymous)

;; Packager: produce single .js files to be included to execute a
;; program.  Follows module dependencies.






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
  (define packaging-configuration
    (make-Configuration

     should-follow?

     ;; on
     (lambda (ast stmts)
       (assemble/write-invoke stmts op)
       (fprintf op "(MACHINE, function() { "))

     ;; after
     (lambda (ast stmts)
       (fprintf op " }, FAIL, PARAMS);"))

     ;; last
     (lambda ()
       (fprintf op "SUCCESS();"))))



  (let ([source-code-op (open-output-bytes)])
    (fprintf op "var invoke = (function(MACHINE, SUCCESS, FAIL, PARAMS) {")
    (make/dependencies (cons only-bootstrapped-code
                             (list source-code))
                       packaging-configuration)
    (fprintf op "});\n")))



;; (define (package-standalone-xhtml a-module-path op)
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