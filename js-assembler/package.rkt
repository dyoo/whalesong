#lang racket/base

(require "assemble.rkt"
         "get-runtime.rkt"
         "../quote-cdata.rkt"
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


    (fprintf op "var invoke = (function(MACHINE, SUCCESS, FAIL, PARAMS) {")
    (make/dependencies (cons only-bootstrapped-code
                             (list source-code))
                       packaging-configuration)
    (fprintf op "});\n"))




;; package-standalone-xhtml: X output-port -> void
(define (package-standalone-xhtml source-code op)
  (fprintf op #<<EOF
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
<head>
  <meta charset="utf-8"/>
  <title>Example</title>
</head>
<script>
EOF
           )
  (display (quote-as-cdata (get-runtime)) op)
  (let ([buffer (open-output-string)])
    (package source-code
             #:should-follow? (lambda (p) #t)
             #:output-port buffer)
    (write-string (quote-as-cdata (get-output-string buffer))
                  op))


  ;; FIXME: Finally, invoke the main module.
  (fprintf op #<<EOF

var invokeMainModule = function() {
    var MACHINE = new plt.runtime.Machine();
    invoke(MACHINE,
           function() {}, 
           function() {},
           {
               currentDisplayer : function(v) {
                   document.body.appendChild(document.createTextNode(String(v)));
               }
           });
};
</script>
<body onload='invokeMainModule()'>
</body>
</html>
EOF
          ))