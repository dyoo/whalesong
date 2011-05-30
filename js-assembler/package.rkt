#lang racket/base

(require "assemble.rkt"
         "quote-cdata.rkt"
         "../make.rkt"
         "../make-structs.rkt"
         (prefix-in runtime: "get-runtime.rkt")
         (prefix-in racket: racket/base))



(provide package
         package-anonymous
         package-standalone-xhtml
         get-code
         get-runtime)

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




;; package: Source (path -> boolean) output-port -> void

;; Compile package for the given source program.  should-follow?
;; indicates whether we should continue following module paths.
;;
;; The generated output defines a function called 'invoke' with
;; four arguments (MACHINE, SUCCESS, FAIL, PARAMS).  When called, it will
;; execute the code to either run standalone expressions or
;; load in modules.
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
    (fprintf op "    plt.runtime.ready(function() {")
    (make (list (make-MainModuleSource source-code))
          packaging-configuration)
    (fprintf op "    });");
    (fprintf op "});\n"))




;; package-standalone-xhtml: X output-port -> void
(define (package-standalone-xhtml source-code op)
  (display *header* op)
  (display (quote-cdata (get-runtime)) op)
  (display (quote-cdata (get-code source-code)) op)
  (display *footer* op))



;; get-runtime: -> string
(define (get-runtime)
  (let* ([buffer (open-output-string)]
         [packaging-configuration
          (make-Configuration
           ;; should-follow?
           (lambda (p) #t)
           ;; on
           (lambda (ast stmts)
             (assemble/write-invoke stmts buffer)
             (fprintf buffer "(MACHINE, function() { "))
           
           ;; after
           (lambda (ast stmts)
             (fprintf buffer " }, FAIL, PARAMS);"))
           
           ;; last
           (lambda ()
             (fprintf buffer "SUCCESS();")))])

    (display (runtime:get-runtime) buffer)
    (newline buffer)
    (fprintf buffer "(function(MACHINE, SUCCESS, FAIL, PARAMS) {")
    (make (list only-bootstrapped-code) packaging-configuration)
    (fprintf buffer "})(new plt.runtime.Machine(),\nfunction(){ plt.runtime.setReadyTrue(); },\nfunction(){},\n{});\n")
    (get-output-string buffer)))




(define *header*
  #<<EOF
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
  <head>
    <meta charset="utf-8"/>
    <title>Example</title>
  </head>
  <script>

EOF
)


(define (get-code source-code)
  (let ([buffer (open-output-string)])
    (package source-code
             #:should-follow? (lambda (p) #t)
             #:output-port buffer)
    (get-output-string buffer)))


(define *footer*
  #<<EOF

<![CDATA[
var invokeMainModule = function() {
    var MACHINE = new plt.runtime.Machine();
    invoke(MACHINE,
           function() {
                MACHINE.modules['*main*'].invoke(
                    MACHINE,
                    function() {
                        // On main module invokation success
                    },
                    function(MACHINE, e) {
                        // On main module invokation failure
                        if (console && console.log) {
                            console.log(e.stack || e);
                        }
                    })}, 
           function() {
               // On module loading failure
               if (console && console.log) {
                   console.log(e.stack || e);
               }                       
           },
           {});
};
  
  $(document).ready(invokeMainModule);
]]>
  </script>
  <body></body>
</html>
EOF
)
