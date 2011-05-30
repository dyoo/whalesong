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
         get-standalone-code
         write-standalone-code
         get-runtime
         write-runtime)


;; Packager: produce single .js files to be included to execute a
;; program.



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
  (let ([buffer (open-output-string)])
    (write-runtime buffer)
    (get-output-string buffer)))


;; write-runtime: output-port -> void
(define (write-runtime op)
  (let ([packaging-configuration
         (make-Configuration
          ;; should-follow?
          (lambda (p) #t)
          ;; on
          (lambda (ast stmts)
            (assemble/write-invoke stmts op)
            (fprintf op "(MACHINE, function() { "))
          
          ;; after
          (lambda (ast stmts)
            (fprintf op " }, FAIL, PARAMS);"))
          
          ;; last
          (lambda ()
            (fprintf op "SUCCESS();")))])

    (display (runtime:get-runtime) op)
    (newline op)
    (fprintf op "(function(MACHINE, SUCCESS, FAIL, PARAMS) {")
    (make (list only-bootstrapped-code) packaging-configuration)
    (fprintf op "})(plt.runtime.currentMachine,\nfunction(){ plt.runtime.setReadyTrue(); },\nfunction(){},\n{});\n")))

  



;; *header* : string
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


;; get-code: source -> string
(define (get-code source-code)
  (let ([buffer (open-output-string)])
    (package source-code
             #:should-follow? (lambda (p) #t)
             #:output-port buffer)
    (get-output-string buffer)))



;; get-standalone-code: source -> string
(define (get-standalone-code source-code)
  (let ([buffer (open-output-string)])
    (write-standalone-code source-code buffer)
    (get-output-string buffer)))


;; write-standalone-code: source output-port -> void
(define (write-standalone-code source-code op)
  (package-anonymous source-code
                     #:should-follow? (lambda (p) #t)
                     #:output-port op)
  (fprintf op "()(plt.runtime.currentMachine, function() {}, function() {}, {});\n"))




(define *footer*
  #<<EOF

<![CDATA[
var invokeMainModule = function() {
    var MACHINE = plt.runtime.currentMachine;
    invoke(MACHINE,
           function() {
                plt.runtime.invokeMains(
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
