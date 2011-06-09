#lang racket/base

(require "assemble.rkt"
         "quote-cdata.rkt"
         "../make/make.rkt"
         "../make/make-structs.rkt"
         "../parameters.rkt"
         "../compiler/expression-structs.rkt"
         "../parser/path-rewriter.rkt"
         "../parser/parse-bytecode.rkt"
         racket/match
         (prefix-in query: "../lang/js/query.rkt")
         (planet dyoo/closure-compile:1:1)
         (prefix-in runtime: "get-runtime.rkt")
         (prefix-in racket: racket/base))


;; TODO: put proper contracts here


(provide package
         package-anonymous
         package-standalone-xhtml
         get-standalone-code
         write-standalone-code
         get-runtime
         write-runtime)



;; notify: string (listof any)* -> void
;; Print out log message during the build process.
(define (notify msg . args)
  (displayln (apply format msg args)))





(define-struct js-impl (name ;; symbol
                        real-path ;; path
                        src ;; string
                        )
  #:transparent)


;; Packager: produce single .js files to be included to execute a
;; program.



(define (package-anonymous source-code
                           #:should-follow-children? should-follow?
                           #:output-port op)  
  (fprintf op "(function() {\n")
  (package source-code
           #:should-follow-children? should-follow?
           #:output-port op)
  (fprintf op " return invoke; })\n"))



;; source-is-javascript-module?: Source -> boolean
;; Returns true if the source looks like a Javascript-implemented module.
(define (source-is-javascript-module? src)
  (cond
   [(StatementsSource? src)
    #f]
   [(MainModuleSource? src)
    (source-is-javascript-module? (MainModuleSource-source src))]
   [(ModuleSource? src)
    (query:has-javascript-implementation? `(file ,(path->string (ModuleSource-path src))))]
   [(SexpSource? src)
    #f]
   [(UninterpretedSource? src)
    #f]))


;; get-javascript-implementation: source -> UninterpretedSource
(define (get-javascript-implementation src)

  (define (get-provided-name-code bytecode)
    (match bytecode
      [(struct Top [_ (struct Module (name path prefix requires provides code))])
       (apply string-append
              (map (lambda (p)
                     (format "modrec.namespace[~s] = exports[~s];\n"
                             (symbol->string (ModuleProvide-internal-name p))
                             (symbol->string (ModuleProvide-external-name p))))
                   provides))]
      [else
       ""]))
  (cond
   [(StatementsSource? src)
    (error 'get-javascript-implementation src)]
   [(MainModuleSource? src)
    (get-javascript-implementation (MainModuleSource-source src))]
   [(ModuleSource? src)
    (let ([name (rewrite-path (ModuleSource-path src))]
          [text (query:query `(file ,(path->string (ModuleSource-path src))))]
          [bytecode (parse-bytecode (ModuleSource-path src))])
      (make-UninterpretedSource
       (format "
MACHINE.modules[~s] =
    new plt.runtime.ModuleRecord(~s,
        function(MACHINE) {
           if(--MACHINE.callsBeforeTrampoline<0) { throw arguments.callee; }
           var modrec = MACHINE.modules[~s];
           var exports = {};
           modrec.isInvoked = true;
           (function(MACHINE, RUNTIME, EXPORTS){~a})(MACHINE, plt.runtime, exports);
           // FIXME: we need to inject the namespace with the values defined in exports.
           ~a
           return MACHINE.control.pop().label(MACHINE);
        });
"
                  (symbol->string name)
                  (symbol->string name)
                  (symbol->string name)
                  text
                  (get-provided-name-code bytecode))))]
   [(SexpSource? src)
    (error 'get-javascript-implementation)]
   [(UninterpretedSource? src)
    (error 'get-javascript-implementation)]))

        






;; package: Source (path -> boolean) output-port -> void

;; Compile package for the given source program.
;;
;; should-follow-children?  indicates whether we should continue
;; following module paths of a source's dependencies.
;;
;; The generated output defines a function called 'invoke' with
;; four arguments (MACHINE, SUCCESS, FAIL, PARAMS).  When called, it will
;; execute the code to either run standalone expressions or
;; load in modules.
(define (package source-code
                 #:should-follow-children? should-follow?
                 #:output-port op)  

  
  ;; wrap-source: source -> source
  ;; Translate all JavaScript-implemented sources into uninterpreted sources;
  ;; we'll leave its interpretation to on-visit-src.
  (define (wrap-source src)
    (cond
     [(source-is-javascript-module? src)
      (get-javascript-implementation src)]
     [else
      src]))

  
  (define (on-visit-src src ast stmts)
    (cond
     [(UninterpretedSource? src)
      (fprintf op (UninterpretedSource-datum src))]
     [else
      (assemble/write-invoke stmts op)
      (fprintf op "(MACHINE, function() { ")]))


  (define (after-visit-src src ast stmts)
    (cond
     [(UninterpretedSource? src)
      (void)]
     [else
      (fprintf op " }, FAIL, PARAMS);")]))


  (define (on-last-src)
    (fprintf op "SUCCESS();"))
  

  (define packaging-configuration
    (make-Configuration
     wrap-source
     
     should-follow?

     ;; on
     on-visit-src

     ;; after
     after-visit-src
     
     ;; last
     on-last-src))

  
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



;; write-runtime: output-port -> void
(define (write-runtime op)
  
  (define (wrap-source src) src)
  (let ([packaging-configuration
         (make-Configuration

          wrap-source
          
          ;; should-follow-children?
          (lambda (src) #t)
          ;; on
          (lambda (src ast stmts)
            (assemble/write-invoke stmts op)
            (fprintf op "(MACHINE, function() { "))
          
          ;; after
          (lambda (src ast stmts)
            (fprintf op " }, FAIL, PARAMS);"))
          
          ;; last
          (lambda ()
            (fprintf op "SUCCESS();")))])

    (display (runtime:get-runtime) op)

    (newline op)
    (fprintf op "(function(MACHINE, SUCCESS, FAIL, PARAMS) {")
    (make (list only-bootstrapped-code) packaging-configuration)
    (fprintf op "})(plt.runtime.currentMachine,\nfunction(){ plt.runtime.setReadyTrue(); },\nfunction(){},\n{});\n")))



(define (compress x)
  (if (current-compress-javascript?)
      (closure-compile x)
      x))



(define *the-runtime*
  (let ([buffer (open-output-string)])
    (write-runtime buffer)
    (compress
     (get-output-string buffer))))

  
;; get-runtime: -> string
(define (get-runtime)
  *the-runtime*)





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
             #:should-follow-children? (lambda (src) #t)
             #:output-port buffer)
    (compress
     (get-output-string buffer))))



;; get-standalone-code: source -> string
(define (get-standalone-code source-code)
  (let ([buffer (open-output-string)])
    (write-standalone-code source-code buffer)
    (compress
     (get-output-string buffer))))


;; write-standalone-code: source output-port -> void
(define (write-standalone-code source-code op)
  (package-anonymous source-code
                     #:should-follow-children? (lambda (src) #t)
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
                        MACHINE.params.currentErrorDisplayer(
                             MACHINE, $(plt.helpers.toDomNode(e.stack || e)).css('color', 'red'));
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
