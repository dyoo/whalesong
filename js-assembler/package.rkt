#lang racket/base

(require "assemble.rkt"
         "quote-cdata.rkt"
         "../logger.rkt"
         "../make/make.rkt"
         "../make/make-structs.rkt"
         "../parameters.rkt"
         "../compiler/expression-structs.rkt"
         "../parser/path-rewriter.rkt"
         "../parser/parse-bytecode.rkt"
         "../resource/structs.rkt"
	 "../promise.rkt"
         racket/match
         racket/list
         racket/promise
         racket/set
         racket/path
         racket/string
         racket/port
         (prefix-in query: "../lang/js/query.rkt")
         (prefix-in resource-query: "../resource/query.rkt")
         (prefix-in runtime: "get-runtime.rkt")
         (prefix-in racket: racket/base))

;; There is a dynamic require for (planet dyoo/closure-compile) that's done
;; if compression is turned on.


;; TODO: put proper contracts here


(provide package
         package-standalone-xhtml
         get-inert-code
         get-standalone-code
         write-standalone-code
         get-runtime
         write-runtime
         current-on-resource
         get-html-template)



;; notify: string (listof any)* -> void
;; Print out log message during the build process.
(define (notify msg . args)
  (displayln (apply format msg args)))



(define current-on-resource
  (make-parameter (lambda (r)
                    (log-debug "Resource ~s should be written"
                               (resource-path r))
                    (void))))


(define-struct cached-entry (real-path ;; path to a module. 
                             whalesong-version ;; string
                             md5   ;; md5 of the original source in real-path
                             bytes)
  #:transparent) ;; bytes




(define-struct js-impl (name ;; symbol
                        real-path ;; path
                        src ;; string
                        )
  #:transparent)


;; Packager: produce single .js files to be included to execute a
;; program.



;; (define (package-anonymous source-code
;;                            #:should-follow-children? should-follow?
;;                            #:output-port op)
;;   (fprintf op "(function() {\n")
;;   (package source-code
;;            #:should-follow-children? should-follow?
;;            #:output-port op)
;;   (fprintf op " return invoke; })\n"))



;; source-is-javascript-module?: Source -> boolean
;; Returns true if the source looks like a Javascript-implemented module.
(define (source-is-javascript-module? src)
  (cond
    [(StatementsSource? src)
     #f]
    [(MainModuleSource? src)
     (query:has-javascript-implementation?
      `(file ,(path->string (MainModuleSource-path src))))]
    [(ModuleSource? src)
     (query:has-javascript-implementation?
      `(file ,(path->string (ModuleSource-path src))))]
    [(SexpSource? src)
     #f]
    [(UninterpretedSource? src)
     #f]))

(define (source-resources src)
  (cond
    [(StatementsSource? src)
     empty]
    [(MainModuleSource? src)
     (resource-query:query
      `(file ,(path->string (MainModuleSource-path src))))]
    [(ModuleSource? src)
     (resource-query:query
      `(file ,(path->string (ModuleSource-path src))))]
    [(SexpSource? src)
     empty]
    [(UninterpretedSource? src)
     empty]))



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


  (define (get-implementation-from-path path)
    (let* ([name (rewrite-path path)]
	   [paths (query:query `(file ,(path->string path)))]
	   [text (string-join
		  (map (lambda (p)
			 (call-with-input-file p port->string))
		       paths)
		  "\n")]
	   [module-requires (query:lookup-module-requires path)]
	   [bytecode (parse-bytecode path)])
      (when (not (empty? module-requires))
	    (log-debug "~a requires ~a"
		       path
		       module-requires))
      (let ([module-body-text
	     (format "
             if(--M.cbt<0) { throw arguments.callee; }
             var modrec = M.modules[~s];
             var exports = {};
             modrec.isInvoked = true;
             (function(MACHINE, EXPORTS){~a})(M, exports);
             ~a
             modrec.privateExports = exports;
             return M.c.pop().label(M);"
		     (symbol->string name)
		     text
		     (get-provided-name-code bytecode))])
	
	(make-UninterpretedSource
	 (format "
M.modules[~s] =
    new plt.runtime.ModuleRecord(~s,
        function(M) {
            ~a
        });
"
		 (symbol->string name)
		 (symbol->string name)
		 (assemble-modinvokes+body module-requires module-body-text))
	 
	 (map (lambda (p) (make-ModuleSource (normalize-path p)))
	      module-requires)))))



  (cond
    [(StatementsSource? src)
     (error 'get-javascript-implementation src)]
    [(MainModuleSource? src)
     (get-implementation-from-path (MainModuleSource-path src))]
    [(ModuleSource? src)
     (get-implementation-from-path (ModuleSource-path src))]
    
    
    [(SexpSource? src)
     (error 'get-javascript-implementation)]
    [(UninterpretedSource? src)
     (error 'get-javascript-implementation)]))


(define (assemble-modinvokes+body paths after)
  (cond
    [(empty? paths)
     after]
    [(empty? (rest paths))
     (assemble-modinvoke (first paths) after)]
    [else
     (assemble-modinvoke (first paths)
                         (assemble-modinvokes+body (rest paths) after))]))


(define (assemble-modinvoke path after)
  (let ([name (rewrite-path (path->string path))]
        [afterName (gensym 'afterName)])
    (format "var ~a = function() { ~a };
             if (! M.modules[~s].isInvoked) {
                 M.modules[~s].internalInvoke(M,
                                            ~a,
                                            M.params.currentErrorHandler);
             } else {
                 ~a();
             }"
            afterName
            after
            (symbol->string name)
            (symbol->string name)
            afterName
            afterName)))




;; package: Source (path -> boolean) output-port -> void

;; Compile package for the given source program.
;;
;; should-follow-children?  indicates whether we should continue
;; following module paths of a source's dependencies.
;;
;; The generated output defines a function called 'invoke' with
;; four arguments (M, SUCCESS, FAIL, PARAMS).  When called, it will
;; execute the code to either run standalone expressions or
;; load in modules.
(define (package source-code
                 #:should-follow-children? should-follow?
                 #:output-port op
                 #:next-file-path (next-file-path (lambda () (error 'package))))
  (define resources (set))
  
  
  ;; wrap-source: source -> source
  ;; Translate all JavaScript-implemented sources into uninterpreted sources;
  ;; we'll leave its interpretation to on-visit-src.
  (define (wrap-source src)
    (log-debug "Checking if the source has a JavaScript implementation")
    (cond
      [(source-is-javascript-module? src)
       (log-debug "Replacing implementation with JavaScript one.")
       (get-javascript-implementation src)]
      [else
       src]))
  

  (define (maybe-with-fresh-file thunk)
    (cond
     [(current-one-module-per-file?)
      (define old-port op)
      (define temp-string (open-output-string))
      (set! op temp-string)
      (thunk)
      (set! op old-port)
      (call-with-output-file (next-file-path)
        (lambda (op)
          (display (compress (get-output-string temp-string)) op))
        #:exists 'replace)]
     [else
      (thunk)]))

  
  (define (on-visit-src src ast stmts)
    ;; Record the use of resources on source module visitation...
    (set! resources (set-union resources (list->set (source-resources src))))

    (maybe-with-fresh-file
     (lambda ()
       (fprintf op "\n// ** Visiting ~a\n" (source-name src))
       (define start-time (current-inexact-milliseconds))
       (cond
        [(UninterpretedSource? src)
         (fprintf op "(function(M) { ~a }(plt.runtime.currentMachine));" (UninterpretedSource-datum src))]
        [else      
         (fprintf op "(")
         (on-source src stmts op)
         (fprintf op ")(plt.runtime.currentMachine,
                     function() {
                          if (window.console && window.console.log) {
                              window.console.log('loaded ' + ~s);
                          }
                     },
                     function(err) {
                          if (window.console && window.console.log) {
                              window.console.log('error: unable to load ' + ~s);
                          }
                     },
                     {});"
                  (format "~a" (source-name src))
                  (format "~a" (source-name src)))
         (define stop-time (current-inexact-milliseconds))
         (fprintf (current-timing-port) "  assembly: ~s milliseconds\n" (- stop-time start-time))
         (void)]))))
  
  
  (define (after-visit-src src)
    (void))
  
  
  (define (on-last-src)
    (void))

  
  
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
  
  (make (list source-code) packaging-configuration)
  
  (for ([r resources])
    ((current-on-resource) r)))



;; on-source: Source (Promise (Listof Statement)) OutputPort -> void
;; Generates the source for the statements here.
(define (on-source src stmts op)
  (cond
   [(ModuleSource? src)
    (assemble/write-invoke (my-force stmts) op)]
   [(MainModuleSource? src)
    (assemble/write-invoke (my-force stmts) op)]
   [else
    (assemble/write-invoke (my-force stmts) op)]))






;; package-standalone-xhtml: X output-port -> void
(define (package-standalone-xhtml source-code op)
  (display *header* op)
  (display (quote-cdata
            (string-append (get-runtime)
                           (get-inert-code source-code
                                           (lambda () (error 'package-standalone-xhtml)))
                           invoke-main-module-code)) op)
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
            (on-source src stmts op)
            (fprintf op "(M, function() { "))
          
          ;; after
          (lambda (src)
            (fprintf op " }, FAIL, PARAMS);"))
          
          ;; last
          (lambda ()
            (fprintf op "SUCCESS();")))])
    
    (display (runtime:get-runtime) op)
    
    (newline op)
    (fprintf op "(function(M, SUCCESS, FAIL, PARAMS) {")
    (make (list only-bootstrapped-code) packaging-configuration)
    (fprintf op "})(plt.runtime.currentMachine,\nfunction(){ plt.runtime.setReadyTrue(); },\nfunction(){},\n{});\n")))


(define closure-compile-ns (make-base-namespace))
(define (compress x)
  (cond [(current-compress-javascript?)
         (log-debug "compressing javascript...")
         (parameterize ([current-namespace closure-compile-ns])
           (define closure-compile (dynamic-require '(planet dyoo/closure-compile) 'closure-compile))
           (closure-compile x))]
        [else
         (log-debug "not compressing javascript...")
         x]))



(define *the-runtime*
  (delay (let ([buffer (open-output-string)])
           (write-runtime buffer)
           (compress
            (get-output-string buffer)))))


;; get-runtime: -> string
(define (get-runtime)
  (force *the-runtime*))





;; *header* : string
(define *header*
  #<<EOF
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
  <head>
    <meta name="viewport" content="initial-scale=1.0, width=device-width, height=device-height, minimum-scale=1.0, maximum-scale=1.0, user-scalable=no" />
    <meta charset="utf-8"/>
    <title></title>
  </head>
  <script>

EOF
  )


;; get-html-template: (listof string) -> string
(define (get-html-template js-files)
  (format #<<EOF
<!DOCTYPE html>
<html>
  <head>
    <meta name="viewport" content="initial-scale=1.0, width=device-width, height=device-height, minimum-scale=1.0, maximum-scale=1.0, user-scalable=no" />
    <meta charset="utf-8"/>
    <title></title>
~a
  <script>
  ~a
  </script>
  </head>
  <body>
  </body>
  </html>
EOF

  (string-join (map (lambda (js)
                      (format "  <script src='~a'></script>\n" js))
                    js-files)
               "")
  invoke-main-module-code))


;; get-inert-code: source (-> path) -> string
(define (get-inert-code source-code next-file-path)
  (let ([buffer (open-output-string)])
    (package source-code
             #:should-follow-children? (lambda (src) #t)
             #:output-port buffer
             #:next-file-path next-file-path)
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
  (package source-code
           #:should-follow-children? (lambda (src) #t)
           #:output-port op))






(define invoke-main-module-code
  #<<EOF
var invokeMainModule = function() {
    var M = plt.runtime.currentMachine;
    var startTime = new Date().valueOf();
    plt.runtime.invokeMains(
        M,
        function() {
            // On main module invokation success:
            var stopTime = new Date().valueOf();                                
            if (window.console && window.console.log) {
                window.console.log('evaluation took ' + (stopTime - startTime) + ' milliseconds');
            }
        },
        function(M, e) {
            var contMarkSet, context, i, appName;
            // On main module invokation failure
            if (window.console && window.console.log) {
                window.console.log(e.stack || e);
            }
            
            M.params.currentErrorDisplayer(
                M, $(plt.baselib.format.toDomNode(e.stack || e)).css('color', 'red'));

            if (e.hasOwnProperty('racketError') &&
                plt.baselib.exceptions.isExn(e.racketError)) {
                contMarkSet = plt.baselib.exceptions.exnContMarks(e.racketError);
                if (contMarkSet) {
                    context = contMarkSet.getContext(M);
                    for (i = 0; i < context.length; i++) {
                        if (plt.runtime.isVector(context[i])) {
                            M.params.currentErrorDisplayer(
                                M,
                                $('<div/>').text('  at ' + context[i].elts[0] +
                                                 ', line ' + context[i].elts[2] +
                                                 ', column ' + context[i].elts[3])
                                    .addClass('stacktrace')
                                    .css('margin-left', '10px')
                                    .css('whitespace', 'pre')
                                    .css('color', 'red'));
                        } else if (plt.runtime.isProcedure(context[i])) {
                            M.params.currentErrorDisplayer(
                                M,
                                $('<div/>').text('  in ' + context[i].displayName)
                                    .addClass('stacktrace')
                                    .css('margin-left', '10px')
                                    .css('whitespace', 'pre')
                                    .css('color', 'red'));
                        }                                     
                    }
                }
            }
        });
};
  $(document).ready(invokeMainModule);
EOF
  )

(define *footer*
  #<<EOF
  </script>
  <body></body>
</html>
EOF
  )
