#lang racket/base


;; Provides a harness for running programs on the browser and
;; examining their results.

;; Provides a test form that expects the path of a program and its
;; expected output.


(require (planet dyoo/browser-evaluate)
         "../js-assembler/package.rkt"
         "../make/make-structs.rkt"
         racket/port
         racket/path
         racket/runtime-path
         racket/runtime-path
         (for-syntax racket/base
                     racket/path
                     racket/port))

(define evaluate (make-evaluate 
                  (lambda (program op)

                    (fprintf op "(function () {")
                    
   		    (displayln (get-runtime) op)
                    
                    (newline op)
                    
                    (fprintf op "var innerInvoke = ")
                    (package-anonymous program
                                       #:should-follow-children? (lambda (src) #t)
                                       #:output-port op)
                    (fprintf op "();\n")
                    
                    (fprintf op #<<EOF
return (function(succ, fail, params) {
            var machine = new plt.runtime.Machine();
            var myParams = { currentDisplayer : function(MACHINE, v) {
                                                   params.currentDisplayer(v);
                                                }
                           };
            return innerInvoke(machine,
                               function() {
                                   plt.runtime.invokeMains(machine, succ, fail);
                               },
                               function(MACHINE, e) {
                                   return fail(e);
                               },
                               myParams);
        });
});
EOF
                             )
                    
                    )))


;; Flatten the paths out.
(define (strip-paths s)
  (regexp-replace* #px"#<path:[^>]+>"
                   s
                   "<path:...>"))

                  

;; We use a customized error structure that supports
;; source location reporting.
(define-struct (exn:fail:error-on-test exn:fail)
  (srcloc)
  #:property prop:exn:srclocs
             (lambda (a-struct)
               (list (exn:fail:error-on-test-srcloc a-struct))))



(define (test/loc original-source-file-path source-file-path
                  original-expected-file-path expected-file-path
                  loc-thunk)
  (printf "running test on ~s..." original-source-file-path)
  (flush-output (current-output-port))
  (let* ([exp (call-with-input-file expected-file-path port->string)]
         [src-path source-file-path]
         [result (evaluate (make-MainModuleSource (make-ModuleSource src-path)))]
         [output (evaluated-stdout result)])
    (cond [(string=? (strip-paths output)
                     (strip-paths exp))
           (printf " ok (~a milliseconds)\n" (evaluated-t result))]
          [else
           (printf " error!\n")
           (raise (make-exn:fail:error-on-test
                   (format "Expected ~s, got ~s" exp output)
                   (current-continuation-marks)
                   (loc-thunk)))])))



(define-syntax (test stx)
  (syntax-case stx ()
    [(_ source-file-path)
     (with-syntax ([expected-file-path
                    (regexp-replace "\\.rkt$"
                                    (syntax-e
                                     #'source-file-path)
                                    ".expected")])

       #'(test source-file-path expected-file-path))]
    [(_ original-source-file-path original-expected-file-path)
     (with-syntax ([stx stx]
                   [source-file-path (parameterize ([current-directory
                                        (current-load-relative-directory)])
                                       (normalize-path (syntax-e #'original-source-file-path)))]
                   [expected-file-path (parameterize ([current-directory
                                                       (current-load-relative-directory)])
                                         (normalize-path (syntax-e #'original-expected-file-path)))])
       (quasisyntax/loc #'stx
         (test/loc original-source-file-path source-file-path
                   original-expected-file-path expected-file-path
                   (lambda ()
                     (srcloc '#,(syntax-source #'stx)
                             '#,(syntax-line #'stx)
                             '#,(syntax-column #'stx)
                             '#,(syntax-position #'stx)
                             '#,(syntax-span #'stx))))))]))
           

(provide test)