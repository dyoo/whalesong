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
         racket/string
         racket/runtime-path
         racket/runtime-path
         (for-syntax racket/base
                     racket/path
                     racket/port))

(define first-run #t)

(define evaluate (make-evaluate 
                  (lambda (program op)
                    
                    (fprintf op "(function () {")
                    (fprintf op "if (typeof console !== 'undefined') { console.log('loading'); }")
                    (newline op)
                    
                    (when first-run
                      (display (get-runtime) op)
                      (set! first-run #f))
                    (display "return (function(succ, fail, params) {
                                           var machine = new plt.runtime.Machine();
                                           plt.runtime.currentMachine = machine;" op)
                    
                    (package program
                             #:should-follow-children? (lambda (src) #t)
                             #:output-port op)
                    (display "             machine.params.currentDisplayer = function(MACHINE, v) {
                                                 params.currentDisplayer(v);
                                           };
                                           plt.runtime.invokeMains(machine,
                                                                   succ,
                                                                   function(e) {
                                                                       params.currentDisplayer(jQuery('<span/>').text(e.message).get(0));
                                                                       succ();
                                                                    });
                                       });
                              });" op))))


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



(define (clean s)
  (string-trim (strip-paths s)
               #:left? #f
               #:right? #t))


(define (test/loc original-source-file-path source-file-path
                  original-expected-file-path expected-file-path
                  loc-thunk)
  (printf "running test on ~s..." original-source-file-path)
  (flush-output (current-output-port))
  (let* ([expected (call-with-input-file expected-file-path port->string)]
         [src-path source-file-path]
         [result ;; (U evaluated exn)
          (with-handlers ([exn:fail? (lambda (exn)
                                       ;; On errors, we check to see if the
                                       ;; exception string matches what
                                       ;; we expected.
                                       exn)])
            (evaluate (make-MainModuleSource src-path)))]
         [output (if (exn? result)
                     (exn-message result)
                     (evaluated-stdout result))])
    (cond [(string=? (clean output) (clean expected))
           (if (exn? result)
               (printf " ok\n")
               (printf " ok (~a milliseconds)\n" (evaluated-t result)))]
          [else
           (printf " error!\n")
           (displayln (exn-message (make-exn:fail:error-on-test
                                    (format "Expected ~s, got ~s" (clean expected) (clean output))
                                    (current-continuation-marks)
                                    (loc-thunk))))])))



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
                                                     (or (current-load-relative-directory) 
                                                         (current-directory))])
                                       (normalize-path (syntax-e #'original-source-file-path)))]
                   [expected-file-path (parameterize ([current-directory
                                                       (or (current-load-relative-directory)
                                                           (current-directory))])
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
