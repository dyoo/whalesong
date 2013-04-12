#lang racket/base

(require racket/match
         racket/file
         racket/path
         racket/port
         racket/date
         racket/runtime-path
         racket/pretty
         json
         "parser/parse-bytecode.rkt"
         "parser/path-rewriter.rkt"
         "compiler/compiler.rkt"
         "compiler/compiler-structs.rkt"
         "make/make-structs.rkt"
         "js-assembler/package.rkt"
         "resource/structs.rkt"
         "logger.rkt"
         "parameters.rkt"
         "js-assembler/check-valid-module-source.rkt"
         (for-syntax racket/base))

(provide (all-defined-out))






(define current-verbose? (make-parameter #f))
(define current-output-dir (make-parameter (build-path (current-directory))))
(define current-write-resources? (make-parameter #t))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IE Compatbility stuff:
(define-runtime-paths (excanvas.js canvas-text.js optimer-normal-normal.js)
  (values (build-path "ie-compat" "excanvas.js")
          (build-path "ie-compat" "canvas.text.js")
          (build-path "ie-compat" "optimer-normal-normal.js")))

(define ie-resources
  (list (resource excanvas.js "excanvas.js" #"")
        (resource canvas-text.js "canvas.text.js" #"")
        (resource optimer-normal-normal.js "optimer-normal-normal.js" #"")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (with-catchall-exception-handler thunk)
  (with-handlers
      ([exn:invalid-module-source?
        (lambda (exn)
          (fprintf (current-report-port) "~a\n"
                   (exn-message exn))
          (fprintf (current-report-port) "------------------\n")
          (fprintf (current-report-port) "\nAborting compilation.\n"))]
       [void (lambda (exn)
               (fprintf (current-report-port) "ERROR: Whalesong has encountered an internal error.\n\n")
               (fprintf (current-report-port) "Please send the following error report log to dyoo@hashcollision.org.\n\n")
               (define op (open-output-string))
               (parameterize ([current-error-port op])
                 ((error-display-handler) (exn-message exn) exn))
               (fprintf (current-report-port) "------------------\n")
               (displayln (get-output-string op) (current-report-port))
               (fprintf (current-report-port) "------------------\n")
               (fprintf (current-report-port) "\nAborting compilation.\n"))])
    (thunk)))




(define (same-file? p1 p2)
  (or (equal? (normalize-path p1) (normalize-path p2))
      (bytes=? (call-with-input-file p1 port->bytes)
               (call-with-input-file p2 port->bytes))))


(define (turn-on-logger!)
  (void (thread (lambda ()
                  (let ([receiver
                         (make-log-receiver whalesong-logger
                                            (if (current-verbose?)
                                                'debug
                                                'info))])
                    (let loop ()
                      (let ([msg (sync receiver)])
                        (match msg
                          [(vector level msg data)
                           (fprintf (current-report-port)"~a: ~a\n" level msg)
                           (flush-output (current-report-port))]
                          [else
                           (fprintf (current-report-port)"~a\n" msg)
                           (flush-output (current-report-port))]))
                      (loop)))))))

(define (build-standalone-xhtml f)
  (with-catchall-exception-handler
   (lambda ()
     (turn-on-logger!)
     (let-values ([(base filename dir?)
                   (split-path f)])
       (let ([output-filename
              (build-path
               (regexp-replace #rx"[.](rkt|ss)$"
                               (path->string filename)
                               ".xhtml"))])
         (unless (directory-exists? (current-output-dir))
           (fprintf (current-report-port) "Creating destination directory ~s\n" (current-output-dir))
           (make-directory* (current-output-dir)))
         (parameterize ([current-on-resource
                         (lambda (r)
                           (cond
                            [(file-exists? (build-path (current-output-dir)
                                                       (resource-key r)))
                             (cond [(same-file? (build-path (current-output-dir)
                                                            (resource-key r))
                                                (resource-path r))
                                    (void)]
                                   [else
                                    (error 'whalesong "Unable to write resource ~s; this will overwrite a file that already exists."
                                           (build-path (current-output-dir)
                                                       (resource-key r)))])]
                            [else
                             (fprintf (current-report-port)
                                      (format "Writing resource ~s\n" (build-path (current-output-dir)
                                                                                  (resource-key r))))
                             (copy-file (resource-path r) 
                                        (build-path (current-output-dir)
                                                    (resource-key r)))]))])
           (fprintf (current-report-port)
                    (format "Writing program ~s\n" (build-path (current-output-port) output-filename)))
           (call-with-output-file* (build-path (current-output-dir) output-filename)
                                   (lambda (op)
                                     (package-standalone-xhtml
                                      (make-MainModuleSource 
                                       (normalize-path (build-path f)))
                                      op))
                                   #:exists 'replace)))))))



(define (build-html-and-javascript f)
  (with-catchall-exception-handler
   (lambda ()
     (turn-on-logger!)

     (define written-js-paths '())
     (define written-resources '())
     (define module-mappings (make-hash))

     (define make-output-js-filename
       (let ([n 0])
         (lambda (module-name)
           (define result (build-path (current-output-dir)
                                      (string-append
                                       (regexp-replace #rx"[.](rkt|ss)$"
                                                       (path->string (file-name-from-path f))
                                                       "")
                                       (if (= n 0)
                                           ".js"
                                           (format "_~a.js" n)))))
           (set! written-js-paths (cons result written-js-paths))
           (set! n (add1 n))
           (fprintf (current-report-port)
                    (format "Writing program ~s\n" result))

           (when module-name
             (hash-set! module-mappings module-name result))
           result)))
     
     (define (on-resource r)
       (cond
        [(file-exists? (build-path (current-output-dir) (resource-key r)))
         (cond [(same-file? (build-path (current-output-dir)
                                        (resource-key r))
                            (resource-path r))
                (fprintf (current-report-port)
                         (format "Skipping writing resource ~s; already exists\n"
                                 (build-path (current-output-dir)
                                             (resource-key r))))
                (void)]
               [else
                (error 'whalesong "Unable to write resource ~s; this will overwrite a file that already exists."
                       (build-path (current-output-dir)
                                   (resource-key r)))])]
        [else
         (fprintf (current-report-port)
                  (format "Writing resource ~s\n" (build-path (current-output-dir)
                                                              (resource-key r))))
         (copy-file (resource-path r) 
                    (build-path (current-output-dir)
                                (resource-key r)))])
       (set! written-resources (cons (resource-key r) written-resources)))
     

     
     (define start-time (current-inexact-milliseconds))
     (let ([title
            (regexp-replace #rx"([.](rkt|ss))$"
                            (path->string (file-name-from-path f))
                            "")]
           [output-html-filename
            (build-path
             (string-append (regexp-replace #rx"[.](rkt|ss)$"
                                            (path->string (file-name-from-path f))
                                            "")
                            ".html"))]
           [output-manifest-filename
            (build-path
             (string-append
              (regexp-replace #rx"[.](rkt|ss)$"
                              (path->string (file-name-from-path f))
                              "")
              ".appcache"))]
           [output-js-module-manifest-filename
            (build-path
             (string-append
              (regexp-replace #rx"[.](rkt|ss)$"
                              (path->string (file-name-from-path f))
                              "")
              "-module-manifest.js"))])
       (unless (directory-exists? (current-output-dir))
         (fprintf (current-report-port) "Creating destination directory ~s\n" (current-output-dir))
         (make-directory* (current-output-dir)))


       ;; Write out the main module and its other module dependencies.
       (parameterize ([current-on-resource on-resource])
         (call-with-output-file* (make-output-js-filename #f)
                                 (lambda (op)
                                   (display (get-runtime) op))
                                 #:exists 'replace)
         (call-with-output-file* (make-output-js-filename #f)
                                 (lambda (op)
                                   (display (get-inert-code (make-MainModuleSource 
                                                             (normalize-path (build-path f)))
                                                            make-output-js-filename)
                                            op))
                                 #:exists 'replace))
       
       ;; We want to get the symbolic name of the main module:
       (define main-module-key (rewrite-path (normalize-path (build-path f))))
       
       
       (when (current-with-legacy-ie-support?)
         (for ([r ie-resources]) (on-resource r)))
       
       (fprintf (current-report-port)
                (format "Writing html ~s\n" (build-path (current-output-dir) output-html-filename)))
       (define dynamically-loaded-modules
         (for/list ([(key path) module-mappings]
                    #:unless (eq? key main-module-key))
           (file-name-from-path path)))
       (call-with-output-file* (build-path (current-output-dir) output-html-filename)
                               (lambda (op)
                                 (display (get-html-template
                                           (for*/list ([p (reverse written-js-paths)]
                                                       [name (in-value (file-name-from-path p))]
                                                       #:unless (member name dynamically-loaded-modules))
                                             name)
                                           #:title title
                                           #:manifest output-manifest-filename
                                           #:module-mappings (for/hash ([(key path) module-mappings])
                                                               (values key (path->string (file-name-from-path path)))))
                                          op))
                               #:exists 'replace)

       ;; Write the manifest
       (fprintf (current-report-port)
                (format "Writing manifest ~s\n" (build-path (current-output-dir) output-manifest-filename)))
       (call-with-output-file* (build-path (current-output-dir) output-manifest-filename)
                               (lambda (op)
                                 (fprintf op "CACHE MANIFEST\n")
                                 (fprintf op "## Timestamp: ~a\n" (date->string (current-date) #t))
                                 (for [(js-name (map file-name-from-path (reverse written-js-paths)))]
                                      (fprintf op "~a\n" js-name))
                                 (for [(resource-name written-resources)]
                                      (fprintf op "~a\n" resource-name))
                                 (fprintf op "~a\n" output-js-module-manifest-filename)
                                 (fprintf op "\n# All other resources (e.g. sites) require the user to be online.\nNETWORK:\n*\n"))
                               
                               #:exists 'replace)

       ;; Write out the js module manifest:
       (fprintf (current-report-port)
                (format "Writing js module manifest ~s\n" (build-path (current-output-dir) output-js-module-manifest-filename)))
       (call-with-output-file* (build-path (current-output-dir) output-js-module-manifest-filename)
                               (lambda (op)
                                 (fprintf op "plt.runtime.currentModuleManifest=")
                                 (write-json (for/hash ([(key path) module-mappings])
                                               (values key (path->string (file-name-from-path path))))
                                             op)
                                 (fprintf op ";\n"))
                               #:exists 'replace)


       (define stop-time (current-inexact-milliseconds))
       (fprintf (current-timing-port) "Time taken: ~a milliseconds\n" (- stop-time start-time))))))




(define (print-the-runtime)
  (with-catchall-exception-handler
   (lambda ()
     (turn-on-logger!)
     (display (get-runtime) (current-output-port)))))



(define (get-javascript-code filename)
  (with-catchall-exception-handler
   (lambda ()
     (turn-on-logger!)
     (display (get-standalone-code
               (make-MainModuleSource 
                (normalize-path (build-path filename))))
              (current-output-port)))))


(define (print-il filename)
  (with-catchall-exception-handler
   (lambda ()
     (turn-on-logger!)
     (define path (normalize-path (build-path filename)))
     (define bytecode (parse-bytecode path))
     (define translation (compile bytecode 'val return-linkage))
     (pretty-print translation))))



#;(define (print-version)
  (fprintf (current-report-port) "~a\n" (this-package-version)))
