#lang racket/base

(require racket/match
         racket/file
         racket/path
         racket/port
         racket/date
         racket/runtime-path
         "make/make-structs.rkt"
         "js-assembler/package.rkt"
         "resource/structs.rkt"
         "logger.rkt"
         "parameters.rkt"
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
                           (fprintf (current-error-port)"~a: ~a\n" level msg)
                           (flush-output (current-error-port))]))
                      (loop)))))))

(define (build-standalone-xhtml f)
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
                                #:exists 'replace)))))



(define (build-html-and-javascript f)
  (turn-on-logger!)

  (define written-js-paths '())
  (define written-resources '())
  (define make-output-js-filename
    (let ([n 0])
      (lambda ()
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
           ".appcache"))])
      (unless (directory-exists? (current-output-dir))
        (fprintf (current-report-port) "Creating destination directory ~s\n" (current-output-dir))
        (make-directory* (current-output-dir)))

      (parameterize ([current-on-resource on-resource])
        (call-with-output-file* (make-output-js-filename)
                                (lambda (op)
                                  (display (get-runtime) op)
                                  (display (get-inert-code (make-MainModuleSource 
                                                            (normalize-path (build-path f)))
                                                           make-output-js-filename)
                                           op))
                                #:exists 'replace))

      (when (current-with-legacy-ie-support?)
        (for ([r ie-resources]) (on-resource r)))
      
      (fprintf (current-report-port)
               (format "Writing html ~s\n" (build-path (current-output-dir) output-html-filename)))
      (call-with-output-file* (build-path (current-output-dir) output-html-filename)
                              (lambda (op)
                                (display (get-html-template
                                          (map file-name-from-path
                                               (reverse written-js-paths))
                                          #:title title
                                          #:manifest output-manifest-filename)
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
                                     (fprintf op "~a\n" resource-name)))
                              #:exists 'replace)
      (define stop-time (current-inexact-milliseconds))

      (fprintf (current-timing-port) "Time taken: ~a milliseconds\n" (- stop-time start-time))))




(define (print-the-runtime)
  (turn-on-logger!)
  (display (get-runtime) (current-output-port)))



(define (get-javascript-code filename)
  (turn-on-logger!)
  (display (get-standalone-code
            (make-MainModuleSource 
             (normalize-path (build-path filename))))
           (current-output-port)))
