#lang racket/base

(require racket/match
         racket/file
         racket/path
         racket/port
         "make/make-structs.rkt"
         "js-assembler/package.rkt"
         "resource/structs.rkt"
         "logger.rkt"
         "parameters.rkt")

(provide (all-defined-out))


(define current-verbose? (make-parameter #f))
(define current-output-dir (make-parameter (build-path (current-directory))))
(define current-write-resources? (make-parameter #t))



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
        (fprintf (current-report-form) "Creating destination directory ~s" (current-output-dir))
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
                                  (error 'whalesong "Unable to write resource ~s; this will overwrite a file"
                                         (build-path (current-output-dir)
                                                     (resource-key r)))])]
                          [else
                           (fprintf (current-report-port)
                                    (format "Writing resource ~s" (build-path (current-output-dir)
                                                                              (resource-path r))))
                           (copy-file (resource-path r) 
                                      (build-path (current-output-dir)
                                                  (resource-key r)))]))])
        (fprintf (current-report-port)
                 (format "Writing program ~s" (build-path (current-output-port) output-filename)))
        (call-with-output-file* (build-path (current-output-dir) output-filename)
                                (lambda (op)
                                  (package-standalone-xhtml
                                   (make-ModuleSource (build-path f))
                                   op))
                                #:exists 'replace)))))



(define (build-html-and-javascript f)
  (turn-on-logger!)
  (let-values ([(base filename dir?)
                (split-path f)])
    (let ([output-js-filename (build-path
                               (regexp-replace #rx"[.](rkt|ss)$"
                                               (path->string filename)
                                               ".js"))]
          [output-html-filename
           (build-path
            (regexp-replace #rx"[.](rkt|ss)$"
                            (path->string filename)
                            ".html"))])
      (unless (directory-exists? (current-output-dir))
        (fprintf (current-report-form) "Creating destination directory ~s" (current-output-dir))
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
                                  (error 'whalesong "Unable to write resource ~s; this will overwrite a file"
                                         (build-path (current-output-dir)
                                                     (resource-key r)))])]
                          [else
                           (fprintf (current-report-port)
                                    (format "Writing resource ~s" (build-path (current-output-dir)
                                                                              (resource-path r))))
                           (copy-file (resource-path r) 
                                      (build-path (current-output-dir)
                                                  (resource-key r)))]))])
        (fprintf (current-report-port)
                 (format "Writing program ~s" (build-path (current-output-port) output-js-filename)))
        (call-with-output-file* (build-path (current-output-dir) output-js-filename)
                                (lambda (op)
                                  (display (get-runtime) op)
                                  (display (get-code (make-ModuleSource (build-path f)))
                                           op))
                                #:exists 'replace)

        (fprintf (current-report-port)
                 (format "Writing html ~s" (build-path (current-output-port) output-html-filename)))
        (call-with-output-file* (build-path (current-output-dir) output-html-filename)
                                (lambda (op)
                                  (display (get-html-template output-js-filename) op))
                                #:exists 'replace)
        ))))




(define (print-the-runtime)
  (turn-on-logger!)
  (display (get-runtime) (current-output-port)))



(define (get-javascript-code filename)
  (turn-on-logger!)
  (display (get-standalone-code
            (make-ModuleSource (build-path filename)))
           (current-output-port)))
