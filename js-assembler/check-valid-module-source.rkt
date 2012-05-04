#lang racket/base

(provide check-valid-module-source
         [struct-out exn:invalid-module-source])

(require syntax/kerncase
         syntax/modresolve
         racket/path
         "../parameters.rkt"
         "../parser/path-rewriter.rkt")


(struct exn:invalid-module-source exn:fail ())


(define (abort-abort #:reason (reason "Invalid module source"))
  (fprintf (current-report-port) "Aborting compilation.\n")
  (raise (exn:invalid-module-source reason
                                    (current-continuation-marks))))


(define ns (make-base-namespace))




(define (looks-like-old-moby-or-js-vm? module-source-path)
  (or (call-with-input-file* module-source-path
                             (lambda (ip) (regexp-match #px"^\\s*#lang\\s+planet\\s+dyoo/moby" ip)))
      (call-with-input-file* module-source-path
                             (lambda (ip) (regexp-match #px"^\\s*#lang\\s+planet\\s+dyoo/js-vm" ip)))))




(define (check-valid-module-source module-source-path)
  ;; Check that the file exists.
  (unless (file-exists? module-source-path)
    (fprintf (current-report-port) "ERROR: Can't read a Racket module from ~e.  The file does not appear to exist.\n"
             module-source-path)
    (abort-abort))


  ;; Is the file one that we know how to symbolically resolve?
  (cond [(rewrite-path module-source-path)
         (void)]
        [else
         (fprintf (current-report-port)
                  "ERROR: The file ~e appears to be outside the root package directory ~e.  You may need to use --root-dir.\n"
                 module-source-path
                 (current-root-path))
         (abort-abort)])

  
  ;; Does it look like something out of moby or js-vm?  Abort early, because if we don't do
  ;; this up front, Racket will try to install the deprecated module, and that's bad.
  (when (looks-like-old-moby-or-js-vm? module-source-path)
    (fprintf (current-report-port) "ERROR: The program in ~e appears to be written using the deprecated project js-vm or Moby.\n\nPlease change the lang line to:\n\n    #lang planet dyoo/whalesong\n\ninstead.\n"
            module-source-path)
    (abort-abort))
    

  ;; Check that it looks like a module.
  (define stx
    (with-handlers ([exn:fail?
                     (lambda (exn)
                       ;; We can't even get the bytecode for the file.
                       ;; Fail immediately.
                       (fprintf (current-report-port) "ERROR: Can't read a Racket module from ~e.  The file may be ill-formed or be written in a language that Whalesong doesn't recognize.\n"
                               module-source-path)
                       (fprintf (current-report-port) "\nFor reference, the error message produced when trying to read ~e is:\n\n" module-source-path)
                       (fprintf (current-report-port) "~a\n" (exn-message exn))
                       (abort-abort))])
      (parameterize ([read-accept-reader #t])
        (call-with-input-file* module-source-path
                               (lambda (ip)
                                 (port-count-lines! ip)
                                 (read-syntax module-source-path ip))))))

  (define relative-language-stx
    (kernel-syntax-case stx #t
      [(module name language body ...)
       #'language]
      [else
       (fprintf (current-report-port) "ERROR: Can't read a Racket module from ~e.  The file exists, but does not appear to be a Racket module.\n"
               module-source-path)
       (abort-abort)]))


  ;; Check that the module is written in a language that we allow.
  (define resolved-language-path
    (resolve-module-path (syntax->datum relative-language-stx)
                         module-source-path))
  (cond
   [(eq? resolved-language-path '#%kernel)
    (void)]
   [(path? resolved-language-path)
    (define normalized-resolved-language-path
      (normalize-path resolved-language-path))

    (cond
     [(within-root-path? normalized-resolved-language-path)
      (void)]

     [(within-whalesong-path? normalized-resolved-language-path)
      (void)]

     [else
      ;; Something bad is about to happen, as the module is written
      ;; in a language that we, most likely, can't compile.
      ;;
      ;; Let's see if we can provide a good error message here
      (fprintf (current-report-port) "ERROR: The file ~e is a Racket module, but is written in the language ~a [~e], which Whalesong does not know how to compile.\n"
              module-source-path
              (syntax->datum relative-language-stx)
              normalized-resolved-language-path)
      (abort-abort)])])


  ;; Once we know that the module is in a language we allow, we 
  ;; check that the file compiles.
  (with-handlers ([exn:fail?
                   (lambda (exn)
                     (fprintf (current-report-port) "ERROR: the racket module ~e raises a compile-time error during compilation." module-source-path)
                     (fprintf (current-report-port) "\n\nFor reference, the error message produced during compilation is the following:\n\n")
                     (fprintf (current-report-port) "~a\n" (exn-message exn))
                     (newline (current-report-port))
                     (abort-abort))])
    (parameterize ([current-namespace ns]
                   [current-load-relative-directory
                    (path-only module-source-path)]
                   [current-directory
                    (path-only module-source-path)])
      (compile stx))))