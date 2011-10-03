#lang racket/base

(provide check-valid-module-source)

(require syntax/kerncase
         syntax/modresolve
         racket/path
         "../parameters.rkt"
         "../parser/path-rewriter.rkt")


(define (abort-abort)
  (printf "Aborting compilation.\n")
  (exit))


(define ns (make-base-namespace))


(define (check-valid-module-source module-source-path)
  ;; Check that the file exists.
  (unless (file-exists? module-source-path)
    (printf "ERROR: Can't read a Racket module from ~e.  The file does not appear to exist.\n"
            module-source-path)
    (abort-abort))


  ;; Is the file one that we know how to symbolically resolve?
  (cond [(rewrite-path module-source-path)
         (void)]
        [else
         (printf "ERROR: The file ~e appears to be outside the root package directory ~e.  You may need to use --root-dir.\n"
                 module-source-path
                 (current-root-path))
         (abort-abort)])
                 
  

  ;; Check that it looks like a module.
  (define stx
    (with-handlers ([exn:fail?
                     (lambda (exn)
                       ;; We can't even get the bytecode for the file.
                       ;; Fail immediately.
                       (printf "ERROR: Can't read a Racket module from ~e.  The file may be ill-formed.\n"
                               module-source-path)
                       (printf "\nFor reference, the error message produced when trying to read ~e is:\n\n" module-source-path)
                       (printf "~a\n" (exn-message exn))
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
       (printf "ERROR: Can't read a Racket module from ~e.  The file exists, but does not appear to be a Racket module.\n"
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
      (printf "ERROR: The file ~e is a Racket module, but is written in the language ~a [~e], which Whalesong does not know how to compile.\n"
              module-source-path
              (syntax->datum relative-language-stx)
              normalized-resolved-language-path)
      (abort-abort)])])


  ;; Once we know that the module is in a language we allow, we 
  ;; check that the file compiles.
  (with-handlers ([exn:fail?
                   (lambda (exn)
                     (printf "ERROR: the racket module ~e raises a compile-time error during compilation." module-source-path)
                     (printf "\n\nFor reference, the error message produced during compilation is the following:\n\n")
                     (printf "~a\n" (exn-message exn))
                     (newline)
                     (abort-abort))])
    (parameterize ([current-namespace ns]
                   [current-load-relative-directory
                    (path-only module-source-path)]
                   [current-directory
                    (path-only module-source-path)])
      (compile stx))))