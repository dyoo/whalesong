#lang racket


(require racket/path
         racket/runtime-path
         "../make/make-structs.rkt"
         "../js-assembler/package.rkt"
         "../parameters.rkt"
         (for-syntax racket/base))

(provide write-repl-runtime-files)

(define-runtime-path collects-path (build-path "htdocs" "collects"))
(define-runtime-path language-path 
  (build-path "../wescheme/lang/semantics.rkt"))



;; write-repl-runtime-files: -> void
;; Write out the support library necessary to run Whalesong programs.
;; Includes the basic runtime as well as the language.
(define (write-repl-runtime-files)

  (unless (directory-exists? collects-path)
    (make-directory collects-path))
  
  
  (define written-js-paths '())
  (define module-mappings (make-hash))
  
  
  
  (define make-output-js-filename
    (lambda (module-name)
      (define result
        (build-path collects-path
                    (string-append
                     (regexp-replace #rx"[.](rkt|ss)$" (symbol->string module-name) "")
                     ".js")))
      (set! written-js-paths (cons result written-js-paths))
      (fprintf (current-report-port)
               (format "Writing program ~s\n" result))
      
      (when module-name
        (hash-set! module-mappings module-name result))
      result))
  
  
  (call-with-output-file* (make-output-js-filename 'runtime)
                          (lambda (op)
                            (display (get-runtime) op))
                          #:exists 'replace)
  
  (call-with-output-file* (make-output-js-filename 'library)
                          (lambda (op)
                            (display (get-inert-code (make-ModuleSource 
                                                      (normalize-path language-path))
                                                     make-output-js-filename)
                                     op))
                          #:exists 'replace)
  
  
  ;(for/hash ([(key path) module-mappings])
  ;  (values key (path->string (file-name-from-path path))))
  
  )