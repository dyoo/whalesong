#lang racket/base
(require racket/path
         racket/runtime-path
         syntax/modcode
         "language-namespace.rkt"
         "logger.rkt"
         "expand-out-images.rkt")

(provide get-module-bytecode)


(define-runtime-path kernel-language-path
  "lang/kernel.rkt")






(define (get-module-bytecode x)
  (log-debug "grabbing module bytecode for ~s" x)

  (define-values (compiled-code alternative-f)
    (cond
      ;; Assumed to be a path string
      [(string? x)
       (log-debug "assuming path string")
       (values (get-compiled-code-from-path (normalize-path (build-path x)))
               (lambda ()
                 (call-with-input-file* (normalize-path (build-path x))
                   get-compiled-code-from-port)
                 ))]
      
      [(path? x)
       (values (get-compiled-code-from-path x)
               (lambda ()
                 (call-with-input-file* x get-compiled-code-from-port)))]
      
      ;; Input port is assumed to contain the text of a module.
      [(input-port? x)
       (values (get-compiled-code-from-port x)
               (lambda ()
                 (get-compiled-code-from-port x)))]
      
      [else
       (error 'get-module-bytecode)]))

  (with-handlers ([exn? (lambda (exn)
                          ;(printf "error? ~s\n" (exn-message exn))
                          (define op (open-output-bytes))
                          ;(printf "trying alternative to get bytecode\n")
                          (define bytecode (alternative-f))
                          ;(printf "got the bytecode\n")
                          (write bytecode op)
                          (get-output-bytes op))])
    (define op (open-output-bytes))
    (write compiled-code op)
    (get-output-bytes op)))




(define base-namespace
  (make-base-namespace))
  ;(lookup-language-namespace
   ;;'racket/base
   ;;`(file ,(path->string kernel-language-path)))
   ;(make-base-namespace)))



;; Tries to use get-module-code to grab at module bytecode.  Sometimes
;; this fails because it appears get-module-code tries to write to
;; compiled/.
(define (get-compiled-code-from-path p)
  (log-debug "get-compiled-code-from-path")
  (with-handlers ([exn? (lambda (exn)
                          ;; Failsafe: try to do it from scratch
                          (log-debug "parsing from scratch")
                          (call-with-input-file* p
                            (lambda (ip)
                              (get-compiled-code-from-port ip)))
                          )])
    (parameterize ([compile-context-preservation-enabled #f])
      (get-module-code p))))


;; get-compiled-code-from-port: input-port -> compiled-code
;; Compiles the source from scratch.
(define (get-compiled-code-from-port ip)
  ;(printf "get-compiled-code-from-port\n")
  (parameterize ([read-accept-reader #t]
                 [compile-context-preservation-enabled #f]
                 [current-namespace base-namespace])
    (define stx (read-syntax (object-name ip) ip))
    ;(printf "got stx; now expanding out the images\n")
    (define expanded-stx (expand-out-images stx))
    ;(printf "now trying to compile the expanded syntax\n")
    (compile expanded-stx)))