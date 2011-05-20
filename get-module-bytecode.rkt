#lang racket/base
(require racket/contract 
         racket/path
         syntax/modcode)
(provide/contract [get-module-bytecode ((or/c string? path? input-port?) . -> . bytes?)])


(define base-namespace (make-base-namespace))


(define (get-module-bytecode x)
  (let ([compiled-code
         (cond
           ;; Assumed to be a path string
           [(string? x)
            (get-compiled-code-from-path (normalize-path (build-path x)))]

           [(path? x)
            (get-compiled-code-from-path x)]

           ;; Input port is assumed to contain the text of a module.
           [(input-port? x)
            (get-compiled-code-from-port x)]

           [else
            (error 'get-module-bytecode)])])
    (let ([op (open-output-bytes)])
      (write compiled-code op)
      (get-output-bytes op))))


;; Tries to use get-module-code to grab at module bytecode.  Sometimes this fails
;; because it appears get-module-code tries to write to compiled/.
(define (get-compiled-code-from-path p)
  (with-handlers ([void (lambda (exn)
                          ;; Failsafe: try to do it from scratch
                          (call-with-input-file* p
                            (lambda (ip)
                              (get-compiled-code-from-port ip))))])
    (get-module-code p)))


(define (get-compiled-code-from-port ip)
  (parameterize ([read-accept-reader #t]
                 [current-namespace base-namespace])
    (compile (read-syntax (object-name ip) ip))))