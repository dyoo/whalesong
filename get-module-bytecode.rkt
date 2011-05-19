#lang racket/base
(require racket/contract racket/path)
(provide/contract [get-module-bytecode ((or/c string? path? input-port?) . -> . bytes?)])


(define base-namespace (make-base-namespace))


(define (get-module-bytecode x)
  (let ([compiled-code
         (cond
           [(string? x)
            (call-with-input-file x
              (lambda (ip)
                (get-compiled-code-from-port ip)))]
           [(path? x)
            (call-with-input-file x
              (lambda (ip)
                (get-compiled-code-from-port ip)))]
           [else
            (get-compiled-code-from-port x)])])
    (let ([op (open-output-bytes)])
      (write compiled-code op)
      (get-output-bytes op))))



(define (get-compiled-code-from-port ip)
  (parameterize ([read-accept-reader #t]
                 [current-namespace base-namespace])
    (compile (read-syntax (object-name ip) ip))))