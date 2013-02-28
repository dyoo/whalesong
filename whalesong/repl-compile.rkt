#lang racket/base

(define this-namespace (make-base-empty-namespace))

(define (make-repl-namespace [module-path 'racket/base])
  (parameterize ([current-namespace this-namespace])
    (dynamic-require module-path 0))
  (define ns (make-empty-namespace))
  (parameterize ([current-namespace ns])
    (namespace-attach-module this-namespace module-path)  
    (namespace-require/copy module-path))
  ns)


(define memoized-table (make-weak-hash))
(define (repl-compile body #:lang [module-path 'racket/base])
  (define key (cons body module-path))
  (hash-ref memoized-table key (lambda ()
                                 (parameterize ([current-namespace (make-repl-namespace module-path)])
                                   (define compiled (compile body))
                                   (hash-set! memoized-table key compiled)
                                   compiled))))



(for ([i 10])
  (time (repl-compile '(* x 3) #:lang 'whalesong/lang/whalesong))
  (time (repl-compile '(/ x 3) #:lang 'whalesong/lang/whalesong)))
