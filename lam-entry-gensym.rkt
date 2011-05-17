#lang typed/racket/base


(define-values (make-lam-label reset-lam-label-counter!/unit-testing)
  (let ([n 0])
    (values
     (lambda ()
       (set! n (add1 n))
       (string->symbol (format "lamEntry~a" n)))
     (lambda ()
       (set! n 0)))))


(provide make-lam-label reset-lam-label-counter!/unit-testing)