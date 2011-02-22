#lang racket
(require "browser-evaluate.rkt")

;; test-find-toplevel-variables
(define-syntax (test stx)
  (syntax-case stx ()
    [(_ s exp)
     (with-syntax ([stx stx])
       (syntax/loc #'stx
         (let-values ([(output time) (evaluate s)])
           (unless (string=? output exp)
             (raise-syntax-error #f (format "Expected ~s, got ~s" exp output)
                                 #'stx)))))]))


(test '(begin (define (f x) 
                (if (= x 0)
                    0
                    (+ x (f (- x 1)))))
              (display (f 3))
              (display "\n")
              (display (f 4))
              (display "\n")
              (display (f 10000)))
      "6\n10\n50005000")



"ok"