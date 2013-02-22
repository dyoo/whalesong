#lang whalesong/base
(define (make-gen gen)
 (let ([cont #f])
   (lambda ()
     (call/cc (lambda (caller)
                (if cont
                    (cont caller)
                    (gen (lambda (v)
                           (call/cc (lambda (gen-k)
                                      (begin
                                        (set! cont gen-k)
                                        (caller v))))))))))))

(define g1 (make-gen (lambda (return)
                      (begin
                        (return "a")
                        (return "b")
                        (return "c")))))

(g1)
(g1)
(g1)
