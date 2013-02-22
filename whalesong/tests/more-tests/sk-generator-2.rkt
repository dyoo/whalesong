#lang whalesong/base
(define (make-gen gen)
 (let ([cont (box #f)])
   (lambda ()
     (call/cc (lambda (caller)
                (if (unbox cont)
                    ((unbox cont) caller)
                    (gen (lambda (v)
                           (call/cc (lambda (gen-k)
                                      (begin
                                        (set-box! cont gen-k)
                                        (caller v))))))))))))

(define g1 (make-gen (lambda (return)
                      (begin
                        (return "a")
                        (return "b")
                        (return "c")))))

(define g2 (make-gen (lambda (ret)
                      (begin
                        (ret 1)
                        (ret 2)
                        (ret 3)))))

(g1)
(g1)
(g1)

(g2)
(g2)
(g2)
