#lang racket/base
(require "browser-evaluate.rkt")

(evaluate '(begin (define (f x) 
                                (if (= x 0)
                                    1
                                    (+ x (f (- x 1)))))
                              (display (f 3))
                              (display "\n")
                              (display (f 4))
                              (display "\n")
                              (display (f 10000))))



(evaluate '(begin (define (f x) 
                                (if (= x 0)
                                    1
                                    (+ x (f (- x 1)))))
                              (display (f 3))
                              (display "\n")
                              (display (f 4))
                              (display "\n")
                              (display (f 100000))))