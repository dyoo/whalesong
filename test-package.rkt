#lang racket/base

(require "package.rkt")

(define (test s-exp)
  (package s-exp (open-output-string) #;(current-output-port)))


(test '(define (factorial n)
           (if (= n 0)
               1
               (* (factorial (- n 1))
                  n))))
(test '(begin
         (define (factorial n)
           (fact-iter n 1))
         (define (fact-iter n acc)
           (if (= n 0)
               acc
               (fact-iter (- n 1) (* acc n))))))

(test '(define (gauss n)
         (if (= n 0)
             0
             (+ (gauss (- n 1))
                n))))

(test '(define (fib n)
           (if (< n 2)
               1
               (+ (fib (- n 1))
                  (fib (- n 2))))))