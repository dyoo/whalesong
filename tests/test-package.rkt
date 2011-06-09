#lang racket/base

(require "../js-assembler/package.rkt"
         "../make/make-structs.rkt")

(printf "test-package.rkt\n")


(define (follow? src)
  #t)

(define (test s-exp)
  (package (make-SexpSource s-exp)
           #:should-follow-children? follow?
           #:output-port (open-output-string) #;(current-output-port)))


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