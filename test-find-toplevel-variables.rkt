#lang racket
(require "find-toplevel-variables.rkt"
         "parse.rkt")

;; test-find-toplevel-variables
(define-syntax (test stx)
  (syntax-case stx ()
    [(_ s exp)
     (with-syntax ([stx stx])
       (syntax/loc #'stx
         (let ([results (find-toplevel-variables (parse s))])
           (unless (equal? results exp)
             (raise-syntax-error #f (format "Expected ~s, got ~s" exp results)
                                 #'stx)))))]))


(test '(define (factorial n)
           (if (= n 0)
               1
               (* (factorial (- n 1))
                  n)))
      
      '(* - = factorial))

(test '(begin
         (define (factorial n)
           (fact-iter n 1))
         (define (fact-iter n acc)
           (if (= n 0)
               acc
               (fact-iter (- n 1) (* acc n)))))
      '(* - = fact-iter factorial))

(test '(define (gauss n)
         (if (= n 0)
             0
             (+ (gauss (- n 1))
                n)))
      '(+ - = gauss))

(test '(define (fib n)
           (if (< n 2)
               1
               (+ (fib (- n 1))
                  (fib (- n 2)))))
      '(+ - < fib))