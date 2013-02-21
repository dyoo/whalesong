#lang racket/base


(define (gauss n)
  (gauss-iter n 0))

(define (gauss-iter n acc)
  (if (= n 0)
      acc
      (gauss-iter (sub1 n) (+ acc n))))


(define n (string->number (vector-ref (current-command-line-arguments) 0)))
(define start (current-inexact-milliseconds))
(define result (gauss n))
(define end (current-inexact-milliseconds))

(printf "~a (~a milliseconds)\n" result (- end start))