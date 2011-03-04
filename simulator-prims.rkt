#lang racket/base
(require "simulator-structs.rkt")

(provide lookup-primitive)

(define (lookup-primitive name)
  (cond
    [(eq? name '+)
     (make-primitive-proc +)]
    [(eq? name '=)
     (make-primitive-proc =)]
    [else
     (void)]))