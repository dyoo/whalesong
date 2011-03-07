#lang racket

(require "simulator.rkt"
         "simulator-structs.rkt"
         "compile.rkt"
         "parse.rkt")

;; Test out the compiler, using the simulator.
(define-syntax (test stx)
  (syntax-case stx ()
    [(_ code exp)
     (with-syntax ([stx stx])
       (syntax/loc #'stx
         (let* ([a-machine (run (new-machine (compile (parse code) 'val 'next)))]
                [actual (machine-val a-machine)])
           (unless (equal? actual exp)
             (raise-syntax-error #f (format "Expected ~s, got ~s" exp actual)
                                 #'stx)))))]))

;; run: machine -> machine
;; Run the machine to completion.
(define (run m)
  (cond
    [(can-step? m)
     (run (step m))]
    [else
     m]))


(test 42 42)
(test '(begin (define x 42)
              (+ x x))
      84)


;(simulate (compile (parse '42) 'val 'next))
;(compile (parse '(+ 3 4)) 'val 'next)