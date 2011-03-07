#lang racket

(require "simulator.rkt"
         "simulator-structs.rkt"
         "compile.rkt"
         "parse.rkt")


(define (run-compiler code)
  (compile (parse code) 'val 'next))
  

;; Test out the compiler, using the simulator.
(define-syntax (test stx)
  (syntax-case stx ()
    [(_ code exp)
     (with-syntax ([stx stx])
       (syntax/loc #'stx
         (begin
           (printf "Running ~s...\n" 'code)
           (let*-values([(a-machine num-steps) 
                         (run (new-machine (run-compiler 'code)))]
                        [(actual) (machine-val a-machine)])
             (unless (equal? actual exp)
               (raise-syntax-error #f (format "Expected ~s, got ~s" exp actual)
                                   #'stx))
             (printf "ok. ~s steps\n\n" num-steps)))))]))

;; run: machine -> (machine number)
;; Run the machine to completion.
(define (run m)
  (let loop ([m m]
             [steps 0])
    (cond
      [(can-step? m)
       (loop (step m) (add1 steps))]
      [else
       (values m steps)])))


;; Atomic expressions
(test 42 42)
(test "hello world" "hello world")
(test #t true)
(test #f false)

;; quoted 
(test '(+ 3 4)
      '(+ 3 4))

;; Simple definitions
(test (begin (define x 42)
             (+ x x))
      84)

(test (begin (define x 6)
             (define y 7)
             (define z 8)
             (* x y z))
      (* 6 7 8))


;(simulate (compile (parse '42) 'val 'next))
;(compile (parse '(+ 3 4)) 'val 'next)