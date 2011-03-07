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

;; test, and expect an error
(define-syntax (test/exn stx)
  (syntax-case stx ()
    [(_ code)
     (with-syntax ([stx stx])
       (syntax/loc #'stx
         (begin
           (printf "Running/exn ~s ...\n" 'code)
           (let/ec return
             (with-handlers ([exn:fail? (lambda (exn)
                                          (printf "ok\n\n")
                                          (return))])
               (run (new-machine (run-compiler 'code))))
             (raise-syntax-error #f (format "Expected an exception")
                                 #'stx)))))]))



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

;; Simple branching
(test (if #t 'ok 'not-ok)
      'ok)

(test (if #f 'not-ok 'ok)
      'ok)

;; Sequencing
(test (begin 1
             2
             3)
      3)
(test (begin 1)
      1)

;; Simple application
(test ((lambda (x) x) 42)
      42)
(test ((lambda (x) 
         (begin (* x x))) 42)
      1764)
(test ((lambda (x y z) x) 3 4 5)
      3)
(test ((lambda (x y z) y) 3 4 5)
      4)
(test ((lambda (x y z) z) 3 4 5)
      5)


;; We should see an error here, since the arity is wrong
(test/exn ((lambda (x y z) x) 3))
(test/exn ((lambda (x y z) x) 3 4 5 6))




;(simulate (compile (parse '42) 'val 'next))
;(compile (parse '(+ 3 4)) 'val 'next)