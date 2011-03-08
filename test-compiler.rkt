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
    [(_ code exp options ...)
     (with-syntax ([stx stx])
       (syntax/loc #'stx
         (begin
           (printf "Running ~s ...\n" 'code)
           (let*-values([(a-machine num-steps) 
                         (run (new-machine (run-compiler 'code)) options ...)]
                        [(actual) (machine-val a-machine)])
             (unless (equal? actual exp)
               (raise-syntax-error #f (format "Expected ~s, got ~s" exp actual)
                                   #'stx))
             (unless (= (length (machine-env a-machine)) 1)
               (raise-syntax-error #f (format "Stack is not back to the prefix as expected!")
                                   #'stx))
             (printf "ok. ~s steps.\n\n" num-steps)))))]))

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
(define (run m 
             #:debug? (debug? false)
             #:stack-limit (stack-limit false))
  (let loop ([m m]
             [steps 0])
    (when debug?
      (when (can-step? m)
        (printf "env-depth=~s instruction=~s\n" 
                (length (machine-env m))
                (current-instruction m))))
    (when stack-limit
      (when (stack-overflow? (machine-env m) stack-limit)
        (error 'run "Stack overflow")))

    (cond
      [(can-step? m)
       (loop (step m) (add1 steps))]
      [else
       (values m steps)])))

(define (stack-overflow? l n)
  (cond
    [(empty? l)
     #f]
    [(= n 0)
     #t]
    [else
     (stack-overflow? (rest l) (sub1 n))]))


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


(test (+ (* 3 4) 5)
      17)



;; Square
(test (begin (define (f x)
               (* x x))
             (f 3))
      9)



;; composition of square
(test (begin (define (f x)
               (* x x))
             (f (f 3)))
      81)


;; Slightly crazy expression
(test (begin (define (f x)
               (* x x))
             (define (g x)
               (* x x x))
             (- (g (f (+ (g 3)
                         (f 3))))
                1))
      2176782335)


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

;; And this should fail because it's not a lambda
(test/exn (not-a-procedure 5))

;; We should see an error here, since the arity is wrong
(test/exn ((lambda (x y z) x) 3))
(test/exn ((lambda (x y z) z) 3))
(test/exn ((lambda (x y z) x) 3 4 5 6))




; factorial
(test (begin (define (f x)
               (if (= x 0)
                   1
                   (* x (f (sub1 x)))))
             (f 0))
      1)
(test (begin (define (f x)
               (if (= x 0)
                   1
                   (* x (f (sub1 x)))))
             (f 1))
      1)
(test (begin (define (f x)
               (if (= x 0)
                   1
                   (* x (f (sub1 x)))))
             (f 2))
      2)

(test (begin (define (f x)
               (if (= x 0)
                   1
                   (* x (f (sub1 x)))))
             (f 100))
      93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
      )



;; Tail calling behavior: watch that the stack never grows beyond 8.
(test (begin (define (f x acc)
                 (if (= x 0)
                     acc
                     (f (sub1 x) (* x acc))))
               (f 1000 1))
      (letrec ([f (lambda (x)
                    (if (= x 0)
                        1
                        (* x (f (sub1 x)))))])
        (f 1000))
      #:stack-limit 8)
      

;; tak test
(test (begin (define (tak x y z)
               (if (>= y x)
                   z
                   (tak (tak (- x 1) y z)
                        (tak (- y 1) z x)
                        (tak (- z 1) x y))))
             (tak 18 12 6))
      7)
      

(test (begin (define (tak x y z)
               (if (>= y x)
                   z
                   (tak (tak (- x 1) y z)
                        (tak (- y 1) z x)
                        (tak (- z 1) x y))))
             (tak 18 12 6))
      7
      #:stack-limit 120)



;(simulate (compile (parse '42) 'val 'next))
;(compile (parse '(+ 3 4)) 'val 'next)