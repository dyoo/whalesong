#lang racket
(require "browser-evaluate.rkt"
         "package.rkt")

(define evaluate (make-evaluate package-anonymous))

;; test-find-toplevel-variables
(define-syntax (test stx)
  (syntax-case stx ()
    [(_ s exp)
     (with-syntax ([stx stx])
       (syntax/loc #'stx
         (begin
           (printf "running test...")
           (let ([result (evaluate s)])
             (let ([output (evaluated-stdout result)])
               (unless (string=? output exp)
                 (printf " error!\n")
                 (raise-syntax-error #f (format "Expected ~s, got ~s" exp output)
                                     #'stx))))
           (printf " ok\n"))))]))


(test '(begin (define (f x) 
                (if (= x 0)
                    0
                    (+ x (f (- x 1)))))
              (display (f 3))
              (display "\n")
              (display (f 4))
              (display "\n")
              (display (f 10000)))
      "6\n10\n50005000")

(test '(begin (define (length l)
                (if (null? l)
                    0
                    (+ 1 (length (cdr l)))))
              (display (length (list 1 2 3 4 5 6)))
              (newline)
              (display (length (list "hello" "world")))
              (newline))
              
      "6\n2\n")

(test '(begin (define (tak x y z)
                (if (< y x)
                    (tak (tak (- x 1) y z)
                         (tak (- y 1) z x)
                         (tak (- z 1) x y))
                    z))
              (display (tak 18 12 6)))
      "7")


(test '(begin (define (fib x)
                (if (< x 2)
                    x
                    (+ (fib (- x 1))
                       (fib (- x 2)))))
              (displayln (fib 3))
              (displayln (fib 4))
              (displayln (fib 5))
              (displayln (fib 6)))
      "2\n3\n5\n8\n")
      

(test '(begin (define (tak x y z)
               (if (>= y x)
                   z
                   (tak (tak (- x 1) y z)
                        (tak (- y 1) z x)
                        (tak (- z 1) x y))))
             (displayln (tak 18 12 6)))
        "7\n")

(test '(begin (define program (lambda ()
                                (let ((y (call/cc (lambda (c) c))))
                                  (display 1)
                                  (call/cc (lambda (c) (y c)))
                                  (display 2)
                                  (call/cc (lambda (c) (y c)))
                                  (display 3))))
              (program))
      "11213")


(test '(begin (define (f return)
                (return 2)
                3)
              (display (f (lambda (x) x))) ; displays 3
              (display (call/cc f)) ;; displays 2
              )
      "32")

