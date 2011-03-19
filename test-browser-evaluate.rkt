#lang racket
(require "browser-evaluate.rkt"
         "package.rkt"
         racket/runtime-path)


(define-runtime-path runtime.js "runtime.js")

(define evaluate (make-evaluate 
                  (lambda (program op)

                    (fprintf op "(function () {")
                    
                    ;; The runtime code
                    (call-with-input-file* runtime.js
                      (lambda (ip)
                        (copy-port ip op)))
                    
                    (newline op)
                    
                    (fprintf op "var innerInvoke = ")
                    (package-anonymous program op)
                    (fprintf op "();\n")
                    
                    (fprintf op #<<EOF
return (function(succ, fail, params) {
            return innerInvoke(MACHINE, succ, fail, params);
        });
});
EOF
                             )
                    
                    )))

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
                                     #'stx)))
             (printf " ok (~a milliseconds)\n" (evaluated-t result))))))]))

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

(test  '(begin
          (define (ctak x y z)
            (call-with-current-continuation
             (lambda (k)
               (ctak-aux k x y z))))
          
          (define (ctak-aux k x y z)
            (cond ((not (< y x))  ;xy
                   (k z))
                  (else (call-with-current-continuation
                         (ctak-aux
                          k
                          (call-with-current-continuation
                           (lambda (k)
                             (ctak-aux k
                                       (- x 1)
                                       y
                                       z)))
                          (call-with-current-continuation
                           (lambda (k)
                             (ctak-aux k
                                       (- y 1)
                                       z
                                       x)))
                          (call-with-current-continuation
                           (lambda (k)
                             (ctak-aux k
                                       (- z 1)
                                       x
                                       y))))))))
          (displayln (ctak 18 12 6)))
       "7\n")

(test '(letrec ([f (lambda (x)
                    (if (= x 0)
                        1
                        (* x (f (sub1 x)))))])
         (display (f 10)))
      "3628800")

(test '(letrec ([tak (lambda (x y z)
                       (if (>= y x)
                           z
                           (tak (tak (- x 1) y z)
                                (tak (- y 1) z x)
                                (tak (- z 1) x y))))])
         (displayln (tak 18 12 6)))
        "7\n")



#;(test (read (open-input-file "tests/conform/program0.sch"))
      (port->string (open-input-file "tests/conform/expected0.txt")))