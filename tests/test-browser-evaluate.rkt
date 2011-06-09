#lang racket
(require "browser-evaluate.rkt"
         "../js-assembler/package.rkt"
         "../make/make-structs.rkt")

(printf "test-browser-evaluate.rkt\n")


(define should-follow? (lambda (src) #t))

(define evaluate (make-evaluate 
                  (lambda (program op)

                    (fprintf op "(function () {")
                    
                    ;; The runtime code
                    (displayln (get-runtime) op)
                    
                    (newline op)
                    
                    (fprintf op "var innerInvoke = ")
                    (package-anonymous (make-SexpSource program)
                                       #:should-follow-children? should-follow?
                                       #:output-port op)
                    (fprintf op "();\n")
                    
                    (fprintf op #<<EOF
return (function(succ, fail, params) {
            return innerInvoke(new plt.runtime.Machine(), succ, fail, params);
        });
});
EOF
                             )
                    
                    )))

(define-syntax (test stx)
  (syntax-case stx ()
    [(_ s exp)
     (with-syntax ([stx stx])
       (syntax/loc #'stx
         (begin
           (printf "running test... ~s" (syntax->datum #'stx))
           (let ([result (evaluate s)])
             (let ([output (evaluated-stdout result)])
               (unless (string=? output exp)
                 (printf " error!\n")
                 (raise-syntax-error #f (format "Expected ~s, got ~s" exp output)
                                     #'stx)))
             (printf " ok (~a milliseconds)\n" (evaluated-t result))))))]))

(define-syntax (test/exn stx)
  (syntax-case stx ()
    [(_ s exp)
     (with-syntax ([stx stx])
       (syntax/loc #'stx
         (begin
           (printf "running test...")
           (let ([an-error-happened 
                  (with-handlers ([error-happened?
                                   (lambda (exn)
                                     exn)])
                    (let ([r (evaluate s)])
                      (raise-syntax-error #f (format "Expected exception, but got ~s" r)
                                          #'stx)))]) 
             (unless (regexp-match (regexp-quote exp) (error-happened-str an-error-happened))
               (printf " error!\n")
               (raise-syntax-error #f (format "Expected ~s, got ~s" exp (error-happened-str an-error-happened))
                                   #'stx))
             (printf " ok (~a milliseconds)\n" (error-happened-t an-error-happened))))))]))








(test '(display 42)
      "42")

(test '(displayln (+))
      "0\n")

(test '(displayln (*))
      "1\n")

(test '(displayln (- 3))
      "-3\n")

(test '(displayln (- 3 4))
      "-1\n")

(test '(displayln (- 3 4 -10))
      "9\n")


(test '(display (+ 3 4))
      "7")

(test/exn (evaluate '(+ "hello" 3))
          "Error: +: expected number as argument 1 but received \"hello\"")


(test '(display (/ 100 4))
      "25")
;; fixme: symbols need to be represented separately from strings.
(test/exn (evaluate '(/ 3 'four))
          "Error: /: expected number as argument 2 but received \"four\"")


(test '(display (- 1))
      "-1")

(test/exn '(- 'one)
          "Error: -: expected number as argument 1 but received \"one\"")

(test '(display (- 5 4))
      "1")

(test '(display (* 3 17))
      "51")

(test/exn '(* "three" 17)
          "Error: *: expected number as argument 1 but received \"three\"")

(test '(display '#t)
      "true")

(test '(display '#f)
      "false")

(test '(displayln (not #t))
      "false\n")

(test '(displayln (not #f))
      "true\n")

(test '(displayln (not 3))
      "false\n")

(test '(displayln (not (not 3)))
      "true\n")

(test '(displayln (not 0))
      "false\n")


(test '(displayln (add1 1))
      "2\n")


(test '(displayln (if 0 1 2))
      "1\n")


(test/exn '(displayln (add1 "0"))
          "Error: add1: expected number as argument 1 but received \"0\"")

(test '(displayln (sub1 1))
      "0\n")

(test/exn '(displayln (sub1 "0"))
          "Error: sub1: expected number as argument 1 but received \"0\"")

(test '(displayln (< 1 2))
      "true\n")

(test '(displayln (< 2 1 ))
      "false\n")

(test '(displayln (< 1 1 ))
      "false\n")


(test '(displayln (<= 1 2))
      "true\n")

(test '(displayln (<= 2 1))
      "false\n")

(test '(displayln (<= 2 2))
      "true\n")

(test '(displayln (= 1 2))
      "false\n")

(test '(displayln (= 1 1))
      "true\n")


(test '(displayln (> 1 2))
      "false\n")

(test '(displayln (> 2 1))
      "true\n")

(test '(displayln (> 2 2))
      "false\n")

(test '(displayln (>= 1 2))
      "false\n")

(test '(displayln (>= 2 1))
      "true\n")

(test '(displayln (>= 2 2))
      "true\n")


(test '(displayln (car (cons 3 4)))
      "3\n")

(test '(displayln (cdr (cons 3 4)))
      "4\n")

(test '(displayln (let ([x (cons 5 6)])
                    (car x)))
      "5\n")

(test '(displayln (let ([x (cons 5 6)])
                    (cdr x)))
      "6\n")

(test '(displayln (length (list 'hello 4 5)))
      "3\n")



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



(test '(begin (displayln (+ 42 (call/cc (lambda (k) 3)))) )
      "45\n")


(test '(begin (displayln (+ 42 (call/cc (lambda (k) (k 100) 3)))) )
      "142\n")

(test '(begin (displayln (+ 42 (call/cc (lambda (k) 100 (k 3))))) )
      "45\n")


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




(test '(begin (define counter 0)
              (set! counter (add1 counter))
              (displayln counter))
      "1\n")

(test '(begin (define x 16)
              (define (f x)
                (set! x (add1 x))
                x)
              (displayln (f 3))
              (displayln (f 4))
              (displayln x))
      "4\n5\n16\n")
      

(test/exn '(let ([x 0])
             (set! x "foo")
             (add1 x))
          "Error: add1: expected number as argument 1 but received \"foo\"")



(test '(for-each displayln (member 5 '(1 2 5 4 3)))
      "5\n4\n3\n")

(test '(displayln (member 6 '(1 2 5 4 3)))
      "false\n")


(test '(displayln (length (reverse '())))
      "0\n")

(test '(displayln (car (reverse '("x"))))
      "x\n")

(test '(displayln (car (reverse '("x" "y"))))
      "y\n")

(test '(displayln (car (cdr (reverse '("x" "y")))))
      "x\n")

(test '(displayln (car (reverse '("x" "y" "z"))))
      "z\n")
(test '(displayln (car (cdr (reverse '("x" "y" "z")))))
      "y\n")
(test '(displayln (car (cdr (cdr (reverse '("x" "y" "z"))))))
      "x\n")


(test '(begin (displayln (vector-length (vector))))
      "0\n")

(test '(begin (displayln (vector-length (vector 3 1 4))))
      "3\n")

(test '(begin (displayln (vector-ref (vector 3 1 4) 0)))
      "3\n")

(test '(begin (displayln (vector-ref (vector 3 1 4) 1)))
      "1\n")

(test '(begin (displayln (vector-ref (vector 3 1 4) 2)))
      "4\n")

(test '(begin (define v (vector "hello" "world"))
	      (vector-set! v 0 'hola)
	      (displayln (vector-ref v 0)))
      "hola\n")

(test '(begin (define v (vector "hello" "world"))
	      (vector-set! v 0 'hola)
	      (displayln (vector-ref v 1)))
      "world\n")



(test '(begin (define l (vector->list (vector "hello" "world")))
	      (displayln (length l))
	      (displayln (car l))
	      (displayln (car (cdr l))))
      "2\nhello\nworld\n")


(test '(displayln (equal? '(1 2 3)
			  (append '(1) '(2) '(3))))
      "true\n")


(test '(displayln (equal? '(1 2 3)
			  (append '(1 2) '(3))))
      "true\n")

(test '(displayln (equal? '(1 2 3)
			  (append '(1 2) 3)))
      "false\n")

(test '(displayln (equal? "hello"
			  (string-append "he" "llo")))
      "true\n")


(test '(displayln (equal? '(1 2 (3))
			  '(1 2 (3))))
      "true\n")


(test '(displayln (equal? (list 1 2 (vector 3))
			  (list 1 2 (vector 3))))
      "true\n")


(test '(displayln (equal? (list 1 2 (vector 4))
			  (list 1 2 (vector 3))))
      "false\n")

      
;;(test '(displayln 2/3)
;;      "2/3\n")


;;(test '(displayln -2/3)
;;      "-2/3\n")


(test '(displayln -0.0)
      "-0.0\n")

(test '(displayln +nan.0)
      "+nan.0\n")

(test '(displayln +inf.0)
      "+inf.0\n")

(test '(displayln -inf.0)
      "-inf.0\n")








;; Knuth's Man-or-boy-test.
;; http://rosettacode.org/wiki/Man_or_boy_test
(test '(begin (define (A k x1 x2 x3 x4 x5)
		(letrec ([B (lambda ()
			   (set! k (- k 1))
			   (A k B x1 x2 x3 x4))])
		  (if (<= k 0)
		      (+ (x4) (x5))
		      (B))))
	      (displayln (A 10
			    (lambda () 1) 
			    (lambda () -1) 
			    (lambda () -1)
			    (lambda () 1) 
			    (lambda () 0))))
      "-67\n")



#;(test (read (open-input-file "tests/conform/program0.sch"))
      (port->string (open-input-file "tests/conform/expected0.txt")))