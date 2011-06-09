#lang racket

(require "../simulator/simulator.rkt"
         "../simulator/simulator-structs.rkt"
         "../simulator/simulator-helpers.rkt"
         "../parameters.rkt"
         "test-helpers.rkt"
         racket/runtime-path
         rackunit)

(printf "test-compiler.rkt\n")

(define-runtime-path this-test-path ".")

  

;; Test out the compiler, using the simulator.
(define-syntax (test stx)
  (syntax-case stx ()
    [(_ code exp options ...)
     (with-syntax ([stx stx])
       (syntax/loc #'stx
         (begin
           (printf "Running ~s ...\n" code)
           (let*-values([(a-machine num-steps) 
                         (run code options ...)]
                        [(actual) (PrimitiveValue->racket (machine-val a-machine))])
             (unless (equal? actual exp)
               (raise-syntax-error #f (format "Expected ~s, got ~s" exp actual)
                                   #'stx))
             (unless (= (machine-stack-size a-machine) 0)
               (raise-syntax-error #f (format "Stack is not back to empty as expected!")

                                   #'stx))
             (unless (null? (machine-control a-machine))
               (raise-syntax-error #f (format "Control is not empty as expected!")
                                   #'stx))
             (printf "ok. ~s steps.\n\n" num-steps)))))]))

;; test, and expect an error
(define-syntax (test/exn stx)
  (syntax-case stx ()
    [(_ code options ...)
     (with-syntax ([stx stx])
       (syntax/loc #'stx
         (begin
           (printf "Running/exn ~s ...\n" code)
           (let/ec return
             (with-handlers ([exn:fail? (lambda (exn)
                                          (printf "ok\n\n")
                                          (return))])
               (run code options ...))
             (raise-syntax-error #f (format "Expected an exception")
                                 #'stx)))))]))



;; run: machine -> (machine number)
;; Run the machine to completion.
(define (run code
             #:debug? (debug? false)
             #:stack-limit (stack-limit false)
             #:control-limit (control-limit false)
             #:with-bootstrapping? (with-bootstrapping? false)
             #:as-main-module (as-main-module #f))
  (let ([m (new-machine (run-compiler code) with-bootstrapping?)])
    (let loop ([steps 0])
      (when debug?
        (when (can-step? m)
          (printf "|env|=~s, |control|=~s,  instruction=~s\n" 
                  (length (machine-env m))
                  (length (machine-control m))
                  (current-instruction m))))
      (when stack-limit
        (when (> (machine-stack-size m) stack-limit)
          (error 'run "Stack overflow")))
      
      (when control-limit
        (when (> (machine-control-size m) control-limit)
          (error 'run "Control overflow")))
      
      (cond
        [(can-step? m)
         (step! m)
         (loop (add1 steps))]
        [else
         (cond
           [as-main-module
            ;; Set the pc to the module's entry point
            ;; Set the return point to halt on exit.
            (invoke-module-as-main m as-main-module)
            (set! as-main-module #f)
            (loop (add1 steps))]
           [else         
            (values m steps)])]))))


;; Atomic expressions
(test '42 42)
(test '"hello world" "hello world")
(test '#t true)
(test '#f false)

;; quoted 
(test ''(+ 3 4)
      '(+ 3 4))

;; Simple definitions
(test '(let () (define x 42)
             (+ x x))
      84)

(test '(let () (define x 6)
             (define y 7)
             (define z 8)
             (* x y z))
      (* 6 7 8))

;; Simple branching
(test '(if #t 'ok 'not-ok)
      'ok)

(test '(if #f 'not-ok 'ok)
      'ok)

;; Sequencing
(test '(begin 1
             2
             3)
      3)
(test '(begin 1)
      1)


(test '(+ (* 3 4) 5)
      17)



;; Square
(test '(begin (define (f x)
               (* x x))
             (f 3))
      9)


;; Other simple expressions
(test '(+ 137 349)
      486)


(test '(/ 10 5)
      2)

(test '(- 1 2)
      -1)

(test '(- 3)
      -3)

(test '(*)
      1)

;; composition of square
(test '(let () (define (f x)
               (* x x))
             (f (f 3)))
      81)

(test '(let () (define pi 3.14159)
             (define radius 10)
             (* pi (* radius radius)))
      314.159)

(test '(let () (define pi 3.14159)
             (define radius 10)
             (define circumference (* 2 pi radius))
             circumference)
      62.8318)

;; Slightly crazy expression
(test '(let () (define (f x)
               (* x x))
             (define (g x)
               (* x x x))
             (- (g (f (+ (g 3)
                         (f 3))))
                1))
      2176782335)


;; Simple application
(test '((lambda (x) x) 42)
      42)
(test '((lambda (x) 
         (begin (* x x))) 42)
      1764)
(test '((lambda (x y z) x) 3 4 5)
      3)
(test '((lambda (x y z) y) 3 4 5)
      4)
(test '((lambda (x y z) z) 3 4 5)
      5)

;; And this should fail because it's not a lambda
(test/exn '(not-a-procedure 5))

;; We should see an error here, since the arity is wrong
(test/exn '((lambda (x y z) x) 3))
(test/exn '((lambda (x y z) z) 3))
(test/exn '((lambda (x y z) x) 3 4 5 6))




; factorial
(test '(let () (define (f x)
               (if (= x 0)
                   1
                   (* x (f (sub1 x)))))
             (f 0))
      1)
(test '(let () (define (f x)
               (if (= x 0)
                   1
                   (* x (f (sub1 x)))))
             (f 1))
      1)
(test '(let () (define (f x)
               (if (= x 0)
                   1
                   (* x (f (sub1 x)))))
             (f 2))
      2)

(test '(let () (define (f x)
               (if (= x 0)
                   1
                   (* x (f (sub1 x)))))
             (f 100))
      93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
      )




;; Tail calling behavior: watch that the stack never grows beyond some ceiling.
(test '(let () (define (f x acc)
                 (if (= x 0)
                     acc
                     (f (sub1 x) (* x acc))))
               (f 1000 1))
      (letrec ([f (lambda (x)
                    (if (= x 0)
                        1
                        (* x (f (sub1 x)))))])
        (f 1000))
      #:control-limit 3
      #:stack-limit 15
      ;;#:debug? #t
      )


;; And from experimental testing, anything below 7 will break.
(test/exn '(let () (define (f x acc)
                   (if (= x 0)
                       acc
                       (f (sub1 x) (* x acc))))
                 (f 1000 1))
          (letrec ([f (lambda (x)
                        (if (= x 0)
                            1
                            (* x (f (sub1 x)))))])
            (f 1000))
          #:stack-limit 7)





(test '((let* ([x 42]
               [f (lambda () x)])
          f))
      42)

(test '((let* ([x 42]
               [y 43]
               [f (lambda () (list x y))])
          f))
      (list 42 43))

(test '(let () (define (m f l)
                (if (null? l)
                    l
                    (cons (f (car l))
                          (m f (cdr l)))))
              (m (lambda (x) (add1 x)) '(2 3 4 5)))
      '(3 4 5 6))


(test '(+ (+ 3 4)
          6)
      (+ 3 4 6))

(test '(+ (+ 3 (+ 4 5))
          6)
      (+ 3 4 5 6))



(test '(let () 
         (define (foo y)
           y)
         (define (sum-iter x acc)
           (if (= x 0)
               acc
               (let* ([y (sub1 x)]
                      [z (+ x acc)])
                 (foo y)
                 (sum-iter y z))))
         (sum-iter 300 0))
      45150
      #:stack-limit 20
      #:control-limit 4)


(test '(pair? '(+ (* 3 x x) (* a x x) (* b x) 5))
      #t)

(test '(eq? (car '(+ (* 3 x x) (* a x x) (* b x) 5)) 
	    '+)
      #t)




(test '(letrec ([even? (lambda (x)
			 (if (= x 0)
			     #t
			     (odd? (sub1 x))))]
		[odd? (lambda (x)
			(if (= x 0)
			    #f
			    (even? (sub1 x))))])
	 (list (even? 1024)
	       (even? 1023)
	       (even? 2172)
	       (even? 2171)))
      (list #t #f #t #f))




(test '(letrec-values ([(even? odd?)
			(values
			 (lambda (x)
			   (if (= x 0)
			       #t
			       (odd? (sub1 x))))
			 (lambda (x)
			   (if (= x 0)
			       #f
			       (even? (sub1 x)))))])
	 (list (even? 1024)
	       (even? 1023)
	       (even? 2172)
	       (even? 2171)))
      (list #t #f #t #f)
      #:with-bootstrapping? #t)





(test '(letrec ([fact (lambda (x)
			(if (= x 0)
			    1
			    (* x (fact (sub1 x)))))])
	 (list (fact 3) (fact 4) (fact 5)))
      '(6 24 120))





;; deriv
(test '(let ()
         (define (deriv-aux a) (list '/ (deriv a) a))
         (define (map f l)
           (if (null? l)
               l
               (cons (f (car l))
                     (map f (cdr l)))))
         (define (deriv a)
           (if (not (pair? a))
               (if (eq? a 'x) 1  0)
               (if (eq? (car a) '+)
                   (cons '+ (map deriv (cdr a)))
                   (if (eq? (car a) '-)
                       (cons '- (map deriv (cdr a)))
                       (if (eq? (car a) '*)
                           (list '*
                                 a
                                 (cons '+ (map deriv-aux (cdr a))))
                           (if (eq? (car a) '/)
                               (list '-
                                     (list '/
                                           (deriv (cadr a))
                                           (caddr a))
                                     (list '/
                                           (cadr a)
                                           (list '*
                                                 (caddr a)
                                                 (caddr a)
                                                 (deriv (caddr a)))))
                               'error))))))
         (deriv '(+ (* 3 x x) (* a x x) (* b x) 5)))
      '(+ (* (* 3 x x) (+ (/ 0 3) (/ 1 x) (/ 1 x)))
          (* (* a x x) (+ (/ 0 a) (/ 1 x) (/ 1 x)))
          (* (* b x) (+ (/ 0 b) (/ 1 x)))
          0))



(test '(map (lambda (x) (* x x))
            '(1 2 3 4 5))
      '(1 4 9 16 25)
      #:with-bootstrapping? #t)


;; Foldl
(test '(let()
	 (define (foldl f acc lst)
	   (if (null? lst)
	       acc
	       (foldl f (f (car lst) acc) (cdr lst))))
	 (foldl (lambda (x acc)
		  (* x acc))
		1
		'(1 2 3 4 5 6 7 8 9 10)))
      (* 1 2 3 4 5 6 7 8 9 10))



;; iterating, with some crazy expressions
(test '(begin (define (iterate f x n)
                 (if (= n 0)
                     x
                     (iterate f (f x) (sub1 n))))
               (list (iterate (lambda (x) (* x x)) 20 2)
                     (iterate add1 1 1000)
                     (iterate (lambda (x)
                                (iterate (lambda (y)
                                           (+ x y))
                                         x
                                         x))
                              1
                              3)))
      (list 160000 1001 42))

;; Trying out closures
(test '(begin
        (define delta 1)
        (define (diff f)
          (lambda (x)
            (/ (- (f (+ x delta))
                  (f x))
               delta)))
        (define 2x (diff (lambda (x) (* x x))))
        (define two (diff 2x))
        (list (2x 1000)
              (two 2011)))
      (list 2001 2))



(test '(begin (define (square x)
               (* x x))
             (square (square 3)))
      81)


(test '(begin (define (square x)
               (* x x))
             (define (sum-of-squares x y)
               (+ (square x) (square y)))
             (sum-of-squares 3 4))
      25)


(test '(let() (define (sqrt-iter guess x)
               (if (good-enough? guess x)
                   guess
                   (sqrt-iter (improve guess x)
                              x)))
             (define (improve guess x)
               (average guess (/ x guess)))
             
             (define (square x)
               (* x x))
             
             (define (average x y)
               (/ (+ x y) 2))
             
             (define (good-enough? guess x)
               (< (abs (- (square guess) x)) 0.001))
             
             (define (sqrt x)
               (sqrt-iter 1.0 x))
             
             (list (sqrt 9) (sqrt 23942) (sqrt 31337)))

      '(3.00009155413138 154.73202642085838 177.02259745919164))

                  



;; Exponentiation
(test '(let () (define (expt b n)
               (if (= n 0)
                   1
                   (* b (expt b (- n 1)))))
             (expt 2 30))
      (expt 2 30))

(test '(let () 
        (define (expt b n)
          (expt-iter b n 1))
        
        (define (expt-iter b counter product)
          (if (= counter 0)
              product
              (expt-iter b
                         (- counter 1)
                         (* b product))))
        (expt 2 30))
      (expt 2 30))

(test '(let()
        (define (fast-expt b n)
          (cond ((= n 0) 1)
                ((even? n) (square (fast-expt b (/ n 2))))
                (else (* b (fast-expt b (- n 1))))))
        (define (square x) (* x x))
        (define (expt b n)
          (fast-expt b n))
        (define (even? n)
          (= (remainder n 2) 0))
        
        (list (expt 2 30)
              (expt 2 23984000)))
      
      (list (expt 2 30)
            (expt 2 23984000)))


(test '(let () (define (length l)
               (if (null? l)
                   0
                   (+ 1 (length (cdr l)))))
             (display (length (list 1 2 3 4 5 6)))
             (newline)
             #;(display (length (list "hello" "world")))
             #;(newline))
      (void))



(test '(let () (define (f x)
                (* x x))
              (f 3)
              (f 4)
              (f 5))
      25)


(test '(let () (define (sum-integers a b)
               (if (> a b)
                   0
                   (+ a (sum-integers (+ a 1) b))))        
             (sum-integers 1 100))
      (* 50 101))


(test '(let () (define (sum term a next b)
               (if (> a b)
                   0
                   (+ (term a)
                      (sum term (next a) next b))))
             (define (inc n) (+ n 1))
             (define (identity x) x)
             (define (cube x) (* x x x))
             (define (sum-cubes a b) (sum cube a inc b))
             (define (sum-integers a b) (sum identity a inc b))
             
             (list (sum-cubes 1 10)
                   (sum-integers 1 10)))
      (list 3025 55))


;; Lexical scope bug: make sure that parameters shadow toplevels.
(test '(let ()
         (define x 42)
         (define (f x)
           (+ x 1))
         (f 16))
      17)


(test '(let () 5) 5)

(test '(let* ([x 3]
             [y 4]
             [z 17])
        (+ x y z))
      24)
      

(test '(list (let* ([x 3]
                   [y (+ x 1)]
                   [z (+ x y)])
               (list x y z))
            4)
      (list (list 3 4 7)
            4))


(test '(list (let* ([x 3]
                   [y (+ x 1)]
                   [z (+ x y)])
               (list x y z))
            (let* ([x 17]
                   [y (+ x 1)]
                   [z (+ x y)])
               (list x y z)))
      (list (list 3 4 7)
            (list 17 18 35)))

(test '(let* ([x 0]
             [x (add1 x)]
             [x (add1 x)])
        x)
      2)
 

(test '(begin 
          (define (sum-iter x acc)
            (if (= x 0)
                acc
                (let* ([y (sub1 x)]
                       [z (+ x acc)])
                  (sum-iter y z))))
          (sum-iter 300 0))
      45150
      #:stack-limit 8
      #:control-limit 3)







(test '(let ([x 16])
           (call/cc (lambda (k) (+ x x))))
      32
      #:with-bootstrapping? #t)


(test '(add1 (let ([x 16])
               (call/cc (lambda (k) 
                          (k 0)
                          (+ x x)))))
      1
      #:with-bootstrapping? #t)


;; Reference:  http://lists.racket-lang.org/users/archive/2009-January/029812.html
#;(let ([op (open-output-string)])
  (parameterize ([current-simulated-output-port op])
    (test '(let () (define program (lambda ()
                                    (let ((y (call/cc (lambda (c) c))))
                                      (display 1)
                                      (call/cc (lambda (c) (y c)))
                                      (display 2)
                                      (call/cc (lambda (c) (y c)))
                                      (display 3))))
                  (program))
          (void)
          #:with-bootstrapping? #t)
    (unless (string=? (get-output-string op)
                      "11213")
      (error "puzzle failed: ~s" (get-output-string op)))))





(test '(let ([x 3]
            [y 4])
        (let ([x y]
              [y x])
          (list x y)))
      (list 4 3))


(test '(letrec ([f (lambda (x)
                    (if (= x 0)
                        1
                        (* x (f (sub1 x)))))])
        (f 10))
      3628800)

(test '(letrec ([e (lambda (x)
                    (if (= x 0)
                        #t
                        (o (sub1 x))))]
               [o (lambda (x)
                    (if (= x 0)
                        #f
                        (e (sub1 x))))])
        (list (e 1236)
              (e 2391)))
      (list #t #f))
                  



(test '(let loop ([i 0])
         (cond [(= i 5)
	        '(ok)]
               [else (cons i (loop (add1 i)))]))
      '(0 1 2 3 4 ok))
	     


(test '(begin (define counter 0)
              (set! counter (add1 counter))
              counter)
      1)

(test '(begin (define x 16)
              (define (f x)
                (set! x (add1 x))
                x)
              (list (f 3)
                    (f 4)
                    x))
      (list 4 5 16))
      


(test '(begin (define a '(hello))
              (define b '(world))
              (define reset!
                (lambda ()
                  (set! a '())))
              (reset!)
              (list a b))
      '(() (world)))


(test '(begin (define a '(hello))
              (define b '(world))
              (define reset!
                (lambda ()
                  (set! b '())))
              (reset!)
              (list a b))
      '((hello) ()))

(test '(begin (define a '(hello))
              (define b '(world))
              (define reset!
                (lambda ()
                  (set! a '())
                  'ok))
              (list a b (reset!) a b))
      '((hello) (world) ok () (world)))

(test '(begin (define a '(hello))
              (define b '(world))
              (define reset!
                (lambda ()
                  (set! b '())
                  'ok))
              (reset!)
              (list a b))
      '((hello)()))


(test '(begin (define a '(hello))
              (define b '(world))
              (define reset!
                (lambda ()
                  (set! a '())
                  (set! b '())))
              (reset!)
              (list a b))
      '(()()))


(let ([op (open-output-string)])
  (parameterize ([current-simulated-output-port op])
    (test 
     '(letrec ([a (lambda (x)
                    (display "a") (display x) (c (add1 x)))]
               [b (lambda (k)
                    (display "b") (display k) (e (add1 k)))]
               [c (lambda (y)
                    (display "c") (display y) (b (add1 y)))]
               [d (lambda (z)
                    (display "d") (display z) z)]
               [e (lambda (x)
                    (display "e") (display x) (d (add1 x)))])
        (a 0))
     4))
  (unless (string=? "a0c1b2e3d4" (get-output-string op))
    (error 'letrec-failed)))
      

(test '(letrec ([a (lambda ()
                     (b))]
                [b (lambda ()
                     (c))]
                [c (lambda ()
                     (d))]
                [d (lambda ()
                     (e))]
                [e (lambda ()
                     (f))]
                [f (lambda ()
                     (g))]
                [g (lambda ()
                     "gee!")])
         (a))
      "gee!"
      #:stack-limit 20
      #:control-limit 2)



(test '(letrec ([a (lambda ()
                     (b))]
                [b (lambda ()
                     (c))]
                [c (lambda ()
                     (d))]
                [d (lambda ()
                     (e))]
                [e (lambda ()
                     (f))]
                [f (lambda ()
                     (g))]
                [g (lambda ()
                     "gee!")]
                [h (lambda ()
                     "ho!")])
         (a))
      "gee!"
      #:stack-limit 20
      #:control-limit 2)




(test '(letrec ([a (lambda (lst)
                       (b (cons "a" lst)))]
                  [b (lambda (lst)
                       (c (cons "b" lst)))]
                  [c (lambda (lst)
                       (d (cons "c" lst)))]
                  [d (lambda (lst)
                       (e (cons "d" lst)))]
                  [e (lambda (lst)
                       (f (cons "e" lst)))]
                  [f (lambda (lst)
                       (g (cons "f" lst)))]
                  [g (lambda (lst)
                       (cons "gee!" lst))])
           (a '()))
        '("gee!" "f" "e" "d" "c" "b" "a")
        #:stack-limit 20
        #:control-limit 2)







(test '(letrec ([sum-iter (lambda (x acc)
                               (if (= x 0)
                                   acc
                                   (let* ([y (sub1 x)]
                                          [z (+ x acc)])
                                     (sum-iter y z))))])
            (sum-iter 300 0))
        45150
        #:stack-limit 20
        #:control-limit 3)


(test '(begin (define counter
                (let ([x 0])
                  (lambda ()
                    (set! x (add1 x))
                    x)))
              (list (counter) (counter) (counter)))
      '(1 2 3))




(test '(begin
           (define (make-gen gen) 
             (let ([cont (box #f)])     
               (lambda ()
                 (call/cc (lambda (caller)
                            (if (unbox cont)
                                ((unbox cont) caller)
                                (gen (lambda (v)
                                       (call/cc (lambda (gen-k)
                                                  (begin
                                                    (set-box! cont gen-k)
                                                    (caller v))))))))))))
           
           (define g1 (make-gen (lambda (return)
                                  (return "a")
                                  (return "b")
                                  (return "c"))))
        
           (list (g1)))
      
        (list "a")
        #:with-bootstrapping? #t)





(test '(begin (define (f)
                  (define cont #f)
                  (define n 0)
                  (call/cc (lambda (x) (set! cont x)))
                  (set! n (add1 n))
                  (when (< n 10)
                    (cont 'dontcare))
                  n)
                (f))
        10
        #:with-bootstrapping? #t)


;; This should produce 1 because there's a continuation prompt around each evaluation,
;; and the call/cc cuts off at the prompt.
(test '(begin 
           (define cont #f)
           (define n 0)
           (call/cc (lambda (x) (set! cont x)))
           (set! n (add1 n))
           (when (< n 10)
             (cont 'dontcare))
           n)
        1
        #:with-bootstrapping? #t)



(test '(begin
         (define (make-gen gen) 
           (let ([cont (box #f)])     
             (lambda ()
               (call/cc (lambda (caller)
                          (if (unbox cont)
                              ((unbox cont) caller)
                              (gen (lambda (v)
                                     (call/cc (lambda (gen-k)
                                                (begin
                                                  (set-box! cont gen-k)
                                                  (caller v))))))))))))
         
         (define g1 (make-gen (lambda (return)
                                (return "a")
                                (return "b")
                                (return "c"))))
         
         (g1)
         (g1))
      "b"
      #:with-bootstrapping? #t)



(let ([op (open-output-string)])
  (parameterize ([current-simulated-output-port op])
    (test '(begin
             (define (make-gen gen) 
               (let ([cont (box #f)])     
                 (lambda ()
                   (call/cc (lambda (caller)
                              (if (unbox cont)
                                  ((unbox cont) caller)
                                  (gen (lambda (v)
                                         (call/cc (lambda (gen-k)
                                                    (begin
                                                      (set-box! cont gen-k)
                                                      (caller v))))))))))))
             
             (define g1 (make-gen (lambda (return)
                                    (return "a")
                                    (return "b")
                                    (return "c"))))
             
             (displayln (g1))
             (displayln (g1))
             (displayln (g1)))
          (void)
          #:with-bootstrapping? #t))
  (unless (string=? (get-output-string op) "a\nb\nc\n")
    (error 'failure)))


(test '(begin (define K #f)
              (let ([x 3]
                    [y 4]
                    [z 5])
                (+ x y z (call/cc (lambda (r)
                                    (set! K r)
                                    0))))
              (let* ([a 0]
                     [b 1])
                (+ 1024 (K 17))))
      29
      #:with-bootstrapping? #t)



(test '(begin (define (m f x y)
                (f (f x y) y))
              (m + 7 4))
      15)

(test '(begin (define (m f x y)
                (f (f x y) y))
              (m - 7 4))
      -1)


(test '(apply + '(1 2 3))
      6
      #:with-bootstrapping? #t)

(test '(apply + 4 5 6 '(1 2 3))
      21
      #:with-bootstrapping? #t)
(test '(apply string-append '("this" "is" "a" "test"))
      "thisisatest"
      #:with-bootstrapping? #t)

(test '(begin (define (f x y z)
                (cons x (cons y z)))
              (apply f (list "shiny" "happy" "monsters")))
      (cons "shiny" (cons "happy" "monsters"))
      #:with-bootstrapping? #t)


;; Some tests with vararity functions
(test `(begin (define mylist (lambda args args))
              (mylist 3 4 5))
      (list 3 4 5))

(test `(begin (define mylist (lambda args args))
              (apply mylist 3 4 5 '(6 7)))
      (list 3 4 5 6 7)
      #:with-bootstrapping? #t)

(test `(letrec ([f (lambda (x y . rest)
                     (apply g rest))]
                [g (lambda (x y z)
                     (list z y x))])
         (f 3 1 4 1 5))
      (list 5 1 4)
      #:with-bootstrapping? #t)

(test '(letrec ([sum-iter (lambda (x acc)
                               (if (apply = x 0 '())
                                   acc
                                   (let* ([y (apply sub1 x '())]
                                          [z (apply + (list x acc))])
                                     (apply sum-iter (list y z)))))])
            (sum-iter 300 0))
        45150
        #:stack-limit 20
        #:control-limit 3
        #:with-bootstrapping? #t)


(test '(values 3)
      3
      #:with-bootstrapping? #t)


(test '(begin (values "hi" "there")
              (string-append "hello " "world"))
      "hello world"
      #:with-bootstrapping? #t)

(test '(begin (values "hi" "there")
              (string-append (values "hello ") "world"))
      "hello world"
      #:with-bootstrapping? #t)


(test '(begin (values 3 4 5)
              17)
      17
      #:with-bootstrapping? #t)




(test '(with-continuation-mark 'name "danny" 
         (current-continuation-marks))
      (make-ContinuationMarkSet (list (cons 'name "danny"))))



(define-syntax (wcm-test stx)
  (syntax-case stx ()
    [(_ code expected options ...)
     (syntax/loc stx
       (let ([code-val code])
         (test `(begin (define (extract-current-continuation-marks key)
                         (continuation-mark-set->list
                          (current-continuation-marks)
                          key))
                       ,code-val)
               expected
               options ...)))]))


(wcm-test '(with-continuation-mark 'key 'mark
             (extract-current-continuation-marks 'key))
          '(mark))



(wcm-test '(with-continuation-mark 'key1 'mark1
             (with-continuation-mark 'key2 'mark2
               (list
                (extract-current-continuation-marks 'key1)
                (extract-current-continuation-marks 'key2))))
          '((mark1) (mark2)))


(wcm-test '(with-continuation-mark 'key 'mark1
             (with-continuation-mark 'key 'mark2 ; replaces previous mark
               (extract-current-continuation-marks 'key)))
          '(mark2))


(wcm-test '(with-continuation-mark 'key 'mark1
             (list ; continuation extended to evaluate the argument
              (with-continuation-mark 'key 'mark2
                (extract-current-continuation-marks 'key))))
          '((mark2 mark1)))

(wcm-test '(extract-current-continuation-marks 'key)
          '())

(wcm-test '(with-continuation-mark 'key 10 
             (extract-current-continuation-marks 'key))
          '(10))

(wcm-test '(with-continuation-mark 'key 10 
                    (with-continuation-mark 'key 11
                      (extract-current-continuation-marks 'key)))
          '(11))

(wcm-test '(with-continuation-mark 'key 10 
            (with-continuation-mark 'key2 9
              (with-continuation-mark 'key 11
                (extract-current-continuation-marks 'key2))))
          '(9))

(wcm-test '(with-continuation-mark 'key 10 
             (with-continuation-mark 'key2 9
               (with-continuation-mark 'key 11
                 (extract-current-continuation-marks 'key3))))
          '())

(wcm-test '(let ([x (with-continuation-mark 'key 10 (list 100))])
             (extract-current-continuation-marks 'key))
          '())

(wcm-test '(with-continuation-mark 'key 11
             (let ([x (with-continuation-mark 'key 10 (extract-current-continuation-marks 'key))])
               (extract-current-continuation-marks 'key)))
          '(11))

(wcm-test '(with-continuation-mark 'key 11
             (list (extract-current-continuation-marks 'key)
                   (with-continuation-mark 'key 10 (extract-current-continuation-marks 'key))
                   (extract-current-continuation-marks 'key)))
          '((11) (10 11) (11)))




;; Tests with call-with-values.
(test '(call-with-values (lambda () (values 3 4 5))
                         (lambda (x y z)
                           (list x z y)))
      (list 3 5 4)
      #:with-bootstrapping? #t)

(test '(call-with-values (lambda () (values 3 4 5))
                         (lambda (x . z)
                           (list x z)))
      (list 3 '(4 5))
      #:with-bootstrapping? #t)

(test '(call-with-values (lambda () (values))
                         (lambda z z))
                           
      (list)
      #:with-bootstrapping? #t)

(test '(call-with-values (lambda () (values 3 1 4))
                         +)
      8
      #:with-bootstrapping? #t)

(test '(call-with-values 
        (lambda () (values 1 2)) 
        (lambda (x y) y))
      2
      #:with-bootstrapping? #t)

(test '(call-with-values 
        (lambda () (values 1 2)) 
        (lambda z z))
      '(1 2)
      #:with-bootstrapping? #t)
      

(test '(call-with-values * -)
      -1
      #:with-bootstrapping? #t)





(test '(begin (define-values () (values))
              'ok)
      'ok
      #:with-bootstrapping? #t)

(test '(begin (define-values (x y z) (values 3 4 5))
              x)
      3
      #:with-bootstrapping? #t)


(test '(begin (define-values (x y z) (values 3 4 5))
              y)
      4
      #:with-bootstrapping? #t)

(test '(begin (define-values (x y z) (values 3 4 5))
              z)
      5
      #:with-bootstrapping? #t)


(test '(begin (define-values (x) "hello")
              x)
      "hello"
      #:with-bootstrapping? #t)


(test '(begin (define-values (x) (values "hello"))
              x)
      "hello"
      #:with-bootstrapping? #t)


(test '(begin (define (f x)
                (values (* x 2)
                        (/ x 2)))
              (define-values (a b) (f 16))
              (list a b))
      (list 32 8)
      #:with-bootstrapping? #t)
                

(test '((case-lambda [(x) x]) 42)
      42)

(test '(let ([v (case-lambda [(x) x]
                             [(x y) (+ x y)])])
         (list (v 0)
               (v 1 2)))
      (list 0 3))

(test '(let* ([y 42]
              [f (case-lambda [(x) (list x y)]
                              [(x y) (list x y)])])
         (list (f 3)
               (f 4 5)))
      (list (list 3 42)
            (list 4 5)))

(test '(let ([f (case-lambda [(x) (list x)]
                             [(x . y) (cons y x)])])
         (list (f 3)
               (f 4 5 6)))
      (list (list 3)
            (cons '(5 6) 4)))

(test '(begin) (void))



(test '(let*-values ([(k) (lambda () (display "") (values 3 4 5))]
                     [(f g h) (k)])
	  (list f g h))
      (list 3 4 5)
      #:with-bootstrapping? #t)


(test '(let*-values ([(k) (lambda () (display "") (values 3 4 5))]
                     [(f g h) (k)])
	  (display "")
	  (list f g h)
	  (display "")
	  'ok)
      'ok
      #:with-bootstrapping? #t)


(parameterize ([current-module-path (build-path this-test-path "foo.rkt")]
               [current-simulated-output-port (open-output-bytes)])
  (test '(module foo '#%kernel
           (display "hello world")
           (newline))
        (void)
        #:as-main-module 'whalesong/tests/foo.rkt
        #:with-bootstrapping? #t)
  (check-equal? (get-output-bytes (current-simulated-output-port))
                #"hello world\n"))





;; begin0 is still broken.

(test '(letrec ([f (lambda (x)
                     (if (= x 0)
                         0
                         (+ x (f (sub1 x)))))])
         (begin0 (+ (f 3) (f 4) (f 200)) 
                 (display "")))
      20116)


(test '(let () (define (f x y z)
                 (values y x z))
           (call-with-values (lambda () (f 3 1 4))
                             (lambda args (list args))))
        '((1 3 4))
        #:with-bootstrapping? #t)


(test '(let () (define (f x y z)
                 (begin0 (values y x z)
                         (display "")))
           (call-with-values (lambda () (f 3 1 4))
                             (lambda args (list args))))
        '((1 3 4))
        #:with-bootstrapping? #t)

;; TODO: use begin0 in multiple-value context, but where we should get dynamic runtime error since
;; the surrounding context can't consume multiple values.



#;(test (read (open-input-file "tests/conform/program0.sch"))
      (port->string (open-input-file "tests/conform/expected0.txt")))

;(simulate (compile (parse '42) 'val 'next))
;(compile (parse '(+ 3 4)) 'val 'next)