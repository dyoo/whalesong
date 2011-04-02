#lang racket

(require "simulator.rkt"
         "simulator-structs.rkt"
         "simulator-helpers.rkt"
         "compile.rkt"
         "parse.rkt"
         "il-structs.rkt")


(define (run-compiler code)
  (compile (parse code) 'val next-linkage))
  

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
             #:with-bootstrapping? (with-bootstrapping? false))
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
         (values m steps)]))))


;; Atomic expressions
(test '42 42)
(test '"hello world" "hello world")
(test '#t true)
(test '#f false)

;; quoted 
(test ''(+ 3 4)
      '(+ 3 4))

;; Simple definitions
(test '(begin (define x 42)
             (+ x x))
      84)

(test '(begin (define x 6)
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
(test '(begin (define (f x)
               (* x x))
             (f (f 3)))
      81)

(test '(begin (define pi 3.14159)
             (define radius 10)
             (* pi (* radius radius)))
      314.159)

(test '(begin (define pi 3.14159)
             (define radius 10)
             (define circumference (* 2 pi radius))
             circumference)
      62.8318)

;; Slightly crazy expression
(test '(begin (define (f x)
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
(test '(begin (define (f x)
               (if (= x 0)
                   1
                   (* x (f (sub1 x)))))
             (f 0))
      1)
(test '(begin (define (f x)
               (if (= x 0)
                   1
                   (* x (f (sub1 x)))))
             (f 1))
      1)
(test '(begin (define (f x)
               (if (= x 0)
                   1
                   (* x (f (sub1 x)))))
             (f 2))
      2)

(test '(begin (define (f x)
               (if (= x 0)
                   1
                   (* x (f (sub1 x)))))
             (f 100))
      93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
      )




;; Tail calling behavior: watch that the stack never grows beyond 8.
(test '(begin (define (f x acc)
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
      #:stack-limit 8)


;; And from experimental testing, anything below 7 will break.
(test/exn '(begin (define (f x acc)
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




;; tak test
(test '(begin (define (tak x y z)
               (if (>= y x)
                   z
                   (tak (tak (- x 1) y z)
                        (tak (- y 1) z x)
                        (tak (- z 1) x y))))
             (tak 18 12 6))
      7)

(test '((let* ([x 42]
               [f (lambda () x)])
          f))
      42)

(test '((let* ([x 42]
               [y 43]
               [f (lambda () (list x y))])
          f))
      (list 42 43))

(test '(begin (define (m f l)
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



(test '(begin 
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
      #:stack-limit 8
      #:control-limit 4)




;; deriv
(test '(begin (define (deriv-aux a) (list '/ (deriv a) a))
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
                           (cons '- (map deriv
                                         (cdr a)))
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

;; Foldl
(test '(begin (define (foldl f acc lst)
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


(test '(begin (define (sqrt-iter guess x)
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

;; fibonacci
(test '(begin (define (fib n)
               (if (= n 0) 0
                   (if (= n 1) 1
                       (+ (fib (- n 1))
                          (fib (- n 2))))))
             (fib 10))
      55)

;; Fibonacci, iterative.  This should be computable while using at most 10 spots.
(test '(begin 
        (define (fib n)
          (fib-iter 1 0 n))
        
        (define (fib-iter a b count)
          (if (= count 0)
              b
              (fib-iter (+ a b) a (- count 1))))
        (fib 10000))
      33644764876431783266621612005107543310302148460680063906564769974680081442166662368155595513633734025582065332680836159373734790483865268263040892463056431887354544369559827491606602099884183933864652731300088830269235673613135117579297437854413752130520504347701602264758318906527890855154366159582987279682987510631200575428783453215515103870818298969791613127856265033195487140214287532698187962046936097879900350962302291026368131493195275630227837628441540360584402572114334961180023091208287046088923962328835461505776583271252546093591128203925285393434620904245248929403901706233888991085841065183173360437470737908552631764325733993712871937587746897479926305837065742830161637408969178426378624212835258112820516370298089332099905707920064367426202389783111470054074998459250360633560933883831923386783056136435351892133279732908133732642652633989763922723407882928177953580570993691049175470808931841056146322338217465637321248226383092103297701648054726243842374862411453093812206564914032751086643394517512161526545361333111314042436854805106765843493523836959653428071768775328348234345557366719731392746273629108210679280784718035329131176778924659089938635459327894523777674406192240337638674004021330343297496902028328145933418826817683893072003634795623117103101291953169794607632737589253530772552375943788434504067715555779056450443016640119462580972216729758615026968443146952034614932291105970676243268515992834709891284706740862008587135016260312071903172086094081298321581077282076353186624611278245537208532365305775956430072517744315051539600905168603220349163222640885248852433158051534849622434848299380905070483482449327453732624567755879089187190803662058009594743150052402532709746995318770724376825907419939632265984147498193609285223945039707165443156421328157688908058783183404917434556270520223564846495196112460268313970975069382648706613264507665074611512677522748621598642530711298441182622661057163515069260029861704945425047491378115154139941550671256271197133252763631939606902895650288268608362241082050562430701794976171121233066073310059947366875
      
      #:stack-limit 10
      #:control-limit 3)
                  



;; Exponentiation
(test '(begin (define (expt b n)
               (if (= n 0)
                   1
                   (* b (expt b (- n 1)))))
             (expt 2 30))
      (expt 2 30))

(test '(begin 
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

(test '(begin
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


(test '(begin (define (length l)
               (if (null? l)
                   0
                   (+ 1 (length (cdr l)))))
             (display (length (list 1 2 3 4 5 6)))
             (newline)
             #;(display (length (list "hello" "world")))
             #;(newline))
      (void))


(test '(begin (define (sum-integers a b)
               (if (> a b)
                   0
                   (+ a (sum-integers (+ a 1) b))))        
             (sum-integers 1 100))
      (* 50 101))


(test '(begin (define (sum term a next b)
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
(test '(begin
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
(let ([op (open-output-string)])
  (parameterize ([current-simulated-output-port op])
    (test '(begin (define program (lambda ()
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




;; ctak
(test '(begin
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
        (ctak 18 12 6))
      7
      #:with-bootstrapping? #t)


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
      #:stack-limit 9
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
      #:stack-limit 12
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
        #:stack-limit 12
        #:control-limit 2)







(test '(letrec ([sum-iter (lambda (x acc)
                               (if (= x 0)
                                   acc
                                   (let* ([y (sub1 x)]
                                          [z (+ x acc)])
                                     (sum-iter y z))))])
            (sum-iter 300 0))
        45150
        #:stack-limit 10
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
                  (if (< n 10)
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
           (if (< n 10)
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



#;(test (read (open-input-file "tests/conform/program0.sch"))
      (port->string (open-input-file "tests/conform/expected0.txt")))

;(simulate (compile (parse '42) 'val 'next))
;(compile (parse '(+ 3 4)) 'val 'next)