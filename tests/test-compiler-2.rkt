#lang racket

(require "../simulator/simulator.rkt"
         "../simulator/simulator-structs.rkt"
         "../simulator/simulator-helpers.rkt"
         "test-helpers.rkt")

(printf "test-compiler-2.rkt\n")

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




;; tak test
(test '(let () (define (tak x y z)
               (if (>= y x)
                   z
                   (tak (tak (- x 1) y z)
                        (tak (- y 1) z x)
                        (tak (- z 1) x y))))
             (tak 18 12 6))
      7)




;; ctak
(test '(let ()
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




;; fibonacci
(test '(let () (define (fib n)
               (if (= n 0) 0
                   (if (= n 1) 1
                       (+ (fib (- n 1))
                          (fib (- n 2))))))
             (fib 10))
      55)

;; Fibonacci, iterative.  This should be computable while using at most a bounded number of stack slots.
(test '(let () 
        (define (fib n)
          (fib-iter 1 0 n))
        
        (define (fib-iter a b count)
          (if (= count 0)
              b
              (fib-iter (+ a b) a (- count 1))))
        (fib 10000))
      33644764876431783266621612005107543310302148460680063906564769974680081442166662368155595513633734025582065332680836159373734790483865268263040892463056431887354544369559827491606602099884183933864652731300088830269235673613135117579297437854413752130520504347701602264758318906527890855154366159582987279682987510631200575428783453215515103870818298969791613127856265033195487140214287532698187962046936097879900350962302291026368131493195275630227837628441540360584402572114334961180023091208287046088923962328835461505776583271252546093591128203925285393434620904245248929403901706233888991085841065183173360437470737908552631764325733993712871937587746897479926305837065742830161637408969178426378624212835258112820516370298089332099905707920064367426202389783111470054074998459250360633560933883831923386783056136435351892133279732908133732642652633989763922723407882928177953580570993691049175470808931841056146322338217465637321248226383092103297701648054726243842374862411453093812206564914032751086643394517512161526545361333111314042436854805106765843493523836959653428071768775328348234345557366719731392746273629108210679280784718035329131176778924659089938635459327894523777674406192240337638674004021330343297496902028328145933418826817683893072003634795623117103101291953169794607632737589253530772552375943788434504067715555779056450443016640119462580972216729758615026968443146952034614932291105970676243268515992834709891284706740862008587135016260312071903172086094081298321581077282076353186624611278245537208532365305775956430072517744315051539600905168603220349163222640885248852433158051534849622434848299380905070483482449327453732624567755879089187190803662058009594743150052402532709746995318770724376825907419939632265984147498193609285223945039707165443156421328157688908058783183404917434556270520223564846495196112460268313970975069382648706613264507665074611512677522748621598642530711298441182622661057163515069260029861704945425047491378115154139941550671256271197133252763631939606902895650288268608362241082050562430701794976171121233066073310059947366875
      
      #:stack-limit 20
      #:control-limit 3)