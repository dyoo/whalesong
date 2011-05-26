#lang racket/base

(require "../parser/baby-parser.rkt"
         "../parser/lam-entry-gensym.rkt"
         "../compiler/lexical-structs.rkt"
         "../compiler/expression-structs.rkt"
         (for-syntax racket/base))

(printf "test-parse.rkt\n");

; Test out the compiler, using the simulator.
(define-syntax (test stx)
  (syntax-case stx ()
    [(_ expr expt)
     (with-syntax ([stx stx])
       (syntax/loc #'stx
         (begin
           (printf "Running ~s ...\n" (syntax->datum #'expr))
           (reset-lam-label-counter!/unit-testing)
           (let ([expected expt]
                 [actual 
                  (with-handlers ([void
                                   (lambda (exn)
                                     (raise-syntax-error #f (format "Runtime error: got ~s" exn)
                                                         #'stx))])
                    expr)])
             (unless (equal? actual expected)
               (raise-syntax-error #f (format "Expected ~s, got ~s" expected actual)
                                   #'stx))
             (printf "ok.\n\n")))))]))



(test (parse '1) 
      (make-Top (make-Prefix '())
                (make-Constant 1)))

(test (parse ''hello) 
      (make-Top (make-Prefix '()) 
                (make-Constant 'hello)))

(test (parse 'hello) 
      (make-Top (make-Prefix '(hello)) 
                (make-ToplevelRef 0 0)))

(test (parse '(begin hello world))
      (make-Top (make-Prefix '(hello world)) 
                (make-Splice (list (make-ToplevelRef 0 0)
                                (make-ToplevelRef 0 1)))))

(test (parse '(define x y))
      (make-Top (make-Prefix '(x y))
                (make-ToplevelSet 0 0  (make-ToplevelRef 0 1))))

(test (parse '(begin (define x 42)
                     (define y x)))
      (make-Top (make-Prefix '(x y))
                (make-Splice (list (make-ToplevelSet 0 0 (make-Constant 42))
                                   (make-ToplevelSet 0 1 (make-ToplevelRef 0 0))))))

(test (parse '(if x y z))
      (make-Top (make-Prefix '(x y z))
                (make-Branch (make-ToplevelRef 0 0)
                             (make-ToplevelRef 0 1)
                             (make-ToplevelRef 0 2))))

(test (parse '(if x (if y z 1) #t))
      (make-Top (make-Prefix '(x y z))
                (make-Branch (make-ToplevelRef 0 0)
                             (make-Branch (make-ToplevelRef 0 1)
                                          (make-ToplevelRef 0 2)
                                          (make-Constant 1))
                             (make-Constant #t))))

(test (parse '(if x y))
      (make-Top (make-Prefix '(x y))
                (make-Branch (make-ToplevelRef 0 0)
                             (make-ToplevelRef 0 1)
                             (make-Constant (void)))))

(test (parse '(cond [x y]))
      (make-Top (make-Prefix '(x y))
                (make-Branch (make-ToplevelRef 0 0)
                             (make-ToplevelRef 0 1)
                             (make-Constant (void)))))

(test (parse '(cond [x y] [else "ok"]))
      (make-Top (make-Prefix '(x y))
                (make-Branch (make-ToplevelRef 0 0)
                             (make-ToplevelRef 0 1)
                             (make-Constant "ok"))))

(test (parse '(lambda () x))
      (make-Top (make-Prefix '(x))
                (make-Lam 'unknown 0 #f (make-ToplevelRef 0 0) 
                          '(0) 'lamEntry1)))

(test (parse '(lambda args args))
      (make-Top (make-Prefix '())
                (make-Lam 'unknown 0 #t (make-LocalRef 0 #f) 
                          '() 'lamEntry1)))

(test (parse '(lambda (x y . z) x))
      (make-Top (make-Prefix '())
                (make-Lam 'unknown 2 #t 
                          (make-LocalRef 0 #f)
                          '() 'lamEntry1)))

(test (parse '(lambda (x y . z) y))
      (make-Top (make-Prefix '())
                (make-Lam 'unknown 2 #t 
                          (make-LocalRef 1 #f)
                          '() 'lamEntry1)))


(test (parse '(lambda (x y . z) z))
      (make-Top (make-Prefix '())
                (make-Lam 'unknown 2 #t 
                          (make-LocalRef 2 #f)
                          '() 'lamEntry1)))


(test (parse '(lambda (x y z) x))
      (make-Top (make-Prefix '())
                (make-Lam 'unknown 3 #f (make-LocalRef 0 #f) '() 'lamEntry1)))

(test (parse '(lambda (x y z) y))
      (make-Top (make-Prefix '())
                (make-Lam 'unknown 3 #f (make-LocalRef 1 #f) '() 'lamEntry1)))

(test (parse '(lambda (x y z) z))
      (make-Top (make-Prefix '())
                (make-Lam 'unknown 3 #f (make-LocalRef 2 #f) '() 'lamEntry1)))


(test (parse '(lambda (x y z) x y z))
      (make-Top (make-Prefix '())
                (make-Lam 'unknown 3 #f (make-Seq (list (make-LocalRef 0 #f)
                                            (make-LocalRef 1 #f)
                                            (make-LocalRef 2 #f)))
                          '()
                          'lamEntry1)))

(test (parse '(lambda (x y z) k))
      (make-Top (make-Prefix '(k))
                (make-Lam 'unknown 
                          3 
                          #f
                          (make-ToplevelRef 0 0 )
                          '(0)
                          'lamEntry1)))

(test (parse '(lambda (x y z) k x y z))
      (make-Top (make-Prefix '(k))
                (make-Lam 'unknown
                          3 
                          #f
                          (make-Seq (list (make-ToplevelRef 0 0 )
                                            (make-LocalRef 1 #f)
                                            (make-LocalRef 2 #f)
                                            (make-LocalRef 3 #f)))
                          '(0)
                          'lamEntry1)))

(test (parse '(lambda (x)
                (lambda (y)
                  (lambda (z)
                    x 
                    y 
                    z
                    w))))
      (make-Top (make-Prefix '(w))
                (make-Lam 'unknown 1 #f 
                          (make-Lam 'unknown 1 #f
                                    (make-Lam 'unknown 1 #f
                                              (make-Seq (list 
                                                         (make-LocalRef 1 #f)
                                                         (make-LocalRef 2 #f)
                                                         (make-LocalRef 3 #f)
                                                         (make-ToplevelRef 0 0)))
                                              '(0 1 2) ;; w x y
                                              'lamEntry1)
                                    
                                    '(0 1) ;; w x
                                    'lamEntry2)
                          '(0)
                          'lamEntry3)))

(test (parse '(lambda (x)
                (lambda (y)
                  x)))
      (make-Top (make-Prefix '())
                (make-Lam 'unknown 1 #f
                          (make-Lam 'unknown 1 #f
                                    (make-LocalRef 0 #f)
                                    '(0)
                                    'lamEntry1)
                          (list)
                          'lamEntry2)))

(test (parse '(lambda (x)
                (lambda (y)
                  y)))
      (make-Top (make-Prefix '())
                (make-Lam 'unknown 1 #f
                          (make-Lam 'unknown 1 #f
                                    (make-LocalRef 0 #f)
                                    (list)
                                    'lamEntry1)
                          (list)
                          'lamEntry2)))

(test (parse '(+ x x))
      (make-Top (make-Prefix `(,(make-ModuleVariable '+ (make-ModuleLocator '#%kernel '#%kernel))
                               x))
                (make-App (make-ToplevelRef 2 0)
                          (list (make-ToplevelRef 2 1)
                                (make-ToplevelRef 2 1)))))
  

(test (parse '(lambda (x) (+ x x)))
      (make-Top (make-Prefix `(,(make-ModuleVariable '+ (make-ModuleLocator '#%kernel '#%kernel))))
                (make-Lam 'unknown 1 #f
                          (make-App (make-ToplevelRef 2 0)
                                    (list (make-LocalRef 3 #f)
                                          (make-LocalRef 3 #f)))
                          '(0)
                          'lamEntry1)))

(test (parse '(lambda (x) 
                (+ (* x x) x)))
      (make-Top (make-Prefix `(,(make-ModuleVariable '* (make-ModuleLocator '#%kernel '#%kernel))
                               ,(make-ModuleVariable '+ (make-ModuleLocator '#%kernel '#%kernel))))
                (make-Lam 'unknown 1 #f
                          ;; stack layout: [???, ???, prefix, x]
                          (make-App (make-ToplevelRef 2 1)
                                    (list
                                     ;; stack layout: [???, ???, ???, ???, prefix, x]
                                     (make-App (make-ToplevelRef 4 0)
                                               (list (make-LocalRef 5 #f)
                                                     (make-LocalRef 5 #f)))
                                     (make-LocalRef 3 #f)))
                          '(0)
                          'lamEntry1)))

(test (parse '(let ()
                x))
      (make-Top (make-Prefix '(x))
                (make-ToplevelRef 0 0)))

(test (parse '(let ([x 3])
                x))
      (make-Top (make-Prefix '())
                (make-Let1 (make-Constant 3)
                           (make-LocalRef 0 #f))))

(test (parse '(let ([x 3]
                    [y 4])
                x
                y))
      (make-Top (make-Prefix '())
                (make-LetVoid 2
                              (make-Seq (list (make-InstallValue 1 0 (make-Constant 3) #f)
                                              (make-InstallValue 1 1 (make-Constant 4) #f)
                                              (make-Seq (list (make-LocalRef 0 #f)
                                                              (make-LocalRef 1 #f)))))
                              #f)))

(test (parse '(let ([x 3]
                    [y 4])
                (let ([x y]
                      [y x])
                  x
                  y)))
      (make-Top (make-Prefix '())
                (make-LetVoid 2
                              (make-Seq (list (make-InstallValue 1 0 (make-Constant 3) #f)
                                              (make-InstallValue 1 1 (make-Constant 4) #f)
                                              (make-LetVoid 2
                                                            (make-Seq (list (make-InstallValue 1 0 (make-LocalRef 3 #f) #f)
                                                                            (make-InstallValue 1 1 (make-LocalRef 2 #f) #f)
                                                                            (make-Seq (list (make-LocalRef 0 #f)
                                                                                            (make-LocalRef 1 #f)))))
                                                            #f)))
                              #f)))



(test (parse '(let* ([x 3]
                     [x (add1 x)])
                (add1 x)))
      (make-Top (make-Prefix `(,(make-ModuleVariable 'add1 (make-ModuleLocator '#%kernel '#%kernel))))
                
                ;; stack layout: [prefix]
                
                (make-Let1 (make-Constant 3)
                           
                           ;; stack layout: [x_0, prefix]

                           (make-Let1
                            
                            ;; stack layout: [???, x_0, prefix]

                            (make-App 
                             
                             ;; stack layout: [???, ???, x_0, prefix]
                             (make-ToplevelRef 3 0) (list (make-LocalRef 2 #f)))
                            
                            ;; stack layout [???, x_1, x_0, prefix]
                            (make-App (make-ToplevelRef 3 0)
                                      (list (make-LocalRef 1 #f)))))))


(test (parse '(let* ()
                42))
      (make-Top (make-Prefix '()) (make-Constant 42)))




(test (parse '(letrec ([omega (lambda () (omega))])
                (omega)))
      (make-Top (make-Prefix '())
                (make-LetVoid 1
                              (make-LetRec (list (make-Lam 'omega 0 #f (make-App (make-LocalRef 0 #f) 
                                                                                 (list)) '(0) 'lamEntry1))
                                           (make-App (make-LocalRef 0 #f) (list)))
                              #f)))



(test (parse '(letrec ([a (lambda () (b))]
                       [b (lambda () (c))]
                       [c (lambda () (a))])
                (a)))
      (make-Top (make-Prefix '())
                (make-LetVoid 3
                              (make-LetRec (list (make-Lam 'a 0 #f (make-App (make-LocalRef 0 #f) '()) '(1) 'lamEntry1)
                                                 (make-Lam 'b 0 #f (make-App (make-LocalRef 0 #f) '()) '(2) 'lamEntry2)
                                                 (make-Lam 'c 0 #f (make-App (make-LocalRef 0 #f) '()) '(0) 'lamEntry3))
                                           (make-App (make-LocalRef 0 #f) '()))
                              #f)))
      


(test (parse '(letrec ([x (lambda (x) x)]
                       [y (lambda (x) x)])
                (set! x x)
                (x y)))
      (make-Top (make-Prefix '())
                (make-LetVoid 2
                              (make-Seq 
                               (list 
                                (make-InstallValue 1 0
                                                   (make-Lam 'x 1 #f (make-LocalRef 0 #f) '() 'lamEntry1)
                                                   #t)
                                (make-InstallValue 1 1 
                                                   (make-Lam 'y 1 #f (make-LocalRef 0 #f) '() 'lamEntry2)
                                                   #t)
                                ;; stack layout: ??? x y
                                (make-Seq (list (make-Seq (list (make-InstallValue 1 0 (make-LocalRef 0 #t) #t)
                                                                (make-Constant (void))))
                                                (make-App (make-LocalRef 1 #t)
                                                          (list (make-LocalRef 2 #t)))))))
                              #t)))




;; This hsould only show 0, because there should be a prompt that controls continuation capture
#;(test '(begin (define cont #f)
                (define n 0)
                (call/cc (lambda (x) (set! cont x)))
                (displayln n) 
                (set! n (add1 n))
                (when (< n 10)
                  (cont))))


;; This should show the numbers 0 through 10
#;(test '(begin (define (f)
                  (define cont #f)
                  (define n 0)
                  (call/cc (lambda (x) (set! cont x)))
                  (displayln n) 
                  (set! n (add1 n))
                  (when (< n 10)
                    (cont)))
                (f)))


#;(test (parse '(letrec ([x (lambda (x) (y x))]
                       [y (lambda (x) (x y))])
                  (x y)))
        (make-Top (make-Prefix '())
                  (make-LetVoid 2
                                (make-Seq 
                                 (list 
                                  (make-InstallValue 1 0
                                                     (make-Lam 'x 1 #f
                                                               (make-App (make-LocalRef 1 #t) 
                                                                         (list (make-LocalRef 2 #f)))
                                                               '(1)
                                                               'lamEntry1)
                                                     #t)
                                  (make-InstallValue 1 1 
                                                     (make-Lam 'y 1 #f
                                                               (make-App (make-LocalRef 2 #f)
                                                                         (list (make-LocalRef 1 #t)))
                                                               '(1)
                                                               'lamEntry2)
                                                     #t)
                                  ;; stack layout: ??? x y
                                  (make-App (make-LocalRef 1 #t)
                                            (list (make-LocalRef 2 #t)))))
                                #t)))

(test (parse '(let ([x 0])
                (lambda ()
                  (set! x (add1 x)))))
      (make-Top (make-Prefix `(,(make-ModuleVariable 'add1 (make-ModuleLocator '#%kernel '#%kernel))))
                (make-Let1 (make-Constant 0)
                           (make-BoxEnv 0
                                        (make-Lam 'unknown 0 #f
                                                  (make-Seq (list (make-InstallValue 
                                                                   1 1 
                                                                   (make-App (make-ToplevelRef 1 0)
                                                                             (list (make-LocalRef 2 #t)))
                                                                   #t)
                                                                  (make-Constant (void))))
                                                  '(1 0)
                                                  'lamEntry1))))) ;; x is 0, prefix is 1



(test (parse '(let ([x 0]
                    [y 1])
                (lambda ()
                  (set! x (add1 x)))))
      (make-Top (make-Prefix `(,(make-ModuleVariable 'add1 (make-ModuleLocator '#%kernel '#%kernel))))
                (make-LetVoid 2
                              (make-Seq (list
                                         (make-InstallValue 1 0 (make-Constant 0) #t)
                                         (make-InstallValue 1 1 (make-Constant 1) #t)
                                         (make-Lam 'unknown 0 #f
                                                   (make-Seq
                                                    (list (make-InstallValue 
                                                           1 1 
                                                           (make-App (make-ToplevelRef 1 0)
                                                                     (list (make-LocalRef 2 #t)))
                                                           #t)
                                                          (make-Constant (void))))
                                                   '(2 0)
                                                   'lamEntry1)))
                              #t)))



(test (parse '(begin (define a '(hello))
                     (define b '(world))
                     (define reset!
                       (lambda ()
                         (set! a '())
                         (set! b '())))
                     (reset!)
                     (list a b)))
      (make-Top
       (make-Prefix `(a b ,(make-ModuleVariable 'list (make-ModuleLocator '#%kernel '#%kernel)) reset!))
       (make-Splice
        (list
         (make-ToplevelSet 0 0 (make-Constant '(hello)))
         (make-ToplevelSet 0 1 (make-Constant '(world)))
         (make-ToplevelSet
          0
          3
          (make-Lam
           'reset!
           0
           #f
           (make-Seq
             (list
              (make-Seq (list (make-ToplevelSet 0 0 (make-Constant '())) (make-Constant (void))))
              (make-Seq (list (make-ToplevelSet 0 1 (make-Constant '())) (make-Constant (void))))))
           '(0)
           'lamEntry1))
         (make-App (make-ToplevelRef 0 3) '())
         (make-App (make-ToplevelRef 2 2) (list (make-ToplevelRef 2 0) (make-ToplevelRef 2 1)))))))



(test (parse '(with-continuation-mark x y z))
      (make-Top (make-Prefix '(x y z))
                (make-WithContMark (make-ToplevelRef 0 0)
                                   (make-ToplevelRef 0 1)
                                   (make-ToplevelRef 0 2))))



(test (parse '(call-with-values x y))
      (make-Top (make-Prefix '(x y))
                (make-ApplyValues (make-ToplevelRef 0 1)
                                  (make-App (make-ToplevelRef 0 0) (list)))))

(test (parse '(call-with-values (lambda () x) y))
      (make-Top (make-Prefix '(x y))
                (make-ApplyValues (make-ToplevelRef 0 1)
                                  (make-ToplevelRef 0 0))))



(test (parse '(define-values () (values)))
      (make-Top (make-Prefix '(values))
                (make-DefValues '()
                                (make-App (make-ToplevelRef 0 0) '()))))

(test (parse '(define-values (x y z) (values 'hello 'world 'testing)))
      (make-Top (make-Prefix '(values x y z))
                (make-DefValues (list (make-ToplevelRef 0 1)
                                      (make-ToplevelRef 0 2)
                                      (make-ToplevelRef 0 3))
                                (make-App (make-ToplevelRef 3 0) 
                                          (list (make-Constant 'hello)
                                                (make-Constant 'world)
                                                (make-Constant 'testing))))))





;; CaseLam
(test (parse '(case-lambda))
      (make-Top (make-Prefix '())
                (make-CaseLam 'unknown (list) 'lamEntry1)))


(test (parse '(case-lambda [(x) x]))
      (make-Top (make-Prefix '())
                (make-CaseLam 
                 'unknown
                 (list (make-Lam 'unknown 1 #f (make-LocalRef 0 #f) '() 'lamEntry2))
                 'lamEntry1)))


(test (parse '(case-lambda [(x) x]
                           [(x y) x]))
      (make-Top (make-Prefix '())
                (make-CaseLam 
                 'unknown
                 (list (make-Lam 'unknown 1 #f (make-LocalRef 0 #f) '() 'lamEntry2)
                       (make-Lam 'unknown 2 #f (make-LocalRef 0 #f) '() 'lamEntry3))
                 'lamEntry1)))

(test (parse '(case-lambda [(x) x]
                           [(x y) y]))
      (make-Top (make-Prefix '())
                (make-CaseLam 
                 'unknown
                 (list (make-Lam 'unknown 1 #f (make-LocalRef 0 #f) '() 'lamEntry2)
                       (make-Lam 'unknown 2 #f (make-LocalRef 1 #f) '() 'lamEntry3))
                 'lamEntry1)))


(test (parse '(case-lambda [(x y) y]
                           [(x) x]))
      (make-Top (make-Prefix '())
                (make-CaseLam 
                 'unknown
                 (list (make-Lam 'unknown 2 #f (make-LocalRef 1 #f) '() 'lamEntry2)
                       (make-Lam 'unknown 1 #f (make-LocalRef 0 #f) '() 'lamEntry3))
                       
                 'lamEntry1)))
