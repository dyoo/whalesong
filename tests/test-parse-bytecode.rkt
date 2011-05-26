#lang racket/base

(require compiler/zo-parse
         rackunit
         racket/match
         racket/path
         "../parameters.rkt"
         "../parser/parse-bytecode.rkt"
         "../compiler/lexical-structs.rkt"
         "../compiler/expression-structs.rkt"
         racket/runtime-path
         (for-syntax racket/base))

(printf "test-parse-bytecode.rkt\n")

(define-runtime-path this-test-path ".")

(define (run-zo-parse stx)
  (parameterize ([current-namespace (make-base-namespace)]
                 [compile-context-preservation-enabled #t])
    (let ([bc (compile stx)]
          [op (open-output-bytes)])
      (write bc op)
      (zo-parse (open-input-bytes (get-output-bytes op))))))

(define (run-my-parse stx)
  (parameterize ([current-namespace (make-base-namespace)]
                 [compile-context-preservation-enabled #t])
    (let ([bc (compile stx)]
          [op (open-output-bytes)])
      (write bc op)
      (parse-bytecode (open-input-bytes (get-output-bytes op))))))


(define (run-my-parse/file path)
  (parameterize ([current-namespace (make-base-namespace)])
    (let-values  ([(base name dir?) (split-path path)])
      (let ([src-dir (cond
                       [(path? base)
                        base]
                       [else
                        (current-directory)])])
        (parameterize ([current-directory src-dir]
                       [current-load-relative-directory src-dir])
          (let ([bc (compile (parameterize ([read-accept-reader #t])
                               (read (open-input-file path))))]
                [op (open-output-bytes)])
            (write bc op)
            (parse-bytecode (open-input-bytes (get-output-bytes op)))))))))



(check-equal? (run-my-parse #''hello) 
              (make-Top (make-Prefix '()) 
                        (make-Constant 'hello)))

(check-equal? (run-my-parse #'"hello world")
              (make-Top (make-Prefix (list))
                        (make-Constant "hello world")))
              
(check-equal? (run-my-parse #'42)
              (make-Top (make-Prefix (list))
                        (make-Constant 42)))

;; global variables
(check-equal? (run-my-parse #'x)
              (make-Top (make-Prefix (list (make-GlobalBucket 'x)))
                        (make-ToplevelRef 0 0)))
 


(check-equal? (run-my-parse #'(begin (define x 3)
                                     x))
              (make-Top (make-Prefix (list (make-GlobalBucket 'x)))
                        (make-Splice (list (make-DefValues (list (make-ToplevelRef 0 0))
                                                           (make-Constant 3))
                                           (make-ToplevelRef 0 0)))))


;; Lambdas
(let ([parsed (run-my-parse #'(lambda (x) x))])
  (check-true (Lam? (Top-code parsed)))
  (check-equal? (Lam-num-parameters (Top-code parsed)) 1)
  (check-equal? (Lam-rest? (Top-code parsed)) #f)
  (check-equal? (Lam-body (Top-code parsed))
                (make-LocalRef 0 #f)))

(let ([parsed (run-my-parse #'(lambda (x y) x))])
  (check-true (Lam? (Top-code parsed)))
  (check-equal? (Lam-num-parameters (Top-code parsed)) 2)
  (check-equal? (Lam-rest? (Top-code parsed)) #f)
  (check-equal? (Lam-body (Top-code parsed))
                (make-LocalRef 0 #f)))

(let ([parsed (run-my-parse #'(lambda (x y) y))])
  (check-true (Lam? (Top-code parsed)))
  (check-equal? (Lam-num-parameters (Top-code parsed)) 2)
  (check-equal? (Lam-rest? (Top-code parsed)) #f)
  (check-equal? (Lam-body (Top-code parsed))
                (make-LocalRef 1 #f)))

(let ([parsed (run-my-parse #'(lambda x x))])
  (check-true (Lam? (Top-code parsed)))
  (check-equal? (Lam-num-parameters (Top-code parsed)) 0)
  (check-equal? (Lam-rest? (Top-code parsed)) #t)
  (check-equal? (Lam-body (Top-code parsed))
                (make-LocalRef 0 #f)))

(let ([parsed (run-my-parse #'(lambda (x . y) x))])
  (check-true (Lam? (Top-code parsed)))
  (check-equal? (Lam-num-parameters (Top-code parsed)) 1)
  (check-equal? (Lam-rest? (Top-code parsed)) #t)
  (check-equal? (Lam-body (Top-code parsed))
                (make-LocalRef 0 #f)))



;; let1's
(check-equal? (run-my-parse #'(let ([y (f)])
                                'ok))
              (make-Top (make-Prefix (list (make-GlobalBucket 'f)))
                        (make-Let1 (make-App (make-ToplevelRef 1 0) (list))
                                   (make-Constant 'ok))))

(check-equal? (run-my-parse #'(let ([y (f)]
                                    [z (g)])
                                'ok))
              (make-Top (make-Prefix (list (make-GlobalBucket 'f) (make-GlobalBucket 'g)))
                        (make-Let1 (make-App (make-ToplevelRef 1 0) (list))
                                   (make-Let1 (make-App (make-ToplevelRef 2 1) (list))
                                              (make-Constant 'ok)))))

(check-equal? (run-my-parse #'(let* ([y (f)]
                                     [z (g)])
                                y
                                z))
              (make-Top (make-Prefix (list (make-GlobalBucket 'f) (make-GlobalBucket 'g)))
                        (make-Let1 (make-App (make-ToplevelRef 1 0) (list))
                                   (make-Let1 (make-App (make-ToplevelRef 2 1) (list))
                                              ;; racket's compiler optimizes away the sequence and lookup to y.
                                              #;(make-Seq (list (make-LocalRef 1 #f) 
                                                                (make-LocalRef 0 #f)))
                                              (make-LocalRef 0 #f)))))
                                              
;; Another example of an optimization that Racket is doing for us.  it is smart enough
;; to turn this parallel let into nested let1's.
(check-equal? (run-my-parse #'(let ([y (f)]
                                    [z (g)])
                                y
                                z))
              (make-Top (make-Prefix (list (make-GlobalBucket 'f) (make-GlobalBucket 'g)))
                        (make-Let1 (make-App (make-ToplevelRef 1 0) (list))
                                   (make-Let1 (make-App (make-ToplevelRef 2 1) (list))
                                              (make-LocalRef 0 #f)))))




;; branches
(check-equal? (run-my-parse #'(if (f) (g) (h)))
              (make-Top (make-Prefix (list (make-GlobalBucket 'f)
                                           (make-GlobalBucket 'g)
                                           (make-GlobalBucket 'h)))
                        (make-Branch (make-App (make-ToplevelRef 0 0) '())
                                     (make-App (make-ToplevelRef 0 1) '())
                                     (make-App (make-ToplevelRef 0 2) '()))))


;; Another example where Racket's compiler is helping: constant propagation, dead code removal.
(check-equal? (run-my-parse #'(if 3 (g) (h)))
              (make-Top (make-Prefix (list (make-GlobalBucket 'g)))
                        (make-App (make-ToplevelRef 0 0) '())))




(check-equal? (run-my-parse #'(if x (if y z 1) #t))
              (make-Top (make-Prefix (list (make-GlobalBucket 'x)
                                           (make-GlobalBucket 'y)
                                           (make-GlobalBucket 'z)))
                        (make-Branch (make-ToplevelRef 0 0)
                                     (make-Branch (make-ToplevelRef 0 1)
                                                  (make-ToplevelRef 0 2)
                                                  (make-Constant 1))
                                     (make-Constant #t))))


(check-equal? (run-my-parse #'(cond [x y]))
              (make-Top (make-Prefix (list (make-GlobalBucket 'x)
                                           (make-GlobalBucket 'y)))
                        (make-Branch (make-ToplevelRef 0 0)
                                     (make-ToplevelRef 0 1)
                                     (make-Constant (void)))))




(check-equal? (run-my-parse #'+)
              (make-Top (make-Prefix (list))
                        (make-PrimitiveKernelValue '+)))


(check-equal? (run-my-parse #'(+ (* x x) x))
              (make-Top (make-Prefix (list (make-GlobalBucket 'x)))
                        (make-App (make-PrimitiveKernelValue '+)
                                  (list (make-App (make-PrimitiveKernelValue '*)
                                                  (list (make-ToplevelRef 4 0) 
                                                        (make-ToplevelRef 4 0)))
                                        (make-ToplevelRef 2 0)))))

(check-equal? (run-my-parse #'list)
              (make-Top (make-Prefix (list))
                        (make-PrimitiveKernelValue 'list)))

(check-equal? (run-my-parse #'append)
              (make-Top (make-Prefix (list))
                        (make-PrimitiveKernelValue 'append)))


(check-equal? (run-my-parse #'(let () x))
              (make-Top (make-Prefix (list (make-GlobalBucket 'x)))
                        (make-ToplevelRef 0 0)))



;; the letrec gets translated into a closure call
(begin
  (reset-lam-label-counter!/unit-testing)
  (check-equal? (run-my-parse '(letrec ([omega (lambda () (omega))])
                                 (omega)))
                (make-Top (make-Prefix '())
                          (make-App (make-Lam 'omega 0 #f (make-App (make-EmptyClosureReference 'omega 0 #f 'lamEntry1) '())
                                              '() 'lamEntry1)
                                    '()))))


;; FIXME: make this a real test.
(begin
  (reset-lam-label-counter!/unit-testing)
  (void (run-my-parse #'(letrec ([e (lambda (y)
                                              (if (= y 0)
                                                  #t
                                                  (o (sub1 y))))]
                                         [o (lambda (y)
                                              (if (= y 0)
                                                  #f
                                                  (e sub1 y)))])
                          e))))
                                                      


(check-equal? (run-my-parse #'(let ([x 3])
                                (set! x (add1 x))
                                x))
              (make-Top (make-Prefix '())
                        (make-Let1 
                         (make-Constant 3)
                         (make-BoxEnv 0
                                      (make-Seq 
                                       (list 
                                        (make-InstallValue
                                         1 0 
                                         (make-App (make-PrimitiveKernelValue 'add1)
                                                   (list (make-LocalRef 1 #t)))
                                         #t)
                                        (make-LocalRef 0 #t)))))))


(check-equal? (run-my-parse #'(set! pi 3.14))
              (make-Top (make-Prefix (list (make-GlobalBucket 'pi)))
                        (make-ToplevelSet 0 0 (make-Constant 3.14)))) 




(check-equal? (run-my-parse #'(call-with-values (lambda () (f)) g))
              (make-Top (make-Prefix (list (make-GlobalBucket 'f)
                                           (make-GlobalBucket 'g)))
                        (make-ApplyValues (make-ToplevelRef 0 1)
                                          (make-App (make-ToplevelRef 0 0) '()))))



(check-equal? (run-my-parse #'(with-continuation-mark 'key 'value (current-continuation-marks)))
              (make-Top 
               (make-Prefix '())
               (make-WithContMark (make-Constant 'key)
                                  (make-Constant 'value)
                                  (make-App (make-PrimitiveKernelValue 'current-continuation-marks) '()))))


(begin (reset-lam-label-counter!/unit-testing)
       (check-true (match (run-my-parse #'(case-lambda))
                     [(struct Top ((struct Prefix (list))
                                   (struct CaseLam (_ (list) 'lamEntry1))))
                      #t])))

(begin (reset-lam-label-counter!/unit-testing)
       (check-true (match (run-my-parse #'(case-lambda [(x) x]
                                                       [(x y) x]
                                                       [(x y) y]))
                     [(struct Top ((struct Prefix (list))
                                   (struct CaseLam (_
                                                    (list (struct Lam (_
                                                                       1
                                                                       #f
                                                                       (struct LocalRef ('0 '#f))
                                                                       '()
                                                                       'lamEntry2))
                                                          (struct Lam (_
                                                                       2
                                                                       #f
                                                                       (struct LocalRef ('0 '#f))
                                                                       '()
                                                                       'lamEntry3))
                                                          (struct Lam (_
                                                                       2
                                                                       #f
                                                                       (struct LocalRef ('1 '#f))
                                                                       '()
                                                                       'lamEntry4)))
                                                    'lamEntry1))))
                      #t])))


(check-equal? (run-my-parse #'(begin0 (f)))
              (make-Top (make-Prefix (list (make-GlobalBucket 'f)))
                        (make-App (make-ToplevelRef 0 0) '())))
              
(check-equal? (run-my-parse #'(begin0 (f) (g)))
              (make-Top (make-Prefix (list (make-GlobalBucket 'f)
                                           (make-GlobalBucket 'g)))
                        (make-Begin0 (list (make-App (make-ToplevelRef 0 0) '())
                                           (make-App (make-ToplevelRef 0 1) '())))))


;; Compiling modules
(check-true
 (match (run-my-parse #'(module foo racket/base
                          42))
   [(struct Top ((struct Prefix (list))
                 (struct Module ((? symbol?)
                                 (? ModuleLocator?)
                                 (? Prefix?) ;; the prefix will include a reference to print-values.
                                 _  ;; requires
                                 (struct Splice ((list (struct ApplyValues 
                                                         ((struct ToplevelRef ('0 '0)) (struct Constant ('42)))))))))))
    #t]))


(check-true
 (match (run-my-parse #'(module foo racket/base
                          (provide x)
                          (define x "x")))
   [(struct Top ((struct Prefix ((? list?)))
                 (struct Module ((? symbol?)
                                 (? ModuleLocator?)
                                 (? Prefix?) ;; the prefix will include a reference to print-values.
                                 _  ;; requires
                                 (struct Splice ((list (struct DefValues 
                                                         ((list (struct ToplevelRef ('0 '0)))
                                                          (struct Constant ("x")))))))))))
    #t]))




;; Variable reference
(check-equal? (run-my-parse #'(#%variable-reference x))
              (make-Top (make-Prefix (list (make-GlobalBucket 'x)))
                       (make-VariableReference (make-ToplevelRef 0 0))))

;; todo: see what it would take to run a typed/racket/base language.
(void 
 (run-my-parse '(module foo typed/racket/base 
                        (provide x) 
                        (: x Number)
                        (define x (add1 41)))))



(void
 (run-my-parse #'(case-lambda [(x) x]
                              [(x y) (list x y)])))

(void
 (run-my-parse #'(letrec ([g (lambda () (g))])
                   (g))))




(void
 (run-my-parse #'(letrec ([g (case-lambda [() (g)]
                                          [(x y) (g x y)])])
                   (g))))



(void
 (run-my-parse #'(module foo '#%kernel
                   (define-values (f) 42)
                   (#%provide f))))
              

(parameterize ([current-root-path this-test-path]
               [current-module-path (build-path this-test-path "foo.rkt")])
  (check-true
   (match (run-my-parse #'(module foo racket/base))
     [(struct Top ((? Prefix?)
                   (struct Module ('foo
                                   (struct ModuleLocator 
                                     ('whalesong/tests/foo.rkt
                                      (? (lambda (p)
                                           (and (path? p)
                                                (equal? (normalize-path p)
                                                        (normalize-path 
                                                         (build-path this-test-path "foo.rkt"))))))))
                                      
                                   (struct Prefix (list))
                                   (list (struct ModuleLocator ('collects/racket/base.rkt
                                                             _)))
                                   (struct Splice ('()))))))
      #t]
     [else
      #f])))


#;(check-true
 (match (parameterize ([current-root-path (build-path "/blah")]
                       [current-module-path (build-path "/blah" "foo" "bar.rkt")])
                (run-my-parse '(module foo '#%kernel
                                 (define-values (f) 'ok)
                                 (#%provide f))))
   [(struct Top ((struct Prefix ((list '#f)))
                 (struct Module ('foo
                                 (struct ModuleLocator ('self _ #;(build-path "root/foo/bar.rkt")))
                                 (struct Prefix ((list 'f)))
                                 (list (struct ModuleLocator ('#%kernel '#%kernel)))
                                 (struct Splice ((list (struct DefValues ((list (struct ToplevelRef (0 0)))
                                                                          (struct Constant ('ok)))))))))))
    '#t]))
 
 
#;(parameterize ([current-module-path
                  "/home/dyoo/local/racket-5.1.1/lib/racket/collects/racket/private/foo.rkt"])
    (run-my-parse/file "/home/dyoo/local/racket-5.1.1/lib/racket/collects/racket/private/for.rkt"))




;(run-zo-parse #'(lambda (x) (* x x)))
;(run-my-parse #'(lambda (x) (* x x)))