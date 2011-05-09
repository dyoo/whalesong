#lang racket/base

(require compiler/zo-parse
         rackunit
         (for-syntax racket/base)
         "parse-bytecode-5.1.1.rkt"
         "lexical-structs.rkt"
         "expression-structs.rkt")


(define (run-zo-parse stx)
  (parameterize ([current-namespace (make-base-namespace)])
    (let ([bc (compile stx)]
          [op (open-output-bytes)])
      (write bc op)
      (zo-parse (open-input-bytes (get-output-bytes op))))))

(define (run-my-parse stx)
  (parameterize ([current-namespace (make-base-namespace)])
    (let ([bc (compile stx)]
          [op (open-output-bytes)])
      (write bc op)
      (parse-bytecode (open-input-bytes (get-output-bytes op))))))



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









;; make sure we don't see an infinite loop
#;(run-zo-parse #'(letrec ([g (lambda () (g))])
                  (g)))
(void (run-my-parse #'(letrec ([g (lambda () (g))])
                        (g))))
;; todo: add tests to make sure we're parsing this as expected.



#;(run-zo-parse #'(letrec ([g (lambda () (h))]
                         [h (lambda () (g))])
                  (g)))
;; FIXME: we need to handle closure cycles here.








;(run-zo-parse #'(lambda (x) (* x x)))
;(run-my-parse #'(lambda (x) (* x x)))