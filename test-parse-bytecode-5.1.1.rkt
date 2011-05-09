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











;; make sure we don't see an infinite loop
#;(run-zo-parse #'(letrec ([g (lambda () (g))])
                  (g)))
(void (run-my-parse #'(letrec ([g (lambda () (g))])
                        (g))))



#;(run-zo-parse #'(letrec ([g (lambda () (h))]
                         [h (lambda () (g))])
                  (g)))
;; FIXME: we need to handle closure cycles here.








;(run-zo-parse #'(lambda (x) (* x x)))
;(run-my-parse #'(lambda (x) (* x x)))