#lang whalesong

(define infinite-ones
  (shared ([a (cons 1 a)])
     a))

(car infinite-ones)
(car (cdr infinite-ones))
(car (cdr (cdr infinite-ones)))

(newline)
(equal? (shared ([a (cons 1 (cons 2 a))])
                a)
        (shared ([a (cons 1 b)]
                 [b (cons 2 c)]
                 [c (cons 1 b)])
                a))
(equal? (shared ([a (cons 1 (cons 2 a))])
                a)
        (shared ([a (cons 1 b)]
                 [b (cons 2 b)])
                a))





(newline)
(define 1-and-2 (shared ([a (cons 1 b)]
                         [b (cons 2 a)])
                        a))
(car 1-and-2)
(car (cdr 1-and-2))
(car (cdr (cdr 1-and-2)))
(car (cdr (cdr (cdr 1-and-2))))



(define vector-and-boxes
  (shared ([a (vector b b b)]
           [b (box 1)])
          (set-box! b 5)
          a))
(unbox (vector-ref vector-and-boxes 0))
(unbox (vector-ref vector-and-boxes 1))
(unbox (vector-ref vector-and-boxes 2))



(let ([v (shared ([a (cons 1 b)]
                  [b 7])
                 a)])
  (displayln (car v))
  (displayln (cdr v)))



(let ([v (shared ([a (cons 1 b)] ; b is early...
                  [b a])
                 a)])
  (displayln (car v))
  (displayln (cdr v)))




(let ([v (shared ([a (box b)]
                  [b (vector (unbox a)   ; unbox after a is patched
                             (unbox c))] ; unbox before c is patched
                  [c (box b)])
                 b)])
  (displayln (eq? (vector-ref v 0) v))
  (displayln (vector-ref v 1))
  (displayln (vector-length v)))



(define-struct person (name friends))
(let-values ([(a b c)
              (shared ([a (make-person "jill" (list b c))]
                       [b (make-person "jack" (list a c))]
                       [c (make-person "jane" (list))])
                      (values a b c))])
  (for-each displayln (map person-name (person-friends a)))
  (newline)
  (for-each displayln (map person-name (person-friends b)))
  (newline)
  (for-each displayln (map person-name (person-friends c)))
  (newline))



;; Make sure cyclic lists are treated correctly by list?
(shared ([a (cons 1 a)])
  (begin
    (displayln (pair? a))
    (displayln (list? a))))




(shared ([a (cons 1 a)])
  a)



(shared ([a (cons 1 b)]
         [b (cons 2 a)])
        a)
