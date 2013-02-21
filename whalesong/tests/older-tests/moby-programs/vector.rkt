#lang s-exp "../../lang/wescheme.rkt"

(require "../../lang/check-expect/test-expect.rkt")
(printf "vector.rkt\n")


(define v (build-vector 5 (lambda (a) a)))
(test-expect v #(0 1 2 3 4))
(test-expect (vector-length v) 5)

(test-expect (vector? v) true)
(test-expect (vector? '(not a vector)) false)


(define v2 (build-vector 5 (lambda (a) (* a a))))
(test-expect v2 #(0 1 4 9 16))

(test-expect (vector->list #()) '())
(test-expect (vector->list v2) '(0 1 4 9 16))


(test-expect (list->vector '()) #())
(test-expect (list->vector '(a b c)) #(a b c))


(define v3 (vector 'hello 'world))
(test-expect v3 '#(hello world))
(vector-set! v3 0 'hola)
(test-expect v3 '#(hola world))
(test-expect (vector-ref v3 0) 'hola)




(printf "vector.rkt end\n")

