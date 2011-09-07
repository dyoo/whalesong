#lang planet dyoo/whalesong/base

'(1 2 3)
(list "hello" "world")

(list? empty)
(list? '(1))
(list? '(1 2))
(list? 1)

(empty? empty)
(empty? '())
(cons? '())
(cons? '(hello))
(first '(hello))
(rest '(hello))
(second '(hello world))

(andmap even? '(2 4 6 8))
(andmap even? '())
(andmap even? '(2 4 5 8))
(andmap even? '(5))
(andmap even? '(1 3 5 7))
(andmap even? '(1 3 8 7))

(ormap even? '(2 4 6 8))
(ormap even? '())
(ormap even? '(2 4 5 8))
(ormap even? '(5))
(ormap even? '(1 3 5 7))
(ormap even? '(1 3 8 7))


(vector->list #(3 1 4))
