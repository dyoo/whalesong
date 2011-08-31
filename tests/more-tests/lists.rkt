#lang planet dyoo/whalesong/base

'(1 2 3)
(list "hello" "world")

(empty? empty)
(empty? '())
(cons? '())
(cons? '(hello))
(first '(hello))
(rest '(hello))
(second '(hello world))