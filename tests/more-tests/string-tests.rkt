#lang planet dyoo/whalesong

(substring "hello world" 0)
(substring "hello world" 1)
(substring "hello world" 2)
(substring "hello world" 3)
(substring "hello world" 0)
(substring "hello world" 1 1)
(substring "hello world" 1 2)
(substring "hello world" 1 3)
(substring "hello world" 2 2)
(substring "hello world" 2 3)
(substring "hello world" 3 3)
(substring "hello world" 3 4)
(substring "hello world" 3 5)


(list->string '())
(list->string '(#\h #\e #\l #\l #\o))
