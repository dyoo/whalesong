#lang whalesong
(require "module-scoping-helper.rkt")
x             ;; 0
(get-x)       ;; 0

x+1
x             ;; 1
(get-x)       ;; 1

x++
x
(get-x)

x=0
x
(get-x)

x++
x
(get-x)
x++
x
(get-x)

x+1
x
(get-x)
