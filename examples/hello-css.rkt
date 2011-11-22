#lang planet dyoo/whalesong/base
(require (planet dyoo/whalesong/web-world)
         (planet dyoo/whalesong/resource))

(define-resource "hello-css.css")
(define-resource "hello-css-main.html")

(big-bang 0 (initial-view hello-css-main.html))
