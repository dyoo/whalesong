#lang planet dyoo/whalesong
(require (planet dyoo/whalesong/world))

(display "hello again")
(newline)

(is-color? "red")
(is-color? "blue")
(is-color? 42)