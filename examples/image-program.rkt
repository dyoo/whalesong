#lang planet dyoo/whalesong
(require (planet dyoo/whalesong/image))

(display "hello again")
(newline)

(is-color? "red")
(is-color? "blue")
(is-color? 42)