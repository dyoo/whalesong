#lang planet dyoo/whalesong
(require (planet dyoo/whalesong/image))

(is-color? "red")
(is-color? "blue")
(is-color? 42)

(is-color? (make-color 3 4 5 0))
(is-color? "color")
