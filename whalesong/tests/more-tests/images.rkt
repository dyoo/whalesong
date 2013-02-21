#lang planet dyoo/whalesong/base
(require (planet dyoo/whalesong/image))

(image-color? "red")
(image-color? "blue")
(image-color? 42)

(image-color? (make-color 3 4 5 0))
(image-color? "color")
