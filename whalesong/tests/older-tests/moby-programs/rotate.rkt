#lang s-exp "../../lang/wescheme.rkt"


"rotate and scale"

(printf "Three images at 30, 60, 90 degree rotation:\n")

(rotate 30 (image-url "http://racket-lang.org/logo.png"))
(rotate 60 (image-url "http://racket-lang.org/logo.png"))
(rotate 90 (image-url "http://racket-lang.org/logo.png"))


(printf "scaling small and large")
(scale 1/2 (image-url "http://racket-lang.org/logo.png"))
(scale 2 (image-url "http://racket-lang.org/logo.png"))

(scale/xy 1 2 (image-url "http://racket-lang.org/logo.png"))
(scale/xy 2 1 (image-url "http://racket-lang.org/logo.png"))

"This should be the normal image"
(scale/xy 1 1 (image-url "http://racket-lang.org/logo.png"))


"Rotated, huge image"
(rotate 30 (scale 3 (image-url "http://racket-lang.org/logo.png")))

"rotate and scale end"