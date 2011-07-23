#lang planet dyoo/whalesong

(require (planet dyoo/whalesong/js))
(require (planet dyoo/whalesong/world))

;; A slide is either a simple string or an image.


(define slides
  (list "Whalesong: a Racket to JavaScript Compiler"
        "Why Whalesong?"
        "World programs on the web"
        "Reusing Racket..."
        "Hello world!"
        "What's missing?"
        "http://hashcollision.org/whalesong"))




(define (WIDTH)
  (viewport-width))

(define (HEIGHT)
  (viewport-height))

(define (BACKGROUND)
  (empty-scene (WIDTH) (HEIGHT)))


(define (key w a-key)
  (cond
   [(key=? a-key "left")
    (my-max (sub1 w) 0)]
   [(key=? a-key "right")
    (my-min (sub1 w) (length slides))]
   [else w]))


(define (draw w)
  (let ([a-slide (list-ref slides w)]
        [bg (BACKGROUND)])
    (cond
     [(string? a-slide)
      (place-image (text a-slide 300 "black")
                   (quotient (image-width bg) 2)
                   (quotient (image-height bg) 2)
                   bg)]

     [(image? a-slide)
      (place-image a-slide
                   (quotient (image-width bg) 2)
                   (quotient (image-height bg) 2)
                   bg)])))
                   

(define (my-max x y)
  (if (> x y)
      x
      y))

(define (my-min x y)
  (if (< x y)
      x
      y))



(big-bang 0
          (on-key key)
          (on-draw draw))