#lang planet dyoo/whalesong

(require (planet dyoo/whalesong/js))
(require (planet dyoo/whalesong/world))

;; A slide is either a simple string or an image.

(define font-size 50)


(define-struct label (id slide))

(define slides
  (list 
        (above
         (text "Whalesong:" 100 "black")
         (text "a Racket to JavaScript Compiler" 80 "black")
         (square 20 "solid" "white")
         (scale 2 (image-url "file:///home/dyoo/work/whalesong/racketcon/plt-logo.png"))
         (square 20 "solid" "white")
         (text "Danny Yoo (dyoo@hashcollision.org)" 50 "darkblue"))
        "Why Whalesong?"
        "World programs on the web"
        (above (text "Reusing Racket's compiler..." font-size "black")
               (square 20 "solid" "white")
               (text "Hello world?" (floor (* font-size 2/3)) "black"))
        "Web programs can use Racket"
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
    (my-min (add1 w) (sub1 (length slides)))]
   [else w]))


(define (draw w)
  (let ([a-slide (list-ref slides w)]
        [bg (BACKGROUND)])
    (cond
     [(string? a-slide)
      (place-image (text a-slide font-size "black")
                   (quotient (image-width bg) 2)
                   (quotient (image-height bg) 2)
                   bg)]

     [(image? a-slide)
      (place-image a-slide
                   (quotient (image-width bg) 2)
                   (quotient (image-height bg) 2)
                   bg)])))

(define (tick w)
  w)


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
          (on-tick tick)
          (to-draw draw))
