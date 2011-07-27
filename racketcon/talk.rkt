#lang planet dyoo/whalesong

(require (planet dyoo/whalesong/js))
(require (planet dyoo/whalesong/world))


(define-struct world (index scaling rotate))

;; A slide is either a simple string or an image.

(define font-size 50)



(define slides
  (list 
        (above
         (text "Whalesong:" 100 "black")
         (text "a Racket to JavaScript Compiler" 80 "black")
         (square 20 "solid" "white")
         (scale 2 (image-url "file:///home/dyoo/work/whalesong/racketcon/plt-logo.png"))
         (square 20 "solid" "white")
         (text "Danny Yoo (dyoo@hashcollision.org)" 50 "darkblue"))
        (above (text "Why Whalesong?" font-size "black")
               (square 20 "solid" "white")
               (scale 2 (image-url "file:///home/dyoo/work/whalesong/racketcon/bootstrap.gif")))
        "World programs on the web"
        (above (text "Reusing Racket's compiler..." font-size "black")
               (square 20 "solid" "white")
               (text "Hello world?" (floor (* font-size 2/3)) "black"))
        "Web programs can use Racket too!"
        "What's next?"
        (text "http://hashcollision.org/whalesong" 80 "black")))




(define (WIDTH)
  (viewport-width))

(define (HEIGHT)
  (viewport-height))

(define (BACKGROUND)
  (empty-scene (WIDTH) (HEIGHT)))


(define (key w a-key)
  (cond
   [(key=? a-key "left")
    (make-world (my-max (sub1 (world-index w)) 0)
                (world-scaling w)
                (world-rotate w))]
   [(or (key=? a-key "right") (key=? a-key " ") (key=? a-key "enter"))
    (make-world (my-min (add1 (world-index w))
                        (sub1 (length slides)))
                (world-scaling w)
                (world-rotate w))]

   [(key=? a-key "up")
    (make-world (world-index w)
                (+ (world-scaling w) .1)
                (world-rotate w))]

   [(key=? a-key "down")
    (make-world (world-index w)
                (- (world-scaling w) .1)
                (world-rotate w))]

   [(key=? a-key "r")
    (make-world 0 1 0)]

   [(key=? a-key "q")
    (make-world (world-index w)
                (world-scaling w)
                (modulo (- (world-rotate w) 1) 360))]

   [(key=? a-key "w")
    (make-world (world-index w)
                (world-scaling w)
                (modulo (+ (world-rotate w) 1) 360))]
   
   [else w]))


(define (draw w)
  (rotate (world-rotate w)
          (scale (world-scaling w)
                 (let ([a-slide (list-ref slides (world-index w))]
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
                                  bg)])))))

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



(big-bang (make-world 0 1 0)
          (on-key key)
          (on-tick tick)
          (to-draw draw))
