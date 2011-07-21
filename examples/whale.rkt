#lang planet dyoo/whalesong

(require (planet dyoo/whalesong/world))

(define-struct world (x direction))


(define whale-image (image-url "http://hashcollision.org/whalesong/humpback.jpg"))

(define scene-width (* (image-width whale-image) 5))

(define (draw w)
  (place-image whale-image
               (world-x w)
               (/ (image-height whale-image) 2)
               (empty-scene scene-width
                            (image-height whale-image))))

(define (tick w)
  (make-world (modulo (+ (world-x w)
                         (world-direction w))
                      (+ scene-width (image-width whale-image)))
              (world-direction w)))


(define (key w a-key)
  (cond
   [(key=? a-key "left")
    (make-world (world-x w) (sub1 (world-direction w)))]
   [(key=? a-key "right")
    (make-world (world-x w) (add1 (world-direction w)))]))


(big-bang (make-world 0 5)
          (on-tick tick)
          (to-draw draw)
          (on-key key))
