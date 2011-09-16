#lang planet dyoo/whalesong

(require (planet dyoo/whalesong/world)
         (planet dyoo/whalesong/image))

(define width 640)
(define height 480)

(define-struct posn (x y))

(define (mouse world x y type)
  (cond
   [(string=? type "click")
    (make-posn x y)]
   [else
    world]))

(define (draw w)
  (place-image (circle 20 'solid 'red)
               (posn-x w)
               (posn-y w)
               (empty-scene width height)))
                       

(big-bang (make-posn 0 0)
          (on-mouse mouse)
          (to-draw draw))