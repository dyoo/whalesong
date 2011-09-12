#lang planet dyoo/whalesong
(require (planet dyoo/whalesong/web-world)
         (planet dyoo/whalesong/resource))

(define-resource index.html)


;; draw: world view -> view
(define (draw w dom)
  (update-view-text (view-focus dom "counter") w))



;; tick: world view -> world
(define (tick w v)
  (add1 w))

(define (stop? world dom)
  (> world 10))

(big-bang 0
          (initial-view index.html)
          (to-draw draw)
          (on-tick tick 1)
          (stop-when stop?))
