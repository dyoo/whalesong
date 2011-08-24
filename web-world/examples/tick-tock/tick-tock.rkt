#lang planet dyoo/whalesong
(require (planet dyoo/whalesong/web-world)
         (planet dyoo/whalesong/resource))

(define-resource index.html)

;; draw: world view -> view
(define (draw w v)
  v
  ;(view-text (view-focus v "#counter") w)
  )

;; tick: world view -> world
(define (tick w v)
  (printf "Tick\n")
  (add1 w))
  
(big-bang 0
          (initial-view index.html)
          (to-draw draw)
          (on-tick tick 1))
