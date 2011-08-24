#lang planet dyoo/whalesong
(require (planet dyoo/whalesong/web-world)
         (planet dyoo/whalesong/resource))

(define-resource index.html)


;; draw: world view -> view
(define (draw w v)
  (update-view-text (view-focus v "#counter") w))



;; tick: world view -> world
(define (tick w v)
  (printf "Tick ~s\n" w)
  (+ w 1))

(big-bang 0
          (initial-view index.html)
          (to-draw draw)
          (on-tick tick 1)
          (stop-when (lambda (w v)
                       (> w 10))))
