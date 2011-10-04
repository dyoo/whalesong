#lang planet dyoo/whalesong/cs019

(define-resource index.html)


(define: (draw [world : Number$] [dom : View$]) -> View$
  (update-view-text (view-focus dom "counter") world))


(define: (tick [world : Number$] [dom : View$]) -> Number$
  (add1 world))


(define: (stop? [world : Number$] [dom : View$]) -> Boolean$
  (> world 10))

(big-bang 0
          (initial-view index.html)
          (to-draw draw)
          (on-tick tick 1)
          (stop-when stop?))
