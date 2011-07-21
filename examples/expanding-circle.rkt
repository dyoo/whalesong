#lang planet dyoo/whalesong

(require (planet dyoo/whalesong/world))


(define handler (on-tick add1 1))
handler

"big bang should follow:"


(define (draw w)
  (circle w 'solid 'blue))


(big-bang 1
          (on-tick add1 1/28)
          (stop-when (lambda (w) (> w 500)))
          (to-draw draw)
          )


"all done"