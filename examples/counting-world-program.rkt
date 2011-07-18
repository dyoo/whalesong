#lang planet dyoo/whalesong

(require (planet dyoo/whalesong/world))


(define handler (on-tick add1 1))
handler

"big bang should follow:"

(big-bang 1
          (on-tick add1 1)
          ;;(on-tick (lambda (w) (* w 2)) 1)
          (stop-when (lambda (w) (> w 10)))
          )


"all done"