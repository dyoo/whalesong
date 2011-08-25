#lang planet dyoo/whalesong

(require (planet dyoo/whalesong/resource)
         (planet dyoo/whalesong/web-world))

(define-resource index.html)
(define-resource style.css)

(define (tick w v)
  (modulo (add1 w) 10))

(define (draw w v)
  (define base (update-view-attr (view-focus v (format "#~a" w))
                                 "class"
                                 "selectedBlock"))
  (define with-left (if (> w 0)
                        (view-right (update-view-attr (view-left base)
                                                      "class"
                                                      "offsetBlock"))
                        base))
  (define with-right (if (< w 9)
                         (view-left (update-view-attr (view-right base)
                                                      "class"
                                                      "offsetBlock"))
                         with-left))
  with-right)



(big-bang 0
          (initial-view index.html)
          (on-tick tick 1/2)
          (to-draw draw))