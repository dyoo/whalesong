#lang planet dyoo/whalesong

(require (planet dyoo/whalesong/resource)
         (planet dyoo/whalesong/web-world))

(define-resource index.html)
(define-resource style.css)

(define (tick w v)
  (modulo (add1 w) 10))


;; pick-block: world view -> view
;; Focus the view on block i.
(define (pick-block v i)
  (view-focus v (format "#~a" i)))


(define (draw w v)
  (define v1 (update-view-attr
              (pick-block v w)
              "class"
              "selectedBlock"))
  (define v2 (update-view-attr
              (pick-block v1 (modulo (sub1 w) 10))
              "class"
              "offsetBlock"))
  (define v3 (update-view-attr
              (pick-block v2 (modulo (add1 w) 10))
              "class"
              "offsetBlock"))
  (define v4 (update-view-attr
              (pick-block v3 (modulo (- w 2) 10))
                          "class"
                          "block"))
  (define v5 (update-view-attr
              (pick-block v4 (modulo (+ w 2) 10))
              "class"
              "block"))
  v5)


(big-bang 0
          (initial-view index.html)
          (on-tick tick)
          (to-draw draw))
