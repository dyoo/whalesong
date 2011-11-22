#lang planet dyoo/whalesong/cs019

(define-resource index.html)

(define base-view (->view index.html))

(define view-with-buttons
  (foldl (lambda (name a-view)
           (view-bind (view-focus a-view name)
                      "click"
                      (lambda (world a-view)
                        (begin
                          (printf "here: ~s\n" (view-attr
                                                a-view
                                                "checked"))
                          world))))
         base-view
         '("hot" "cross" "buns")))

(define (draw w v)
  v)

(big-bang '()
          (initial-view view-with-buttons)
          (to-draw draw))