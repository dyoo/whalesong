#lang planet dyoo/whalesong/cs019

(define-resource index.html)

(define base-view (->view index.html))

(define (remove-all x elts)
  (cond
   [(empty? elts)
    empty]
   [(equal? x (first elts))
    (remove-all x (rest elts))]
   [else
    (cons (first elts)
          (remove-all x (rest elts)))]))

(define view-with-buttons
  (foldl (lambda (name a-view)
           (view-bind (view-focus a-view name)
                      "click"
                      (lambda (world a-view)
                        (cond
                         [(view-has-attr? a-view "checked")
                          (cons name world)]
                         [else
                          (remove-all name world)]))))
         base-view
         '("hot" "cross" "buns")))

(define view-with-buttons-and-reset
  (view-bind (view-focus view-with-buttons "reset")
             "click"
             (lambda (world a-view)
               empty)))


(define (draw w v)
  (update-view-text (view-focus v "mydiv")
                    (format "~s" w)))

(big-bang '()
          (initial-view view-with-buttons)
          (to-draw draw))