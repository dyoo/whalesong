#lang planet dyoo/whalesong
(require (planet dyoo/whalesong/web-world)
         (planet dyoo/whalesong/resource))

;; The world is our TODO list, represented as a list of strings.

(define-resource index.html)



(define (on-add world view)
  (local [(define text (view-form-value (view-focus view "#next-item")))]
    (cons text world)))


(define (to-draw world view)
  )


(define the-view
  (view-bind (view-focus (->view index.html) "#add-button")
             "click"
             on-add))


(big-bang '()
          (initial-view the-view))