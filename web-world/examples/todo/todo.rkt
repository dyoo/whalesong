#lang planet dyoo/whalesong
(require (planet dyoo/whalesong/web-world)
         (planet dyoo/whalesong/resource))

;; The world is our TODO list, represented as a list of strings.

(define-resource index.html)



(define (on-add world view)
  (local [(define text (view-text (view-focus view "next-item")))]
    (printf "Ok, added\n")
    (cons text world)))


(define the-view
  (view-bind (view-focus (->view index.html)
                         "add-button"
                         on-add)))


(big-bang '()
          (initial-view the-view))