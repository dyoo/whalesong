#lang planet dyoo/whalesong

(require (planet dyoo/whalesong/web-world))

(define-resource view.html)

(define bound-view
  (view-bind-many view.html
                  ["red-button" "click" click]
                  ["white-button" "click" click]
                  ["blue-button" "click" click]
                  ["orange-button" "click" click]
                  ["green-button" "click" click]
                  ["black-button" "click" click]))


(big-bang (initial-view bound-view)