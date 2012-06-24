#lang planet dyoo/whalesong

(require (planet dyoo/whalesong/web-world))

;; A small drag-and-drop example using the web-world library.
;;
;; The world consists of a set of boxes.
;;
;; A box has an id and a position.

(define-struct world (boxes))
(define-struct box (id x y))


