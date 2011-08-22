#lang planet dyoo/whalesong

;; Maze generation via Recursive Backtracking technique.
;; http://weblog.jamisbuck.org/2010/12/27/maze-generation-recursive-backtracking

(define-struct cell (carved visited) #:mutable #:transparent)

(define (make-grid width height f)
  (build-vector 
   width
   (lambda (i)
     (build-vector 
      height
      (lambda (j)
        (f i j))))))

(define grid (make-grid 20 20 
                        (lambda (i j)
                          (make-cell 0 #f))))

grid