#lang planet dyoo/whalesong/base

;; A tree is either a symbol or a node.
(define-struct node (l r))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (list-fringe lst success-f fail-f)
  (cond
    [(empty? lst)
     (fail-f)]
    [else
     (success-f (first lst)
                (lambda ()
                  (list-fringe (rest lst) success-f fail-f)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-structure generator value)
  (define (on-success elt restart)
    (displayln elt)
    (restart))
  (define (on-fail)
    (displayln "Done!"))
  (generator value on-success on-fail))


(define (step-structure generator value)
  (define (on-success elt restart)
    (displayln elt)
    (void (read-line))
    (restart))
  (define (on-fail)
    (displayln "Done!"))
  (generator value on-success on-fail))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print-structure list-fringe '(1 2 3 4 5))

  




(define a-tree 
  (make-node 
   (make-node 'a (make-node 'b 'c)) 
   (make-node 'd 'e)))

(define (tree-fringe a-tree success-f fail-f)
  (cond
    [(symbol? a-tree)
     (success-f a-tree fail-f)]
    
    [(node? a-tree)
     (tree-fringe (node-l a-tree)      
                  success-f
                  (lambda ()
                    (tree-fringe (node-r a-tree)
                                 success-f
                                 fail-f)))]))

(print-structure tree-fringe 'a)
(print-structure tree-fringe (make-node 'a 'b))
(print-structure tree-fringe (make-node (make-node 'a 'b) 'c))
(print-structure tree-fringe (make-node 'a (make-node 'b 'c)))
(print-structure tree-fringe (make-node (make-node 'a 'b) (make-node 'c 'd)))
(print-structure tree-fringe (make-node 'a (make-node 'b (make-node 'c 'd))))
(print-structure tree-fringe (make-node (make-node (make-node 'a 'b) 'c) 'd))
