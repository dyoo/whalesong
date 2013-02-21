#lang typed/racket/base


;; Union-find hardcoded to do symbols.

(provide (all-defined-out))


;; A forest contains a collection of its nodes keyed by element.
;; The elements are compared by eq?
(define-struct: forest 
  ([ht : (HashTable Symbol node)]))


;; A node is an element, a parent node, and a numeric rank.
(define-struct: node 
  ([elt : Symbol] 
   [p : (U False node)]
   [rank : Natural])
  #:mutable)



;; Builds a new, empty forest.
(: new-forest (-> forest))
(define (new-forest)
  (make-forest (make-hash))) 


;; lookup-node: forest X -> node
;; Returns the node that's associated with this element.
(: lookup-node (forest Symbol -> node))
(define (lookup-node a-forest an-elt)
  (unless (hash-has-key? (forest-ht a-forest) an-elt)
    (make-set a-forest an-elt))
  (hash-ref (forest-ht a-forest)
            an-elt))



;; make-set: forest X -> void
;; Adds a new set into the forest.
(: make-set (forest Symbol -> Void))
(define (make-set a-forest an-elt)
  (unless (hash-has-key? (forest-ht a-forest) an-elt)
    (let ([a-node (make-node an-elt #f 0)])
      (set-node-p! a-node a-node)
      (hash-set! (forest-ht a-forest) an-elt a-node))))



(: find-set (forest Symbol -> Symbol))
;; Returns the representative element of elt.
(define (find-set a-forest an-elt)
  (let ([a-node (lookup-node a-forest an-elt)])
    (node-elt (get-representative-node a-node))))



(: get-representative-node (node -> node))
;; Returns the representative node of a-node, doing path
;; compression if we have to follow links.
(define (get-representative-node a-node)
  (let ([p (node-p a-node)])
    (cond [(eq? a-node p)
           a-node]
          [(node? p)
           (let ([rep (get-representative-node p)])
             ;; Path compression is here:
             (set-node-p! a-node rep)
             rep)]
          [else
           ;; impossible situation
           (error 'get-representative-node)])))


(: union-set (forest Symbol Symbol -> Void))
;; Joins the two elements into the same set.
(define (union-set a-forest elt1 elt2)
  (let ([rep1 (get-representative-node
               (lookup-node a-forest elt1))]
        [rep2 (get-representative-node
               (lookup-node a-forest elt2))])
    (cond
      [(< (node-rank rep1) (node-rank rep2))
       (set-node-p! rep1 rep2)]
      [(> (node-rank rep1) (node-rank rep2))
       (set-node-p! rep2 rep1)]
      [else
       (set-node-p! rep1 rep2)
       (set-node-rank! rep1 (add1 (node-rank rep1)))])))