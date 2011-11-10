#lang planet dyoo/whalesong/cs019

(define-struct f (x))
(define-struct g (a b))
(check-expect (build-list 5 add1) (list 1 2 3 4 5))
(check-expect (make-g 1 2) (make-g 1 2))
(check-expect (make-g 'b empty) (make-g 'b empty))

(check-expect (cond
                [(memq 'x (list 'a 'x 'b)) 1]
                [else 2])
              1)

(define i (open-image-url "http://racket-lang.org/logo.png"))
(check-expect (image-height i) 85)
(check-expect (image-width i) 88)

;; Whalesong currently has no support for hashes.
;(define ht (hash))
;(define ht2 (hash-set ht "x" 10))
;(check-expect(hash-ref ht2 "x") 10)
;(check-error (hash-ref ht "x") "hash-ref: no value found for key: \"x\"")

;; INSERTION SORT

(define: (isort [l : (Listof: Number$)]) -> (Listof: Number$)
  (cond
    [(empty? l) l]
    [(cons? l) (insert (first l)
                       (isort (rest l)))]))

(define: (insert [e : Number$] [l : (Listof: Number$)]) -> (Listof: Number$)
  (cond
    [(empty? l) (cons e l)]
    [(cons? l) (if (<= e (first l))
                   (cons e l)
                   (cons (first l)
                         (insert e (rest l))))]))

(check-expect (isort (list 3 1 2 4)) (list 1 2 3 4))

;; TREE ZIP

;(struct: None ())
;(struct: (a) Some ([v : a]))
;(define-type (Opt a) (U None (Some a)))
(define-struct: None ())
(define-struct: Some ([v : Any$]))
(define Opt$ (or: None$ Some$))

;(struct: (a) Node ([value : a] [kids : (Listof (Tree a))]) #:transparent)
;(struct: MtNode () #:transparent)
;(define-type Tree (All (a) (U (Node a) MtNode)))
(define-struct: Node ([value : Any$] [kids : (Listof: Tree$)]))
(define-struct: MtNode ())
(define Tree$ (or: Node$ MtNode$))

;(struct: (a) BackPtr ([n : (Node a)] [p : Integer]) #:transparent)
;(struct: (a) Cursor ([below : (Tree a)] [above : (Listof (BackPtr a))]) #:transparent)
(define-struct: BackPtr ([n : Node$] [p : (Sig: integer?)]))
(define-struct: Cursor ([below : Tree$] [above : (Listof: BackPtr$)]))

(define Opt-Cursor$ (Sig: (lambda (v)
                            (or (None? v)
                                (and (Some? v)
                                     (Cursor? (Some-v v)))))))

;(: find (All (a) ((Tree a) (a -> Boolean) -> (Cursor a))))
(define: (find [t : Tree$] [p : (Any$ -> Boolean$)]) -> Cursor$
  (local
    [
     ;(: find-helper (All (a) ((Tree a) (Listof (BackPtr a)) -> (Opt (Cursor a)))))
     (define: (find-helper [t : Tree$] [above : (Listof: BackPtr$)]) -> Opt-Cursor$
       (cond
         [(MtNode? t) (make-None)]
         [(Node? t)
          (if (p (Node-value t))
              (make-Some (make-Cursor t above))
              (let ([v (search-kids (Node-kids t) 0 t above)])
                (if (Some? v) v (make-None))))]))

;       (: search-kids (All (a) ((Listof (Tree a)) 
;                           Integer
;                           (Node a)
;                           (Listof (BackPtr a)) -> (Opt (Cursor a)))))
     (define: (search-kids [kids : (Listof: Tree$)]
                           [n : (Sig: integer?)]
                           [first-above : Node$]
                           [rest-above : (Listof: BackPtr$)]) -> Opt-Cursor$
       (cond
         [(empty? kids) (make-None)]
         [(cons? kids)
          (let ([v (find-helper (first kids) 
                                (cons (make-BackPtr first-above n) rest-above))])
            (if (Some? v) 
                v
                (search-kids (rest kids) (add1 n) first-above rest-above)))]))
     ]
    (let ([v (find-helper t empty)])
      (if (None? v) (error 'find "no such node") (Some-v v)))))

;(: down (All (a) ((Cursor a) Integer -> (Cursor a))))
(define: (down [c : Cursor$] [n : (Sig: integer?)]) -> Cursor$
  (let ([v (Cursor-below c)])
    (cond
      [(MtNode? v) (error 'down "impossible to go down")]
      [(Node? v)
       (if (empty? (Node-kids v))
           (error 'down "impossible to go down")
           (make-Cursor (list-ref (Node-kids v) n)
                        (cons (make-BackPtr v n) (Cursor-above c))))])))

;(: replace (All (a) ((Cursor a) (Tree a) -> (Cursor a))))
(define: (replace [c : Any$] [t : Tree$]) -> Cursor$
  (make-Cursor t (Cursor-above c)))

;(: reconstruct/1 (All (a) ((BackPtr a) (Tree a) -> (Node a))))
(define: (reconstruct/1 [one-up : BackPtr$] [replace-with : Tree$]) -> Node$
  (let ([node (BackPtr-n one-up)]
        [posn (BackPtr-p one-up)])
    (let ([val (Node-value node)]
          [kids (Node-kids node)])
      (make-Node val
                 (build-list (length kids)
                             (lambda: ([i : (Sig: integer?)]) -> Tree$
                                      (if (= i posn)
                                          replace-with
                                          (list-ref kids i))))))))

;(: up (All (a) ((Cursor a) -> (Cursor a))))
(define: (up [c : Cursor$]) -> Cursor$
  (if (empty? (Cursor-above c))
      (error 'up "impossible to go up")
      (make-Cursor (reconstruct/1 (first (Cursor-above c))
                                  (Cursor-below c))
                   (rest (Cursor-above c)))))

;(: ->tree (All (a) ((Cursor a) -> (Tree a))))
(define: (->tree [c : Cursor$]) -> Tree$
  (if (empty? (Cursor-above c))
      (Cursor-below c)
      (->tree (up c))))

(define T (make-Node 7 (list (make-Node 3 empty) (make-MtNode) (make-Node 5 empty))))
(define c0 (find T (lambda: ([n : (Sig: integer?)]) -> Boolean$ (= n 3))))
(define c2 (find T (lambda: ([n : (Sig: integer?)]) -> Boolean$ (= n 5))))
(define c3                (replace (down (up c0) 1) T)             )
(define c4 (replace (down (replace (down (up c0) 1) T) 0) (make-MtNode)))

(check-expect T (->tree c0))
(check-expect T (->tree c2))
(check-expect (->tree c3)
              (make-Node 7
                         (list (make-Node 3 empty)
                               (make-Node 7 (list (make-Node 3 empty)
                                                  (make-MtNode)
                                                  (make-Node 5 empty)))
                               (make-Node 5 empty))))
(check-expect (->tree c4)
              (make-Node 7
                         (list (make-Node 3 empty)
                               (make-Node 7 (list (make-MtNode)
                                                  (make-MtNode)
                                                  (make-Node 5 empty)))
                               (make-Node 5 empty))))
