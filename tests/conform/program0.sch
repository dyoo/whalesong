(begin

;; (define (caar l)
;;   (car (car l)))

;; (define (map f l)
;;   (if (null? l)
;;       null
;;       (cons (f (car l))
;;             (map f (cdr l)))))

;; (define (for-each f l)
;;   (if (null? l)
;;       null
;;       (begin (f (car l))
;; 	     (for-each f (cdr l)))))

;; (define (memq x l)
;;   (if (null? l)
;;       #f
;;       (if (eq? x (car l))
;;           l
;; 	  (memq x (cdr l)))))


;; (define (assq x l)
;;   (if (null? l)
;;       #f
;;       (if (eq? x (caar l))
;;           (car l)
;; 	  (assq x (cdr l)))))


;; (define (length l)
;;   (if (null? l)
;;       0
;;       (add1 (length (cdr l)))))


;; (define (append l1 l2)
;;   (if (nullb? l1) 
;;       l2
;;       (cons (car l1) (append (cdr l1) l2))))


(define vector-copy
  (lambda (v)
    (let ((length (vector-length v)))
      (let ((result (make-vector length)))
        ((letrec ((loop
                   (lambda (n) (vector-set! result n (vector-ref v n)) (if (= n length) v (loop (+ n '1))))))
           loop)
         '0)))))
(define sort
  (lambda (obj pred)
    (letrec ((loop (lambda (l) (if (if (pair? l) (pair? (cdr l)) '#f) (split l '() '()) l)))
             (split
              (lambda (l one two)
                (if (pair? l) (split (cdr l) two (cons (car l) one)) (merge (loop one) (loop two)))))
             (merge
              (lambda (one two)
                (if (null? one)
                    (begin two)
                    (if (pred (car two) (car one))
                        (begin (cons (car two) (merge (cdr two) one)))
                        (begin (cons (car one) (merge (cdr one) two))))))))
      (if (let ((or-part (pair? obj))) (if or-part or-part (null? obj)))
          (begin (loop obj))
          (if (vector? obj)
              (begin (sort! (vector-copy obj) pred))
              (begin (error '"sort: argument should be a list or vector" obj)))))))
(define sort!
  (lambda (v pred)
    (letrec ((sort-internal!
              (lambda (vec temp low high)
                (if (< low high)
                    (let ((middle (quotient (+ low high) '2)))
                      (let ((next (+ middle '1)))
                        (sort-internal! temp vec low middle)
                        (sort-internal! temp vec next high)
                        ((letrec ((loop
                                   (lambda (p p1 p2)
                                     (if (not (> p high))
                                         (if (> p1 middle)
                                             (begin
                                               (vector-set! vec p (vector-ref temp p2))
                                               (loop (+ p '1) p1 (+ p2 '1)))
                                             (if (let ((or-part (> p2 high)))
                                                   (if or-part
                                                       or-part
                                                       (pred (vector-ref temp p1) (vector-ref temp p2))))
                                                 (begin
                                                   (vector-set! vec p (vector-ref temp p1))
                                                   (loop (+ p '1) (+ p1 '1) p2))
                                                 (begin
                                                   (vector-set! vec p (vector-ref temp p2))
                                                   (loop (+ p '1) p1 (+ p2 '1)))))
                                         (void)))))
                           loop)
                         low
                         low
                         next)))
                    (void)))))
      (if (not (vector? v)) (error '"sort!: argument not a vector" v) (void))
      (sort-internal! v (vector-copy v) '0 (- (vector-length v) '1))
      v)))
(define adjoin (lambda (element set) (if (memq element set) set (cons element set))))
(define eliminate
  (lambda (element set)
    (if (null? set)
        (begin set)
        (if (eq? element (car set)) (begin (cdr set)) (begin (cons (car set) (eliminate element (cdr set))))))))
(define intersect
  (lambda (list1 list2)
    ((letrec ((loop
               (lambda (l)
                 (if (null? l)
                     (begin '())
                     (if (memq (car l) list2) (begin (cons (car l) (loop (cdr l)))) (begin (loop (cdr l))))))))
       loop)
     list1)))
(define union (lambda (list1 list2) (if (null? list1) list2 (union (cdr list1) (adjoin (car list1) list2)))))
(define make-internal-node vector)
(define internal-node-name (lambda (node) (vector-ref node '0)))
(define internal-node-green-edges (lambda (node) (vector-ref node '1)))
(define internal-node-red-edges (lambda (node) (vector-ref node '2)))
(define internal-node-blue-edges (lambda (node) (vector-ref node '3)))
(define set-internal-node-name! (lambda (node name) (vector-set! node '0 name)))
(define set-internal-node-green-edges! (lambda (node edges) (vector-set! node '1 edges)))
(define set-internal-node-red-edges! (lambda (node edges) (vector-set! node '2 edges)))
(define set-internal-node-blue-edges! (lambda (node edges) (vector-set! node '3 edges)))
(define make-node
  (lambda (name blue-edges)
    (let ((name (if (symbol? name) (symbol->string name) name))
          (blue-edges (if (null? blue-edges) 'NOT-A-NODE-YET (car blue-edges))))
      (make-internal-node name '() '() blue-edges))))
(define copy-node (lambda (node) (make-internal-node (name node) '() '() (blue-edges node))))
(define name internal-node-name)
(define make-edge-getter
  (lambda (selector)
    (lambda (node)
      (if (let ((or-part (none-node? node))) (if or-part or-part (any-node? node)))
          (error '"Can't get edges from the ANY or NONE nodes")
          (selector node)))))
(define red-edges (make-edge-getter internal-node-red-edges))
(define green-edges (make-edge-getter internal-node-green-edges))
(define blue-edges (make-edge-getter internal-node-blue-edges))
(define make-edge-setter
  (lambda (mutator!)
    (lambda (node value)
      (if (any-node? node)
          (begin (error '"Can't set edges from the ANY node"))
          (if (none-node? node) (begin 'OK) (begin (mutator! node value)))))))
(define set-red-edges! (make-edge-setter set-internal-node-red-edges!))
(define set-green-edges! (make-edge-setter set-internal-node-green-edges!))
(define set-blue-edges! (make-edge-setter set-internal-node-blue-edges!))
(define make-blue-edge vector)
(define blue-edge-operation (lambda (edge) (vector-ref edge '0)))
(define blue-edge-arg-node (lambda (edge) (vector-ref edge '1)))
(define blue-edge-res-node (lambda (edge) (vector-ref edge '2)))
(define set-blue-edge-operation! (lambda (edge value) (vector-set! edge '0 value)))
(define set-blue-edge-arg-node! (lambda (edge value) (vector-set! edge '1 value)))
(define set-blue-edge-res-node! (lambda (edge value) (vector-set! edge '2 value)))
(define operation blue-edge-operation)
(define arg-node blue-edge-arg-node)
(define res-node blue-edge-res-node)
(define set-arg-node! set-blue-edge-arg-node!)
(define set-res-node! set-blue-edge-res-node!)
(define lookup-op
  (lambda (op node)
    ((letrec ((loop
               (lambda (edges)
                 (if (null? edges)
                     (begin '())
                     (if (eq? op (operation (car edges))) (begin (car edges)) (begin (loop (cdr edges))))))))
       loop)
     (blue-edges node))))
(define has-op? (lambda (op node) (not (null? (lookup-op op node)))))
(define make-internal-graph vector)
(define internal-graph-nodes (lambda (graph) (vector-ref graph '0)))
(define internal-graph-already-met (lambda (graph) (vector-ref graph '1)))
(define internal-graph-already-joined (lambda (graph) (vector-ref graph '2)))
(define set-internal-graph-nodes! (lambda (graph nodes) (vector-set! graph '0 nodes)))
(define make-graph (lambda (nodes) (make-internal-graph nodes (make-empty-table) (make-empty-table))))
(define graph-nodes internal-graph-nodes)
(define already-met internal-graph-already-met)
(define already-joined internal-graph-already-joined)
(define add-graph-nodes!
  (lambda (graph nodes) (set-internal-graph-nodes! graph (cons nodes (graph-nodes graph)))))
(define copy-graph
  (lambda (g)
    (letrec ((copy-list (lambda (l) (vector->list (list->vector l)))))
      (make-internal-graph (copy-list (graph-nodes g)) (already-met g) (already-joined g)))))
(define clean-graph
  (lambda (g)
    (letrec ((clean-node
              (lambda (node)
                (if (not (let ((or-part (any-node? node))) (if or-part or-part (none-node? node))))
                    (begin (set-green-edges! node '()) (set-red-edges! node '()))
                    (void)))))
      (for-each clean-node (graph-nodes g))
      g)))
(define canonicalize-graph
  (lambda (graph classes)
    (letrec ((fix
              (lambda (node)
                (letrec ((fix-set
                          (lambda (object selector mutator)
                            (mutator
                             object
                             (map
                              (lambda (node) (find-canonical-representative node classes))
                              (selector object))))))
                  (if (not (let ((or-part (none-node? node))) (if or-part or-part (any-node? node))))
                      (begin
                        (fix-set node green-edges set-green-edges!)
                        (fix-set node red-edges set-red-edges!)
                        (for-each
                         (lambda (blue-edge)
                           (set-arg-node! blue-edge (find-canonical-representative (arg-node blue-edge) classes))
                           (set-res-node! blue-edge (find-canonical-representative (res-node blue-edge) classes)))
                         (blue-edges node)))
                      (void))
                  node)))
             (fix-table
              (lambda (table)
                (letrec ((canonical? (lambda (node) (eq? node (find-canonical-representative node classes))))
                         (filter-and-fix
                          (lambda (predicate-fn update-fn list)
                            ((letrec ((loop
                                       (lambda (list)
                                         (if (null? list)
                                             (begin '())
                                             (if (predicate-fn (car list))
                                                 (begin (cons (update-fn (car list)) (loop (cdr list))))
                                                 (begin (loop (cdr list))))))))
                               loop)
                             list)))
                         (fix-line
                          (lambda (line)
                            (filter-and-fix
                             (lambda (entry) (canonical? (car entry)))
                             (lambda (entry)
                               (cons (car entry) (find-canonical-representative (cdr entry) classes)))
                             line))))
                  (if (null? table)
                      '()
                      (cons
                       (car table)
                       (filter-and-fix
                        (lambda (entry) (canonical? (car entry)))
                        (lambda (entry) (cons (car entry) (fix-line (cdr entry))))
                        (cdr table))))))))
      (make-internal-graph
       (map (lambda (class) (fix (car class))) classes)
       (fix-table (already-met graph))
       (fix-table (already-joined graph))))))



(define none-node (make-node 'none '(#t)))
(define none-node? (lambda (node) (eq? node none-node)))
(define any-node (make-node 'any '(())))
(define any-node? (lambda (node) (eq? node any-node)))
(define green-edge?
  (lambda (from-node to-node)
    (if (any-node? from-node)
        (begin '#f)
        (if (none-node? from-node)
            (begin '#t)
            (if (memq to-node (green-edges from-node)) (begin '#t) (begin '#f))))))
(define red-edge?
  (lambda (from-node to-node)
    (if (any-node? from-node)
        (begin '#f)
        (if (none-node? from-node)
            (begin '#t)
            (if (memq to-node (red-edges from-node)) (begin '#t) (begin '#f))))))
(define sig
  (let ((none-comma-any (cons none-node any-node)))
    (lambda (op node)
      (let ((the-edge (lookup-op op node)))
        (if (not (null? the-edge)) (cons (arg-node the-edge) (res-node the-edge)) none-comma-any)))))
(define arg (lambda (pair) (car pair)))
(define res (lambda (pair) (cdr pair)))
(define conforms?
  (lambda (t1 t2)
    (letrec ((nodes-with-red-edges-out (box '()))
             (add-red-edge!
              (lambda (from-node to-node)
                (set-red-edges! from-node (adjoin to-node (red-edges from-node)))
                (set-box! nodes-with-red-edges-out (adjoin from-node (unbox nodes-with-red-edges-out)))))
             (greenify-red-edges!
              (lambda (from-node)
                (set-green-edges! from-node (append (red-edges from-node) (green-edges from-node)))
                (set-red-edges! from-node '())))
             (delete-red-edges! (lambda (from-node) (set-red-edges! from-node '())))
             (does-conform
              (lambda (t1 t2)
                (if (let ((or-part (none-node? t1))) (if or-part or-part (any-node? t2)))
                    (begin '#t)
                    (if (let ((or-part (any-node? t1))) (if or-part or-part (none-node? t2)))
                        (begin '#f)
                        (if (green-edge? t1 t2)
                            (begin '#t)
                            (if (red-edge? t1 t2)
                                (begin '#t)
                                (begin
                                  (add-red-edge! t1 t2)
                                  ((letrec ((loop
                                             (lambda (blues)
                                               (if (null? blues)
                                                   '#t
                                                   (let ((current-edge (car blues)))
                                                     (let ((phi (operation current-edge)))
                                                       (if (has-op? phi t1)
                                                           (if (does-conform (res (sig phi t1)) (res (sig phi t2)))
                                                               (if (does-conform (arg (sig phi t2)) (arg (sig phi t1)))
                                                                   (loop (cdr blues))
                                                                   '#f)
                                                               '#f)
                                                           '#f)))))))
                                     loop)
                                   (blue-edges t2))))))))))
      (let ((result (does-conform t1 t2)))
        (for-each (if result greenify-red-edges! delete-red-edges!) (unbox nodes-with-red-edges-out))
        result))))
(define equivalent? (lambda (a b) (if (conforms? a b) (conforms? b a) '#f)))
(define classify
  (lambda (nodes)
    ((letrec ((node-loop
               (lambda (classes nodes)
                 (if (null? nodes)
                     (map
                      (lambda (class)
                        (sort
                         class
                         (lambda (node1 node2) (< (string-length (name node1)) (string-length (name node2))))))
                      classes)
                     (let ((this-node (car nodes)))
                       (letrec ((add-node
                                 (lambda (classes)
                                   (if (null? classes)
                                       (begin (list (list this-node)))
                                       (if (equivalent? this-node (caar classes))
                                           (begin (cons (cons this-node (car classes)) (cdr classes)))
                                           (begin (cons (car classes) (add-node (cdr classes)))))))))
                         (node-loop (add-node classes) (cdr nodes))))))))
       node-loop)
     '()
     nodes)))
(define find-canonical-representative
  (lambda (element classification)
    ((letrec ((loop
               (lambda (classes)
                 (if (null? classes)
                     (begin (error '"Can't classify" element))
                     (if (memq element (car classes)) (begin (car (car classes))) (begin (loop (cdr classes))))))))
       loop)
     classification)))
(define reduce
  (lambda (graph) (let ((classes (classify (graph-nodes graph)))) (canonicalize-graph graph classes))))
(define make-empty-table (lambda () (list 'TABLE)))
(define lookup
  (lambda (table x y)
    (let ((one (assq x (cdr table)))) (if one (let ((two (assq y (cdr one)))) (if two (cdr two) '#f)) '#f))))
(define insert!
  (lambda (table x y value)
    (letrec ((make-singleton-table (lambda (x y) (list (cons x y)))))
      (let ((one (assq x (cdr table))))
        (if one
            (set-cdr! one (cons (cons y value) (cdr one)))
            (set-cdr! table (cons (cons x (make-singleton-table y value)) (cdr table))))))))
(define blue-edge-operate
  (lambda (arg-fn res-fn graph op sig1 sig2)
    (make-blue-edge op (arg-fn graph (arg sig1) (arg sig2)) (res-fn graph (res sig1) (res sig2)))))




(define meet
  (lambda (graph node1 node2)
    (if (eq? node1 node2)
        (begin node1)
        (if (let ((or-part (any-node? node1))) (if or-part or-part (any-node? node2)))
            (begin any-node)
            (if (none-node? node1)
                (begin node2)
                (if (none-node? node2)
                    (begin node1)
                    (let ((c17352 (lookup (already-met graph) node1 node2)))
                      (if c17352
                          c17352
                          (if (conforms? node1 node2)
                              (begin node2)
                              (if (conforms? node2 node1)
                                  (begin node1)
                                  (begin
                                    (let ((result (make-node (string-append '"(" (name node1) '" ^ " (name node2) '")") '())))
                                      (add-graph-nodes! graph result)
                                      (insert! (already-met graph) node1 node2 result)
                                      (set-blue-edges!
                                       result
                                       (map
                                        (lambda (op) (blue-edge-operate join meet graph op (sig op node1) (sig op node2)))
                                        (intersect (map operation (blue-edges node1)) (map operation (blue-edges node2)))))
                                      result))))))))))))
(define join
  (lambda (graph node1 node2)
    (if (eq? node1 node2)
        (begin node1)
        (if (any-node? node1)
            (begin node2)
            (if (any-node? node2)
                (begin node1)
                (if (let ((or-part (none-node? node1))) (if or-part or-part (none-node? node2)))
                    (begin none-node)
                    (let ((c17353 (lookup (already-joined graph) node1 node2)))
                      (if c17353
                          c17353
                          (if (conforms? node1 node2)
                              (begin node1)
                              (if (conforms? node2 node1)
                                  (begin node2)
                                  (begin
                                    (let ((result (make-node (string-append '"(" (name node1) '" v " (name node2) '")") '())))
                                      (add-graph-nodes! graph result)
                                      (insert! (already-joined graph) node1 node2 result)
                                      (set-blue-edges!
                                       result
                                       (map
                                        (lambda (op) (blue-edge-operate meet join graph op (sig op node1) (sig op node2)))
                                        (union (map operation (blue-edges node1)) (map operation (blue-edges node2)))))
                                      result))))))))))))
(define make-lattice
  (lambda (g print?)
    (letrec ((step
              (lambda (g)
                (let ((copy (copy-graph g)))
                  (let ((nodes (graph-nodes copy)))
                    (for-each
                     (lambda (first)
                       (for-each (lambda (second) (meet copy first second) (join copy first second)) nodes))
                     nodes)
                    copy))))
             (loop
              (lambda (g count)
                (if print? (display count) (void))
                (let ((lattice (step g)))
                  (if print? (begin (display '" -> ") (display (length (graph-nodes lattice)))) (void))
                  (let ((new-g (reduce lattice)))
                    (let ((new-count (length (graph-nodes new-g))))
                      (if (= new-count count)
                          (begin (if print? (newline) (void)) new-g)
                          (begin
                            (if print? (begin (display '" -> ") (display new-count) (newline)) (void))
                            (loop new-g new-count)))))))))
      (let ((graph (make-graph (adjoin any-node (adjoin none-node (graph-nodes (clean-graph g)))))))
        (loop graph (length (graph-nodes graph)))))))
(define a (box '()))
(define b (box '()))
(define c (box '()))
(define d (box '()))
(define reset
  (lambda ()
    (set-box! a (make-node 'a '()))
    (set-box! b (make-node 'b '()))
    (set-blue-edges! (unbox a) (list (make-blue-edge 'phi any-node (unbox b))))
    (set-blue-edges! (unbox b) (list (make-blue-edge 'phi any-node (unbox a)) (make-blue-edge 'theta any-node (unbox b))))
    (set-box! c (make-node '"c" '()))
    (set-box! d (make-node '"d" '()))
    (set-blue-edges! (unbox c) (list (make-blue-edge 'theta any-node (unbox b))))
    (set-blue-edges! (unbox d) (list (make-blue-edge 'phi any-node (unbox c)) (make-blue-edge 'theta any-node (unbox d))))
    '(made a b c d)))
(define test
  (lambda () (reset) (map name (graph-nodes (make-lattice (make-graph (list (unbox a) (unbox b) (unbox c) (unbox d) any-node none-node)) '#t)))))
(define go
  (lambda ()
    (reset)
    (let ((result
           '("(((b v d) ^ a) v c)"
             "(c ^ d)"
             "(b v (a ^ d))"
             "((a v d) ^ b)"
             "(b v d)"
             "(b ^ (a v c))"
             "(a v (c ^ d))"
             "((b v d) ^ a)"
             "(c v (a v d))"
             "(a v c)"
             "(d v (b ^ (a v c)))"
             "(d ^ (a v c))"
             "((a ^ d) v c)"
             "((a ^ b) v d)"
             "(((a v d) ^ b) v (a ^ d))"
             "(b ^ d)"
             "(b v (a v d))"
             "(a ^ c)"
             "(b ^ (c v d))"
             "(a ^ b)"
             "(a v b)"
             "((a ^ d) ^ b)"
             "(a ^ d)"
             "(a v d)"
             "d"
             "(c v d)"
             "a"
             "b"
             "c"
             "any"
             "none")))
      (if (equal? (test) result) (display '" ok.") (display '" um."))
      (newline))))


(void ((letrec ((loop (lambda (n) (if (zero? n) 'done (begin (go) (loop (- n '1))))))) loop) '10)))