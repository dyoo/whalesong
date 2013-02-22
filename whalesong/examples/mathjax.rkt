#lang whalesong
(require whalesong/web-world)

;;; This demonstrates how to use MathJax to write equations.
;;; Use  --include-script mathjax-script.js  to include the MathJax.
;;; racket whalesong.rkt build --include-script mathjax-script.js mathjax.rkt

;; tick: world view -> world
(define (tick n view)
  (add1 n))
 
;; draw: world view -> view
(define (draw n view)
  (->view
   (xexp->dom `(p "This equation has no integer solutions: "
                  ,(let ([n (number->string n)])
                     (format "$$ x^~a + y^~a = z^~a $$" n n n))))))
 
(big-bang 3 
          (initial-view (xexp->dom '(html (head) (body))))
          (on-tick tick 5)
          (to-draw draw))
