#lang whalesong

;; Eli's puzzle
;;
;;    http://lists.racket-lang.org/users/archive/2011-July/046849.html

(require whalesong/world)
          
(define-struct world (seq output))

;; streak: (listof number) -> number
(define (streak lst)
  (let ([elt (car lst)])
    (let loop ([lst lst])
      (cond
        [(null? lst) 0]
        [(= (car lst)
            elt)
         (add1 (loop (cdr lst)))]
        [else
         0]))))
            
(define (my-drop lst n)
  (cond
    [(= n 0)
     lst]
    [else
     (my-drop (cdr lst) (sub1 n))]))

(define (tick w)
  (let* ([streak-length (streak (world-seq w))]
         [next-self-describing-chunk
          (list streak-length (car (world-seq w)))])
    (make-world (append (my-drop (world-seq w) streak-length) 
                        next-self-describing-chunk)
                (append (world-output w)
                        (list streak-length
                              (car (world-seq w)))))))

(define (draw w)
  (world-output w))

(big-bang (make-world '(1) '())
          (on-tick tick 1)
          (to-draw draw))
