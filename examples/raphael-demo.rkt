#lang planet dyoo/whalesong
(require (planet dyoo/whalesong/resource)
         (planet dyoo/whalesong/web-world)
         (planet dyoo/whalesong/js))

; This is a small demonstration of the Javascript
; graphics library Raphael from http://raphaeljs.com/ .


; The example below the bindings draws a Lissajous curve.


;;;
;;; Whalesong binding of Raphael
(load-script "http://yandex.st/raphael/1.5.2/raphael.js")
;;;

(define paper #f)

(define (raphael-init id width height)
  (unless paper    
    (set! paper
          (js-eval 
           (format "Raphael(~s, ~a, ~a)"
                   id width height)))))

(define (raphael-rect x1 y1 x2 y2 . more)
  (case (length more)
    [(0) (call-method paper "rect" x1 y1 x2 y2)]
    [(1) (call-method paper "rect" x1 y1 x2 y2 (car more))]
    [else (error 'raphael-rect "too many arguments")]))

(define (raphael-circle x y r)
  (call-method paper "circle" x y r))

(define (raphael-ellipse x y rx ry)
  (call-method paper "ellipse" x y rx ry))

(define (raphael-image src-uri x y w h)
  (call-method paper "image" x y w h))

(define (raphael-set)
  (call-method paper "set"))

(define (raphael-push set . elems)
  (for-each (λ (e) (call-method paper "push" e)) elems))

(define (raphael-text x y str)
  (call-method paper "text" x y str))

(define (raphael-path str) ; str in SVG path string format
  (call-method paper "path" str))

(define (raphael-line x1 y1 x2 y2)
  (raphael-path (format "M~a ~aL~a ~a" x1 y1 x2 y2)))

(define (raphael-clear) 
  (call-method paper "clear"))

(define (raphael-node c)
  (call-method c "node"))

(define (raphael-hide c)
  (call-method c "hide"))

(define (raphael-show c)
  (call-method c "show"))


(define (raphael-remove c)
  (call-method c "remove"))

(define (raphael-rotate c deg . more)
  (case (length more)
    [(0) (call-method c "rotate" deg)]
    [(1) (let ([is-absolute (car more)])
           (call-method c "rotate" deg is-absolute))]
    [(2) (let ([cx (car more)]
               [cy (cadr more)])
           ; (cx,xy) is the center
           (call-method c "rotate" deg cx cy))]))

(define (raphael-translate c dx dy)
  (call-method c "translate" dx dy))

(define (raphael-scale c xtimes ytimes . more)
  (case (length more)
    [(0) (call-method c "scale" xtimes ytimes)]
    [(2) (let ([centerx (car more)]
               [centery (cadr more)])
           (call-method c "scale" xtimes ytimes centerx centery))]
    [else (error 'raphael-scale "wrong number of arguments")]))

(define (raphael-attr c . more)
  (case (length more)
    [(2) (let* ([attribute-name (car more)]
                [attribute-value (cadr more)]
                [attribute-value (if (number? attribute-value)
                                     (number->string attribute-value)
                                     attribute-value)])
           (call-method c "attr" attribute-name attribute-value))]
    [(1) (cond
           [(string? (car more))
            ; return current attribute values
            (call-method c "attr" (car more))]
           [(list? (car more))
            (for-each (λ (p) (let ([name (car p)]
                                   [val (cadr p)])
                               (raphael-attr c name val)))
                      (car more))]
           [else (error 'raphael-attr "wrong argument type: string or list-of-two-element-lists expected")])]
    [else (error 'raphael-attr "expected 2 or 3 arguments")]))

;;;
;;; Demonstration of the Raphael bindings
;;;

(define WIDTH 400)
(define HEIGHT 400)

(define XMIN -1.0)
(define XMAX  1.0)
(define YMIN -1.0)
(define YMAX  1.0)

(define FRAMES-PER-SECOND 30)

(define SECONDS-PER-ORBIT 20)

(define (count->time c)
  (let ([seconds (/ (remainder c (* SECONDS-PER-ORBIT FRAMES-PER-SECOND)) FRAMES-PER-SECOND)])
    (* 2 pi (/ seconds SECONDS-PER-ORBIT))))

(define screen-x
  (let ([dx (- XMAX XMIN)])
    (lambda (x)
      (let* ([x (max x XMIN)]
             [x (min x XMAX)])
        (/ (* (- x XMIN) WIDTH) dx)))))

(define screen-y
  (let ([dy (- YMAX YMIN)])
    (lambda (y)
      (let* ([y (max y YMIN)]
             [y (min y XMAX)])
        (/ (* (- (- y) YMIN) HEIGHT) dy)))))

(define-struct world (count dot))

;;; See http://en.wikipedia.org/wiki/Lissajous_curve for
;;; other values of a and b to try.
(define a 5)
(define b 4)

(define (x t)
  (* 0.8 (sin (* a t))))

(define (y t)
  (* 0.8 (sin (* b t))))

;; tick: world view -> world
(define (tick world view)
  (let* ([c (world-count world)]
         [d (world-dot world)]
         [t (count->time c)]
         [t2 (count->time (sub1 c))])
    (cond
      [(zero? c)     
       (raphael-init "raphael_area" WIDTH HEIGHT)       
       (make-world 1 (raphael-circle (screen-x (x t)) (screen-y (y t)) 3))]
      [else
       (raphael-remove d)
       (raphael-line (screen-x (x t2)) (screen-y (y t2)) (screen-x (x t)) (screen-y (y t)))
       (make-world (add1 c) (raphael-circle (screen-x (x t)) (screen-y (y t)) 3))])))

;; draw: world view -> view
(define (draw world view)
  (let ([c (world-count world)]
        [r (world-dot world)])
    (cond
      [(zero? c)  'skip]
      [else       'skip])
    view))

(big-bang 
 (make-world 0 #f)
 (initial-view (xexp->dom '(html (head) (body (div (@ (id "raphael_area")))))))
 (on-tick tick (/ 1 FRAMES-PER-SECOND))
 (to-draw draw))
