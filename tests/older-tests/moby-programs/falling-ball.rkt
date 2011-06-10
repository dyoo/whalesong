#lang s-exp "../../lang/wescheme.ss"

;; Simple falling ball example.  A red ball falls down the screen
;; until hitting the bottom.


(printf "falling-ball.rkt\n")

(define-struct world (radius y))


;; The dimensions of the screen:
(define WIDTH 320)
(define HEIGHT 480)

;; The radius of the red circle.
(define RADIUS 15)

;; The world is the distance from the top of the screen.
(define INITIAL-WORLD (make-world RADIUS 0))

;; tick: world -> world
;; Moves the ball down.
(define (tick w)
  (make-world RADIUS (+ (world-y w) 5)))


;; hits-floor?: world -> boolean
;; Returns true when the distance reaches the screen height.
(define (hits-floor? w)
  (>= (world-y w) HEIGHT))

;; We have some simple test cases.
(check-expect (hits-floor? (make-world RADIUS 0)) false)
(check-expect (hits-floor? (make-world RADIUS HEIGHT)) true)

;; render: world -> scene
;; Produces a scene with the circle at a height described by the world.
(define (render w)
  (place-image (circle RADIUS "solid" "red") (/ WIDTH 2) (world-y w)
               (empty-scene WIDTH HEIGHT)))

;; Start up a big bang, 15 frames a second.
(check-expect (big-bang INITIAL-WORLD
			   (on-tick tick 1/15)
			   (to-draw render)
			   (stop-when hits-floor?))
	      (make-world 15 480))
