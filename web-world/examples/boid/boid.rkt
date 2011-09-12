#lang planet dyoo/whalesong

(require (planet dyoo/whalesong/js)
         (planet dyoo/whalesong/web-world))
         

;; Boid flocking behavior.
;;
;; http://www.vergenet.net/~conrad/boids/pseudocode.html
;;
;; 

;; A Boid has a velocity and position vector, as well as a color
(define-struct boid (velocity position color))


(define width (viewport-width))
(define height (viewport-height))


;; A vec represents a vector in 2d space.
(define-struct vec (x y))

;; A mass-data represents the total velocity and position
;; of a collection of boids.
(define-struct mass-data (center-velocity center-position))



;; (: vec+ (vec vec -> vec))
;; Adds two vecs together.
(define (vec+ v1 v2)
  (make-vec (+ (vec-x v1)
               (vec-x v2))
            (+ (vec-y v1)
               (vec-y v2))))

;; Let's test vec+.
(check-expect (vec+ (make-vec 3 4)
                    (make-vec 5 6))
              (make-vec  8 10))

(check-expect (vec+ (make-vec 1024 2)
                    (make-vec -1024 -1))
              (make-vec 0 1))



;; (: vec- (vec vec -> vec))
;; Subtracts one vec from another.
(define (vec- v1 v2)
  (make-vec (- (vec-x v1)
               (vec-x v2))
            (- (vec-y v1)
               (vec-y v2))))


(check-expect (vec- (make-vec 3 4)
                    (make-vec 5 6))
              (make-vec -2 -2))

(check-expect (vec- (make-vec 1024 2)
                    (make-vec -1024 -1))
              (make-vec 2048 3))


;; (: vec-scale (vec Real -> vec))
;; Scales a vector by a certain scalar.
(define (vec-scale v n)
  (make-vec (* (vec-x v) n)
            (* (vec-y v) n)))

(check-expect (vec-scale (make-vec 3 4) 7)
              (make-vec 21 28))

(check-expect (vec-scale (make-vec 1 2) 1/2)
              (make-vec 1/2 1))



;; (: square (Real -> Real))
(define (sqr x) (* x x))
(check-expect (sqr 10) 100)
(check-expect (sqr -2) 4)



;; (: vec-distance^2 (vec vec -> Real))
;; Produces the square of the distance between two vecs.
(define (vec-distance^2 v1 v2)
  (+ (sqr (- (vec-x v1) (vec-x v2)))
     (sqr (- (vec-y v1) (vec-y v2)))))

(check-expect (vec-distance^2 (make-vec 0 0)
                              (make-vec 3 4))
              25)
(check-expect (vec-distance^2 (make-vec 1924 2329)
                              (make-vec 1924 2328))
              1)



;; (: vec-center ((Listof vec) -> vec))
;; Given a list of vecs, produces the center, assuming each vec has the same mass.
(define (vec-center vecs)
  (cond
    [(empty? vecs)
     ;; We'll raise a runtime error if we ever try to take the center of the
     ;; empty set.
     (error 'vec-center "trying to take the center of an empty collection of vecs")]
    [else
     (vec-scale (foldl vec+ (first vecs) (rest vecs))
                (/ 1 (length vecs)))]))

(check-expect (vec-center (list (make-vec 1 1)
                                (make-vec 2 7)
                                (make-vec 1 3)))
              (make-vec 4/3 11/3))

(check-expect (vec-center (list (make-vec 5 0)
                                (make-vec 4 2)))
              (make-vec 9/2 1))
                          

(define (vec-mag v)
  (sqrt (+ (sqr (vec-x v))
           (sqr (vec-y v)))))


;; (: vec-normalize (vec -> vec))
;; Produces a vector of length 1 going in the same direction.
(define (vec-normalize v)
  (let ([n (vec-mag v)])
    (make-vec (/ (vec-x v) n) (/ (vec-y v) n))))


;; collect-mass-data: (Listof boid) -> mass-data
(define (collect-mass-data boids)
  (let ([the-center-velocity
	 (vec-center (map boid-velocity boids))]
	[the-center-position 
	  (vec-center (map boid-position boids))])
    (make-mass-data the-center-velocity the-center-position)))





;; (: rule-1 (boid (Listof boid) -> vec))
;; Boids try to fly toward the center of mass of neighboring boids.
;; Produce a vector pointing to the center of the boid cloud.
(define (rule-1 boid boids mass-data)
  (vec- (mass-data-center-position mass-data)
	(boid-position boid)))



;; (: rule-2 (boid (Listof boid) -> vec))
;; Boids try to keep a small distance away from other boids.
(define (rule-2 boid boids)
  (foldl (lambda (neighbor the-center)
	    (cond
             [(eq? boid neighbor)
              the-center]
	     [(too-close? boid neighbor)
	      (vec- the-center (vec- (boid-position neighbor)
				     (boid-position boid)))]
	     [else
	      the-center]))
         (make-vec 0 0)
         boids))


;; (: too-close? (boid boid -> Boolean))
;; Produces true if the two boids are too close for comfort.
(define (too-close? b1 b2)
  (let ([threshold (sqr 15)])
    (< (vec-distance^2 (boid-position b1)
                       (boid-position b2))
       threshold)))



;; (: rule-3 (boid (Listof boid) -> vec))
;; Boids try to match velocity with near boids.
(define (rule-3 boid boids mass-data)
  (vec- (mass-data-center-velocity mass-data)
	(boid-velocity boid)))



;; (: move-boid (boid (Listof boid) -> boid))
;; Moves a boid according to the rules.
(define (move-boid b boids mass-data)
  (let ([rule-1-scale-factor 0.05]
	[rule-2-scale-factor 0.1]
	[rule-3-scale-factor 1/8]
	[rule-4-scale-factor 0.8])
    (let ([new-velocity
	   (vec+ (boid-velocity b) 
		 (vec+ (vec-scale (rule-1 b boids mass-data) 
				  rule-1-scale-factor)
		       (vec+ (vec-scale (rule-2 b boids)
					rule-2-scale-factor)
			     (vec+ (vec-scale (rule-3 b boids mass-data)
					      rule-3-scale-factor)
				   (vec-scale (rule-4 b)
					      rule-4-scale-factor)))))]
	  [new-position
	   (vec+ (boid-position b)
		 (boid-velocity b))])
    (make-boid new-velocity
	       new-position
	       (boid-color b)))))


;; Boids should avoid going out of bounds.  If they stray out of bounds,
;; nudge them toward the center of the screen.
(define (rule-4 boid)
  (cond
   [(out-of-bounds? (boid-position boid))
    (vec-normalize
     (vec- (make-vec (random width) (random height))
	   (boid-position boid)))]
   [else
    (make-vec 0 0)]))


(define (out-of-bounds? v)
  (or (not (<= 100 (vec-x v) 540))
      (not (<= 100 (vec-y v) 380))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (: tick ((Listof boid) dom -> (Listof boid)))
(define (tick boids dom)
  (for/list ([b boids])
     (let ([mass-data (collect-mass-data (boid-neighborhood b boids 40))])
       (cap-boid-velocity 
	(move-boid b boids mass-data)
	15))))



(define (boid-neighborhood b boids n)
  (filter (lambda (b2)
	    (< (vec-mag (vec- (boid-position b)
			      (boid-position b2)))
	       n))
	  boids))


(define (cap-boid-velocity b mag)
  (make-boid (vec-cap (boid-vel b) mag)
             (boid-pos b)
             (boid-color b)))


(define (vec-cap v n)
  (cond
   [(> (vec-mag v) n)
    (vec-scale (vec-normalize v) n)]
   [else
    v]))
   

(define (slow-down-boids boids)
  (map (lambda (b)
         (make-boid (vec-scale (boid-vel b) 0.9)
                    (boid-pos b)
                    (boid-color b)))
       boids))

(define (speed-up-boids boids)
  (map (lambda (b)
         (make-boid (vec-scale (boid-vel b) 1.1)
                    (boid-pos b)
                    (boid-color b)))
       boids))


;; draw: (listof boid) -> scene
#;(define (draw boids)
  (for/fold ([scene (place-image (rectangle width height 'solid 'black)
				 320 240 
				 (empty-scene width height))])
            ([b boids])
    (place-image (circle 3 'solid (boid-color b))
                 (vec-x (boid-position b))
                 (vec-y (boid-position b))
                 scene)))



;; make-random-boid: -> boid
;; Makes a random boid that starts near the upper left corner,
;; drifting downward.
(define (make-random-boid)
  (make-boid (make-vec (random 10)
		       (random 10))
	     (make-vec (+ 20 (random 600))
		       (+ 20 (random 300)))
	     (make-color (random 255)
			 (random 255)
			 (random 255))))


(define (new-population)
  (build-list 25 (lambda (i) (make-random-boid))))


;; visualize: -> void
;; Animates a scene of the boids flying around.
(define (visualize)
  (big-bang (new-population)
            (on-tick tick)
            #;(to-draw draw)
            ))


(visualize)