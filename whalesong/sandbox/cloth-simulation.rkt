#lang whalesong

;; A rewrite of the cloth simulation application from the Codea project
;; into world form.

;; 121 particles,
;; 100 constraints.
;;
;; There are a few things about the program that aren't stated up front, but are implied
;; by the original code:
;;
;; 1.  The upper edge is fixed.  The use of the orx, ory attributes is somewhat redundant.
;; 2.  The particles

(define-struct constraint (p1 ;; particle
                           p2 ;; particle
                           d ;; number
                           ))

(define-struct particle (x ;; number
                         y ;; number 
                         ox ;; number -- the very previous x position 
                         oy ;; number -- the very previous y position
                         fx ;; number -- force in the x direction
                         fy ;; number -- force in the y direction
                         orx ;; number -- the original x position
                         ory ;; number -- the original y position 
                         ))

;; The world consists of a list of particles and constraints.
(define-struct world (particles ;; (listof particle)
                      constraints ;; (listof constraint)
                      ))
                         
                         
                         
(define (new-particle x y)
  (make-particle x y x y 0 0 x y))
   


;; apply-global-forces: (listof parts) number number -> (listof parts)
;; Globally apply the forces on all the parts.
(define (apply-global-forces parts fx fy)
  (map (lambda (part) (apply-force part fx fy)) 
       parts))


;; apply-force: particle number number -> particle
;; Update a particle with the appropriate force.
(define (apply-force part fx fy)
  (make-particle (particle-x part)
                 (particle-y part)
                 (particle-ox part)
                 (particle-oy part)
                 fx
                 fy
                 (particle-orx part)
                 (particle-ory part)))


;; new-constraint: point point -> constraint
;; Construct a new initial constraint from points p1 and p2.
(define (new-constraint p1 p2)
  (make-constraint p1 p2 (partdist p1 p2)))



;; partdist: point point -> number
;; Computes the distance between two points.
(define (partdist p1 p2)
  (sqrt (sqr (- (point-x p1)
                (point-x p2)))
        (sqr (- (point-y p1)
                (point-y p2)))))
