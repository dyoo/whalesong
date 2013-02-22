#lang whalesong

(require whalesong/web-world
         whalesong/resource)

(define-resource index.html)


;; The world consists of the doors, and at what point in the
;; game we're in.
(define-struct door (opened? has-treasure?))
(define-struct world (stage door1 door2 door3))


;; open-door: door -> door
(define (open-door door)
  (make-door #t (door-has-treasure? door)))


;; new-world: -> world
;; Creates a world where all the doors are closed, and
;; behind one of the doors is the treasure.
(define (new-world)
  (define with-treasure (random 3))
  (make-world (make-door #f (= with-treasure 0))
              (make-door #f (= with-treasure 1))
              (make-door #f (= with-treasure 2))))


