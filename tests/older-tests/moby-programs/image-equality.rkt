#lang s-exp "../../lang/wescheme.rkt"
(require "../../image/image.rkt")

;; Tests on images.
;;
;; An image can be a circle, star, ns:rectangle, rectangle, triangle, ellipse, line, text, place-image, overlay, underlay

(define a-circle (circle 20 'solid 'green))
(define a-star (star 5 20 30 'solid 'blue))
(define a-nw-rect (nw:rectangle 20 30 'solid 'turquoise))
(define a-rect (rectangle 50 60 'outline 'black))
(define a-triangle (triangle 50 'solid 'magenta))
(define a-line (line 30 40 'red))
(define a-text (text "hello world" 20 "black"))


;; Let's show these at the toplevel to make sure the drawing is working
;; ok
(printf "should be a circle:") a-circle
(printf "should be a star:") a-star
(printf "should be a nw:rectangle:") a-nw-rect
(printf "should be a rectangle:") a-rect
(printf "should be a triangle:") a-triangle
(printf "should be a line:") a-line
(printf "should be a text:") a-text


;; check-fail-contract: (-> void) -> void
;; Make sure we fail with a contract error.
(define (check-fail-contract thunk)
  (with-handlers ([exn:fail:contract? void])
    (thunk)
    (raise (format "failure expected: ~s" thunk))))



;; Now do image comparisons
(printf "running image comparison tests\n")

;; circles
(check-expect (equal? (circle 20 'solid 'green)
                      (circle 20 'solid 'green))
              true)
(check-expect (equal? (circle 20 'solid 'green)
                      (circle 21 'solid 'green))
              false)
(check-expect (equal? (circle 20 'solid 'green)
                      (circle 20 'solid 'blue))
              false)
(check-fail-contract (lambda () (circle 20 "foobar" "green")))
(check-fail-contract (lambda () (circle 20 "outline" "not-a-color")))
(check-fail-contract (lambda () (circle 20 'green 'outline)))
(check-fail-contract (lambda () (circle 'green 'outline 20)))
(check-fail-contract (lambda () (circle)))
(check-fail-contract (lambda () (circle 20)))
(check-fail-contract (lambda () (circle 20 'outline)))
(check-fail-contract (lambda () (circle 20 'outline "black" "too-many-args")))


;; star
(check-expect (equal? (star 20 10 60 'solid 'purple)
                      (star 20 10 60 'solid 'purple))
              true)
(check-expect (equal? (star 20 9 60 'solid 'purple)
                      (star 20 10 60 'solid 'purple))
              false)
(check-expect (equal? (star 20 10 60 'solid 'purple)
                      (star 20 10 49 'solid 'purple))
              false)
(check-expect (equal? (star 20 10 60 'solid 'purple)
                      (star 20 10 60 'outline 'purple))
              false)
(check-expect (equal? (star 20 10 60 'solid 'purple)
                      (star 20 10 60 'solid 'magenta))
              false)
(check-fail-contract (lambda () (star 20 10 60 "foobar" 'green)))
(check-fail-contract (lambda () (star 20 10 60 "outline" 'not-a-color)))
(check-fail-contract (lambda () (star 20 10 60 "green" 'outline)))
(check-fail-contract (lambda () (star 10 60 "green" 'outline 20)))
(check-fail-contract (lambda () (star)))
(check-fail-contract (lambda () (star 10)))
(check-fail-contract (lambda () (star 10 60)))
(check-fail-contract (lambda () (star 10 60 50)))
(check-fail-contract (lambda () (star 10 60 50 'outline)))
(check-fail-contract (lambda () (star 10 60 50 'outline 'green 'too-many-args)))
 

;; nw:rect
(check-expect (equal? (nw:rectangle 10 20 'solid 'black)
                      (nw:rectangle 10 20 'solid 'black))
              true)
(check-expect (equal? (nw:rectangle 20 10 'solid 'black)
                      (nw:rectangle 10 20 'solid 'black))
              false)
(check-expect (equal? (nw:rectangle 10 10 'solid 'black)
                      (nw:rectangle 10 20 'solid 'black))
              false)
(check-expect (equal? (nw:rectangle 10 20 'solid 'black)
                      (nw:rectangle 10 20 'outline 'black))
              false)
(check-expect (equal? (nw:rectangle 10 20 'solid 'black)
                      (nw:rectangle 10 20 'outline 'white))
              false)
(check-fail-contract (lambda () (nw:rectangle 10 20 "foobar" 'green)))
(check-fail-contract (lambda () (nw:rectangle 10 20 "outline" 'not-a-color)))
(check-fail-contract (lambda () (nw:rectangle 10 20 'green 'outline)))
(check-fail-contract (lambda () (nw:rectangle 20 'green 'outline 10)))
(check-fail-contract (lambda () (nw:rectangle)))
(check-fail-contract (lambda () (nw:rectangle 10)))
(check-fail-contract (lambda () (nw:rectangle 10 20)))
(check-fail-contract (lambda () (nw:rectangle 10 20 'solid)))
(check-fail-contract 
 (lambda () (nw:rectangle 10 20 'solid 'green 'too-many-args)))


;; rect
(check-expect (equal? (rectangle 10 20 'solid 'black)
                      (rectangle 10 20 'solid 'black))
              true)
(check-expect (equal? (rectangle 20 10 'solid 'black)
                      (rectangle 10 20 'solid 'black))
              false)
(check-expect (equal? (rectangle 10 10 'solid 'black)
                      (rectangle 10 20 'solid 'black))
              false)
(check-expect (equal? (rectangle 10 20 'solid 'black)
                      (rectangle 10 20 'outline 'black))
              false)
(check-expect (equal? (rectangle 10 20 'solid 'black)
                      (rectangle 10 20 'outline 'white))
              false)
(check-fail-contract (lambda () (rectangle 10 20 "foobar" 'green)))
(check-fail-contract (lambda () (rectangle 10 20 "outline" 'not-a-color)))
(check-fail-contract (lambda () (rectangle 10 20 'green 'outline)))
(check-fail-contract (lambda () (rectangle 20 'green 'outline 10)))
(check-fail-contract (lambda () (rectangle)))
(check-fail-contract (lambda () (rectangle 10)))
(check-fail-contract (lambda () (rectangle 10 20)))
(check-fail-contract (lambda () (rectangle 10 20 'solid)))
(check-fail-contract (lambda () (rectangle 10 20 'solid 'green 'too-many-args)))
 
 

;; triangle
(check-expect (equal? (triangle 10 'solid 'green)
                      (triangle 10 'solid 'green))
              true)

(check-expect (equal? (triangle 10 'solid 'green)
                      (triangle 9 'solid 'green))
              false)
(check-expect (equal? (triangle 10 'solid 'green)
                      (triangle 10 'outline 'green))
              false)
(check-expect (equal? (triangle 10 'solid 'green)
                      (triangle 10 'solid 'olive))
              false)
(check-fail-contract (lambda () (triangle 10 'foobar 'green)))
(check-fail-contract (lambda () (triangle 10 'outline 'not-a-color)))
(check-fail-contract (lambda () (triangle 10 'green 'outline)))
(check-fail-contract (lambda () (triangle 'green 'outline 10)))
(check-fail-contract (lambda () (triangle)))
(check-fail-contract (lambda () (triangle 'outline)))
(check-fail-contract (lambda () (triangle 10)))
(check-fail-contract (lambda () (triangle 10 'outline)))
(check-fail-contract (lambda () (triangle 10 'outline 'green 'too-many-args)))


;; line
(check-expect (equal? (line 10 20 'blue)
                      (line 10 20 'blue))
              true)
(check-expect (equal? (line 10 20 'blue)
                      (line 20 10 'blue))
              false)
(check-fail-contract (lambda () (line 10 20 'not-a-color)))
(check-fail-contract (lambda () (line 'not-a-color 20 10)))
(check-fail-contract (lambda () (line)))
(check-fail-contract (lambda () (line 10)))
(check-fail-contract (lambda () (line 10 20)))
(check-fail-contract (lambda () (line 10 20 "black" "too-many-args")))


;; text
(check-expect (equal? (text "hello" 20 'yellow)
                      (text "hello" 20 'yellow))
              true)

(check-expect (equal? (text "hello" 20 'yellow)
                      (text "hi" 20 'yellow))
              false)
(check-fail-contract (lambda () (text "hello")))
(check-fail-contract (lambda () (text "hello" 20)))
(check-fail-contract (lambda () (text "hello" 20 'yellow 'too-many-args)))
(check-fail-contract (lambda () (text 'hi 20 'yellow)))
(check-fail-contract (lambda () (text "hello" 'yellow 20)))


              

;; empty scenes
(check-expect (empty-scene 10 20)
              (empty-scene 10 20))
(check-expect (equal? (empty-scene 10 20)
                      (empty-scene 11 20))
              false)
(check-fail-contract (lambda () (empty-scene 'one 'two)))
(check-fail-contract (lambda () (empty-scene 10 20 30)))
(check-fail-contract (lambda () (empty-scene 10)))


;; place images
(check-fail-contract (lambda () (place-image)))
(check-fail-contract (lambda () (place-image 10)))
(check-fail-contract (lambda () (place-image (circle 20 'solid 'green))))
(check-fail-contract (lambda () (place-image (circle 20 'solid 'green) 10)))
(check-fail-contract (lambda () (place-image (circle 20 'solid 'green) 10 20)))
(check-fail-contract 
 (lambda () 
   (place-image (circle 20 'solid 'green) 10 20 (empty-scene 3 4) 
                "too-many-args")))
(check-fail-contract 
 (lambda () 
   (place-image 10 20 (circle 20 'solid 'green) (empty-scene 3 4))))


(check-expect (place-image (circle 10 'solid 'green)
                           50
                           50
                           (empty-scene 100 100))
              (place-image (circle 10 'solid 'green)
                           50
                           50
                           (empty-scene 100 100)))
                         
(check-expect (equal? (place-image (circle 10 'solid 'green)
                                   50
                                   50
                                   (empty-scene 100 100))
                      (place-image (circle 10 'solid 'green)
                                   50
                                   50
                                   (empty-scene 100 100)))
              true)

(check-expect (equal? (place-image (circle 9 'solid 'green)
                                   50
                                   50
                                   (empty-scene 100 100))
                      (place-image (circle 10 'solid 'green)
                                   50
                                   50
                                   (empty-scene 100 100)))
              false)


(check-expect (equal? (place-image (circle 10 'solid 'green)
                                   50
                                   50
                                   (empty-scene 100 100))
                      (place-image (circle 10 'solid 'green)
                                   40
                                   50
                                   (empty-scene 100 100)))
              false)

(check-expect (equal? (place-image (circle 10 'solid 'green)
                                   50
                                   50
                                   (empty-scene 100 100))
                      (place-image (circle 10 'solid 'green)
                                   50
                                   40
                                   (empty-scene 100 100)))
              false)

(check-expect (equal? (place-image (circle 10 'solid 'green)
                                   50
                                   50
                                   (empty-scene 100 100))
                      (place-image (circle 10 'solid 'green)
                                   50
                                   50
                                   (empty-scene 100 99)))
              false)



;; overlay
(check-fail-contract (lambda () (overlay)))

(check-expect (overlay (rectangle 10 20 'solid 'blue)
                       (circle 20 'solid 'green))
              (overlay (rectangle 10 20 'solid 'blue)
                       (circle 20 'solid 'green)))
(check-expect (equal? (overlay (rectangle 10 20 'solid 'blue)
                               (circle 20 'solid 'green))
                      (overlay (circle 20 'solid 'green)
                               (rectangle 10 20 'solid 'blue)))
              false)



;; underlay
(check-fail-contract (lambda () (underlay)))
(check-expect (underlay (rectangle 10 20 'solid 'blue)
                        (circle 20 'solid 'green))
              (underlay (rectangle 10 20 'solid 'blue)
                        (circle 20 'solid 'green)))
(check-expect (equal? (underlay (rectangle 10 20 'solid 'blue)
                                (circle 20 'solid 'green))
                      (underlay (circle 20 'solid 'green)
                                (rectangle 10 20 'solid 'blue)))
              false)




(printf "ran image comparison tests\n")
